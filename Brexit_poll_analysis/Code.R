### Analysing Brexit voting polls and the final election result of leaving the EU.

library(tidyverse)
library(dslabs)
options(digits=3)
data(brexit_polls) # Download Brexit polling data

p <- 0.481    # actual proportion voting "Remain"
d <- 2*p-1    # actual spread

### Expected value and standard error of poll

# The final proportion of voters choosing "Remain" was  p=0.481. 
# Consider a poll with a sample of N=1500  voters.
n <- 1500
(ev_total_remain <- n*p)  # Expected amount of 'remain' voters
(se_total_remain <- sqrt(n*p*(1-p)))     # Standard error of amount of 'remain' voters

(ev_prop <- p) # Expected proportion 'remain' voters
(se_prop <- sqrt(p*(1-p)*(1/n))) # Standard error of the proportion

(ev_spread <- d) # Expected value of the spread
(se_spread <- 2*se_prop) # Standard error of the spread

### Actual Brexit poll estimates
brexit_polls <- brexit_polls %>% 
  mutate(x_hat = (spread+1)/2)  # estimate proportion of 'remain' voters based on the spread   

brexit_polls %>% summarize(mean_spread = mean(spread), sd_spread = sd(spread), 
                           avg_x_hat= mean(x_hat), sd_x_hat=sd(x_hat))

### Confidence interval of polls and pollster bias
brexit_polls <- brexit_polls %>% mutate(lower_ci_x=qnorm(0.025, x_hat, sqrt(x_hat*(1-x_hat)*(1/samplesize))), 
                                       upper_ci_x=qnorm(0.975, x_hat, sqrt(x_hat*(1-x_hat)*(1/samplesize))),
                                       hit_x= (p >= lower_ci_x & p < upper_ci_x),
                                       lower_ci_spread=qnorm(0.025,(2*x_hat)-1, 2*(sqrt(x_hat*(1-x_hat)*(1/samplesize)))),
                                       upper_ci_spread=qnorm(0.975,(2*x_hat)-1, 2*(sqrt(x_hat*(1-x_hat)*(1/samplesize)))),
                                       hit_spread=(d >= lower_ci_spread & d < upper_ci_spread))


june_polls <- brexit_polls %>% filter(enddate >= "2016-06-01")
mean(0 >= june_polls$lower_ci_spread & 0 < ju0ne_polls$upper_ci_spread) # % of conf_intervals that include 0
mean(june_polls$lower_ci_spread > 0) # % of conf_intervals that predict 'remain' ('leave' is not in conf_interval)
mean(june_polls$hit_spread) # % of conf_intervals that include the actual spread


june_polls %>% group_by(pollster) %>%
  summarize(prop_hits=mean(hit_spread), nr_polls=n()) %>%
  arrange(desc(prop_hits)) # Analysis to detect a consistent bias among pollsters
# The proportion of conf_intervals that covers the actual spread is much lower than 95% -> evidence for pollster bias

### Bias by poll type: phone or online 
june_polls %>% ggplot(aes(x=poll_type, y=spread)) 
  + geom_boxplot() # Does polling online or by telephone influence the result?
# Online polls show a greater IQR for the spread and tend more towards 'leave' than telephone polls. -> The poll type introduces bias

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2,
            se_hat = 2*(sqrt(p_hat*(1-p_hat)*(1/N))),
            ci_lower=qnorm(0.025, spread, se_hat),
            ci_upper=qnorm(0.975, spread, se_hat))
# Since both poll types include 0, we cannot predict the outcome of the poll with the 95% conf_intervals

### Chi-squared test
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

(two_by_two <- brexit_hit %>% count(poll_type, hit) %>% spread(hit, n))
chisq.test(two_by_two[-1]) # The test shows a significant difference in the hit rate of phone and online polls


(prop_hits <- cbind(two_by_two[1], rowSums(two_by_two[-1]), two_by_two[3]/rowSums(two_by_two[-1]),two_by_two[2]/rowSums(two_by_two[-1])))
names(prop_hits) <- c("poll_type","total","prop_true","prop_false")
(prop_hits <- prop_hits %>% mutate(odds_true_vs_false=(prop_true/total)/(prop_false/total)))
prop_hits$odds[1]/prop_hits$odds[2] # Online polls are over 4 times as likely to hit, than telephone polls

### Bias over time
brexit_polls %>% ggplot(aes(x=enddate, y=spread, color=poll_type)) +
  geom_smooth(method="loess", span=0.4) +
  geom_point() +
  geom_hline(yintercept=d)

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))
brexit_long %>% ggplot(aes(x=enddate, y=proportion, color=vote)) +
    geom_smooth(method="loess", span=0.3)
