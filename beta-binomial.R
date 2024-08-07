library(bayesrules)
library(bayestestR)
library(tidyverse)

# beta_binomial distribution for each group
# control
summarize_beta_binomial(alpha = 1, beta = 1, y = 32, n = 43) #0.73

# loss framing
summarize_beta_binomial(alpha = 1, beta = 1, y = 38, n = 50)#0.75

# young male
summarize_beta_binomial(alpha = 1, beta = 1, y = 29, n = 31)#0.91
# old male
summarize_beta_binomial(alpha = 1, beta = 1, y = 63, n = 110)#0.57
# young female
summarize_beta_binomial(alpha = 1, beta = 1, y = 2, n = 2)
# old female
summarize_beta_binomial(alpha = 1, beta = 1, y = 6, n = 9)

# social influence
summarize_beta_binomial(alpha = 1, beta = 1, y = 81, n = 100)#0.8

# difference beta-binomial distribution 
# simulations from beta distributions
set.seed(123)
sim_c_b<-rbeta(n=10000,33,12) # control
sim_s_b<-rbeta(n=10000,82,20) # social influence
sim_f_b<-rbeta(n=10000,39,13) # loss framing
sim_old_male<-rbeta(10000,64,48) # old male
sim_young_male<-rbeta(10000,30,3) # young male


#social influence - control
dif<-sim_s_b-sim_c_b
describe_posterior(dif,centrality = "mean")
ci_hpd<-ci(dif,method="HDI")
dif %>% 
  estimate_density(extend = TRUE) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_area(fill = "grey") +
  theme_classic() +
  # HPD in red
  geom_vline(xintercept = ci_hpd$CI_low, color = "red", size = 0.5) +
  geom_vline(xintercept = ci_hpd$CI_high, color = "red", size = 0.5)+
  ggtitle("Probability difference between social influence and control group")+xlab("probability difference")+ylab("Density")+
  annotate("text",x=c(0.4,0.42),y=c(4,3.5),label=c("95% HPD CI: [-0.07,0.23]","mean:0.07"))

# loss framing - control
dif2<-sim_f_b-sim_c_b
describe_posterior(dif2,centrality = "mean")
ci_hpd<-ci(dif2,method="HDI")
dif2 %>% 
  estimate_density(extend = TRUE) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_area(fill = "grey") +
  theme_classic() +
  # HPD in red
  geom_vline(xintercept = ci_hpd$CI_low, color = "red", size = 0.5) +
  geom_vline(xintercept = ci_hpd$CI_high, color = "red", size = 0.5)+
  ggtitle("Probability difference between loss framing and control group")+xlab("probability difference")+ylab("Density")+
  annotate("text",x=c(0.3,0.32),y=c(4,3.5),label=c("95% HPD CI: [-0.15,0.19]","mean:0.02"))

# young male - control
dif3<-sim_young_male-sim_c_b
ci_eti<-ci(dif3,method="HDI")
describe_posterior(dif3,centrality = "mean")
dif3 %>% 
  estimate_density(extend = TRUE) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_area(fill = "grey") +
  theme_classic() +
  # HPD in red
  geom_vline(xintercept = ci_eti$CI_low, color = "red", size = 0.5) +
  geom_vline(xintercept = ci_eti$CI_high, color = "red", size = 0.5)+
  ggtitle("Probability difference between storytelling_young male and control group")+xlab("probability difference")+ylab("Density")+
  annotate("text",x=c(0.52,0.53),y=c(4,3.5),label=c("95% HPD CI: [0.01,0.33]","mean:0.18"))

# old male - control
dif4<-sim_old_male-sim_c_b
ci_hpd<-ci(dif4,method="HDI")
describe_posterior(dif4,centrality = "mean")
dif4 %>% 
  estimate_density(extend = TRUE) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_area(fill = "grey") +
  theme_classic() +
  # HPD in red
  geom_vline(xintercept = ci_hpd$CI_low, color = "red", size = 0.5) +
  geom_vline(xintercept = ci_hpd$CI_high, color = "red", size = 0.5)+
  ggtitle("Probability difference between storytelling_old male and control group")+xlab("probability difference")+ylab("Density")+
  annotate("text",x=c(0.2,0.2),y=c(4,3.5),label=c("95% HPD CI: [-0.32,-0.01]","mean:-0.16"))
