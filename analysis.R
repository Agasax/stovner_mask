# setup ----
library(tidyverse)
library(here)
library(rio)
library(brms)
library(tidybayes)

# https://www.mdpi.com/1660-4601/18/17/8971

# inport and wrangling ----
mask_data <- import(here("data","stovner_mask_data.csv")) %>%
  rename(correctly_masked=masked,total_customers=total) %>%
  mutate(total_masked=correctly_masked+incorrectlymasked) %>%
  mutate(across(c(store,date,host_1,host_2,treated),as.factor)) %>%
  mutate(across(c(host_1,host_2),~fct_explicit_na(.x,na_level = "0")))

#model for proportion of correctly masked and total masked with group level intercepts
model1_prior <- brm(correctly_masked|trials(total_customers)~1+treated+(1|store)+(1|date),family = binomial,data=mask_data,
              prior = prior(normal(0,1.5),class="b")+
                prior(student_t(3,0,2.5),class="sd"),
              sample_prior = "only")
model1_prior
model1_correct <- brm(correctly_masked|trials(total_customers)~1+treated+(1+treated|store)+(1+treated|date),family = binomial,data=mask_data,
                      prior = prior(normal(0,1.5),class="b")+
                        prior(student_t(3,0,2.5),class="sd"))

model1_total <- brm(total_masked|trials(total_customers)~1+treated+(1+treated|store)+(1+treated|date),family = binomial,data=mask_data,
                    prior = prior(normal(0,1.5),class="b")+
                      prior(student_t(3,0,2.5),class="sd"))



#model for proportion of correctly masked and total masked with group level intercepts and treatment effects
model2_prior <- brm(correctly_masked|trials(total_customers)~1+treated+(1+treated|store)+(1+treated|date),family = binomial,data=mask_data,
                    prior = prior(normal(0,1.5),class="b")+
                      prior(student_t(3,0,2.5),class="sd")+
                      prior(lkj(2),class="cor"),
                    sample_prior ="only")
model2_correct<- brm(correctly_masked|trials(total_customers)~1+treated+(1+treated|store)+(1+treated|date),family = binomial,data=mask_data,
                    prior = prior(normal(0,1.5),class="b")+
                      prior(student_t(3,0,2.5),class="sd")+
                      prior(lkj(2),class="cor"))
model2_total<- brm(total_masked|trials(total_customers)~1+treated+(1+treated|store)+(1+treated|date),family = binomial,data=mask_data,
              prior = prior(normal(0,1.5),class="b")+
                prior(student_t(3,0,2.5),class="sd")+
                prior(lkj(2),class="cor"))



# fixed effect on ar, or and rr scale
model2_correct %>%
  spread_draws(`b_.*`,regex = TRUE) %>%
  mutate(or=exp(b_treated1),rr=inv_logit_scaled((b_Intercept+b_treated1))/ inv_logit_scaled(b_Intercept),ar=inv_logit_scaled((b_Intercept+b_treated1))- inv_logit_scaled(b_Intercept)) %>%
  pivot_longer(cols=c(or,rr,ar)) %>%
  group_by(name) %>%
  mean_hdi(value)

model2_correct %>%
  spread_draws(`b_.*`,regex = TRUE) %>%
  mutate(or=exp(b_treated1),rr=inv_logit_scaled((b_Intercept+b_treated1))/ inv_logit_scaled(b_Intercept),ar=inv_logit_scaled((b_Intercept+b_treated1))- inv_logit_scaled(b_Intercept)) %>%
  pivot_longer(cols=c(or,rr,ar)) %>%
  group_by(name) %>%
  ggplot(aes(x=value))+
  stat_halfeye()+
  theme_tidybayes()+
  facet_wrap(~name,dir="v",scales  = "free")






