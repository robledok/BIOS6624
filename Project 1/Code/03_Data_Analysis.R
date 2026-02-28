## Loading packages
library(tidyverse)
library(gtsummary)
library(brms)
library(jtools)
library(loo)

## Reading in the data
dat_p1 <- read.csv("DataProcessed/dat_p1_clean_kr.csv")

##*******************************************************************
## ------------------ Table 1  ----------------------
##*******************************************************************
##

## Changing reference levels of education, race, adherence, and smoking
dat_p1$educ_binary <- relevel(factor(dat_p1$educ_binary), ref = "No College Degree")
dat_p1$race_binary <- relevel(factor(dat_p1$race_binary), ref = "White, Non-Hispanic")
dat_p1$adh_binary <- relevel(factor(dat_p1$adh_binary), ref = "Less than 95%")
dat_p1$smoke_binary <- relevel(factor(dat_p1$smoke_binary), ref = "Not a Current Smoker")

## Making sure drugs_base is a categorical variable
dat_p1$drugs_base <- as.factor(dat_p1$drugs_base)

dat_p1 %>%
  tbl_summary(include = c(age_base, bmi_base, smoke_binary, educ_binary, race_binary, adh_binary),
              by = drugs_base,
              label = list(
                age_base ~ "Age at Baseline (years)",
                bmi_base ~ "BMI at Baseline",
                smoke_binary ~ "Smoking Status at Baseline",
                educ_binary ~ "Education",
                race_binary ~ "Race/Ethnicity",
                adh_binary ~ "Adherence at Year 2"
              ),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              missing = 'ifany',
              missing_text = "(Missing)") %>%
  modify_header(stat_1 = "**No Hard-Drug Usage**  \nN = 427") %>%
  modify_header(stat_2 = "**Hard-Drug Usage**  \nN = 36") %>%
  add_overall(last = TRUE)

##*******************************************************************
## ------------------ Frequentist Models  ----------------------
##*******************************************************************
##

## CD4 Models
# Model with adherence
freq_mod1_cd4 <- lm(LEU3N ~ drugs_base + cd4_base + age_base + 
                     bmi_base + smoke_binary + educ_binary + race_binary + adh_binary,
                   data = dat_p1)
# Model without adherence
freq_mod2_cd4 <- lm(LEU3N ~ drugs_base + cd4_base + age_base + 
                      bmi_base + smoke_binary + educ_binary + race_binary,
                    data = dat_p1)

## Viral Load Models
# Model with adherence
freq_mod1_vl <- lm(log10(VLOAD) ~ drugs_base + log10(vl_base) + age_base + 
                     bmi_base + smoke_binary + educ_binary + race_binary + adh_binary,
                   data = dat_p1)
# Model without adherence
freq_mod2_vl <- lm(log10(VLOAD) ~ drugs_base + log10(vl_base) + age_base + 
                     bmi_base + smoke_binary + educ_binary + race_binary,
                   data = dat_p1)

## Mental Quality of Life Models
# Model with adherence
freq_mod1_mqol <- lm(AGG_MENT ~ drugs_base + mqol_base + age_base + 
                     bmi_base + smoke_binary + educ_binary + race_binary + adh_binary,
                   data = dat_p1)
# Model without adherence
freq_mod2_mqol <- lm(AGG_MENT ~ drugs_base + mqol_base + age_base + 
                       bmi_base + smoke_binary + educ_binary + race_binary,
                     data = dat_p1)

## Physical Quality of Life Models
# Model with adherence
freq_mod1_pqol <- lm(AGG_PHYS ~ drugs_base + pqol_base + age_base + 
                     bmi_base + smoke_binary + educ_binary + race_binary + adh_binary,
                   data = dat_p1)
# Model without adherence
freq_mod2_pqol <- lm(AGG_PHYS ~ drugs_base + pqol_base + age_base + 
                       bmi_base + smoke_binary + educ_binary + race_binary,
                     data = dat_p1)


##*******************************************************************
## ------------------ Bayesian Models  ----------------------
##*******************************************************************
##

## USE NON-INFORMATIVE PRIORS:
## Half-Normal prior for sigma: mean = 0, var = 10000
## Normal prior for intercept: mean = 1, var = 10000
## Normal prior for other betas: mean = 0, var = 10000

## MCMC SETTINGS:
## NUTS sampler
## 5000 warmup
## 25000 iterations
## 4 chains
## seed of 6624

## CD4 Models
# Model with adherence
bayes_mod1_cd4 <- brm(LEU3N ~ drugs_base + cd4_base + age_base + 
                      bmi_base + smoke_binary + educ_binary + race_binary + adh_binary,
                      data = dat_p1,
                      seed = 6624,
                      prior = c(set_prior("normal(0, 10000)", class = "b"),
                                set_prior("normal(1,10000)", class = "Intercept"),
                                set_prior("normal(0,10000)", class = "sigma", lb = 0)),
                      chains = 4, iter = 25000, warmup = 5000, refresh = 0)
# Model without adherence
bayes_mod2_cd4 <- brm(LEU3N ~ drugs_base + cd4_base + age_base + 
                      bmi_base + smoke_binary + educ_binary + race_binary,
                      data = dat_p1,
                      seed = 6624,
                      prior = c(set_prior("normal(0, 10000)", class = "b"),
                                set_prior("normal(1,10000)", class = "Intercept"),
                                set_prior("normal(0,10000)", class = "sigma", lb = 0)),
                      chains = 4, iter = 25000, warmup = 5000, refresh = 0)

## Viral Load Models
# Model with adherence
bayes_mod1_vl <- brm(log10(VLOAD) ~ drugs_base + log10(vl_base) + age_base + 
                     bmi_base + smoke_binary + educ_binary + race_binary + adh_binary,
                     data = dat_p1,
                     seed = 6624,
                     prior = c(set_prior("normal(0, 10000)", class = "b"),
                               set_prior("normal(1,10000)", class = "Intercept"),
                               set_prior("normal(0,10000)", class = "sigma", lb = 0)),
                     chains = 4, iter = 25000, warmup = 5000, refresh = 0)
# Model without adherence
bayes_mod2_vl <- brm(log10(VLOAD) ~ drugs_base + log10(vl_base) + age_base + 
                     bmi_base + smoke_binary + educ_binary + race_binary,
                     data = dat_p1,
                     seed = 6624,
                     prior = c(set_prior("normal(0, 10000)", class = "b"),
                               set_prior("normal(1,10000)", class = "Intercept"),
                               set_prior("normal(0,10000)", class = "sigma", lb = 0)),
                     chains = 4, iter = 25000, warmup = 5000, refresh = 0)

## Mental Quality of Life Models
# Model with adherence
bayes_mod1_mqol <- brm(AGG_MENT ~ drugs_base + mqol_base + age_base + 
                       bmi_base + smoke_binary + educ_binary + race_binary + adh_binary,
                       data = dat_p1,
                       seed = 6624,
                       prior = c(set_prior("normal(0, 10000)", class = "b"),
                                 set_prior("normal(1,10000)", class = "Intercept"),
                                 set_prior("normal(0,10000)", class = "sigma", lb = 0)),
                       chains = 4, iter = 25000, warmup = 5000, refresh = 0)
# Model without adherence
bayes_mod2_mqol <- brm(AGG_MENT ~ drugs_base + mqol_base + age_base + 
                       bmi_base + smoke_binary + educ_binary + race_binary,
                       data = dat_p1,
                       seed = 6624,
                       prior = c(set_prior("normal(0, 10000)", class = "b"),
                                 set_prior("normal(1,10000)", class = "Intercept"),
                                 set_prior("normal(0,10000)", class = "sigma", lb = 0)),
                       chains = 4, iter = 25000, warmup = 5000, refresh = 0)

## Physical Quality of Life Models
# Model with adherence
bayes_mod1_pqol <- brm(AGG_PHYS ~ drugs_base + pqol_base + age_base + 
                       bmi_base + smoke_binary + educ_binary + race_binary + adh_binary,
                       data = dat_p1,
                       seed = 6624,
                       prior = c(set_prior("normal(0, 10000)", class = "b"),
                                 set_prior("normal(1,10000)", class = "Intercept"),
                                 set_prior("normal(0,10000)", class = "sigma", lb = 0)),
                       chains = 4, iter = 25000, warmup = 5000, refresh = 0)
# Model without adherence
bayes_mod2_pqol <- brm(AGG_PHYS ~ drugs_base + pqol_base + age_base + 
                       bmi_base + smoke_binary + educ_binary + race_binary,
                       data = dat_p1,
                       seed = 6624,
                       prior = c(set_prior("normal(0, 10000)", class = "b"),
                                 set_prior("normal(1,10000)", class = "Intercept"),
                                 set_prior("normal(0,10000)", class = "sigma", lb = 0)),
                       chains = 4, iter = 25000, warmup = 5000, refresh = 0)

##*******************************************************************
## ------------------ Trace Plots  ----------------------
##*******************************************************************
##

## CD4 Models
plot(bayes_mod1_cd4)
plot(bayes_mod2_cd4)
## Viral Load Models
plot(bayes_mod1_vl)
plot(bayes_mod2_vl)
## Mental Quality of Life Models
plot(bayes_mod1_mqol)
plot(bayes_mod2_mqol)
## Physical Quality of Life Models
plot(bayes_mod1_pqol)
plot(bayes_mod2_pqol)

##*******************************************************************
## ------------------ Comparing Summary Output and Goodness of Fit  ----------------------
##*******************************************************************
##

# CD4 Models
summ(freq_mod1_cd4, confint = TRUE)
summary(bayes_mod1_cd4)$fixed
summ(freq_mod2_cd4, confint = TRUE)
summary(bayes_mod2_cd4)$fixed
AIC(freq_mod1_cd4, freq_mod2_cd4)
loo(bayes_mod1_cd4, bayes_mod2_cd4)

# Viral Load Models
summ(freq_mod1_vl, confint = TRUE)
summary(bayes_mod1_vl)$fixed
summ(freq_mod2_vl, confint = TRUE)
summary(bayes_mod2_vl)$fixed
AIC(freq_mod1_vl, freq_mod2_vl)
loo(bayes_mod1_vl, bayes_mod2_vl)

# Mental Quality of Life Models
summ(freq_mod1_mqol, confint = TRUE)
summary(bayes_mod1_mqol)$fixed
summ(freq_mod2_mqol, confint = TRUE)
summary(bayes_mod2_mqol)$fixed
AIC(freq_mod1_mqol, freq_mod2_mqol)
loo(bayes_mod1_mqol, bayes_mod2_mqol)

# Physical Quality of Life Models
summ(freq_mod1_pqol, confint = TRUE)
summary(bayes_mod1_pqol)$fixed
summ(freq_mod2_pqol, confint = TRUE)
summary(bayes_mod2_pqol)$fixed
AIC(freq_mod1_pqol, freq_mod2_pqol)
loo(bayes_mod1_pqol, bayes_mod2_pqol)

##*******************************************************************
## ------------------ Computing Posterior Probability  ----------------------
##*******************************************************************
##

## CD4 Model
post_mod1_cd4 <- as_draws_df(bayes_mod1_cd4)
prob_mod1_cd4 <- mean(abs(post_mod1_cd4$b_drugs_base1) > 50)
prob_mod1_cd4

post_mod2_cd4 <- as_draws_df(bayes_mod2_cd4)
prob_mod2_cd4 <- mean(abs(post_mod2_cd4$b_drugs_base1) > 50)
prob_mod2_cd4

## Viral Load Model
post_mod1_vl <- as_draws_df(bayes_mod1_vl)
prob_mod1_vl <- mean(abs(post_mod1_vl$b_drugs_base1) > 0.5)
prob_mod1_vl

post_mod2_vl <- as_draws_df(bayes_mod2_vl)
prob_mod2_vl <- mean(abs(post_mod2_vl$b_drugs_base1) > 0.5)
prob_mod2_vl

## Mental Quality of Life Model
post_mod1_mqol <- as_draws_df(bayes_mod1_mqol)
prob_mod1_mqol <- mean(abs(post_mod1_mqol$b_drugs_base1) > 2)
prob_mod1_mqol

post_mod2_mqol <- as_draws_df(bayes_mod2_mqol)
prob_mod2_mqol <- mean(abs(post_mod2_mqol$b_drugs_base1) > 2)
prob_mod2_mqol

## Physical Quality of Life Model
post_mod1_pqol <- as_draws_df(bayes_mod1_pqol)
prob_mod1_pqol <- mean(abs(post_mod1_pqol$b_drugs_base1) > 2)
prob_mod1_pqol

post_mod2_pqol <- as_draws_df(bayes_mod2_pqol)
prob_mod2_pqol <- mean(abs(post_mod2_pqol$b_drugs_base1) > 2)
prob_mod2_pqol

##*******************************************************************
## ------------------ Summary Table  ----------------------
##*******************************************************************
##

dat_sum_tbl <- data.frame(
  Model = c("CD4+ T Cell Counts", "log10(Viral Load Counts)", "Mental Quality of Life Score", "Physical Quality of Life Score"),
  freq_est = c(round(freq_mod1_cd4$coefficients[2], 2), round(freq_mod1_vl$coefficients[2], 2), 
               round(freq_mod1_mqol$coefficients[2], 2), round(freq_mod1_pqol$coefficients[2], 2)),
  freq_ci = c(paste0("(", round(summ(freq_mod1_cd4, confint = TRUE)$coeftable[2,2], 2), ", ", 
                     round(summ(freq_mod1_cd4, confint = TRUE)$coeftable[2,3], 2), ")"),
              paste0("(", round(summ(freq_mod1_vl, confint = TRUE)$coeftable[2,2], 2), ", ", 
                     round(summ(freq_mod1_vl, confint = TRUE)$coeftable[2,3], 2), ")"),
              paste0("(", round(summ(freq_mod1_mqol, confint = TRUE)$coeftable[2,2], 2), ", ", 
                     round(summ(freq_mod1_mqol, confint = TRUE)$coeftable[2,3], 2), ")"),
              paste0("(", round(summ(freq_mod1_pqol, confint = TRUE)$coeftable[2,2], 2), ", ", 
                     round(summ(freq_mod1_pqol, confint = TRUE)$coeftable[2,3], 2), ")")),
  freq_p = c(ifelse(summ(freq_mod1_cd4, confint = TRUE)$coeftable[2,5] < 0.01, "<0.01", 
                    round(summ(freq_mod1_cd4, confint = TRUE)$coeftable[2,5], 2)),
             ifelse(summ(freq_mod1_vl, confint = TRUE)$coeftable[2,5] < 0.01, "<0.01", 
                    round(summ(freq_mod1_vl, confint = TRUE)$coeftable[2,5], 2)),
             ifelse(summ(freq_mod1_mqol, confint = TRUE)$coeftable[2,5] < 0.01, "<0.01", 
                    round(summ(freq_mod1_mqol, confint = TRUE)$coeftable[2,5], 2)),
             ifelse(summ(freq_mod1_pqol, confint = TRUE)$coeftable[2,5] < 0.01, "<0.01", 
                    round(summ(freq_mod1_pqol, confint = TRUE)$coeftable[2,5], 2))),
  bayes_est = c(round(summary(bayes_mod1_cd4)$fixed[2,1], 2), round(summary(bayes_mod1_vl)$fixed[2,1], 2), 
                round(summary(bayes_mod1_mqol)$fixed[2,1], 2), round(summary(bayes_mod1_pqol)$fixed[2,1], 2)),
  bayes_cri = c(paste0("(", round(summary(bayes_mod1_cd4)$fixed[2,3], 2), ", ", 
                       round(summary(bayes_mod1_cd4)$fixed[2,4], 2), ")"),
                paste0("(", round(summary(bayes_mod1_vl)$fixed[2,3], 2), ", ", 
                       round(summary(bayes_mod1_vl)$fixed[2,4], 2), ")"),
                paste0("(", round(summary(bayes_mod1_mqol)$fixed[2,3], 2), ", ", 
                       round(summary(bayes_mod1_mqol)$fixed[2,4], 2), ")"),
                paste0("(", round(summary(bayes_mod1_pqol)$fixed[2,3], 2), ", ", 
                       round(summary(bayes_mod1_pqol)$fixed[2,4], 2), ")")),
  bayes_pp = c(round(prob_mod1_cd4, 4), round(prob_mod1_vl, 2), round(prob_mod1_mqol, 2), round(prob_mod1_pqol, 2))
)

kable(dat_sum_tbl, escape = TRUE, align = "lc", 
      col.names = c("Model",
                    "Estimate", "95% CI", "p-value",
                    "Posterior Mean", "95% CrI", "Posterior Probability")) %>%
  add_header_above(c(" " = 1, "Frequentist" = 3, "Bayesian" = 3)) %>%
  kable_styling(full_width = FALSE, position = "center")

##*******************************************************************
## ------------------ Model Fit Graph  ----------------------
##*******************************************************************
##

## CD4 Model

## Defining data for prediction (hold everything at mean)
new_dat1 <- expand.grid(
  drugs_base = levels(dat_p1$drugs_base),
  cd4_base = mean(dat_p1$cd4_base, na.rm = TRUE),
  age_base = mean(dat_p1$age_base, na.rm = TRUE),
  bmi_base = mean(dat_p1$bmi_base, na.rm = TRUE),
  smoke_binary = "Not a Current Smoker",
  educ_binary = "No College Degree",
  race_binary = "White, Non-Hispanic",
  adh_binary = "Less than 95%"
)

## Get posterior fitted values with 95% credible intervals
pred1 <- fitted(
  bayes_mod1_cd4,
  newdata = new_dat1,
  summary = TRUE,
  probs = c(.025, .975)
)

## Combine data with predictions
new_dat1 <- cbind(new_dat1, pred1)

## Plot the data
ggplot() +
  geom_jitter(data = dat_p1, aes(x = drugs_base, y = LEU3N, color = "Observed Values"), alpha = 0.3, width = 0.1) +
  geom_errorbar(data = new_dat1, aes(x = drugs_base, ymin = Q2.5, ymax = Q97.5, color = "95% CrI"), width = 0.1, linewidth = 1) +
  geom_point(data = new_dat1, aes(x = drugs_base, y = Estimate, color = "Posterior Mean"), size = 3) +
  scale_color_manual(name = "Legend",
                     values = c("95% CrI" = "dodgerblue1",
                                "Posterior Mean" = "violetred1",
                                "Observed Values" = "gray50")) +
  scale_x_discrete(labels = c("0" = "No Hard Drug Use", "1" = "Hard Drug Use")) +
  labs(x = NULL,
       y = "CD4+ T Cell Counts",
       title = "Observed CD4 Values with Posterior Mean and 95% CrI") +
  theme_bw() +
  theme(legend.position = "bottom")

## Viral Load Model
## Defining data for prediction (hold everything at mean)
new_dat2 <- expand.grid(
  drugs_base = levels(dat_p1$drugs_base),
  vl_base = mean(dat_p1$vl_base, na.rm = TRUE),
  age_base = mean(dat_p1$age_base, na.rm = TRUE),
  bmi_base = mean(dat_p1$bmi_base, na.rm = TRUE),
  smoke_binary = "Not a Current Smoker",
  educ_binary = "No College Degree",
  race_binary = "White, Non-Hispanic",
  adh_binary = "Less than 95%"
)
## Get posterior fitted values with 95% credible intervals
pred2 <- fitted(
  bayes_mod1_vl,
  newdata = new_dat2,
  summary = TRUE,
  probs = c(.025, .975)
)

## Combine data with predictions
new_dat2 <- cbind(new_dat2, pred2)

## Plot the data
ggplot() +
  geom_jitter(data = dat_p1, aes(x = drugs_base, y = log10(VLOAD), color = "Observed Values"), alpha = 0.3, width = 0.1) +
  geom_errorbar(data = new_dat2, aes(x = drugs_base, ymin = Q2.5, ymax = Q97.5, color = "95% CrI"), width = 0.1, linewidth = 1) +
  geom_point(data = new_dat2, aes(x = drugs_base, y = Estimate, color = "Posterior Mean"), size = 3) +
  scale_color_manual(name = "Legend",
                     values = c("95% CrI" = "dodgerblue1",
                                "Posterior Mean" = "violetred1",
                                "Observed Values" = "gray50")) +
  scale_x_discrete(labels = c("0" = "No Hard Drug Use", "1" = "Hard Drug Use")) +
  labs(x = NULL,
       y = "log10(Viral Load Counts)",
       title = "Observed Viral Load Values with Posterior Mean and 95% CrI") +
  theme_bw() +
  theme(legend.position = "bottom")

## Mental Quality of Life Model
## Defining data for prediction (hold everything at mean)
new_dat3 <- expand.grid(
  drugs_base = levels(dat_p1$drugs_base),
  mqol_base = mean(dat_p1$mqol_base, na.rm = TRUE),
  age_base = mean(dat_p1$age_base, na.rm = TRUE),
  bmi_base = mean(dat_p1$bmi_base, na.rm = TRUE),
  smoke_binary = "Not a Current Smoker",
  educ_binary = "No College Degree",
  race_binary = "White, Non-Hispanic",
  adh_binary = "Less than 95%"
)
## Get posterior fitted values with 95% credible intervals
pred3 <- fitted(
  bayes_mod1_mqol,
  newdata = new_dat3,
  summary = TRUE,
  probs = c(.025, .975)
)

## Combine data with predictions
new_dat3 <- cbind(new_dat3, pred3)

## Plot the data
ggplot() +
  geom_jitter(data = dat_p1, aes(x = drugs_base, y = AGG_MENT, color = "Observed Values"), alpha = 0.3, width = 0.1) +
  geom_errorbar(data = new_dat3, aes(x = drugs_base, ymin = Q2.5, ymax = Q97.5, color = "95% CrI"), width = 0.1, linewidth = 1) +
  geom_point(data = new_dat3, aes(x = drugs_base, y = Estimate, color = "Posterior Mean"), size = 3) +
  scale_color_manual(name = "Legend",
                     values = c("95% CrI" = "dodgerblue1",
                                "Posterior Mean" = "violetred1",
                                "Observed Values" = "gray50")) +
  scale_x_discrete(labels = c("0" = "No Hard Drug Use", "1" = "Hard Drug Use")) +
  labs(x = NULL,
       y = "Mental Quality of Life Score",
       title = "Observed Mental Quality of Life Scores with Posterior Mean and 95% CrI") +
  theme_bw() +
  theme(legend.position = "bottom")

## Physical Quality of Life Model
## Defining data for prediction (hold everything at mean)
new_dat4 <- expand.grid(
  drugs_base = levels(dat_p1$drugs_base),
  pqol_base = mean(dat_p1$pqol_base, na.rm = TRUE),
  age_base = mean(dat_p1$age_base, na.rm = TRUE),
  bmi_base = mean(dat_p1$bmi_base, na.rm = TRUE),
  smoke_binary = "Not a Current Smoker",
  educ_binary = "No College Degree",
  race_binary = "White, Non-Hispanic",
  adh_binary = "Less than 95%"
)
## Get posterior fitted values with 95% credible intervals
pred4 <- fitted(
  bayes_mod1_pqol,
  newdata = new_dat4,
  summary = TRUE,
  probs = c(.025, .975)
)

## Combine data with predictions
new_dat4 <- cbind(new_dat4, pred4)

## Plot the data
ggplot() +
  geom_jitter(data = dat_p1, aes(x = drugs_base, y = AGG_PHYS, color = "Observed Values"), alpha = 0.3, width = 0.1) +
  geom_errorbar(data = new_dat4, aes(x = drugs_base, ymin = Q2.5, ymax = Q97.5, color = "95% CrI"), width = 0.1, linewidth = 1) +
  geom_point(data = new_dat4, aes(x = drugs_base, y = Estimate, color = "Posterior Mean"), size = 3) +
  scale_color_manual(name = "Legend",
                     values = c("95% CrI" = "dodgerblue1",
                                "Posterior Mean" = "violetred1",
                                "Observed Values" = "gray50")) +
  scale_x_discrete(labels = c("0" = "No Hard Drug Use", "1" = "Hard Drug Use")) +
  labs(x = NULL,
       y = "Physical Quality of Life Score",
       title = "Observed Physical Quality of Life Score with Posterior Mean and 95% CrI") +
  theme_bw() +
  theme(legend.position = "bottom")