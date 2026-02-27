## Loading packages
library(tidyverse)
library(gtsummary)
library(brms)

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
freq_mod1_vl <- lm(log10(VLOAD) ~ drugs_base + vl_base + age_base + 
                     bmi_base + smoke_binary + educ_binary + race_binary + adh_binary,
                   data = dat_p1)
# Model without adherence
freq_mod2_vl <- lm(log10(VLOAD) ~ drugs_base + vl_base + age_base + 
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

## Looking at summary coefficient output and AIC

# CD4 Models
summary(freq_mod1_cd4)$coefficients
summary(freq_mod2_cd4)$coefficients
AIC(freq_mod1_cd4, freq_mod2_cd4)

# Viral Load Models
summary(freq_mod1_vl)$coefficients
summary(freq_mod2_vl)$coefficients
AIC(freq_mod1_vl, freq_mod2_vl)

# Mental Quality of Life Models
summary(freq_mod1_mqol)$coefficients
summary(freq_mod2_mqol)$coefficients
AIC(freq_mod1_mqol, freq_mod2_mqol)

# Physical Quality of Life Models
summary(freq_mod1_pqol)$coefficients
summary(freq_mod2_pqol)$coefficients
AIC(freq_mod1_pqol, freq_mod2_pqol)

##*******************************************************************
## ------------------ Bayesian Models  ----------------------
##*******************************************************************
##

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
bayes_mod1_vl <- brm(log10(VLOAD) ~ drugs_base + vl_base + age_base + 
                     bmi_base + smoke_binary + educ_binary + race_binary + adh_binary,
                     data = dat_p1,
                     seed = 6624,
                     prior = c(set_prior("normal(0, 10000)", class = "b"),
                               set_prior("normal(1,10000)", class = "Intercept"),
                               set_prior("normal(0,10000)", class = "sigma", lb = 0)),
                     chains = 4, iter = 25000, warmup = 5000, refresh = 0)
# Model without adherence
bayes_mod2_vl <- brm(log10(VLOAD) ~ drugs_base + vl_base + age_base + 
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
                                 set_prior("normal(0,10000)", class = "sigma"), lb = 0),
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

## Looking at summary coefficient output

# CD4 Models
summary(bayes_mod1_cd4)$fixed
summary(bayes_mod2_cd4)$fixed

# Viral Load Models
summary(bayes_mod1_vl)$fixed
summary(bayes_mod2_vl)$fixed

# Mental Quality of Life Models
summary(bayes_mod1_mqol)$fixed
summary(bayes_mod2_mqol)$fixed

# Physical Quality of Life Models
summary(bayes_mod1_pqol)$fixed
summary(bayes_mod2_pqol)$fixed