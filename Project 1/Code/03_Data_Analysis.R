## Loading packages
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(gtsummary)

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