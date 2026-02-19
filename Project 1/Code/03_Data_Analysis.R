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

## Changing reference levels of education and race
dat_p1$educ_binary <- relevel(factor(dat_p1$educ_binary), ref = "No College Degree")
dat_p1$race_binary <- relevel(factor(dat_p1$race_binary), ref = "White, Non-Hispanic")


dat_p1 %>%
  mutate(smoke_base = factor(smoke_base, levels = c("1", "2", "3"), 
                        labels = c("Never Smoked", "Former Smoker", "Current Smoker"))) %>%
  tbl_summary(include = c(age_base, bmi_base, smoke_base, educ_binary, race_binary),
              by = drugs_base,
              label = list(
                age_base ~ "Age at Baseline (years)",
                bmi_base ~ "BMI at Baseline",
                smoke_base ~ "Smoking Status at Baseline",
                educ_binary ~ "Education",
                race_binary ~ "Race/Ethnicity"
              ),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              missing = 'ifany',
              missing_text = "(Missing)") %>%
  modify_header(stat_1 = "**No Hard-Drug Usage**  \nN = 427") %>%
  modify_header(stat_2 = "**Hard-Drug Usage**  \nN = 36") %>%
  add_overall(last = TRUE)