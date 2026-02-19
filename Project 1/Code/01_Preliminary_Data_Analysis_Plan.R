## Loading packages
library(tidyverse)
library(gtsummary)
library(ggplot2)
## Reading in the data
dat_p1 <- read.csv("DataRaw/hiv_6624_final.csv")
## Filtering data to look at years 0 and 2
dat_p1_subset <- dat_p1 %>%
  filter(years == 0 | years == 2)
## Exclude individuals who don't have year 2 measurements
dat_p1_clean <- dat_p1_subset %>%
  group_by(newid) %>%
  filter(any(years == 2)) %>% 
  ungroup()
## Checking each individual has 2 observations
dat_p1_clean %>%
  group_by(newid) %>%
  summarise(n_obs = n()) %>%
  filter(n_obs != 2) 
## Seeing how many individuals and observations were excluded
length(unique(dat_p1$newid)) - length(unique(dat_p1_clean$newid))
nrow(dat_p1) - nrow(dat_p1_clean)
nrow(dat_p1_clean)
length(unique(dat_p1_clean$newid))
## Replace all adherence by year 2 adherence
dat_p1_clean <- dat_p1_clean %>%
  arrange(newid, years) %>%      
  group_by(newid) %>%
  mutate(adherence = ifelse(is.na(ADH),
                            lead(ADH),
                            ADH)) %>%
  ungroup()
## Looking at how many observations have adherence of NA
sum(is.na(dat_p1_clean$adherence))
## Defining hard drugs baseline variable
dat_p1_clean <- dat_p1_clean %>%
  arrange(newid, years) %>%
  group_by(newid) %>%
  mutate(base_drugs = hard_drugs[years == 0][1]) %>%
  ungroup()
## Seeing if any outcomes are NA
sum(is.na(dat_p1_clean$AGG_MENT))
sum(is.na(dat_p1_clean$AGG_PHYS))
sum(is.na(dat_p1_clean$LEU3N))
sum(is.na(dat_p1_clean$VLOAD))
## Looking at summary of continuous covariates
summary(dat_p1_clean$BMI)
summary(dat_p1_clean$age)
## Excluding BMI above 250 and seeing how much are above 250 and below 0
sum(dat_p1_clean$BMI > 250, na.rm = TRUE)
sum(dat_p1_clean$BMI < 0, na.rm = TRUE)
dat_p1_clean <- dat_p1_clean %>%
  filter((BMI < 250 & BMI > 0) | is.na(BMI))
## Seeing if any covariates are NA
sum(is.na(dat_p1_clean$age))
sum(is.na(dat_p1_clean$BMI))
sum(is.na(dat_p1_clean$SMOKE))
sum(is.na(dat_p1_clean$EDUCBAS))
sum(is.na(dat_p1_clean$RACE))
## Table 1 baseline
dat_p1_base <- dat_p1_clean %>% filter(years == 0)
dat_p1_base %>%
  mutate(SMOKE = factor(SMOKE, levels = c("1", "2", "3"), 
                        labels = c("Never Smoked", "Former Smoker", "Current Smoker")),
         EDUCBAS = factor(EDUCBAS, levels = c("1", "2", "3", "4", "5", "6", "7"),
                          labels = c("8th grade or less", "9, 10, or 11th grade",
                                     "12th grade", "At least one year college but no degree",
                                     "Four years college / got degree",
                                     "Some graduate work", " Post-graduate degree")),
         RACE = factor(RACE, levels = c("1", "2", "3", "4", "5", "6", "7", "8"),
                       labels = c("White, non-Hispanic", "White, Hispanic", "Black, non-Hispanic",
                                  "Black, Hispanic", "American Indian or Alaskan Native", 
                                  "Asian or Pacific Islander", "Other", 
                                  "Other Hispanic (created for 2001-03 new recruits)"))) %>%
  tbl_summary(include = c(age, BMI, SMOKE, EDUCBAS, RACE),
              by = base_drugs,
              label = list(
                age ~ "Age",
                SMOKE ~ "Smoking Status",
                EDUCBAS ~ "Education",
                RACE ~ "Race, Ethnicity"
              ),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              missing = 'ifany',
              missing_text = "(Missing)") %>%
  modify_header(stat_1 = "**No Hard-Drug Usage**  \nN = 457") %>%
  modify_header(stat_2 = "**Hard-Drug Usage**  \nN = 36") %>%
  add_overall(last = TRUE)
## Outcome distribution
hist(dat_p1_clean$VLOAD)
hist(dat_p1_clean$LEU3N)
hist(dat_p1_clean$AGG_MENT)
hist(dat_p1_clean$AGG_PHYS)