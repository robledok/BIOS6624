## Loading packages
library(tidyverse)
library(gtsummary)
library(ggplot2)

## Reading in the data
dat_p1_raw <- read.csv("DataRaw/hiv_6624_final.csv")

## Filtering data to look at years 0 and 2
dat_p1_subset <- dat_p1_raw %>%
  filter(years == 0 | years == 2)

## Exclude individuals who don't have year 2 measurements
dat_p1_subset <- dat_p1_subset %>%
  group_by(newid) %>%
  filter(any(years == 2)) %>% 
  ungroup()

## Checking each individual has 2 observations
dat_p1_subset %>%
  group_by(newid) %>%
  summarise(n_obs = n()) %>%
  filter(n_obs != 2) 

## Seeing how many individuals and observations were excluded
length(unique(dat_p1_raw$newid)) - length(unique(dat_p1_subset$newid))
nrow(dat_p1_raw) - nrow(dat_p1_subset)
length(unique(dat_p1_subset$newid))

## Creating baseline covariate columns
dat_p1_subset <- dat_p1_subset %>%
  group_by(newid) %>%
  mutate(
    age_base = age[years == 0],
    bmi_base = BMI[years == 0],
    smoke_base = SMOKE[years == 0],
    educ_base = EDUCBAS[years == 0],
    race_base = RACE[years == 0],
    mqol_base = AGG_MENT[years == 0],
    pqol_base = AGG_PHYS[years == 0],
    vl_base = VLOAD[years == 0],
    cd4_base = LEU3N[years == 0],
    drugs_base = hard_drugs[years == 0]
  ) %>%
  ungroup()

## Filtering data to year 2
dat_p1 <- dat_p1_subset %>%
  filter(years == 2)

## Ensuring we did not lose any individuals
length(unique(dat_p1$newid)) == length(unique(dat_p1_subset$newid))

## Filtering the data to exclude any NA's from any outcome
dat_p1 <- dat_p1 %>%
  filter(if_all(c(mqol_base, pqol_base, vl_base, cd4_base, AGG_MENT, AGG_PHYS, VLOAD, LEU3N), ~ !is.na(.)))

## Seeing how many individuals we lost and new total number of observations 
length(unique(dat_p1_subset$newid)) - length(unique(dat_p1$newid))
nrow(dat_p1)

## Collapsing race and education
dat_p1 <- dat_p1 %>%
  mutate(
    race_binary = if_else(race_base == 1, "White, Non-Hispanic", "Other"),
    educ_binary = case_when(
      educ_base == 5 ~ "College Degree or Higher",
      educ_base == 6 ~ "College Degree or Higher",
      educ_base == 7 ~ "College Degree or Higher",
      TRUE ~ "No College Degree"
    )
  )

## Excluding BMI above 250 and below 0 and seeing how many observations we lose
dat_p1_clean <- dat_p1 %>%
  filter(bmi_base < 250 & bmi_base > 0)
nrow(dat_p1) - nrow(dat_p1_clean)

## Saving new dataset
write.csv(dat_p1, "DataProcessed/dat_p1_clean_kr.csv")