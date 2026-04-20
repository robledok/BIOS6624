# Loading packages
library(tidyverse)

# Loading in data
dat_p3_raw <- read.csv("DataRaw/frmgham2.csv")

# Creating our factor variables
dat_p3_raw$SEX <- factor(dat_p3_raw$SEX, levels = c(1, 2), labels = c("Male", "Female"))
dat_p3_raw$DIABETES <- factor(dat_p3_raw$DIABETES, levels = c(0, 1), labels = c("Not a Diabetic", "Diabetic"))
dat_p3_raw$CURSMOKE <- factor(dat_p3_raw$CURSMOKE, levels = c(0, 1), labels = c("Not a current smoker", "Current smoker"))
dat_p3_raw$BPMEDS <- factor(dat_p3_raw$BPMEDS, levels = c(0, 1), labels = c("Not currently used", "Currently used"))
dat_p3_raw$PREVCHD <- factor(dat_p3_raw$PREVCHD, levels = c(0, 1), labels = c("Free of disease", "Prevalent disease"))

# Excluding those who had a stroke before the study
dat_p3_interest <- dat_p3_raw %>%
  filter(TIMESTRK != 0)

# Creating 10 year stroke time variable
dat_p3_interest$time_10yr <- pmin(
  dat_p3_interest$TIMESTRK,
  dat_p3_interest$TIMEDTH,
  10*365.25
)

# Censoring deaths and 10 year stroke
dat_p3_interest$stroke_10yr <- ifelse(
  dat_p3_interest$STROKE == 1 &
    dat_p3_interest$TIMESTRK <= 10*365.25 &
    dat_p3_interest$TIMESTRK <= dat_p3_interest$TIMEDTH,
  1,
  0
)

## Saving new dataset
write.csv(dat_p3_interest, "DataProcessed/dat_p3_clean_kr.csv")