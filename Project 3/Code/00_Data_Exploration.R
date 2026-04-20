# Loading packages
library(tidyverse)
library(gtsummary)
library(survival)
library(survminer)

# Loading in data
dat_p3_raw <- read.csv("DataRaw/frmgham2.csv")

# Looking at number of subjects
length(unique(dat_p3_raw$RANDID))

# Cutting the data to baseline only
dat_p3_base <- dat_p3_raw %>%
  filter(PERIOD == 1)

# Seeing if the number of subjects matches 
length(unique(dat_p3_raw$RANDID)) == length(unique(dat_p3_base$RANDID))

# Creating our factor variables
dat_p3_base$SEX <- factor(dat_p3_base$SEX, levels = c(1, 2), labels = c("Male", "Female"))
dat_p3_base$DIABETES <- factor(dat_p3_base$DIABETES, levels = c(0, 1), labels = c("Not a Diabetic", "Diabetic"))
dat_p3_base$CURSMOKE <- factor(dat_p3_base$CURSMOKE, levels = c(0, 1), labels = c("Not a current smoker", "Current smoker"))
dat_p3_base$BPMEDS <- factor(dat_p3_base$BPMEDS, levels = c(0, 1), labels = c("Not a currently use", "Currently used"))
dat_p3_base$PREVCHD <- factor(dat_p3_base$PREVCHD, levels = c(0, 1), labels = c("Free of disease", "Prevalent disease"))

# Creating a Table 1 of variables of interest
dat_p3_base %>%
  tbl_summary(by = SEX,
              include = c(AGE, SYSBP, DIABETES, CURSMOKE, BMI, TOTCHOL, BPMEDS, PREVCHD),
              missing_text = "Missing",
              label = list(
                AGE ~ "Age (years)",
                SYSBP ~ "Systolic Blood Pressure (mmHg)",
                DIABETES ~ "Diabetes",
                CURSMOKE ~ "Smoking Status",
                BMI ~ "BMI (kg/m^2)",
                TOTCHOL ~ "Serum Total Cholesterol (mg/dL) ",
                BPMEDS ~ "Anti-hypertensive medication",
                PREVCHD ~ "Prevalent Coronary Heart Disease"
              ),
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  add_overall(last = TRUE)


# Excluding those who had a stroke before the study
dat_p3_interest <- dat_p3_base %>%
  filter(TIMESTRK != 0)

# Seeing how many subjects we lost
length(unique(dat_p3_base$RANDID)) - length(unique(dat_p3_interest$RANDID))

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

# Recreating a Table 1 of variables of interest
dat_p3_interest %>%
  tbl_summary(by = SEX,
              include = c(AGE, SYSBP, DIABETES, CURSMOKE, BMI, TOTCHOL, BPMEDS, PREVCHD),
              missing_text = "Missing",
              label = list(
                AGE ~ "Age (years)",
                SYSBP ~ "Systolic Blood Pressure (mmHg)",
                DIABETES ~ "Diabetes",
                CURSMOKE ~ "Smoking Status",
                BMI ~ "BMI (kg/m^2)",
                TOTCHOL ~ "Serum Total Cholesterol (mg/dL) ",
                BPMEDS ~ "Anti-hypertensive medication",
                PREVCHD ~ "Prevalent Coronary Heart Disease"
              ),
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  add_overall(last = TRUE)

# Creating Kaplan-Meier curve 
surv_fit <- survfit(Surv(time_10yr, stroke_10yr)~SEX, data=dat_p3_interest)
ggsurvplot(surv_fit, conf.int=TRUE, censor=F, xlim=c(0, max(dat_p3_interest$time_10yr)), ylim = c(0.95, 1.00), legend = "bottom")