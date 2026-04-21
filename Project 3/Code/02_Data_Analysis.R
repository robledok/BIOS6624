## Loading packages
library(tidyverse)
library(gtsummary)
library(survival)
library(survminer)
library(ggpubr)
library(grid)
library(MASS)
library(dplyr)
library(kableExtra)
library(scales)

## Reading in the data
dat_p3 <- read.csv("DataProcessed/dat_p3_clean_kr.csv")

##*******************************************************************
## ------------------ Creating Baseline Data  ----------------------
##*******************************************************************
##
dat_p3_base <- dat_p3 %>%
  filter(PERIOD == 1)

# Making sure no participants were lost
length(unique(dat_p3$RANDID)) == length(unique(dat_p3_base$RANDID))

# Fixing reference levels
dat_p3_base$CURSMOKE <- relevel(factor(dat_p3_base$CURSMOKE), ref = "Not a current smoker")
dat_p3_base$DIABETES <- relevel(factor(dat_p3_base$DIABETES), ref = "Not a Diabetic")
dat_p3_base$BPMEDS <- relevel(factor(dat_p3_base$BPMEDS), ref = "Not currently used")
dat_p3_base$PREVCHD <- relevel(factor(dat_p3_base$PREVCHD), ref = "Free of disease")

##*******************************************************************
## ------------------ Table 1  ----------------------
##*******************************************************************
##
dat_p3_base$label_stoke_10yr <- factor(dat_p3_base$stroke_10yr, levels = c(0, 1), labels = c("No Stroke", "Stroke"))
dat_p3_base$label_stoke_10yr <- relevel(factor(dat_p3_base$label_stoke_10yr), ref = "Stroke")

tbl1_df <- dat_p3_base %>%
  tbl_summary(by = SEX,
              include = c(time_10yr, label_stoke_10yr, AGE, SYSBP, DIABETES, CURSMOKE, BMI, TOTCHOL, BPMEDS, PREVCHD),
              missing_text = "Missing",
              label = list(
                AGE ~ "Age (years)",
                SYSBP ~ "Systolic Blood Pressure (mmHg)",
                DIABETES ~ "Diabetes",
                CURSMOKE ~ "Smoking Status",
                BMI ~ "Body Mass Index (kg/m²)",
                TOTCHOL ~ "Serum Total Cholesterol (mg/dL) ",
                BPMEDS ~ "Anti-Hypertensive Medication",
                PREVCHD ~ "Prevalent Coronary Heart Disease",
                time_10yr ~ "10-Year Stroke-Free Survival (days)",
                label_stoke_10yr ~ "Incident Stroke During 10-Year Period"
              ),
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  add_overall(last = TRUE) %>%
  as_tibble()

colnames(tbl1_df) <- c(
  "$\\textbf{Characteristic}$",
  "(N = 2,472)",
  "(N = 1,930)",
  "(N = 4,402)"
)

tbl1_df %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .))) %>%
  slice(-c(4, 9, 19, 12, 23)) %>%
  kable(
    align = "lcccc",
    escape = F,
    booktabs = T,
    caption = "<b>Table 1.</b> Baseline characteristics of the Framingham Heart Study"
  ) %>%
  add_header_above(c(
    " " = 1,
    "$\\textbf{Female}$" = 1,
    "$\\textbf{Male}$" = 1,
    "$\\textbf{Overall}$" = 1
  )) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling(bootstrap_options = "condensed") %>%
  row_spec(c(3, 7, 9, 11, 13, 15, 16, 18), extra_css = "padding-left: 20px;") %>%
  footnote(
    general = "Values are presented as mean (SD) for continuous variables and N (%) for categorical variables.",
    general_title = ""
  )
  

##*******************************************************************
## ------------------ Dichotimizing Continuous Variables for Kaplan-Meier Curves  ----------------------
##*******************************************************************
##

# Checking medians
median(dat_p3_base$AGE)
median(dat_p3_base$BMI, na.rm = T)
median(dat_p3_base$TOTCHOL, na.rm = T)

# Creating our categorical variables
dat_p3_base$age_grp <- ifelse(dat_p3_base$AGE <= median(dat_p3_base$AGE), "Below or at Median (49)", "Above Median (49)")
dat_p3_base$sbp_grp <- ifelse(dat_p3_base$SYSBP < 160, "Not High", "High")
dat_p3_base$bmi_grp <- ifelse(dat_p3_base$BMI <= median(dat_p3_base$BMI, na.rm = T), "Below or at Median (25.45)", "Above Median (25.45)")
dat_p3_base$chol_grp <- ifelse(dat_p3_base$TOTCHOL <= median(dat_p3_base$TOTCHOL, na.rm = T), "Below or at Median (234)", "Above Median (234)")

##*******************************************************************
## ------------------ Creating Sex Specific Data  ----------------------
##*******************************************************************
##
dat_p3_fem <- dat_p3_base %>% filter(SEX == "Female")
dat_p3_male <- dat_p3_base %>% filter(SEX == "Male")

##*******************************************************************
## ------------------ Kaplan-Meier Curves for Females (Figure 1)  ----------------------
##*******************************************************************
##

# Age
fem_age <- survfit(Surv(time_10yr, stroke_10yr)~age_grp, data=dat_p3_fem)
fem_age_plot <- ggsurvplot(fem_age, conf.int=FALSE, censor=F, xlim=c(0, 10*365.25), ylim = c(0.8, 1.00),
           palette = c("#ed6779", "#3F6D9E"),
           legend = "bottom",
           xlab = "Time in Days",
           title = "Age (years)",
           legend.title = "",
           legend.labs = c("> Median (49)", "≤ Median (49)"))$plot+
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.4, "cm")
  )

# SBP
fem_sbp <- survfit(Surv(time_10yr, stroke_10yr)~sbp_grp, data=dat_p3_fem)
fem_sbp_plot <- ggsurvplot(fem_sbp, conf.int=FALSE, censor=F, xlim=c(0, 10*365.25), ylim = c(0.8, 1.00),
           palette = c("#ed6779", "#3F6D9E"),              
           legend = "bottom",
           legend.title = "",
           xlab = "Time in Days",
           title = "Systolic Blood Pressure (mmHg)",
           legend.labs = c("High (≥ 160)", "Not High (< 160)"))$plot +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.4, "cm")
  )

# Diabetic
fem_diab <- survfit(Surv(time_10yr, stroke_10yr)~DIABETES, data=dat_p3_fem)
fem_diab_plot <- ggsurvplot(fem_diab, conf.int=FALSE, censor=F, xlim=c(0, 10*365.25), ylim = c(0.8, 1.00),
           palette = c("#3F6D9E", "#ed6779"),
           legend = "bottom",
           legend.title = "",
           xlab = "Time in Days",
           title = "Diabetes",
           legend.labs = c("Not a Diabetic", "Diabetic"))$plot +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.4, "cm")
  )

# Smoking Status
fem_smoke <- survfit(Surv(time_10yr, stroke_10yr)~CURSMOKE, data=dat_p3_fem)
fem_smoke_plot <- ggsurvplot(fem_smoke, conf.int=FALSE, censor=F, xlim=c(0, 10*365.25), ylim = c(0.8, 1.00),
           palette = c("#3F6D9E", "#ed6779"),
           legend = "bottom",
           legend.title = "",
           xlab = "Time in Days",
           title = "Smoking Status",
           legend.labs = c("Not a Current Smoker", "Current Smoker"))$plot +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.4, "cm")
  )

# BMI
fem_bmi <- survfit(Surv(time_10yr, stroke_10yr)~bmi_grp, data=dat_p3_fem)
fem_bmi_plot <- ggsurvplot(fem_bmi, conf.int=FALSE, censor=F, xlim=c(0, 10*365.25), ylim = c(0.8, 1.00),
           palette = c("#ed6779", "#3F6D9E"),
           legend = "bottom",
           legend.title = "",
           xlab = "Time in Days",
           title = "Body Mass Index (kg/m²)",
           legend.labs = c("> Median (25.45)", "≤ Median (25.45)"))$plot +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.4, "cm")
  )

# Cholesterol
fem_chol <- survfit(Surv(time_10yr, stroke_10yr)~chol_grp, data=dat_p3_fem)
fem_chol_plot <- ggsurvplot(fem_chol, conf.int=FALSE, censor=F, xlim=c(0, 10*365.25), ylim = c(0.8, 1.00), 
           palette = c("#ed6779", "#3F6D9E"),
           legend = "bottom",
           legend.title = "",
           xlab = "Time in Days",
           title = "Serum Total Cholesterol (mg/dL)",
           legend.labs = c("> Median (234)", "≤ Median (234)"))$plot +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.4, "cm")
  )

# BP Meds
fem_meds <- survfit(Surv(time_10yr, stroke_10yr)~BPMEDS, data=dat_p3_fem)
fem_meds_plot <- ggsurvplot(fem_meds, conf.int=FALSE, censor=F, xlim=c(0, 10*365.25), ylim = c(0.8, 1.00), 
           palette = c("#3F6D9E", "#ed6779"),  
           legend = "bottom",
           legend.title = "",
           xlab = "Time in Days",
           title = "Anti-Hypertensive Medication",
           legend.labs = c("Not Curently Used", "Currently Used"))$plot +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.4, "cm")
  )

# CHD
fem_chd <- survfit(Surv(time_10yr, stroke_10yr)~PREVCHD, data=dat_p3_fem)
fem_chd_plot <- ggsurvplot(fem_chd, conf.int=FALSE, censor=F, xlim=c(0, 10*365.25), ylim = c(0.8, 1.00), 
           palette = c("#3F6D9E", "#ed6779"),  
           legend = "bottom",
           legend.title = "",
           xlab = "Time in Days",
           title = "Coronary Heart Disease",
           legend.labs = c("Free of Disease", "Prevalent Disease"))$plot +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.4, "cm")
  )

# Combine into a grid
ggarrange(fem_age_plot, fem_sbp_plot, fem_diab_plot, fem_smoke_plot, fem_bmi_plot, fem_chol_plot, fem_meds_plot, fem_chd_plot,
          ncol = 4, nrow = 2) %>%
  annotate_figure(
    top = text_grob(
      "Kaplan–Meier Curves of 10-Year Stroke-Free Survival in Females",
      face = "bold",
      size = 14
    )
  )

##*******************************************************************
## ------------------ Kaplan-Meier Curves for Males (Figure 2)  ----------------------
##*******************************************************************
##

# Age
male_age <- survfit(Surv(time_10yr, stroke_10yr)~age_grp, data=dat_p3_male)
male_age_plot <- ggsurvplot(male_age, conf.int=FALSE, censor=F, xlim=c(0, 10*365.25), ylim = c(0.8, 1.00),
                           palette = c("#ed6779", "#3F6D9E"),
                           legend = "bottom",
                           xlab = "Time in Days",
                           title = "Age (years)",
                           legend.title = "",
                           legend.labs = c("> Median (49)", "≤ Median (49)"))$plot+
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.4, "cm")
  )

# SBP
male_sbp <- survfit(Surv(time_10yr, stroke_10yr)~sbp_grp, data=dat_p3_male)
male_sbp_plot <- ggsurvplot(male_sbp, conf.int=FALSE, censor=F, xlim=c(0, 10*365.25), ylim = c(0.8, 1.00),
                           palette = c("#ed6779", "#3F6D9E"),                
                           legend = "bottom",
                           legend.title = "",
                           xlab = "Time in Days",
                           title = "Systolic Blood Pressure (mmHg)",
                           legend.labs = c("High (≥ 160)" ,"Not High (< 160)"))$plot +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.4, "cm")
  )

# Diabetic
male_diab <- survfit(Surv(time_10yr, stroke_10yr)~DIABETES, data=dat_p3_male)
male_diab_plot <- ggsurvplot(male_diab, conf.int=FALSE, censor=F, xlim=c(0, 10*365.25), ylim = c(0.8, 1.00),
                            palette = c("#3F6D9E", "#ed6779"),  
                            legend = "bottom",
                            legend.title = "",
                            xlab = "Time in Days",
                            title = "Diabetes",
                            legend.labs = c("Not a Diabetic", "Diabetic"))$plot +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.4, "cm")
  )

# Smoking Status
male_smoke <- survfit(Surv(time_10yr, stroke_10yr)~CURSMOKE, data=dat_p3_male)
male_smoke_plot <- ggsurvplot(male_smoke, conf.int=FALSE, censor=F, xlim=c(0, 10*365.25), ylim = c(0.8, 1.00),
                             palette = c("#3F6D9E", "#ed6779"),  
                             legend = "bottom",
                             legend.title = "",
                             xlab = "Time in Days",
                             title = "Smoking Status",
                             legend.labs = c("Not a Current Smoker", "Current Smoker"))$plot +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.4, "cm")
  )

# BMI
male_bmi <- survfit(Surv(time_10yr, stroke_10yr)~bmi_grp, data=dat_p3_male)
male_bmi_plot <- ggsurvplot(male_bmi, conf.int=FALSE, censor=F, xlim=c(0, 10*365.25), ylim = c(0.8, 1.00),
                           palette = c("#ed6779", "#3F6D9E"),  
                           legend = "bottom",
                           legend.title = "",
                           xlab = "Time in Days",
                           title = "Body Mass Index (kg/m²)",
                           legend.labs = c("> Median (25.45)", "≤ Median (25.45)"))$plot +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.4, "cm")
  )

# Cholesterol
male_chol <- survfit(Surv(time_10yr, stroke_10yr)~chol_grp, data=dat_p3_male)
male_chol_plot <- ggsurvplot(male_chol, conf.int=FALSE, censor=F, xlim=c(0, 10*365.25), ylim = c(0.8, 1.00), 
                            palette = c("#ed6779", "#3F6D9E"),  
                            legend = "bottom",
                            legend.title = "",
                            xlab = "Time in Days",
                            title = "Serum Total Cholesterol (mg/dL)",
                            legend.labs = c("> Median (234)", "≤ Median (234)"))$plot +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.4, "cm")
  )

# BP Meds
male_meds <- survfit(Surv(time_10yr, stroke_10yr)~BPMEDS, data=dat_p3_male)
male_meds_plot <- ggsurvplot(male_meds, conf.int=FALSE, censor=F, xlim=c(0, 10*365.25), ylim = c(0.8, 1.00), 
                            palette = c("#3F6D9E", "#ed6779"),  
                            legend = "bottom",
                            legend.title = "",
                            xlab = "Time in Days",
                            title = "Anti-Hypertensive Medication",
                            legend.labs = c("Not Curently Used", "Currently Used"))$plot +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.4, "cm")
  )

# CHD
male_chd <- survfit(Surv(time_10yr, stroke_10yr)~PREVCHD, data=dat_p3_male)
male_chd_plot <- ggsurvplot(male_chd, conf.int=FALSE, censor=F, xlim=c(0, 10*365.25), ylim = c(0.8, 1.00), 
                           palette = c("#3F6D9E", "#ed6779"),  
                           legend = "bottom",
                           legend.title = "",
                           xlab = "Time in Days",
                           title = "Coronary Heart Disease",
                           legend.labs = c("Free of Disease", "Prevalent Disease"))$plot +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.4, "cm")
  )

# Combine into a grid
ggarrange(male_age_plot, male_sbp_plot, male_diab_plot, male_smoke_plot, male_bmi_plot, male_chol_plot, male_meds_plot, male_chd_plot,
          ncol = 4, nrow = 2) %>%
  annotate_figure(
    top = text_grob(
      "Kaplan–Meier Curves of 10-Year Stroke-Free Survival in Males",
      face = "bold",
      size = 14
    )
  )

##*******************************************************************
## ------------------ Backward Selection for Female by AIC  ----------------------
##*******************************************************************
##

# Defining out variables
vars <- c(
  "time_10yr", "stroke_10yr",
  "AGE", "DIABETES", "SYSBP",
  "CURSMOKE", "BMI", "TOTCHOL",
  "BPMEDS", "PREVCHD"
)

dat_cox_fem <- dat_p3_fem[complete.cases(dat_p3_fem[, vars]), ]

# Number of observations retained
nrow(dat_cox_fem)
nrow(dat_p3_fem) - nrow(dat_cox_fem)

# Base model
fem_base_mod <- coxph(
  Surv(time_10yr, stroke_10yr) ~ AGE + DIABETES + SYSBP,
  data = dat_cox_fem
)

# Full model
fem_full_mod <- coxph(
  Surv(time_10yr, stroke_10yr) ~ AGE + DIABETES + SYSBP + CURSMOKE + BMI + TOTCHOL + BPMEDS + PREVCHD,
  data = dat_cox_fem
)

# Backwards selection model
fem_step_mod <- stepAIC(
  fem_full_mod,
  direction = "backward",
  scope = list(
    lower = ~ AGE + DIABETES + SYSBP,
    upper = ~ AGE + DIABETES + SYSBP + CURSMOKE + BMI + TOTCHOL + BPMEDS + PREVCHD
  ),
  trace = 1
)


##*******************************************************************
## ------------------ Backward Selection for Male by AIC  ----------------------
##*******************************************************************
##

# Defining out variables
vars <- c(
  "time_10yr", "stroke_10yr",
  "AGE", "DIABETES", "SYSBP",
  "CURSMOKE", "BMI", "TOTCHOL",
  "BPMEDS", "PREVCHD"
)

dat_cox_male <- dat_p3_male[complete.cases(dat_p3_male[, vars]), ]

# Number of observations retained
nrow(dat_cox_male)
nrow(dat_p3_male) - nrow(dat_cox_male)

# Base model
male_base_mod <- coxph(
  Surv(time_10yr, stroke_10yr) ~ AGE + DIABETES + SYSBP,
  data = dat_cox_male
)

# Full model
male_full_mod <- coxph(
  Surv(time_10yr, stroke_10yr) ~ AGE + DIABETES + SYSBP + CURSMOKE + BMI + TOTCHOL + BPMEDS + PREVCHD,
  data = dat_cox_male
)

# Backwards selection model
male_step_mod <- stepAIC(
  male_full_mod,
  direction = "backward",
  scope = list(
    lower = ~ AGE + DIABETES + SYSBP,
    upper = ~ AGE + DIABETES + SYSBP + CURSMOKE + BMI + TOTCHOL + BPMEDS + PREVCHD
  ),
  trace = 1
)

##*******************************************************************
## ------------------ Final Models  ----------------------
##*******************************************************************
##

fem_mod <- coxph(
  Surv(time_10yr, stroke_10yr) ~ AGE + DIABETES + SYSBP + CURSMOKE,
  data = dat_p3_fem
)
fem_mod

male_mod <- coxph(
  Surv(time_10yr, stroke_10yr) ~ AGE + DIABETES + SYSBP + CURSMOKE,
  data = dat_p3_male
)
male_mod

##*******************************************************************
## ------------------ Checking Multicollinearity ----------------------
##*******************************************************************
## 

rms::vif(fem_mod)
rms::vif(male_mod)

##*******************************************************************
## ------------------ Results Table (Table 2) ----------------------
##*******************************************************************
## 

mod_results <- data.frame(
  Parameter = rep(c("Age", "Diabetic", "Systolic Blood Pressure", "Current Smoker"), 2),
  exp_coef = c(summary(fem_mod)$conf.int[ , 1], summary(male_mod)$conf.int[ , 1]),
  lower_ci = c(summary(fem_mod)$conf.int[ , 3], summary(male_mod)$conf.int[ , 3]),
  upper_ci = c(summary(fem_mod)$conf.int[ , 4], summary(male_mod)$conf.int[ , 4]),
  pval = c(summary(fem_mod)$coefficients[ , 5], summary(male_mod)$coefficients[ , 5])
)

mod_results$ci <- sprintf("(%.2f, %.2f)", mod_results$lower_ci, mod_results$upper_ci)

mod_results %>%
  dplyr::select(Parameter, exp_coef, ci, pval) %>%
  mutate(
    exp_coef = round(as.numeric(exp_coef), 2),
    pval = format.pval(pval, digits = 3, eps = 0.001)
  ) %>%
  kable(
    align = "lccc",
    col.names = c("Variable", "HR", "95% CI", "p-value"),
    escape = FALSE,
    caption = "<b>Table 2. </b> Adjusted hazard ratios (HR), 95% confidence intervals (CI), and p-values from Cox proportional hazards models for stroke-free survival in the Framingham Heart Study by sex"
  ) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling(bootstrap_options = "condensed") %>%
  row_spec(0, bold = T) %>%
  column_spec(1, width = "12em") %>%  
  column_spec(2:4, width = "10em") %>%  
  group_rows("Females", 1, 4) %>%
  group_rows("Males", 5, 8)


##*******************************************************************
## ------------------ Checking Schoenfeld residuals ----------------------
##*******************************************************************
##

# Females
fem_ph_res <- cox.zph(fem_mod)
fem_ph_res
# Plotting by age, diabetes, blood pressure, and smoking status
ggcoxzph(fem_ph_res, se=F, var="AGE")
ggcoxzph(fem_ph_res, se=F, var="DIABETES")
ggcoxzph(fem_ph_res, se=F, var="SYSBP")
ggcoxzph(fem_ph_res, se=F, var="CURSMOKE")

# Males
male_ph_res <- cox.zph(male_mod)
male_ph_res
# Plotting by age, diabetes, blood pressure, and smoking status
ggcoxzph(male_ph_res, se=F, var="AGE")
ggcoxzph(male_ph_res, se=F, var="DIABETES")
ggcoxzph(male_ph_res, se=F, var="SYSBP")
ggcoxzph(male_ph_res, se=F, var="CURSMOKE")

##*******************************************************************
## ------------------ Female Risk Profiles ----------------------
##*******************************************************************
##

# Ages we are interested in
ages <- c(40, 50, 60)

# SBP groups
fem_sbp_low <- median(dat_p3_fem$SYSBP[dat_p3_fem$SYSBP < 160], na.rm = TRUE)
fem_sbp_high <- median(dat_p3_fem$SYSBP[dat_p3_fem$SYSBP >= 160], na.rm = TRUE)

# Healthy group
fem_health <- data.frame(
  AGE = ages,
  DIABETES = rep("Not a Diabetic", 3),
  SYSBP = rep(fem_sbp_low, 3),
  CURSMOKE = rep("Not a current smoker", 3)
)

fem_health_surv <- survfit(fem_mod, newdata = fem_health)
fem_health_surv_dat <- data.frame(
  age = ages,
  scenario = rep("Healthy", 3),
  surv_est = c(summary(fem_health_surv, times = 10*365.25)$surv*100)
)

# High blood pressure group
fem_high_sbp <- data.frame(
  AGE = ages,
  DIABETES = rep("Not a Diabetic", 3),
  SYSBP = rep(fem_sbp_high, 3),
  CURSMOKE = rep("Not a current smoker", 3)
)

fem_high_sbp_surv <- survfit(fem_mod, newdata = fem_high_sbp)
fem_high_sbp_surv_dat <- data.frame(
  age = ages,
  scenario = rep("High Systolic Blood Pressure", 3),
  surv_est = c(summary(fem_high_sbp_surv, times = 10*365.25)$surv*100)
)

# Diabetic group
fem_diab <- data.frame(
  AGE = ages,
  DIABETES = rep("Diabetic", 3),
  SYSBP = rep(fem_sbp_low, 3),
  CURSMOKE = rep("Not a current smoker", 3)
)

fem_diab_surv <- survfit(fem_mod, newdata = fem_diab)
fem_diab_surv_dat <- data.frame(
  age = ages,
  scenario = rep("Diabetic", 3),
  surv_est = c(summary(fem_diab_surv, times = 10*365.25)$surv*100)
)

# High SBP and Diabetic group
fem_high_diab <- data.frame(
  AGE = ages,
  DIABETES = rep("Diabetic", 3),
  SYSBP = rep(fem_sbp_high, 3),
  CURSMOKE = rep("Not a current smoker", 3)
)

fem_high_diab_surv <- survfit(fem_mod, newdata = fem_high_diab)
fem_high_diab_surv_dat <- data.frame(
  age = ages,
  scenario = rep("High Systolic Blood Pressure and Diabetic", 3),
  surv_est = c(summary(fem_high_diab_surv, times = 10*365.25)$surv*100)
)

# Smoker group
fem_smoke <- data.frame(
  AGE = ages,
  DIABETES = rep("Not a Diabetic", 3),
  SYSBP = rep(fem_sbp_high, 3),
  CURSMOKE = rep("Current smoker", 3)
)

fem_smoke_surv <- survfit(fem_mod, newdata = fem_smoke)
fem_smoke_surv_dat <- data.frame(
  age = ages,
  scenario = rep("Smoker", 3),
  surv_est = c(summary(fem_smoke_surv, times = 10*365.25)$surv*100)
)

# Combining into one dataframe:
fem_all_surv_dat <- rbind(
  fem_health_surv_dat,
  fem_high_sbp_surv_dat,
  fem_diab_surv_dat,
  fem_high_diab_surv_dat,
  fem_smoke_surv_dat
)

# Making table with estimates
fem_surv_wide <- fem_all_surv_dat %>%
  pivot_wider(
    names_from = age,
    values_from = surv_est
  )

##*******************************************************************
## ------------------ Male Risk Profiles ----------------------
##*******************************************************************
##

# Ages we are interested in
ages <- c(40, 50, 60)

# SBP groups
male_sbp_low <- median(dat_p3_male$SYSBP[dat_p3_male$SYSBP < 160], na.rm = TRUE)
male_sbp_high <- median(dat_p3_male$SYSBP[dat_p3_male$SYSBP >= 160], na.rm = TRUE)

# Healthy group
male_health <- data.frame(
  AGE = ages,
  DIABETES = rep("Not a Diabetic", 3),
  SYSBP = rep(male_sbp_low, 3),
  CURSMOKE = rep("Not a current smoker", 3)
)

male_health_surv <- survfit(male_mod, newdata = male_health)
male_health_surv_dat <- data.frame(
  age = ages,
  scenario = rep("Healthy", 3),
  surv_est = c(summary(male_health_surv, times = 10*365.25)$surv*100)
)

# High blood pressure group
male_high_sbp <- data.frame(
  AGE = ages,
  DIABETES = rep("Not a Diabetic", 3),
  SYSBP = rep(male_sbp_high, 3),
  CURSMOKE = rep("Not a current smoker", 3)
)

male_high_sbp_surv <- survfit(male_mod, newdata = male_high_sbp)
male_high_sbp_surv_dat <- data.frame(
  age = ages,
  scenario = rep("High Systolic Blood Pressure", 3),
  surv_est = c(summary(male_high_sbp_surv, times = 10*365.25)$surv*100)
)

# Diabetic group
male_diab <- data.frame(
  AGE = ages,
  DIABETES = rep("Diabetic", 3),
  SYSBP = rep(male_sbp_low, 3),
  CURSMOKE = rep("Not a current smoker", 3)
)

male_diab_surv <- survfit(male_mod, newdata = male_diab)
male_diab_surv_dat <- data.frame(
  age = ages,
  scenario = rep("Diabetic", 3),
  surv_est = c(summary(male_diab_surv, times = 10*365.25)$surv*100)
)

# High SBP and Diabetic group
male_high_diab <- data.frame(
  AGE = ages,
  DIABETES = rep("Diabetic", 3),
  SYSBP = rep(male_sbp_high, 3),
  CURSMOKE = rep("Not a current smoker", 3)
)

male_high_diab_surv <- survfit(male_mod, newdata = male_high_diab)
male_high_diab_surv_dat <- data.frame(
  age = ages,
  scenario = rep("High Systolic Blood Pressure and Diabetic", 3),
  surv_est = c(summary(male_high_diab_surv, times = 10*365.25)$surv*100)
)

# Smoker group
male_smoke <- data.frame(
  AGE = ages,
  DIABETES = rep("Not a Diabetic", 3),
  SYSBP = rep(male_sbp_high, 3),
  CURSMOKE = rep("Current smoker", 3)
)

male_smoke_surv <- survfit(male_mod, newdata = male_smoke)
male_smoke_surv_dat <- data.frame(
  age = ages,
  scenario = rep("Smoker", 3),
  surv_est = c(summary(male_smoke_surv, times = 10*365.25)$surv*100)
)

# Combining into one dataframe:
male_all_surv_dat <- rbind(
  male_health_surv_dat,
  male_high_sbp_surv_dat,
  male_diab_surv_dat,
  male_high_diab_surv_dat,
  male_smoke_surv_dat
)

# Making table with estimates
male_surv_wide <- male_all_surv_dat %>%
  pivot_wider(
    names_from = age,
    values_from = surv_est
  )
  
  
##*******************************************************************
## ------------------ Risk Profiles Table (Table 3) ----------------------
##*******************************************************************
##  
surv_wide <- rbind(fem_surv_wide, male_surv_wide)

surv_wide %>%
  kable(digits = 2,
        col.names = c("Risk Profile", "40 Years Old", "50 Years Old", "60 Years Old"),
        align = 'lccc',
        caption = "<b>Table 3.</b> 10-year stroke-free survival probabilities (%) by risk profile, age, and sex in the Framingham Heart Study",
        escape = F) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling(bootstrap_options = "condensed") %>%
  column_spec(1, width = "20em") %>%
  column_spec(2:4, width = "8em") %>%
  row_spec(0, bold = T) %>%
  group_rows("Females", 1, 5) %>%
  group_rows("Males", 6, 10)


##*******************************************************************
## ------------------ Looking at Covariates by Period for Each Sex (Table 4) ----------------------
##*******************************************************************
##

# Creating a summary data frame
period_sum_dat <- dat_p3 %>%
  group_by(SEX, PERIOD) %>%
  summarise(
    n_total = n(),
    mean_sbp = mean(SYSBP, na.rm = T),
    sd_sbp = sd(SYSBP, na.rm = T),
    n_diab = sum(DIABETES == 'Diabetic', na.rm = T),
    tot_diab = sum(!is.na(DIABETES)),
    diab_prop = n_diab/tot_diab,
    
    .groups = "drop"
  )

# Calculating percent change between periods
period_change_dat <- period_sum_dat %>%
  arrange(SEX, PERIOD) %>%
  group_by(SEX) %>%
  mutate(
    sbp_pct_change = paste0(round(100 * (mean_sbp - lag(mean_sbp)) / lag(mean_sbp), 2), "%"), # Used ChatGPT to help
    diab_pct_change  = paste0(round(100 * (diab_prop - lag(diab_prop)) / lag(diab_prop), 2), "%") # Used ChatGPT to help
  ) %>%
  ungroup()

period_change_dat %>%
  mutate(
    SBP = paste0(round(mean_sbp,0), " (", round(sd_sbp,0), ")"),
    Diabetes = paste0(n_diab, " (", round(100 * diab_prop,1), "%)"),
    Period = paste0("Period ", PERIOD, " (N=", comma(n_total), ")"),
    sbp_pct_change = ifelse(sbp_pct_change == "NA%", "–", sbp_pct_change),
    
    diab_pct_change = ifelse(diab_pct_change == "NA%", "–", diab_pct_change)
  )  %>%
  dplyr::select(Period, SBP, sbp_pct_change, Diabetes, diab_pct_change) %>%
  kable(
    align = 'lcccc',
    digits = 2,
    col.names = c(
      "",
      "SBP",
      "SBP % Change",
      "Diabetes",
      "Diabetes % Change"
    ),
    caption = c("<b>Table 4.</b> Changes in systolic blood pressure and diabetes prevalence across study periods by sex, with percent change estimates, in the Framingham Heart Study.")
  ) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling(bootstrap_options = "condensed") %>%
  row_spec(0, bold = T) %>%
  group_rows("Females", 1, 3) %>%
  group_rows("Males", 4, 6) %>%
  footnote(
    general = "Values are presented as mean (SD) or n (%). Percent change reflects relative change from the previous study period. SBP refers to systolic blood pressure (mmHg) and diabetes represents those who are diabetic.",
    general_title = ""
  )