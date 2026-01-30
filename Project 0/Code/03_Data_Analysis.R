## Loading packages
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(gtsummary)

## Reading in the data
dat_p0 <- read.csv("DataProcessed/dat_p0_clean_kr.csv")

##*******************************************************************
## ------------------ RQ 1  ----------------------
##*******************************************************************
##

## Omitting data with NAs in either time
dat_rq1 <- dat_p0 %>%
  filter(!is.na(calc_book_int) & !is.na(calc_mems_int))

## Fitting the lmm model
m1_rq1 <- lmer(calc_mems_int ~ calc_book_int + (1|SubjectID),
              na.action = na.omit,
              data = dat_rq1)

## Looking at model output
summary(m1_rq1)

## Fitted vs predicted
dat_rq1$m1_predict <- predict(m1_rq1)
ggplot(dat_rq1) +
  geom_point(aes(x = calc_book_int, y = calc_mems_int, color = "Observed"), alpha = 0.5) +
  geom_abline(aes(intercept = 0, slope = 1, color = "Ideal"), linewidth = 0.8) +
  geom_line(aes(x = calc_book_int, y = m1_predict, color = "Predicted"), linetype = "dashed", linewidth = 1) +
  scale_color_manual(name = "Legend",
                     values = c("Observed" = "black",
                                "Ideal" = "lightskyblue",
                                "Predicted" = "red")) +
  theme_bw() +
  xlab("Booklet Minutes Since Waking") +
  ylab("Cap Minutes Since Waking") +
  ggtitle("Cap Time vs. Booklet Time") +
  theme(legend.position = "bottom")

## Calculating confidence intervals
confint(m1_rq1)

## Checking QQ-plot and residuals
qqnorm(resid(m1_rq1))
qqline(resid(m1_rq1))

##*******************************************************************
## ------------------ RQ 2  ----------------------
##*******************************************************************
##

## Creating groups based on for booklet time at 30 mins and 600 mins:
## Good adherence: +/- 7.5 minutes
## Adequate adherence: +/- 15 minutes
dat_rq2 <- dat_p0 %>%
  filter(Collection.Sample == 2 | Collection.Sample == 4) %>%
  mutate(
    adherence = case_when(
      abs(calc_book_int - 30)  <= 7.5 | abs(calc_book_int - 600) <= 7.5  ~ "Good adherence",
      abs(calc_book_int - 30)  <= 15  | abs(calc_book_int - 600) <= 15   ~ "Adequate adherence",
      TRUE                                                              ~ "Poor adherence"
    ),
    time_group = case_when(
      Collection.Sample == 2 ~ "30 Minutes",
      Collection.Sample == 4 ~ "10 Hours"
    )
  )
dat_rq2$adherence <- relevel(factor(dat_rq2$adherence), ref = "Good adherence")
dat_rq2$time_group <- relevel(factor(dat_rq2$time_group), ref = "30 Minutes")

## Creating table with proportions based on all observations
dat_rq2 %>%
  tbl_summary(include = adherence,
              label = list(adherence = "Adherence Group"))

## Creating table with proportions based on all observations stratified by time
dat_rq2 %>%
  tbl_summary(by = time_group,
              include = adherence,
              label = list(adherence = "Adherence Group"))

##*******************************************************************
## ------------------ RQ 3  ----------------------
##*******************************************************************
##

## Creating variable for before 30 mins and after based on booklet time
dat_rq3 <- dat_p0 %>%
  filter(!is.na(calc_book_int)) %>%
  mutate(
    pre_30mins = ifelse(calc_book_int < 30, calc_book_int, 30),
    post_30mins = ifelse(calc_book_int >= 30, calc_book_int - 30, 0)
  )

## Seeing if outcomes need log-transformed
hist(dat_rq3$Cortisol..nmol.L.)
hist(dat_rq3$DHEA..nmol.L.)

hist(log(dat_rq3$Cortisol..nmol.L.))
hist(log(dat_rq3$DHEA..nmol.L.))

## Fitting cortisol model
m1_rq3 <- lmer(log(Cortisol..nmol.L.) ~ pre_30mins + post_30mins + (1 | SubjectID),
                REML = TRUE, data = dat_rq3)
summary(m1_rq3)
confint(m1_rq3)

## Checking QQ-plot and residuals
qqnorm(resid(m1_rq3))
qqline(resid(m1_rq3))

ggplot(dat_rq3, aes(x = fitted(m1_rq3), y = resid(m1_rq3))) +
  geom_point(color = "cornflowerblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  labs(
    x = "Fitted values",
    y = "Residuals",
    title = "Fitted vs. Residuals"
  )


## Fitting DHEA model
m2_rq3 <- lmer(log(DHEA..nmol.L.) ~ pre_30mins + post_30mins + (1 | SubjectID),
               REML = TRUE, data = dat_rq3)
summary(m2_rq3)
confint(m2_rq3)

## Checking QQ-plot and residuals
qqnorm(resid(m2_rq3))
qqline(resid(m2_rq3))

ggplot(dat_rq3, aes(x = fitted(m2_rq3), y = resid(m2_rq3))) +
  geom_point(color = "cornflowerblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  labs(
    x = "Fitted values",
    y = "Residuals",
    title = "Fitted vs. Residuals"
  )

## Fitted vs predicted cortisol:
dat_cort_pred <- expand.grid(
  minutes = seq(min(dat_rq3$calc_book_int), max(dat_rq3$calc_book_int), by = 5)
)
dat_cort_pred <- dat_cort_pred %>%
  mutate(
    pre_30mins = ifelse(minutes < 30, minutes, 30),
    post_30mins = ifelse(minutes >= 30, minutes - 30, 0)
  )
dat_cort_pred$fitted <- predict(m1_rq3, newdata = dat_cort_pred,
                               re.form = NA)
ggplot(data = dat_cort_pred, aes(x = minutes, y = exp(fitted))) +
  geom_line(linewidth = 1, col = 'skyblue2') +
  theme_bw() +
  labs(x = "Time Since Waking (Minutes)",
       y = "Cortisol (nmol/L)",
       title = "Estimated Means of Cortisol Over Time") 

## Fitted vs predicted DHEA:
dat_dhea_pred <- expand.grid(
  minutes = seq(min(dat_rq3$calc_book_int), max(dat_rq3$calc_book_int), by = 5)
)
dat_dhea_pred <- dat_dhea_pred %>%
  mutate(
    pre_30mins = ifelse(minutes < 30, minutes, 30),
    post_30mins = ifelse(minutes >= 30, minutes - 30, 0)
  )
dat_dhea_pred$fitted <- predict(m2_rq3, newdata = dat_dhea_pred,
                                re.form = NA)
ggplot(data = dat_dhea_pred, aes(x = minutes, y = exp(fitted))) +
  geom_line(linewidth = 1, col = 'palevioletred2') +
  theme_bw() +
  labs(x = "Time Since Waking (Minutes)",
       y = "DHEA (nmol/L)",
       title = "Estimated Means of DHEA Over Time") 