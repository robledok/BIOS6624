## Loading packages
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)

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
