## Loading packages
library(tidyverse)
library(gtsummary)
library(ggplot2)
## Reading in the data
dat_p1 <- read.csv("DataRaw/hiv_6624_final.csv")
## Variable names
names(dat_p1)
## Number of observations
nrow(dat_p1)
## Looking at measurements per person
dat_p1 %>%
  group_by(newid) %>%
  summarize(n = n()) 
## Seeing if everyone has AIDS
dat_p1 %>%
  filter(hivpos == 1) %>%
  nrow()
## Looking at baseline measurements
dat_p1_base <- dat_p1 %>%
  group_by(newid) %>%
  slice(1)
dat_p1_base %>%
  tbl_summary(include = c(-X, -newid),
              by = hard_drugs,
              missing = 'ifany')
## Looking at 2 year measurements
dat_p1_2 <- dat_p1 %>%
  filter(years == 2) %>%
  group_by(newid)
dat_p1_2 %>%
  tbl_summary(include = c(-X, -newid),
              by = hard_drugs,
              missing = 'ifany')
## Looking at viral load over years
dat_plot_vload <- dat_p1 %>%
  filter(VLOAD < 100)
ggplot(dat_plot_vload, aes(x = years, y = VLOAD)) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Year",
    y = "Viral Load",
    title = "Viral Load by Year"
  ) +
  theme_minimal()

## Looking at CD4+ T cell count over years
dat_plot_cd4 <- dat_p1 %>%
  filter(LEU3N < 100)
ggplot(dat_plot_cd4, aes(x = years, y = LEU3N)) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Year",
    y = " CD4+ T cell count",
    title = " CD4+ T cell count by Year"
  ) +
  theme_minimal()
