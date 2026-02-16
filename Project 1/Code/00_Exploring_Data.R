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
## Number of individuals
length(unique(dat_p1$newid))
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
  filter(years == 0)
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

## Building baseline table 1 based on variables of interest
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
              by = hard_drugs,
              label = list(
                age ~ "Age",
                SMOKE ~ "Smoking Status",
                EDUCBAS ~ "Education",
                RACE ~ "Race, Ethnicity"
              ),
              missing = 'ifany',
              missing_text = "(Missing)") %>%
  modify_header(stat_1 = "**No Hard-Drug Usage**  \nN = 649") %>%
  modify_header(stat_2 = "**Hard-Drug Usage**  \nN = 66") %>%
  add_overall(last = TRUE)