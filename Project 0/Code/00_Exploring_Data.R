## Loading packages
library(tidyverse)
library(gtsummary)
library(ggplot2)
## Reading in the data
dat_p0 <- read.csv("DataRaw/Project0_Clean_v2.csv")
## Looking at measurements per person
dat_p0 %>%
  group_by(SubjectID) %>%
  summarize(n = n()) 
## Looking at baseline measurements
dat_p0_base <- dat_p0 %>%
  group_by(SubjectID) %>%
  slice(1)
dat_p0_base %>%
  tbl_summary(include = c(Cortisol..ug.dl., Cortisol..nmol.L.,
                          DHEA..pg.dl., DHEA..nmol.L.),
              missing = 'ifany')
## Looking at last measurements
dat_p0_last <- dat_p0 %>%
  group_by(SubjectID) %>%
  slice(12)
dat_p0_last %>%
  tbl_summary(include = c(Cortisol..ug.dl., Cortisol..nmol.L.,
                          DHEA..pg.dl., DHEA..nmol.L.),
              missing = 'ifany')
## Looking at summary statistics of lab values
summary(dat_p0$Cortisol..nmol.L.)
summary(dat_p0$Cortisol..ug.dl.)
summary(dat_p0$DHEA..nmol.L.)
summary(dat_p0$DHEA..pg.dl.)
## Looking at trajectories over day number and collection sample
ggplot(dat_p0, aes(x = Collection.Sample, y = Cortisol..nmol.L., group = SubjectID)) +
  geom_line() +
  theme_bw() +
  facet_wrap(~DAYNUMB) +
  labs(x = "Collection Sample", y = "Cortisol (nmol/L)")
ggplot(dat_p0, aes(x = Collection.Sample, y = DHEA..nmol.L., group = SubjectID)) +
  geom_line() +
  theme_bw() +
  facet_wrap(~DAYNUMB) +
  labs(x = "Collection Sample", y = "DHEA (nmol/L)")