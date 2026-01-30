## Loading packages
library(tidyverse)
library(gtsummary)
library(ggplot2)
library(nlme)

## Reading in the data
dat_p0_raw <- read.csv("DataRaw/Project0_Clean_v2.csv")
dat_p0 <- dat_p0_raw

## Converting blanks to NA
dat_p0[dat_p0 == ""] <- NA

## Fill all sleep reported wake times with first value from that day per person
dat_p0 <- dat_p0 %>%
  group_by(SubjectID, DAYNUMB) %>%
  mutate(
    Sleep.Diary.reported.wake.time = ifelse(
      is.na(Sleep.Diary.reported.wake.time),       
      Sleep.Diary.reported.wake.time[!is.na(Sleep.Diary.reported.wake.time)][1], 
      Sleep.Diary.reported.wake.time                      
    )
  ) %>%
  ungroup()