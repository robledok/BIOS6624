## Loading packages
library(tidyverse)

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

## Formatting as times (used ChatGPT to help with first step)
dat_p0$Sleep.Diary.reported.wake.time <- as_hms(parse_date_time(dat_p0$Sleep.Diary.reported.wake.time, orders = "H:M"))
dat_p0$Booket..Clock.Time <- as_hms(parse_date_time(dat_p0$Booket..Clock.Time, orders = "H:M"))
dat_p0$MEMs..Clock.Time <- as_hms(parse_date_time(dat_p0$MEMs..Clock.Time, orders = "H:M"))
dat_p0$calc_book_int <- as.numeric(dat_p0$Booket..Clock.Time - dat_p0$Sleep.Diary.reported.wake.time, unit = "mins")
dat_p0$calc_mems_int <- as.numeric(dat_p0$MEMs..Clock.Time - dat_p0$Sleep.Diary.reported.wake.time, unit = "mins")

## RANGES FOR LABS PER PI
## Use nmol/L
## Cortisol: anything over 26 is high, but exclude over 80
## DHEA: upper detection limit of 5.205 and exclude them and let her know who

## Identifying subjects with upper detection for DHEA to notify PI
dat_p0 %>%
  filter(DHEA..nmol.L. == 5.205) %>%
  pull(SubjectID) %>%
  unique()

## Excluding cortisol over 80 and DHEA of 5.205
dat_p0 <- dat_p0 %>%
  filter(Cortisol..nmol.L. < 80 & DHEA..nmol.L. < 5.205)

## Calculating number of observations excluded
nrow(dat_p0_raw) - nrow(dat_p0)

## Identifying subjects with cortisol over 26
dat_p0 %>%
  filter(Cortisol..nmol.L. > 26) %>%
  pull(SubjectID) %>%
  unique()

## Saving new dataset
write.csv(dat_p0, "DataProcessed/dat_p0_clean_kr.csv")