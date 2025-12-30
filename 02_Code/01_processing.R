################################# Environments #################################
# Load packages
library(tidyverse)  
library(lubridate)  
library(data.table)  
library(dplyr)
library(janitor)
library(ggplot2)
library(hms)

################################## Loading #####################################

# Standardize IDs and dates across all
standardize_id <- function(df) {
  df %>% 
    mutate(Id = as.character(Id))}

read_and_combine <- function(pattern1, pattern2, 
                             path1 = "~/R/Case_Studies/Bellabeat/Data/Raw/Fitabase_Data_3.12.16-4.11.16", 
                             path2 = "~/R/Case_Studies/Bellabeat/Data/Raw/Fitabase_Data_4.12.16-5.12.16") {
  fread(file.path(path1, pattern1)) %>% 
    bind_rows(fread(file.path(path2, pattern2))) %>% 
    as_tibble()}

# Reading datasets
# Daily-level
dailyActivity <- read_and_combine("dailyActivity_merged.csv", "dailyActivity_merged.csv")

setwd("~/R/Case_Studies/Bellabeat/Data/Raw/Fitabase_Data_4.12.16-5.12.16")

sleepDay <- read.csv("sleepDay_merged.csv")

# Minute-level
calories_min <- read_and_combine("minuteCaloriesNarrow_merged.csv", "minuteCaloriesNarrow_merged.csv")
intensities_min <- read_and_combine("minuteIntensitiesNarrow_merged.csv","minuteIntensitiesNarrow_merged.csv")
steps_min <- read_and_combine("minuteStepsNarrow_merged.csv", "minuteStepsNarrow_merged.csv")
met_min <- read_and_combine("minuteMETsNarrow_merged.csv", "minuteMETsNarrow_merged.csv")
sleep_min <- read_and_combine("minuteSleep_merged.csv", "minuteSleep_merged.csv")

# HR & Weight
heartrate_sec <- read_and_combine("heartrate_seconds_merged.csv", "heartrate_seconds_merged.csv")

weight <- read_and_combine("weightLogInfo_merged.csv", "weightLogInfo_merged.csv")

################################# Exploring ####################################
head(daily_activity)
n_distinct(daily_activity$Id)

head(sleep_day)
n_distinct(sleep_day$Id)

head(calories_min)
n_distinct(calories_min$Id)

head(intensities_min)
n_distinct(intensities_min$Id)

head(steps_min)
n_distinct(steps_min$Id)

head(sleep_min)
n_distinct(sleep_min$Id)

head(met_min)
n_distinct(met_min$Id)

head(heartrate_sec)
n_distinct(heartrate_sec$Id)

head(weight)
n_distinct(weight$Id)

#check NA and duplicates
colSums(is.na(dailyActivity))
sum(duplicated(dailyActivity))

colSums(is.na(sleepDay))
sum(duplicated(sleepDay))

colSums(is.na(calories_min))
sum(duplicated(calories_min))

colSums(is.na(intensities_min))
sum(duplicated(intensities_min))

colSums(is.na(steps_min))
sum(duplicated(steps_min))

colSums(is.na(sleep_min))
sum(duplicated(sleep_min))

colSums(is.na(met_min))
sum(duplicated(met_min))

colSums(is.na(heartrate_sec))
sum(duplicated(heartrate_sec))

colSums(is.na(weight))
sum(duplicated(weight))









########### Standardization of Ids, date and dealing with duplicates############


dailyActivity_v2 <- dailyActivity %>% 
  standardize_id() %>% 
  mutate(ActivityDate = mdy(ActivityDate)) %>% 
  select(Id, ActivityDate, SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes,
         VeryActiveMinutes, TotalSteps, TotalDistance, Calories) %>% 
  distinct()

str(daily_activity_v2)

write_csv(dailyActivity_v2,"~/R/Case_Studies/Bellabeat/Data/Processed/dailyActivity_Cleaned_v2.csv")

sleepDay_v2 <- sleepDay %>% 
  standardize_id() %>% 
  mutate(ActivityDate = as_date(mdy_hms(SleepDay))) %>% 
  select(Id, ActivityDate, TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>% 
  distinct()

str(sleep_day_v2)

write_csv(sleepDay_v2,"~/R/Case_Studies/Bellabeat/Data/Processed/sleepDay_Cleaned_v2.csv")

calories_min_v2 <- calories_min %>% 
  standardize_id() %>% 
  mutate(ActivityMinute = mdy_hms(ActivityMinute)) %>% 
  distinct()

str(calories_min_v2)

write_csv(calories_min_v2,"~/R/Case_Studies/Bellabeat/Data/Processed/calories_min_Cleaned_v2.csv")

intensities_min_v2 <- intensities_min %>% 
  standardize_id() %>% 
  mutate(ActivityMinute = mdy_hms(ActivityMinute)) %>% 
  distinct()

str(intensities_min_v2)

write_csv(intensities_min_v2,"~/R/Case_Studies/Bellabeat/Data/Processed/intensities_min_Cleaned_v2.csv")

steps_min_v2 <- steps_min %>% 
  standardize_id() %>% 
  mutate(ActivityMinute = mdy_hms(ActivityMinute)) %>% 
  distinct()

str(steps_min_v2)

write_csv(steps_min_v2,"~/R/Case_Studies/Bellabeat/Data/Processed/steps_min_Cleaned_v2.csv")

met_min_v2 <- met_min %>% 
  standardize_id() %>% 
  mutate(ActivityMinute = mdy_hms(ActivityMinute),
         METs = METs/10) %>%
  distinct()

str(met_min_v2)

write_csv(met_min_v2,"~/R/Case_Studies/Bellabeat/Data/Processed/met_min_Cleaned_v2.csv")

sleep_min_v2 <- sleep_min %>% 
  standardize_id() %>% 
  mutate(ActivityMinute = mdy_hms(date),
         logId = as.character(logId)) %>% 
  select(Id, ActivityMinute, value, logId) %>% 
  distinct()

str(sleep_min_v2)

write_csv(sleep_min_v2,"~/R/Case_Studies/Bellabeat/Data/Processed/sleep_min_Cleaned_v2.csv")

#################################### EDA  ######################################

daily_activity_v2 %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes) %>%
  summary()

# average steps daily = 7281, 
# average distance = 5.22 km, 
# average sedentary minutes = 992.5

sleep_day_v2 %>%  
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed) %>%
  summary()

# Average sleep records = 1.12, 
# average minutes asleep = 419.47, 
# average time in bed = 458.64

sleep_day_v2 %>% 
  summarise(
    avgTosleep = round(mean(TotalTimeInBed - TotalMinutesAsleep),2),
    avgSleep = round(mean(TotalMinutesAsleep),2)
  )

calories_min_v2 %>% 
  summary()

intensities_min_v2 %>% 
  summary()

steps_min_v2 %>% 
  summary()

met_min_v2 %>% 
  summary()

############################### Aggregations ###################################
calories_hourly <- calories_min_v2 %>% 
  mutate(ActivityHour = floor_date(ActivityMinute, "hour")) %>%
  group_by(Id, ActivityHour) %>%
  summarise(
    TotalCalories_h = sum(Calories),
    .groups = "drop"
  )

write_csv(calories_hourly,
          "~/R/Case_Studies/Bellabeat/Data/Processed/Aggregated/calories_hourly.csv")

calories_daily <- calories_hourly %>% 
  mutate(Date = as_date(ActivityHour)) %>% 
  group_by(Id, Date) %>% 
  summarise(
    TotalCalories_d = sum(TotalCalories_h),
    .groups = "drop"
  )

write_csv(calories_daily,
          "~/R/Case_Studies/Bellabeat/Data/Processed/Aggregated/calories_daily.csv")

intensities_hourly <- intensities_min_v2 %>% 
  mutate(ActivityHour = floor_date(ActivityMinute, "hour")) %>%
  group_by(Id, ActivityHour) %>%
  summarise(
    Sedentary_min = sum(Intensity == 0),
    LightActive_min     = sum(Intensity == 1),
    ModerateActive_min  = sum(Intensity == 2),
    VeryActive_min  = sum(Intensity == 3),
    TotalActive_min = LightActive_min + ModerateActive_min + VeryActive_min,
    .groups = "drop"
  )

write_csv(intensities_hourly,
          "~/R/Case_Studies/Bellabeat/Data/Processed/Aggregated/intensities_hourly.csv")

intensities_daily <- intensities_hourly %>% 
  mutate(Date = as_date(ActivityHour)) %>% 
  group_by(Id, Date) %>% 
  summarise(
    Sedentary_min = sum(Sedentary_min),
    LightActive_min     = sum(LightActive_min),
    ModerateActive_min  = sum(ModerateActive_min),
    VeryActive_min  = sum(VeryActive_min),
    TotalActive_min = LightActive_min + ModerateActive_min + VeryActive_min,
    .groups = "drop"
  )

write_csv(intensities_daily,
          "~/R/Case_Studies/Bellabeat/Data/Processed/Aggregated/intensities_daily.csv")

steps_hourly <- steps_min_v2 %>% 
  mutate(ActivityHour = floor_date(ActivityMinute, "hour")) %>%
  group_by(Id, ActivityHour) %>%
  summarise(
    TotalSteps_h = sum(Steps),
    .groups = "drop"
  )

write_csv(steps_hourly,
          "~/R/Case_Studies/Bellabeat/Data/Processed/Aggregated/steps_hourly.csv")

steps_daily <- steps_hourly %>% 
  mutate(Date = as_date(ActivityHour)) %>% 
  group_by(Id, Date) %>% 
  summarise(
    TotalSteps_d = sum(TotalSteps_h),
    .groups = "drop"
  )

write_csv(steps_daily,
          "~/R/Case_Studies/Bellabeat/Data/Processed/Aggregated/steps_daily.csv")

met_hourly <- met_min_v2 %>% 
  mutate(ActivityHour = floor_date(ActivityMinute, "hour")) %>%
  group_by(Id, ActivityHour) %>%
  summarise(
    TotalMET_h = sum(METs) / 10,
    .groups = "drop"
  )

write_csv(met_hourly,
          "~/R/Case_Studies/Bellabeat/Data/Processed/Aggregated/met_hourly.csv")

met_daily <- met_hourly %>% 
  mutate(Date = as_date(ActivityHour)) %>% 
  group_by(Id, Date) %>% 
  summarise(
    TotalMET_d = sum(TotalMET_h),
    .groups = "drop"
  )

write_csv(met_daily,
          "~/R/Case_Studies/Bellabeat/Data/Processed/Aggregated/met_daily.csv")

sleep_hourly <- sleep_min_v2 %>% 
  filter(value == 1) %>%
  mutate(ActivityHour = floor_date(ActivityMinute, "hour")) %>% 
  count(Id, ActivityHour, name = "TotalSleep_min")

write_csv(sleep_hourly,
          "~/R/Case_Studies/Bellabeat/Data/Processed/Aggregated/sleep_hourly.csv")

sleep_daily <- sleep_hourly %>% 
  mutate(Date = as_date(ActivityHour)) %>% 
  group_by(Id, Date) %>% 
  summarise(
    TotalSleep_min = sum(TotalSleep_min),
    .groups = "drop"
  )

write_csv(sleep_daily,
          "~/R/Case_Studies/Bellabeat/Data/Processed/Aggregated/sleep_daily.csv")