########################### Participant Segmentation ###########################

userSegment <- intensities_daily %>%
  group_by(Id) %>%
  summarise(
    avgSedentary = mean(Sedentary_min),
    avgLightActive     = mean(LightActive_min),
    avgModerateActive  = mean(ModerateActive_min),
    avgVeryActive  = mean(VeryActive_min),
    avgActive_min    = mean(TotalActive_min),
    .groups = "drop"
  ) %>%
  mutate(
    userType = case_when(
      avgVeryActive >= 25 ~ "Very Active",
      (avgModerateActive + avgVeryActive) >= 40 ~ "Moderately Active", 
      avgLightActive >= 120 ~ "Lightly Active",
      TRUE ~ "Sedentary"),
    userType = factor(userType,
                      levels = c("Sedentary", "Lightly Active",
                                 "Moderately Active", "Very Active")))

no_userSegment <- userSegment %>% 
  group_by(userType) %>% 
  summarise(
    userCount = n()
  )

################################## Joining #####################################
hourly <- steps_hourly %>% 
  inner_join(intensities_hourly, by = c("Id", "ActivityHour")) %>% 
  inner_join(met_hourly, by = c("Id", "ActivityHour")) %>% 
  inner_join(calories_hourly, by = c("Id", "ActivityHour")) %>% 
  left_join(
    (userSegment %>% select(Id, userType)),
    by = "Id"
  )

hourly_v2 <- hourly %>% 
  mutate(Date = as_date(ActivityHour),
         Hour = as_hms(ActivityHour))

hourly_sleep <- hourly_v2 %>% 
  inner_join(sleep_hourly, by = c("Id", "ActivityHour"))

daily <- steps_daily %>% 
  inner_join(intensities_daily, by = c("Id", "Date")) %>% 
  inner_join(met_daily, by = c("Id", "Date")) %>% 
  inner_join(calories_daily, by = c("Id", "Date")) %>% 
  left_join(
    (userSegment %>% select(Id, userType)),
    by = "Id"
  )

daily_sleep <- daily %>% 
  inner_join(sleep_daily, by = c("Id", "Date"))

dailyActivity_v3 <- dailyActivity_v2 %>% 
  left_join(
    (userSegment %>% select(Id, userType)),
    by = "Id")

dailyActivity_sleep <- dailyActivity_v3 %>% 
  inner_join(sleepDay_v2, by = c("Id", "ActivityDate"))
################################## Results #####################################

# Sleep Quality
sleepQuality <- sleepDay_v2 %>% 
  left_join(userSegment %>% select(Id, userType), by = "Id") %>% 
  group_by(Id, userType) %>% 
  summarise(
    avgSleep_min = mean(TotalMinutesAsleep),
    avgToSleep_min = mean(TotalTimeInBed - TotalMinutesAsleep),
    .groups = "drop"
  ) %>%  
  mutate(Quality = case_when(
    avgSleep_min < 420 ~ "Under Sleep",
    avgSleep_min > 540 ~ "Over Sleep",
    TRUE ~ "Sufficient Sleep"))

sleep_percentage <- sleepQuality %>%
  group_by(Quality) %>%
  summarise(total_users = n()) %>%
  mutate(percentage = (total_users / sum(total_users)) * 100)

avgHourly <- hourly_v2 %>% 
  group_by(Hour, userType) %>% 
  summarise(
    avgStep = round(mean(TotalSteps_h),2),
    avgSedentary = round(mean(Sedentary_min),2),
    avgLightActive = round(mean(LightActive_min),2),
    avgModerateActive = round(mean(ModerateActive_min),2),
    avgVeryActive = round(mean(VeryActive_min),2),
    avgMETs = round(mean(TotalMET_h),2),
    avgCalories = round(mean(TotalCalories_h),2),
    avgActivity = round(mean(TotalActive_min),2),
    .groups = "drop"
  ) 

avgHourlySleep <- hourly_sleep %>% 
  group_by(Hour, userType) %>% 
  summarise(
    avgSleep = round(mean(TotalSleep_min),2),
    .groups = "drop"
  ) 

avgDaily <- daily %>% 
  group_by(userType) %>% 
  summarise(
    avgStep = round(mean(TotalSteps_d),2),
    avgSedentary = round(mean(Sedentary_min),2),
    avgLightActive = round(mean(LightActive_min),2),
    avgModerateActive = round(mean(ModerateActive_min),2),
    avgVeryActive = round(mean(VeryActive_min),2),
    avgMETs = round(mean(TotalMET_d),2),
    avgCalories = round(mean(TotalCalories_d),2),
    avgActivity = round(mean(TotalActive_min),2),
    .groups = "drop"
  )


avgDailySleep <- daily_sleep %>% 
  group_by(userType) %>% 
  summarise(
    avgSleep = round(mean(TotalSleep_min),2),
    .groups = "drop"
  ) 


