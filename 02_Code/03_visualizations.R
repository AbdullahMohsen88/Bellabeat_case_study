################################# Plotting #####################################

userSeg_plot <- ggplot(no_userSegment, aes(x = userType, y = userCount, 
                                           fill = userType)) + 
  geom_col(width = 0.7) +
  geom_label(
    aes(label = paste(userCount)),
    vjust = -0.5,
    size = 3.3,
    fill = "white",
    linewidth = 0,
    label.padding = unit(0.15, "lines")) +
  labs(
    title = "Participant Distribution Across Activity Intensity Category", 
    subtitle = "Based on average minutes spent in each activity intensity category", 
    x = "Participant Segments",
    caption = "Source: Fitbit data 12 Mar 2016 to 12 May 2016.N = 35 participant
    Participants were categorized by the average intensity minutes based on guidlines 
    recommendation from WHO, odphp.") +
  scale_fill_brewer(palette = "Set2") +
  ylim(0, 17) +
  theme_minimal() +
  theme(plot.subtitle = element_text(color = "gray40"),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        plot.caption = element_text(hjust = 0),
        panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5))

ggsave("UserSegments.png", plot = userSeg_plot, width = 8, height = 4.95, dpi = 300)

stepsVScalories_plot <- ggplot(daily, aes(x = TotalSteps_d, y = TotalCalories_d)) +
  geom_point() + 
  geom_smooth() +
  labs(
    title = "Total Steps vs Calories Burned", 
    subtitle = "Relationship between total steps and total calories burned", 
    x = "Total Steps",
    y = "Calories Burned",
    caption = "Source: Fitbit data 12 Mar 2016 to 12 May 2016.N = 35 participant
    The relationship between total steps taken and total calories burned. 
    Positive correlation observed.") +
  theme_minimal() +
  theme(plot.subtitle = element_text(color = "gray40"),
        plot.caption = element_text(hjust = 0),
        panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5))

stepsVSsedentary_plot <- ggplot(daily, aes(x = TotalSteps_d, y = Sedentary_min)) +
  geom_point() + 
  geom_smooth() +
  labs(
    title = "Total Steps vs Sedentary Minutes", 
    subtitle = "Relationship between total steps and sedentary duration", 
    x = "Total Steps",
    y = "Sedentary Minutes",
    caption = "Source: Fitbit data 12 Mar 2016 to 12 May 2016.N = 35 participant
    Negative correlation observed: As sedentary time increases, total steps decrease.") +
  theme_minimal() +
  theme(plot.subtitle = element_text(color = "gray40"),
        plot.caption = element_text(hjust = 0),
        panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5))

ggsave("stepsVSsedentary.png", plot = stepsVSsedentary_plot, width = 8, height = 4.95, 
       dpi = 300)

avgStepsDistance_plot <- dailyActivity_v3 %>%
  group_by(userType) %>% 
  summarise(avgSteps = round(mean(TotalSteps),2),
            avgDistance = mean(TotalDistance * 1000),
            .groups = "drop"   ) %>%
  select(userType, avgSteps, avgDistance) %>%
  pivot_longer(cols = c(avgSteps, avgDistance),
               names_to = "Metric",
               values_to = "value") %>%
  mutate(Metric = case_when(
    Metric == "avgSteps" ~ "Average Steps",
    Metric == "avgDistance" ~ "Average Distance"),
    Metric = factor(Metric, levels = c("Average Steps", "Average Distance"))) %>%
  ggplot(aes(x = userType, y = value, fill = Metric)) +
  geom_col(position = "dodge", width = 0.7, alpha = 0.9) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Daily Average Steps and Distance Comparison by Participant 
       Categories",
       subtitle = "Average Steps and Average Distance in Meters",
       x = "Participant Caregories",
       y = "Value",
       caption = "Source: Fitbit data 12 Mar 2016 to 12 May 2016. N = 35
       participant",
       fill = "") +
  ylim(0, 12500) +
  theme_minimal() +
  theme(plot.subtitle = element_text(color = "gray40"),
        legend.position = "top",
        plot.caption = element_text(hjust = 0),
        panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5))

ggsave("avgStepsDistance.png", plot = avgStepsDistance_plot, width = 8, height = 4.95, 
       dpi = 300)

activeVSmet_plot <- ggplot(daily, aes(x = TotalActive_min, y = TotalMET_d)) +
  geom_point() + 
  geom_smooth() +
  labs(
    title = "Total Active Duration and METs", 
    subtitle = "Visualizing Metabolic Equivalents relative to daily physical activity duration", 
    x = "Total Active Minutes",
    y = "MET Score",
    caption = "Source: Fitbit data 12 Mar 2016 to 12 May 2016. N = 35 participant
    The relationship between total active duration and 
    total metabolic equivalents of task (METs). 
    Positive correlation observed.") +
  theme_minimal() +
  theme(plot.subtitle = element_text(color = "gray40"),
        plot.caption = element_text(hjust = 0),
        panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5))

ggsave("activeVSmet.png", plot = activeVSmet_plot, width = 8, height = 4.95, dpi = 300)

avgActivity_plot <- avgDaily %>% 
  select(userType, avgLightActive, avgModerateActive, avgVeryActive) %>% 
  pivot_longer(
    cols = c(avgLightActive, avgModerateActive, avgVeryActive),
    names_to = "Metric",
    values_to = "value") %>%
  mutate(
    Metric = case_when(
      Metric == "avgLightActive" ~ "Light Intensity",
      Metric == "avgModerateActive" ~ "Fair Intensity",
      Metric == "avgVeryActive" ~ "Vigorous Intensity"),
    Metric = factor(Metric, 
                    levels = c("Light Intensity", "Fair Intensity", "Vigorous Intensity"))) %>%
  ggplot(aes(x = userType, y = value, fill = Metric)) + 
  geom_col(position = "dodge", width = 0.7, alpha = 0.9) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Daily Activity Comparison by Participant Categories",
    subtitle = "Activity Categories Durations",
    x = "Participant Caregories",
    y = "Value in Minutes",
    caption = "Source: Fitbit data 12 Mar 2016 to 12 May 2016. N = 35 participant",
    fill = "") +
  theme_minimal() +
  theme(plot.subtitle = element_text(color = "gray40"),
        legend.position = "top",
        plot.caption = element_text(hjust = 0),
        panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5))

ggsave("avgActivity.png", plot = avgActivity_plot, width = 8, height = 4.95, dpi = 300)

hourlyActive_plot <- ggplot(avgHourly, aes(x = Hour, y = avgActivity)) +
  geom_col(fill = "purple") +
  labs(title = "Distribution of Average Hourly Active Minutes",
       subtitle = "Frequency of average active minutes spent per hour across all participants.",
       y = "Average Active Minutes", 
       x = "Hour of Day",
       caption = "Source: Fitbit data 12 Mar 2016 to 12 May 2016. N = 35 participant") +
  theme_minimal() +
  theme(plot.subtitle = element_text(color = "gray40"),
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(angle = 45),
        panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5))

ggsave("hourlyActive.png", plot = hourlyActive_plot, width = 8, height = 4.95, dpi = 300)

sedentaryVSsleep_plot <- ggplot(dailyActivity_sleep, aes(y = SedentaryMinutes, x = TotalMinutesAsleep)) +
  geom_point() + 
  geom_smooth() +
  labs(
    title = "Sedentary Time VS Sleep Duration", 
    subtitle = "Visualizing sedentary time relation to sleep duration", 
    x = "Sleep Duration",
    y = "Sedentary Minutes",
    caption = "Source: Fitbit data 12 Apr 2016 to 12 May 2016. N = 24 participant
    The relationship between sedentary time and 
    sleep duration.") +
  theme_minimal() +
  theme(plot.subtitle = element_text(color = "gray40"),
        plot.caption = element_text(hjust = 0),
        panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5))

ggsave("sedentaryVSsleep.png", plot = sedentaryVSsleep_plot, width = 8, height = 4.95, dpi = 300)

sleepQuality_plot <- ggplot(sleepQuality, aes(x = userType, fill = Quality)) +
  geom_bar(position = "fill", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Sleep Quality by Participant Categories",
    x = "Participants Categories",
    y = "Sleep Quality",
    fill = "Sleep Quality",
    caption = "Source: Fitbit data. N = 24 participant."
  ) +
  theme_minimal() +
  theme(plot.subtitle = element_text(color = "gray40"),
        legend.position = "top",
        plot.caption = element_text(hjust = 0),
        panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5))

ggsave("sleepQuality.png", plot = sleepQuality_plot, width = 8, height = 4.95, 
       dpi = 300)
