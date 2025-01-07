#Cleaned R script 
pacman::p_load(tidyverse, ggplot2, gganimate, flexdashboard, mosaic, moderndive, effectsize, modelr, rsample, car,ggridges)

Traffic_Log <- innerjoin(randomized_banana_consumption_log, Traffic_Log, by = "date_id")

daily_consumption <- randomized_banana_consumption_log %>%
  group_by(date_id) %>%
  summarise(total_bananas = sum(bananas_consumed))

consumption_date <- inner_join(daily_consumption, Date_Dim, by = "date_id")
consumption_date$date_id <- as.Date(as.character(consumption_date$date_id), format="%Y%m%d")

consumption_traffic <- inner_join(Consumption_Log, Traffic_Log, by = "date_id", "time period")

# Check for duplicates in Traffic_Log
Traffic_Log %>%
  count(date_id, `time_period`) %>%
  filter(n > 1)

Traffic_Log <- Traffic_Log %>% rename(time_period = `time_period`)
consumption_traffic <- inner_join(
  Consumption_Log, 
  Traffic_Log, 
  by = c("date_id" = "date_id", "time_period" = "time_period")
)

write.csv(consumption_traffic, "consumption_traffic.csv", row.names = FALSE)

#1st visuilzation directly showcases that is weekend has an effect on the conssumption of bannas over time 
ggplot(consumption_date, aes(x = as.Date(date_id), y = total_bananas)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(color = "orange", size = 2, alpha = 0.6) +  
  facet_wrap(~is_weekend, labeller = as_labeller(c(`0` = "Non-Weekend", `1` = "Weekend"))) +
  scale_y_continuous(
    limits = c(0, 20),  
    breaks = seq(0, 20, by = 5)  
  ) +
  labs(title = "Daily Banana Consumption Over Time",
       x = "Date",
       y = "Total Bananas Consumed") +
  theme_dark() +
  theme(
    panel.grid.major = element_line(color = "lightgrey", size = 0.5),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "lightblue", color = "darkblue"),
    strip.text = element_text(color = "white", face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.y = element_text(color = "black") 
  )




# Run t-test to compare weekend vs non-weekend consumption to confirm whether or not there is a sigifctn time period 
t.test(total_bananas ~ is_weekend, data = consumption_date, var.equal = FALSE)

ggplot(consumption_date, aes(x = as.factor(is_weekend), y = total_bananas, fill = as.factor(is_weekend))) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.1) +
  labs(
    title = "Distribution of Banana Consumption: Weekdays vs. Weekends",
    x = "Is Weekend (0 = Weekday, 1 = Weekend)",
    y = "Total Bananas Consumed"
  ) +
  theme_minimal()

# Analyze banana consumption by time period
by_time <- consumption_traffic %>% 
  group_by(time_period) %>% 
  summarize(
    total_bananas = sum(bananas_consumed, na.rm = TRUE), 
    avg_bananas = mean(bananas_consumed, na.rm = TRUE)   
  )

by_time_long <- by_time %>% 
  pivot_longer(cols = c(total_bananas, avg_bananas), 
               names_to = "metric", 
               values_to = "value")


#Second visuilization is to determine whether or not there are major differences in times that I consume and these bannas 
ggplot(by_time_long, aes(x = time_period, y = value, fill = metric)) +
  geom_col(position = "dodge") +
  facet_wrap(~metric, scales = "free") + 
  labs(
    title = "Total vs. Average Bananas Consumed per Time Period",
    x = "Time Period",
    y = "Value"
  ) +
  theme_dark() +
  theme(
    panel.grid.major = element_line(color = "lightgrey", size = 0.5),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "lightblue", color = "darkblue"),
    strip.text = element_text(color = "white", face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.y = element_text(color = "black")  
  )


#In order to actuall confirm whether or not there is a sigincant differnce between the states this was th best method in my opinion but I exepct my methd to be very wrong


anova_result <- aov(bananas_consumed ~ time_period, data = Consumption_Log)
summary(anova_result)
TukeyHSD(aov(bananas_consumed ~ time_period, data = Consumption_Log))
plot(TukeyHSD(aov(bananas_consumed ~ time_period, data = Consumption_Log)))

#ripeness optimization with the heatmap of ripeness s it compares to how many I actually eat
# Calculate total bananas by ripeness BEFORE grouping by time_period
total_bananas_data <- bananas_thrown_away %>%
  group_by(ripeness) %>%
  summarise(total_bananas = n(), .groups = "drop")

# Join total_bananas back to the original dataset
bananas_with_totals <- bananas_thrown_away %>%
  left_join(total_bananas_data, by = "ripeness")

# Group by ripeness and time_period to calculate proportions
heatmap_data <- bananas_with_totals %>%
  group_by(ripeness, thrown_away) %>%
  summarise(
    total_thrown_away = sum(thrown_away),  # Count of bananas thrown away
    proportion_thrown_away = total_thrown_away / unique(total_bananas),  # Proportion of waste
    .groups = "drop"
  )


#Quick note that the heatmap must actually be catagorical variable please
#this in my opinion is a really bad visualization because it appears that you need 1 continous random variable as well a one discrete random variable for the heatmap in R 
ggplot(heatmap_data, aes(x = as.factor(ripeness), y = thrown_away, fill = proportion_thrown_away)) +
  geom_tile(color = "white") +
  geom_text(aes(label = scales::percent(proportion_thrown_away, accuracy = 0.1)), color = "orange", size = 4) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Waste Proportion",) +
  labs(
    title = "Heatmap of Banana Waste Proportion by Ripeness and Time Period",
    x = "Ripeness Level",
    y = "Whether or not a bannas was thrown away"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

Pie_chart_thrown <- bananas_thrown_away %>% 
  group_by(ripeness) %>%
  summarize(
    thrown_count = sum(thrown_away),
    total_count = n()
  ) %>%
  mutate(
    throwaway_rate = (thrown_count / total_count) * 100
  )

faceted_data <-  Pie_chart_thrown %>%  
  pivot_longer(
    cols = c(thrown_count, throwaway_rate), 
    names_to = "metric", 
    values_to ="value"
  )



view(Pie_chart_thrown)

ggplot(faceted_data, aes(x = "", y = value, fill = factor(ripeness))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  facet_wrap(
    ~ metric, 
    scales = "free", 
    labeller = as_labeller(
      c(
        "thrown_count" = "Count of Bananas Thrown Away",
        "throwaway_rate" = "Percentage of Bananas Thrown Away"
      )
    )
  ) +
  labs(
    title = "Banana Throwaway Analysis by Ripeness",
    fill = "Ripeness Level"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(
      size = 15,       
      face = "bold",   
      hjust = .5        
    )
  )



#I will use this to determine whether or not the ripeness of bannas has anything to do with the consumption 


ggplot(Consumption_Log_original, aes(x = bananas_consumed, y = factor(ripeness), fill = factor(ripeness))) +
  geom_density_ridges(scale = 1.5, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Density of Banana Consumption by Ripeness",
       x = "Bananas Consumed",
       y = "Ripeness (1 = Least Ripe, 5 = Most Ripe)") +
  scale_fill_viridis_d()

ggplot(Consumption_Log_original, aes(x = bananas_consumed, y = factor(ripeness), fill = factor(ripeness))) +
  geom_boxplot(scale = 1.5, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Density of Banana Consumption by Ripeness",
       x = "Bananas Consumed",
       y = "Ripeness (1 = Least Ripe, 5 = Most Ripe)") +
  scale_fill_viridis_d()


kruskal.test(bananas_consumed ~ ripeness, data = Consumption_Log_original)
pairwise.wilcox.test(Consumption_Log_original$bananas_consumed, Consumption_Log_original$ripeness, p.adjust.method = "bonferroni")




#Overall show the effect that traffic leverl would have on consumption 

ggplot(consumption_traffic, aes(crowd_level, bananas_consumed, fill = crowd_level)) + 
  geom_boxplot() + 
  labs(
    title = "Box plot to check whether or not thhe traffic level of kins would have an effect on my consumption",
      x = "Level of gremlins in the hall",
      y = bquote("Total bannas " * bold("CONSUMED"))
  ) + 
  theme_minimal() + 
  theme(
   plot.title = element_text(size = 10,       
                             face = "bold",   
                             hjust = .75   )
  )

kruskal.test(bananas_consumed ~ crowd_level, data = consumption_traffic)
pairwise.wilcox.test(consumption_traffic$bananas_consumed, consumption_traffic$crowd_level, p.adjust.method = "bonferroni")


consumption_traffic %>%
  group_by(crowd_level) %>%
  summarize(mean_bananas = mean(bananas_consumed),
            sd_bananas = sd(bananas_consumed),
            count = n())

#to find out whether or not there is a corelational relationship between whether traffic and the actual bannas consumed
kruskal.test(bananas_consumed ~ crowd_level, data = consumption_traffic)
#this test suggests that the test is not statistically signifcant





# Predict banana consumption based on weekend status
consumption_weekend <- lm(total_bananas ~ is_weekend, data = consumption_date)
rsquared(consumption_weekend)

write.csv(consumption_date, "consumption_date.csv", row.names = FALSE)

ggplot(consumption_traffic, aes(crowd_level, bananas_consumed), fill = time_period.x) +
  geom_boxplot() + 
  facet_wrap(~time_period.x) +
  labs(
    title = "Consumption of bananas as it relates to the traffic",
    x = "Amount of traffic present",
    y = "Numbers of bananas consumed"
  )



#visulization and testing whether the restock scheduel has any effect on this 

# Merging datasets
merged_data <- Consumption_Log_original %>%
  inner_join(restock_schedule, by = "date_id", suffix = c("_consumption", "_restock"))

# Visualization
ggplot(merged_data, aes(x = factor(date_id), y = bananas_consumed, fill = restock_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_smooth(aes(group = 1), method = "loess", color = "blue", se = FALSE) +
  annotate("text", x = 2, y = max(merged_data$bananas_consumed), label = "Significant peaks observed", color = "red", size = 4, hjust = 0) +
  labs(
    title = "Banana Consumption vs Restock Schedule",
    subtitle = "Analyzing the impact of restocking events on consumption",
    x = "Date",
    y = "Bananas Consumed",
    caption = "Data Source: Provided Schema"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Statistical Analysis
 kruskal.test(bananas_consumed ~ restock_type, data = merged_data)


pairwise_result <- pairwise.wilcox.test(merged_data$bananas_consumed, merged_data$restock_type, p.adjust.method = "bonferroni")
print(pairwise_result)


# Linear regression analysis for traffic consumption
traffic_consumption_r <- lm(bananas_consumed ~ time_period.x, data = consumption_traffic)
rsquared(traffic_consumption_r)

taffic_consumption_banans <- lm(bananas_consumed ~ crowd_level + time_period.x + crowd_level:time_period.x , data = consumption_traffic)  
eta_squared(taffic_consumption_banans, partial=FALSE) 






