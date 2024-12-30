pacman::p_load(tidyverse, ggplot2, gganimate, flexdashboard, mosaic, moderndive,effectsize, modelr, rsample)


Traffic_Log <- innerjoin(randomized_banana_consumption_log,Traffic_Log, by = "date_id",  )


daily_consumption <- randomized_banana_consumption_log %>%
  group_by(date_id) %>%
  summarise(total_bananas = sum(bananas_consumed))

consumption_date <- inner_join(daily_consumption, Date_Dim, by = "date_id")
consumption_date$date_id <- as.Date(as.character(consumption_date$date_id), format="%Y%m%d")

consumption_traffic <- inner_join(Consumption_Log, Traffic_Log, by = "date_id", "time period" )
  #when I join these two columns they actually have two coumns in common so whenever I join them as a result there is a many to many relationship so I am trying to go into the duplicates and findout what I did wrong and how I can improve it 
Traffic_Log %>%
  count(date_id, `time_period`) %>%
  filter(n > 1)
#HMMMMMMMMMM so I actualy did do a decent job of somehow recoridng my data for the month so therefore there is something else wrong with the data and not the duplciates because when I ran this for both of them neither had antyhing in the actual data frame so I fuckd up somewhere else I think 

Traffic_Log <- Traffic_Log %>% rename(time_period = `time_period`)
consumption_traffic <- inner_join(
  Consumption_Log, 
  Traffic_Log, 
  by = c("date_id" = "date_id", "time_period" = "time_period")
)


write.csv(consumption_traffic, "consumption_traffic.csv", row.names = FALSE)

#I wouldn't be able to show the actual traffic log because I reported them by time of day so I couldn't use this to actually on the daily consumption of banans chart 
#Can't also fulll join on some of the data that makes a laot of NA values and when we work with graphs that just makes it really complicated and not easy to work with 
#some of the data that I have 





#This is the graph that shwocases the overall bana consumption and is the first visuilzation on my project
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
#Here I ran a large sample t.test to confirm the difference in means and mak
t.test(total_bananas ~ is_weekend, data = consumption_date, var.equal = FALSE)
?t.test



#The second visuilzation



#The third visuilzation will be the total amount of banans I eat 

#I want to analyze the amount of banas that I consume as a function of the time of the day variable

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






#predicting stuff for the banna consumption based on whether or not it is the weekend 
consumption_weekend <-lm(total_bananas ~ is_weekend, data = consumption_date)
rsquared(consumption_weekend)
#GAH DAMN thats has an r squared value of about 92 so I was correct in assuming that a higher amount of bannas were consumed on the weeekend 

write.csv(Consumption_Log, "Consumption_Log.csv", row.names = FALSE)


#I honeslty have no idea what this box plot is telling me so I will conduct a ANOVA test to hopefully help me in predicting if the trafffic variable helps me with this 


ggplot(consumption_traffic, aes(crowd_level, bananas_consumed), fill = time_period.x) +
  geom_boxplot() + 
  facet_wrap(~time_period.x)+
  labs(
    title = "Consumption of bannas as it relates to the traffic",
    x = "Amount of traffic present at kins ",
    y = "numbers of bannas consumed"
  )


#So when I did the rsquared using the crowd level I found out that I just don't give a damn about how many people are actually present inside of kins I will be getting my bannas either way you best believe
traffic_consumption_r <-lm(bananas_consumed ~ time_period.x, data = consumption_traffic)
rsquared(traffic_consumption_r)
#Intresting so it seesm that the time period in which I colllec bannas does have an effect on how much I actually connect from the banna tree, Initially I thought of it as I would be hungrier in the night time but apprently its still not a great predictor of how many bannas I typically eat

taffic_consumption_banans <- lm(bananas_consumed ~ crowd_level + time_period.x +  crowd_level:time_period.x , data=consumption_traffic)  
taffic_consumption_banans
eta_squared(taffic_consumption_banans, partial=FALSE) 
#Something that is auto correlation and when you work on a chronological Ex: In terms of what the best predictor of tmrs value is that its today value. You need to check for it and then control for the varaible. Basicially by modeling the auto correlation you can model that expliciltu and underscore the effect of the other variables. 
#Ok so I get that the total R squared value shpould be around 23 and alos it seems that I have a low R squared value beteween the actual crowd level and the time period so it seeesm like I will not be working with that 
get_regression_table()

names(consumption_traffic)

heatmap(consumption_date)


?heatmap
dates <- seq.Date(as.Date("2023-09-01"), as.Date("2023-11-08"), by = "day")






#This is where I start to analyze based on the time of day when do I eat the most banans 

#Lowkey what I could do is just to group by and see based on this what time of day ahs the largest amount of consumption 
consumption_by_time <- Consumption_Log %>%  
  group_by(time_period) %>%  
  summarise(Count = n()) %>% 
  arrange(desc(Count)) 
#Somethinf went wrong here I should probably fix it, it is my belief that I probably should not have included the muattae function because that messed up a lot of things 
  
#Next step is to build a grpah to see what is the distribution and total count of things that are in this (count) so basically I will make a histogram and a bar chart 


ggplot(aes(time_period, Count), data = consumption_by_time) + 
  geom_bar(stat = "identity") +
  labs(
    title = "Consumption of bananas by time period",
    x = "Time period"
  ) + 
  theme(
    axis.title.x = element_text(face = "bold") 
  )

    

#this will be the start of the inital machine learning extrapoliation 
#In order to actual come up with a certain thing for evetything to be predicted in I msut 1st make a table that includes evertyhign so that i can fo forward with thisn 
write.csv(consumption_traffic, "consumption_traffic.csv", row.names = FALSE)
write.csv(Restock_Log, "restock_log.csv", row.names = F)
write.csv(Date_Dim, "Date_Dim", row.names = F)
write.csv(consumption_date, "Consumption_date", row.names = F)
write.csv(daily_consumption, "daily_consumption", row.names = F)


#updating the ripeness column of the consumption log csv file 

# Define ripeness probabilities and values
ripeness_probabilities <- c(0.25, 0.625, 0.125)  # Probabilities: Green (1-2), Yellow (3-4), Overripe (5+)
ripeness_values <- c(1, 3, 5)                    # 1: Green, 3: Yellow, 5: Overripe

# Add a new column for ripeness
view(Consumption_Log)
Consumption_Log <- Consumption_Log %>%
  mutate(ripeness = sample(ripeness_values, size = n(), replace = TRUE, prob = ripeness_probabilities))

consumption_date <- consumption_date %>%
  mutate(date_id = as.integer(format(as.Date(date, "%Y-%m-%d"), "%Y%m%d")))

consumption_traffic <- consumption_traffic %>%
  mutate(date_id = as.integer(date_id))



merged_data <- consumption_date %>%
  inner_join(consumption_traffic, by = "date_id") 

split <- initial_split(merged_data, prop = 0.7)
train_data <- training(split)
test_data <- testing(split)

model <- lm(bananas_consumed ~ is_weekend + day_of_week + crowd_level, data = train_data)

test_data <- test_data %>%
  mutate(predictions = predict(model, newdata = test_data))

mae <- test_data %>%
  summarise(MAE = mean(abs(bananas_consumed - predictions))) %>%
  pull(MAE)

mse <- test_data %>%
  summarise(MSE = mean((bananas_consumed - predictions)^2)) %>%
  pull(MSE)

# Print Results
print(paste("Mean Absolute Error (MAE):", mae))
print(paste("Mean Squared Error (MSE):", mse))

# Step 6: Visualize Predictions
# Plot actual vs predicted values
# Add an index column to the data
test_data <- test_data %>%
  mutate(index = row_number())

# Update the plot
ggplot(test_data, aes(x = index)) +
  geom_line(aes(y = bananas_consumed, color = "Actual"), size = 1) +
  geom_line(aes(y = predictions, color = "Predicted"), size = 1, linetype = "dashed") +
  labs(
    title = "Predicted vs Actual Banana Consumption",
    x = "Index (Test Observations)",
    y = "Bananas Consumed",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")




banana_split <-  initial_split(SaratogaHouses, prop=0.75)
