pacman::p_load(tidyverse, ggplot2, gganimate, flexdashboard, mosaic, moderndive, effectsize, modelr, rsample, car)

# Prepare data for modeling
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

# Visualize predictions
test_data <- test_data %>%
  mutate(index = row_number())

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
