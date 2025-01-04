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




# Load libraries
library(caret)
library(e1071)
library(randomForest)
library(xgboost)
library(nnet)
library(dplyr)



# Assume `Consumption_Log` is already loaded into the environment as a data frame
data <- Consumption_Log_original
view(data)
view(Consumption_Log_original)

# Ensure date_id is in Date format if needed
data$date_id <- as.Date(as.character(data$date_id), format = "%Y%m%d")

# Add is_weekend column based on date_id
data$is_weekend <- as.integer(weekdays(data$date_id) %in% c("Saturday", "Sunday"))

# Ensure time_period is a factor with predefined levels

# One-hot encode time_period
time_dummies <- model.matrix(~ time_period - 1, data)
data <- cbind(data, time_dummies)

# Split data into training and testing sets
set.seed(123) # Ensure reproducibility
train_index <- createDataPartition(data$bananas_consumed, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Define a formula dynamically for simplicity
predictors <- colnames(time_dummies)
formula <- as.formula(paste("bananas_consumed ~ is_weekend +", paste(predictors, collapse = " + ")))



write.csv("")

# Linear Regression
lm_model <- train(
  formula, data = train_data, method = "lm", 
  trControl = trainControl(method = "cv", number = 10)
)
lm_preds <- predict(lm_model, test_data)
lm_rmse <- RMSE(lm_preds, test_data$bananas_consumed)

# Support Vector Machine
svm_model <- train(
  formula, data = train_data, method = "svmRadial", 
  trControl = trainControl(method = "cv", number = 10)
)
svm_preds <- predict(svm_model, test_data)
svm_rmse <- RMSE(svm_preds, test_data$bananas_consumed)

# Random Forest
rf_model <- train(
  formula, data = train_data, method = "rf", 
  trControl = trainControl(method = "cv", number = 10)
)
rf_preds <- predict(rf_model, test_data)
rf_rmse <- RMSE(rf_preds, test_data$bananas_consumed)

# Gradient Boosting
xgb_model <- train(
  formula, data = train_data, method = "xgbTree", 
  trControl = trainControl(method = "cv", number = 10)
)
xgb_preds <- predict(xgb_model, test_data)
xgb_rmse <- RMSE(xgb_preds, test_data$bananas_consumed)

# Neural Network
nn_model <- train(
  formula, data = train_data, method = "nnet", 
  trControl = trainControl(method = "cv", number = 10), 
  trace = FALSE, linout = TRUE
)
nn_preds <- predict(nn_model, test_data)
nn_rmse <- RMSE(nn_preds, test_data$bananas_consumed)

# Model Comparison
results <- data.frame(
  Model = c("Linear Regression", "SVM", "Random Forest", "Gradient Boosting", "Neural Network"),
  RMSE = c(lm_rmse, svm_rmse, rf_rmse, xgb_rmse, nn_rmse)
)

# Print results
print(results)

print(results)

