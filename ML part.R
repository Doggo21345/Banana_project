

# Load libraries
library(caret)
library(e1071)
library(randomForest)
library(xgboost)
library(nnet)
library(dplyr)
pacman::p_load(tidyverse, ggplot2, gganimate, flexdashboard, mosaic, moderndive, effectsize, modelr, rsample, car)
install.packages("xgboost")
library(xg)

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

  



data <- Consumption_Log_original
view(data)
view(Consumption_Log_original)

time_dummies <- model.matrix(~ time_period - 1, data)
data <- cbind(data, time_dummies)

set.seed(123) 
train_index <- createDataPartition(data$bananas_consumed, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]




predictors <- colnames(time_dummies)
formula <- as.formula(paste("bananas_consumed ~ is_weekend +", paste(predictors, collapse = " + ")))





# Linear Regression
lm_model <- train(
  formula, data = train_data, method = "lm", 
  trControl = trainControl(method = "cv", number = 10)
)
lm_preds <- predict(lm_model, test_data)
lm_rmse <- RMSE(lm_preds, test_data$bananas_consumed)

lm_model <-  train(
  formula, data = train_data, method = "lm", 
  trControl = trainControl(method = "cv", number = 10)
)



svm_model <- train(
  formula, data = train_data, method = "svmRadial", 
  trControl = trainControl(method = "cv", number = 10)
)
svm_preds <- predict(svm_model, test_data)
svm_rmse <- RMSE(svm_preds, test_data$bananas_consumed)

rf_model <- train(
  formula, data = train_data, method = "rf", 
  trControl = trainControl(method = "cv", number = 10)
)
rf_preds <- predict(rf_model, test_data)
rf_rmse <- RMSE(rf_preds, test_data$bananas_consumed)


xgb_model <- train(
  formula, data = train_data, method = "xgbTree", 
  trControl = trainControl(method = "cv", number = 10)
)
xgb_preds <- predict(xgb_model, test_data)
xgb_rmse <- RMSE(xgb_preds, test_data$bananas_consumed)


nn_model <- train(
  formula, data = train_data, method = "nnet", 
  trControl = trainControl(method = "cv", number = 10), 
  trace = FALSE, linout = TRUE
)
nn_preds <- predict(nn_model, test_data)
nn_rmse <- RMSE(nn_preds, test_data$bananas_consumed)


results <- data.frame(
  Model = c("Linear Regression", "SVM", "Random Forest", "Gradient Boosting", "Neural Network"),
  RMSE = c(lm_rmse, svm_rmse, rf_rmse, xgb_rmse, nn_rmse)
)

results %>%  
  ggplot(aes(Model, RMSE, fill = Model)) +  
  geom_bar(stat = "identity", width = 0.7) +  
  labs(
    title = "RMSE of the various models", 
    x = "Model",
    y = "RMSE of the untrained model"
  ) +  
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, color = "red"), 
    axis.title = element_text(face = "bold", size = 10 )
  )



# Print results
print(results)

rf_model_tuned <- train(
  formula, data = train_data, method = "rf",
  tuneGrid = expand.grid(mtry = sqrt(ncol(train_data) - 1)), # Default mtry tuning
  ntree = 500, # Increase the number of trees
  trControl = trainControl(method = "cv", number = 10)
)
rf_tuned_preds <- predict(rf_model_tuned, test_data)
rf_tuned_rmse <- RMSE(rf_tuned_preds, test_data$bananas_consumed)


rmse_data <- data.frame(
  Model = c("Random Forest (Original)", "Gradient Boosting", "Random Forest (Tuned)"),
  RMSE = c(rf_rmse, xgb_rmse, rf_tuned_rmse)
)

# Plot RMSE values
ggplot(rmse_data, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6) +
  theme_minimal() +
  labs(
    title = "Model Performance Comparison",
    y = "Root Mean Square Error (RMSE)",
    x = "Model"
  ) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb"))




