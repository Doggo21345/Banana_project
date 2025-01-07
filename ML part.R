

# Load libraries
library(caret)
library(e1071)
library(randomForest)
library(xgboost)
library(nnet)
library(dplyr)
pacman::p_load(tidyverse, ggplot2, gganimate, flexdashboard, mosaic, moderndive, effectsize, modelr, rsample, car)
install.packages("xgboost")

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


mse
mae
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

lm_model <-  train(
  formula, data = train_data, method = "lm", 
  trControl = trainControl(method = "cv", number = 10)
)


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
    plot.title = element_text(hjust = 0.5) 
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


# Define a tuning grid
xgb_grid <- expand.grid(
  nrounds = c(100, 200, 300),     # Number of boosting rounds
  max_depth = c(3, 5, 7),         # Tree depth
  eta = c(0.01, 0.1, 0.3),        # Learning rate
  gamma = c(0, 1, 5),             # Minimum loss reduction
  colsample_bytree = c(0.5, 0.8), # Column subsampling
  min_child_weight = c(1, 3, 5),  # Minimum sum of instance weight needed in a child
  subsample = c(0.5, 0.8)         # Row subsampling
)

# Train the model with hyperparameter tuning
xgb_model_tuned <- train(
  formula, data = train_data, method = "xgbTree",
  trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE),
  tuneGrid = xgb_grid
)

# Predictions and RMSE
xgb_tuned_preds <- predict(xgb_model_tuned, test_data)
xgb_tuned_rmse <- RMSE(xgb_tuned_preds, test_data$bananas_consumed)


train_control <- trainControl(method = "cv", number = 10, verboseIter = TRUE)

# Fine-tuning Gradient Boosting
xgb_grid <- expand.grid(
  nrounds = c(100, 200, 300),     
  max_depth = c(3, 5, 7),        
  eta = c(0.01, 0.1, 0.3),        
  gamma = c(0, 1, 5),             
  colsample_bytree = c(0.5, 0.8), 
  min_child_weight = c(1, 3, 5),  
  subsample = c(0.5, 0.8)         
)

xgb_tuned <- train(
  formula, data = train_data, method = "xgbTree",
  trControl = train_control, tuneGrid = xgb_grid
)

# Fine-tuning Random Forest
rf_grid <- expand.grid(mtry = seq(2, sqrt(ncol(train_data)), by = 1))
rf_tuned <- train(
  formula, data = train_data, method = "rf",
  trControl = train_control, tuneGrid = rf_grid, ntree = 500
)

# Fine-tuning AdaBoost
# Train XGBoost model
xgb_model <- xgb.train(
  data = train,
  nrounds = 100,
  objective = "reg:squarederror",
  eval_metric = "rmse"
)

# Make predictions
xgb_preds <- predict(xgb_model, dtest)

# Calculate RMSE
xgb_rmse <- sqrt(mean((xgb_preds - test_label)^2))
print(xgb_rmse)

# Comparing Predictions
models <- list(
  Random_Forest = rf_tuned,
  AdaBoost = adaboost_tuned
)

test_data <- test_data %>% mutate(index = row_number())

model_results <- map(models, ~ {
  preds <- predict(.x, test_data)
  test_data %>%
    mutate(Predictions = preds) %>%
    summarise(
      RMSE = RMSE(Predictions, bananas_consumed),
      MAE = MAE(Predictions, bananas_consumed)
    ) %>%
    mutate(Model = .x$method)
}) %>% bind_rows()

# Visualization of Fine-tuning Progress
xgb_tuning_plot <- ggplot(xgb_tuned$results, aes(x = eta, y = RMSE, color = factor(max_depth))) +
  geom_line() +
  geom_point() +
  labs(title = "Gradient Boosting Tuning Progress", x = "Learning Rate (eta)", y = "RMSE") +
  theme_minimal()

rf_tuning_plot <- ggplot(rf_tuned$results, aes(x = mtry, y = RMSE)) +
  geom_line() +
  geom_point() +
  labs(title = "Random Forest Tuning Progress", x = "mtry", y = "RMSE") +
  theme_minimal()

adaboost_tuning_plot <- ggplot(adaboost_tuned$results, aes(x = mfinal, y = RMSE, color = coeflearn)) +
  geom_line() +
  geom_point() +
  labs(title = "AdaBoost Tuning Progress", x = "Number of Iterations (mfinal)", y = "RMSE") +
  theme_minimal()

# Visualization: Predicted vs Actual
predictions_plot <- test_data %>%
  pivot_longer(cols = starts_with("Predictions"), names_to = "Model", values_to = "Prediction") %>%
  ggplot(aes(x = index)) +
  geom_line(aes(y = bananas_consumed, color = "Actual"), size = 1) +
  geom_line(aes(y = Prediction, color = Model), size = 0.8, linetype = "dashed") +
  labs(title = "Predicted vs Actual Banana Consumption", x = "Index", y = "Bananas Consumed", color = "Legend") +
  theme_minimal()

# Extrapolate Predictions for Next Week
next_week_data <- test_data %>%
  slice_tail(n = 7) %>%
  mutate(date_id = date_id + 7) # Add 7 days for extrapolation

next_week_predictions <- map(models, ~ {
  predict(.x, newdata = next_week_data)
}) %>% bind_cols()

# Display Results
list(
  Tuning_Plots = list(xgb_tuning_plot, rf_tuning_plot, adaboost_tuning_plot),
  RMSE_Comparison = model_results,
  Prediction_Visualization = predictions_plot,
  Extrapolated_Predictions = next_week_predictions
)
