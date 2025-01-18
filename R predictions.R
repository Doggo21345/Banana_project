# Libraries
library(caret)
library(randomForest)
library(ggplot2)
library(tidyverse)


set.seed(123)
rf_grid <- expand.grid( #these are alll lists of hpyer paremeters that I set up potential combinations of and then are tested inside of the training model below 
  mtry = c(2, 3, 4),  # this is the variable that picke the amount of like variables from the model that will be calculated so the ones that are more advanced I.E that have more variables will be ubject to overfit while the smaller ones might not yield the most amount of resuslts 
  splitrule = c("variance"), #for a continous random variable we wan to reduce the total amount of varaince that is present in this so we use the variance splitrule to make sure to reduce that as much as possible 
  min.node.size = c(5, 10, 15)  #this tests the different sizes of the nodes to make sure that each tree has the optimal amount of observations for accuracy. 
)

rf_model_tuned <- train(
  formula, data = train_data, method = "ranger",
  trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE),
  tuneGrid = rf_grid
)

# Evaluate Random Forest
rf_tuned_preds <- predict(rf_model_tuned, test_data_lol)
rf_tuned_rmse <- RMSE(rf_tuned_preds, test_data_lol$bananas_consumed)

# Fine-tune Gradient Boosting using caret's gbm method
gbm_grid <- expand.grid(
  interaction.depth = c(3, 5, 7),  # Tree depth which is basivally how far the decision tree is when it comes to how many bannas are actually consumed the tradeoff with these is that the deeper the tree is the less observations it will have per tree but then as a result it will also be subject to less overfitting vice versa is also true
  n.trees = c(50, 100, 150),       # Number of trees basically the 
  shrinkage = c(0.01, 0.1),        # Learning rate which is how long it will take the model to learn from the past decision tree nodes so gradient boosters are constantly revaulating the weight it places on each tree due to the last ones mistakes but this variables can deicde how much the results of each one of those really matter and then tests which one of them would be best 
  n.minobsinnode = c(5, 10)        # Minimum number of observations in terminal nodes which basically syas how much data should I use in this model and how diverse can I get
)

gbm_model_tuned <- train(
  formula, data = train_data, method = "gbm",
  trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE),
  tuneGrid = gbm_grid
)

# Evaluate Gradient Boosting
gbm_tuned_preds <- predict(gbm_model_tuned, test_data_lol)
gbm_tuned_rmse <- RMSE(gbm_tuned_preds, test_data_lol$bananas_consumed)


# Simulating AdaBoost effect by focusing on high tree counts and low learning rates the ada boost package wouldn't load :(
ada_grid <- expand.grid(
  interaction.depth = c(1, 2),      
  n.trees = c(200, 300),            
  shrinkage = c(0.01),              
  n.minobsinnode = c(5)             
)

ada_model <- train(
  formula, data = train_data, method = "gbm",
  trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE),
  tuneGrid = ada_grid
)

# Evaluate AdaBoost
ada_preds <- predict(ada_model, test_data_lol)
ada_rmse <- RMSE(ada_preds, test_data_lol$bananas_consumed)

# Compare RMSE of Models
results_thing <- data.frame(
  Model = c("Random Forest (Tuned)", "Gradient Boosting (Tuned)", "AdaBoost-Like"),
  RMSE = c(rf_tuned_rmse, gbm_tuned_rmse, ada_rmse)
)

# Visualization: RMSE Comparison
ggplot(results_thing, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6) +
  theme_minimal() +
  labs(
    title = "Model Performance Comparison",
    y = "Root Mean Square Error (RMSE)",
    x = "Model", 
    fill = "Legend" 
  ) + 
  theme(
    plot.title = element_text(face = "bold", colour = "blue", size = 16, hjust = 0.5), 
    axis.title = element_text(face = "bold", colour = "darkgreen", size = 10)
  )

  

# Prediction vs Actual for all Models
test_data_lol <- test_data %>%
  mutate(
    rf_preds = rf_tuned_preds,
    gbm_preds = gbm_tuned_preds,
    ada_preds = ada_preds
  )
test_data_lol <- test_data_lol %>%
  mutate(row_index = row_number())


  
# Reshape the data into long format with a new Model column
test_data_long <- test_data_lol %>%
  mutate(row_index = row_number()) %>%
  select(row_index, bananas_consumed, rf_preds, gbm_preds, ada_preds) %>%
  pivot_longer(
    cols = c(bananas_consumed, rf_preds, gbm_preds, ada_preds),
    names_to = "Model",
    values_to = "Consumption"
  ) %>%
  mutate(
    Model = case_when(
      Model == "bananas_consumed" ~ "Actual",
      Model == "rf_preds" ~ "Random Forest",
      Model == "gbm_preds" ~ "Gradient Boosting",
      Model == "ada_preds" ~ "AdaBoost-Like",
      TRUE ~ Model  # Default case to retain any other values (if applicable)
    )
  )

#faceted plot which is much better looking 
ggplot(test_data_long, aes(x = row_index, y = Consumption, color = Model)) +
  geom_line(size = 1) +
  facet_wrap(~ Model, scales = "free_y") +
  labs(
    title = "Predictions vs Actuals",
    x = "Index",
    y = "Bananas Consumed",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")  


ggplot(test_data)
  

#ewwwww graph
ggplot(test_data_lol, aes(x = row_index)) +
  geom_line(aes(y = bananas_consumed, color = "Actual"), size = 1) +
  geom_line(aes(y = rf_preds, color = "Random Forest"), linetype = "dashed", size = 1) +
  geom_line(aes(y = gbm_preds, color = "Gradient Boosting"), linetype = "dotted", size = 1) +
  geom_line(aes(y = ada_preds, color = "AdaBoost-Like"), linetype = "twodash", size = 1) +
  labs(
    title = "Predictions vs Actuals",
    x = "Index",
    y = "Bananas Consumed",
    color = "Legend"
  ) +
  theme_minimal()



# Define the dates for next week (adjust as needed)
next_week_dates <- seq.Date(from = Sys.Date() + 1, by = "day", length.out = 7)

# Create the next week dataset
next_week <- data.frame(
  date_id = as.integer(format(next_week_dates, "%Y%m%d")),  # Convert dates to date_id format
  time_period = rep(c("Morning", "Afternoon", "Evening", "Late Night"), length.out = 28)  # Example for 7 days (28 time periods)
)

# Check the structure of next_week dataset
head(next_week)


# Add the 'is_weekend' column (just like in the training data)
next_week$is_weekend <- as.integer(weekdays(next_week_dates) %in% c("Saturday", "Sunday"))

# One-hot encode time_period for the new dataset
time_dummies <- model.matrix(~ time_period - 1, data = next_week)  # Adjust this if needed
next_week <- cbind(next_week, time_dummies)

# Generate predictions using the trained models
next_week_predictions <- next_week %>%
  mutate(
    rf_preds = predict(rf_model_tuned, newdata = next_week),
    gbm_preds = predict(gbm_model_tuned, newdata = next_week),
    ada_preds = predict(ada_model, newdata = next_week)
  )




  
  # Reshape the data into long format with a new Model column
  next_week_predictions_long <- next_week_predictions %>%
    mutate(row_index = row_number()) %>%
    select(row_index, rf_preds, gbm_preds, ada_preds) %>%
    pivot_longer(
      cols = c (rf_preds, gbm_preds, ada_preds),
      names_to = "Model",
      values_to = "Consumption"
    ) %>%
    mutate(
      Model = case_when(
        Model == "rf_preds" ~ "Random Forest",
        Model == "gbm_preds" ~ "Gradient Boosting",
        Model == "ada_preds" ~ "AdaBoost-Like",
        TRUE ~ Model  
      )
    )
  
 
  
  ggplot(next_week_predictions_long, aes(x = row_index, y = Consumption, color = Model)) +
    geom_line(size = 1) + 
    facet_wrap(~ Model, scales = "free_y") + 
    labs(
      title = "Predictions for what I should eat for next week",
      x = "Index",
      y = "Bananas Consumed",
      color = "Legend"
    ) + 
    theme_minimal()
  




  
