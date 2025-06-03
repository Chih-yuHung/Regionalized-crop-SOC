library(tidyverse)
library(caret)
library(xgboost)

# ===========================
# 1. Data Preparation
# ===========================
load(file="Input/SiteData_Daycent_5y.RData")

SiteData <- SiteData_5y %>%
  mutate(TEXT = as.factor(TEXT)) %>%
  drop_na()

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
# ✅ Use only these 5 predictors
selected_vars <- c("MAP", "MAT", "MPE", "CARB30", "Cinput")

# Container to store model results
model_results <- list()

# ===========================
# 2. Loop over TEXT groups
# ===========================
for (text_group in levels(SiteData$TEXT)) {
  
  cat("\n----- Running model for TEXT =", text_group, "-----\n")

  # Subset data for the current group
  group_data <- SiteData %>% filter(TEXT == text_group)
  
  # 70/30 split
  set.seed(2025)
  train_idx <- createDataPartition(group_data$deltaSOC, p = 0.7, list = FALSE)
  train_data <- group_data[train_idx, ]
  test_data  <- group_data[-train_idx, ]
  train_data <- ungroup(train_data)
  test_data  <- ungroup(test_data)
  # Prepare matrices
  train_x <- train_data %>%
    select(all_of(selected_vars)) %>%
    mutate_if(is.factor, as.numeric) %>%
    as.matrix()
  train_y <- train_data$deltaSOC
  cat("Unique values in deltaSOC:", length(unique(train_y)), "\n")
  
  test_x <- test_data %>%
    select(all_of(selected_vars)) %>%
    mutate_if(is.factor, as.numeric) %>%
    as.matrix()
  test_y <- test_data$deltaSOC
  
  #expand grid
  xgb_grid <- expand.grid(
    nrounds = c(100, 200, 500),         # Number of boosting rounds
    max_depth = c(3, 6, 9),             # Tree depth (complexity)
    eta = c(0.01, 0.05, 0.1),           # Learning rate (lower = more robust)
    gamma = c(0, 1),                    # Minimum loss reduction to make a split
    colsample_bytree = c(0.6, 0.8),     # Feature sampling ratio
    min_child_weight = c(1, 5),         # Minimum child node weight
    subsample = c(0.7, 0.9)             # Row sampling
  )
  
  # TrainControl setup
  control <- trainControl(
    method = "cv",
    number = 5,
    allowParallel = TRUE,
    verboseIter = FALSE
  )
  
  # Train model
  set.seed(2025)
  xgb_model <- train(
    x = train_x,
    y = train_y,
    method = "xgbTree",
    trControl = control,
    tuneGrid = xgb_grid,
    verbose = FALSE
  )
  
  # Predict & evaluate-
  pred <- predict(xgb_model, newdata = test_x)
  mse <- mean((pred - test_y)^2)
  rmse <- sqrt(mse)
  r2 <- R2(pred, test_y)
  
  # Guard for divide-by-zero in MAPE
  if (any(test_y == 0)) {
    mape <- NA
  } else {
    mape <- mean(abs((pred - test_y) / test_y)) * 100
  }
  
  # Symmetric MAPE (sMAPE)
  smape <- mean(2 * abs(pred - test_y) / (abs(pred) + abs(test_y))) * 100
  
  cat("MSE:", round(mse, 3),
      "| RMSE:", round(rmse, 3),
      "| R²:", round(r2, 3),
      "| MAPE:", round(mape, 2), "%",
      "| sMAPE:", round(smape, 2), "%\n")
  
  # Store model + results
  rf_results[[text_group]] <- list(
    model = rf_model,
    MSE = mse,
    RMSE = rmse,
    R2 = r2,
    MAPE = mape,
    sMAPE = smape
  )
}

# ----- Running model for TEXT = 1 -----
#   Unique values in deltaSOC: 6404 
# RMSE: 0.025 
# R²: 0.935 
# 
# ----- Running model for TEXT = 2 -----
#   Unique values in deltaSOC: 8378 
# RMSE: 0.036 
# R²: 0.888 
# 
# ----- Running model for TEXT = 3 -----
#   Unique values in deltaSOC: 4022 
# RMSE: 0.047 
# R²: 0.859 






## Random Forest
library(tidyverse)
library(caret)
library(randomForest)
library(doParallel)

# ===========================
# 1. Data Preparation
# ===========================
load(file = "Input/SiteData_Daycent_5y.RData")

SiteData <- SiteData_5y %>%
  mutate(TEXT = as.factor(TEXT)) %>%
  drop_na()

# Use only these 5 predictors
selected_vars <- c("MAP", "MAT", "MPE", "CARB30", "Cinput")

# Start parallel backend
cl <- makeCluster(parallel::detectCores() - 1)
registerDoParallel(cl)

# Store results
rf_results <- list()

# ===========================
# 2. Loop over TEXT groups
# ===========================
for (text_group in levels(SiteData$TEXT)) {
  
  cat("\n----- Running Random Forest for TEXT =", text_group, "-----\n")
  
  # Subset data
  group_data <- SiteData %>% filter(TEXT == text_group)
  
  
  # 70/30 split
  set.seed(2025)
  train_idx <- createDataPartition(group_data$deltaSOC, p = 0.7, list = FALSE)
  train_data <- group_data[train_idx, ] %>% ungroup()
  test_data  <- group_data[-train_idx, ] %>% ungroup()
  
  # Prepare matrices
  train_x <- train_data %>%
    select(all_of(selected_vars)) %>%
    mutate_if(is.factor, as.numeric) %>%
    as.data.frame()
  train_y <- train_data$deltaSOC
  
  test_x <- test_data %>%
    select(all_of(selected_vars)) %>%
    mutate_if(is.factor, as.numeric) %>%
    as.data.frame()
  test_y <- test_data$deltaSOC
  
  # Set tuning grid for Random Forest
  rf_grid <- expand.grid(mtry = c(2, 3, 4, 5))
  
  # TrainControl setup
  control <- trainControl(method = "cv", number = 5, allowParallel = TRUE)
  
  # Train the model
  set.seed(2025)
  rf_model <- train(
    x = train_x,
    y = train_y,
    method = "rf",
    tuneGrid = rf_grid,
    trControl = control,
    ntree = 500
  )
  
  # Predict and evaluate
  pred <- predict(rf_model, newdata = test_x)
  mse <- mean((pred - test_y)^2)
  rmse <- sqrt(mse)
  r2 <- R2(pred, test_y)
  
  # Guard for divide-by-zero in MAPE
  if (any(test_y == 0)) {
    mape <- NA
  } else {
    mape <- mean(abs((pred - test_y) / test_y)) * 100
  }
  
  # Symmetric MAPE (sMAPE)
  smape <- mean(2 * abs(pred - test_y) / (abs(pred) + abs(test_y))) * 100
  
  cat("MSE:", round(mse, 3),
      "| RMSE:", round(rmse, 3),
      "| R²:", round(r2, 3),
      "| MAPE:", round(mape, 2), "%",
      "| sMAPE:", round(smape, 2), "%\n")
  
  # Store results
  rf_results[[text_group]] <- list(
    model = rf_model,
    MSE = mse,
    RMSE = rmse,
    R2 = r2,
    MAPE = mape,
    sMAPE = smape
  )
}

# Stop cluster
stopCluster(cl)

