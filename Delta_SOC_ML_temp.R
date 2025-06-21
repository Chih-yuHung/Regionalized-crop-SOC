
library(tidyverse)
library(caret)
cl <- makeCluster(parallel::detectCores() - 1)
registerDoParallel(cl)
# 1. Locate and read the *.rds files
file_paths  <- list.files("Input", pattern = "^SiteData_\\d+y\\.rds$", full.names = TRUE)
data_list   <- map(file_paths, readRDS)

# 2. Prep a container for results
#models <- vector("list", length(data_list))

results_summary <- list()
importance_summary <- list()

# Prepare data and loop
for (i in seq_along(data_list)) {
#for (i in 5) {
  SiteData   <- data_list[[i]]      # <-- give current df the name your code expects
  model_name <- paste0("Model_", i)
  vars <- c("MAP", "MAT", "MPE", "Cinput", "CARB30", "TEXT")
  
  cat("\n\n==== Training", model_name, "====\n")
  
  
  # Data split
  set.seed(2025)
  idx <- createDataPartition(SiteData$deltaSOC, p = 0.7, list = FALSE)
  train_data <- SiteData[idx, ] %>% ungroup()
  test_data  <- SiteData[-idx, ] %>% ungroup()
  
  train_x <- train_data %>% select(all_of(vars)) %>% mutate_if(is.factor, as.numeric)
  train_y <- train_data$deltaSOC
  test_x  <- test_data %>% select(all_of(vars)) %>% mutate_if(is.factor, as.numeric)
  test_y  <- test_data$deltaSOC
  
  
  # Parallel backend
  cl <- makeCluster(parallel::detectCores() - 1)
  registerDoParallel(cl)
  
  # RF training
  rf_model <- train(
    x = train_x,
    y = train_y,
    method = "rf",
    tuneGrid = expand.grid(mtry = 2:4), #Previous showed the lowest RMSE occurs on 3 or 4
    trControl = trainControl(method = "cv", number = 5, allowParallel = TRUE),
    ntree = 500
  )
  
  stopCluster(cl)
  # Prediction
  train_pred <- predict(rf_model, newdata = train_x)
  test_pred <- predict(rf_model, newdata = test_x)
  
  # Metrics
  mse <- mean((test_pred - test_y)^2)
  rmse <- sqrt(mse)
  r2 <- R2(test_pred, test_y)
  mape <- if (any(test_y == 0)) NA else mean(abs((test_pred - test_y) / test_y)) * 100
  smape <- mean(2 * abs(test_pred - test_y) / (abs(test_pred) + abs(test_y))) * 100
  
  # Save model
  save(rf_model, file = file.path("Output", paste0(model_name, ".RData")))
  
  # Save plot
  plot_df <- tibble(
    deltaSOC = c(train_y, test_y),
    Prediction = c(train_pred, test_pred),
    Set = rep(c("Train", "Test"), c(length(train_y), length(test_y)))
  )
  
  p <- ggplot(plot_df, aes(x = deltaSOC, y = Prediction, color = Set)) +
    geom_point(alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    scale_color_manual(values = c("Train" = "blue", "Test" = "red")) +
    labs(
      title = paste(model_name, "- Observed vs Predicted"),
      subtitle = paste("RMSE:", round(rmse, 3), "| R²:", round(r2, 3),
                       "| MAPE:", round(mape, 2), "% | sMAPE:", round(smape, 2), "%"),
      x = "Observed deltaSOC", y = "Predicted deltaSOC"
    ) +
    theme_minimal()
  
  ggsave(filename = file.path("Output", paste0(model_name, "_plot.png")), plot = p, width = 6, height = 5)
  
  # Save results
  results_summary[[model_name]] <- tibble(
    Model = model_name, MSE = mse, RMSE = rmse, R2 = r2,
    MAPE = mape, sMAPE = smape
  )
  
  # Save importance
  imp <- as.data.frame(rf_model$finalModel$importance)
  imp$Variable <- rownames(imp)
  importance_summary[[model_name]] <- imp
}


# Performance table
perf_table <- bind_rows(results_summary)

# 1. Add model-specific column names to each importance table
for (i in seq_along(importance_summary)) {
  model_name <- names(importance_summary)[i]
  importance_summary[[i]] <- importance_summary[[i]] %>%
    rename(!!model_name := IncNodePurity)
}

# 2. Merge by Variable
imp_wide <- reduce(importance_summary, full_join, by = "Variable")

# 3. Reorder columns: Variable first, then models
imp_wide <- imp_wide %>% select(Variable, starts_with("Model_"))

# Save summaries
write.csv(perf_table, "Output/Model_Performance_Moving average.csv", row.names = FALSE)
write.csv(imp_wide, "Output/Variable_Importance_Comparison_Moving average.csv", row.names = FALSE)

# Show performance
print(perf_table)




#Pivot the table
Perf_long <- perf_table %>% 
  pivot_longer(c(MSE, R2, sMAPE), names_to = "metric",
               values_to = "value") %>%
  mutate(year = parse_number(Model))

#Plot results
ggplot(Perf_long,
       aes(year, value)) +
           #colour = texture,
           #group  = texture)) +
  geom_line(size = 1) +
  geom_point() +
  facet_wrap(~ metric, scales = "free_y") +         # each metric gets its own scale
  scale_x_continuous(breaks = sort(unique(Perf_long$year))) +
  labs(x = "Moving-average window (years)",
       y = "Metric value") +
       #colour = "Texture") +
  theme_minimal(base_size = 12)

