---
  title: "3.1 Crop_ΔSOC — Baseline + Incremental RF"
author: "Dr. Chih-Yu Hung"
date: "2025-07-24"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
library(tidyverse)
library(caret)
library(randomForest)
library(mgcv)       # for baseline GAM
library(doParallel)
library(yardstick)  # tidy metrics

df <- read_rds("Input/SiteData_5y.rds")   # <- adjust to your source

# Key columns assumed: deltaSOC, Cinput, MAT, MAP, MPE, Texture, SOC0, ...
thresh_C  <- 2      # t C ha-1 deemed “near-zero”
base_df   <- df %>% filter(Cinput <= thresh_C)

gam_base <- gam(deltaSOC ~ s(MAT) + s(MAP) + s(MPE) +
                  s(CARB30) + TEXT,
                data = base_df, method = "REML")

df <- df %>% mutate(baseline_pred = predict(gam_base, newdata = .),
                    deltaSOC_incr = deltaSOC - baseline_pred)


set.seed(2025)

# ---------------- parallel back-end ----------------
n_cores <- max(1, parallel::detectCores() - 1)
cl      <- makeCluster(n_cores)
registerDoParallel(cl)

# 70/30 split (stratified on sign of deltaSOC_incr to keep balance)
train_id <- createDataPartition(df$deltaSOC_incr, p = .7, list = FALSE)
train    <- df[train_id, ]
test     <- df[-train_id, ]

x_cols   <- c("Cinput", "MAT", "MAP", "MPE", "TEXT",
              "CARB30", "baseline_pred")   # include baseline as a predictor
train_x  <- train %>% select(all_of(x_cols))
test_x   <- test  %>% select(all_of(x_cols))

rf_fit <- train(
  x = train_x,  y = train$deltaSOC_incr,
  method     = "rf",
  tuneGrid   = expand.grid(mtry = 3:5),
  trControl  = trainControl(method = "cv", number = 5,
                            allowParallel = TRUE),
  ntree      = 500
)

stopCluster(cl)


train$pred_incr <- predict(rf_fit, train_x)
test$pred_incr  <- predict(rf_fit, test_x)

bind_rows(train, test) %>%
  mutate(pred_total = baseline_pred + pred_incr,
         set        = if_else(row_number() %in% train_id, "train", "test")) %>%
  group_by(set) %>%
  summarise(
    MSE   = mean((pred_total - deltaSOC)^2),
    RMSE  = sqrt(MSE),
    R2    = yardstick::rsq_trad_vec(deltaSOC, pred_total),
    MAPE  = mean(abs((pred_total - deltaSOC) / deltaSOC)) * 100,
    sMAPE = mean(2 * abs(pred_total - deltaSOC) /
                   (abs(pred_total) + abs(deltaSOC))) * 100,
    .groups = "drop"
  ) -> perf_tbl

knitr::kable(perf_tbl, digits = 3, caption = "Performance summary")

vip <- varImp(rf_fit)$importance %>% rownames_to_column("Variable")
ggplot(vip, aes(reorder(Variable, Overall), Overall)) +
  geom_col() + coord_flip() +
  labs(y = "Inc. Node Purity", x = NULL,
       title = "Random-Forest variable importance")

dir.create("Output", showWarnings = FALSE)
save(gam_base, rf_fit, file = "Output/ΔSOC_baseline+incremental_models.RData")
write_csv(perf_tbl, "Output/ΔSOC_model_performance.csv")



```r
load("Output/ΔSOC_baseline+incremental_models.RData")

newdata <- read_csv("Input/new_prediction_dataset.csv")

newdata <- newdata %>%
  mutate(baseline_pred = predict(gam_base, newdata = .))

final_pred <- predict(rf_fit, newdata) + newdata$baseline_pred