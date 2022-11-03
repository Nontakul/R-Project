library(tidyverse)
library(caret)
library(mlbench)

data("BostonHousing")

df <- BostonHousing

train_test_split <- function(df, train_size=0.8) {
  set.seed(42)
  n <- nrow(df)
  id <- sample(1:n, size=n*train_size)
  train_df <- df[id, ]
  test_df <- df[-id, ]
  return( list(train=train_df, test=test_df) )
}

## 1. Split Data
set.seed(42)
split_data <- train_test_split(BostonHousing, 0.8)
train_df <- split_data$train 
test_df <- split_data$test

## 2. Train Model K-Fold CV
set.seed(42)
ctrl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE
)

knn_model <- train(
  medv ~ .,
  data = train_df %>% 
    select(-age, -dis, -chas),
  method = "knn",
  metric = "RMSE",
  preProcess = c("center", "scale"),
  tuneLength = 5,
  trControl = ctrl
)

## Variable Importance
varImp(knn_model)

## 3. Test Model
p <- predict(knn_model, newdata=test_df)
RMSE(p, test_df$medv)



