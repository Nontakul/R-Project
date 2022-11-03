library(tidyverse)
library(caret)
library(mlbench)

## example linear regression
head(mtcars)

# caret interface
set.seed(42)
model_lm_caret <- train(mpg ~ .,
                        data = mtcars,
                        method = "lm") 

model_lm_caret$finalModel

## your first model - knn
## load data set BostonHousing

data("BostonHousing")

## 1. Split Data
train_test_split <- function(df, train_size=0.8) {
    set.seed(42)
    n <- nrow(df)
    id <- sample(1:n, size=n*train_size)
    train_df <- df[id, ]
    test_df <- df[-id, ]
    return( list(train=train_df, test=test_df) )
}

split_data <- train_test_split(BostonHousing, 0.8)

train_df <- split_data$train
test_df <- split_data$test

## 2. Train Model
## Grid Search Hyperparameter Tuning
set.seed(42)
grid_k <- data.frame(k = 5:9)

ctrl <- trainControl(
  method = "repeatedcv", # k-fold cross validation
  number = 5,
  repeats = 5
)

knn_model <- train(medv ~ .,
                   data = train_df,
                   method = "knn",
                   metric = "Rsquared",
                   tuneGrid = grid_k,
                   trControl = ctrl)

## Predict Train Data
pred_medv_train <- predict(knn_model)
(train_rmse <- sqrt(mean((train_df$medv - pred_medv_train)**2)))

## 3. Test Model
## Scoring => Prediction
pred_medv <- predict(knn_model, newdata = test_df)
(test_rmse <- sqrt(mean((test_df$medv - pred_medv)**2)))

## var importance
varImp(knn_model)

## Save Model
saveRDS(knn_model, "knn_model_25June2022_v1.RDS")

## Load Model
knn_model <- readRDS("knn_model_25June2022_v1.RDS")

