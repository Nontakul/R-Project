library(tidyverse)
library(caret)
library(mlbench)
library(readr)
library(dplyr)
library(MLmetrics)
library(rpart.plot)


## complete

df <- churn

check_complete <- function(df) mean(complete.cases(df))
check_complete(df)

## glimpse data
glimpse(df)

df %>%
    count(churn) %>%
    mutate(pct = n/sum(n))

## 1. spilt data
set.seed(42)
id <- createDataPartition(y = df$churn,
                          p = 0.8,
                          list = FALSE)


train_df <- df[id, ]
test_df <- df[-id, ]

# 2. train model
set.seed(42)
myGrid <- data.frame(cp = seq(0.001, 0.3, by=0.005))
tree_model <- train(churn ~ .,
                    data = train_df,
                    method = "rpart",
                    tuneGrid = myGrid,
                    trControl = trainControl(
                        method = "cv",
                        number = 5
                    ))
# 3. Test model
rpart.plot(tree_model$finalModel) 



