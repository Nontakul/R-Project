library(tidyverse)
library(caret)
library(mlbench)
library(readr)
library(dplyr)
library(MLmetrics)

## Data Transformation
churn <- churn %>%
    mutate(churn  = as.factor(churn),
           internationalplan  = as.factor(internationalplan),
           voicemailplan  = as.factor(voicemailplan)) 

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


## 2. Train data
set.seed(42)

ctrl <- trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE, 
    summaryFunction = prSummary,
    verboseIter = TRUE
)

rf_model <- train(
    churn ~ .,
    data = train_df,
    method = "rf",
    metric = "ROC",
    preProcess = c("center", "scale"),
    trControl = ctrl
)
## 3. Test data
p <- predict(rf_model, newdata=test_df)
mean(p == test_df$churn)


confusionMatrix(p,test_df$churn,
                positive = "Yes",
                mode = "prec_recall")


