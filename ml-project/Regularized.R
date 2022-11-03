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

## ridge / lasso regresion
df <- churn
df %>% glimpse()


## train model
## help woth overfitting
set.seed(42)

myGird <- expand.grid(alpha = 0:1,
                      lambda = seq(0.001,1,length = 20))


regularized_model <- train(
    churn ~ .,
    data = train_df,
    method = "glmnet",
    tuneGrid = myGird,
    trControl = trainControl(
        method = "cv",
        number = 5,
        verboseIter = TRUE
    )
)

## test model
p <- predict(regularized_model, newdata = test_df)

confusionMatrix(p,test_df$churn,
                positive = "Yes",
                mode = "prec_recall")




