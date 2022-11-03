
# Pima Indian Diabetes
df <- PimaIndiansDiabetes

## glimpse data
glimpse(df)

## 1. split data
set.seed(42)
id <- createDataPartition(y = df$diabetes,
                          p = 0.8,
                          list = FALSE)
train_df <- df[id, ]
test_df <- df[-id, ]

## 2. train model
set.seed(99)

myGrid <- data.frame(mtry = 2:7)

rf_model <- train(diabetes ~ .,
                    data = train_df,
                    method = "rf",
                    metric = "AUC",
                    preProcess = c("center", "scale", "nzv"),
                    tuneGrid = myGrid,
                    trControl = trainControl(
                      method = "cv",
                      number = 5,
                      verboseIter = TRUE,
                      classProbs = TRUE,
                      summaryFunction = prSummary
                    ))

p <- predict(rf_model, newdata= test_df)
confusionMatrix(p, 
                test_df$diabetes,
                mode = "prec_recall",
                positive = "pos")









