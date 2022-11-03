## Binary Classification

data("Sonar")

## missing value?
mean(complete.cases(Sonar))

## preview data
Sonar %>% head()

## 1. split data
set.seed(42)
id <- createDataPartition(y = Sonar$Class, 
                    p = 0.7, 
                    list=FALSE)

train_df <- Sonar[id, ]
test_df <- Sonar[-id, ]

## 2. train model
## Logistic Regression
set.seed(42)

ctrl <- trainControl(
  method = "cv",
  number = 3,
  verboseIter = TRUE
)

logistic_model <- train(Class ~ .,
                        data = train_df,
                        method = "glm",
                        trControl = ctrl)

knn_model <- train(Class ~ .,
                        data = train_df,
                        method = "knn",
                        trControl = ctrl)

rf_model <- train(Class ~ .,
                   data = train_df,
                   method = "rf",
                   trControl = ctrl)

## compare three models
result <- resamples(
  list(
    logisticReg = logistic_model,
    knn = knn_model,
    randomForest = rf_model
  )
)
summary(result)


varImp(logistic_model)

## 3. test model
p <- predict(logistic_model, newdata = test_df)
mean(p == test_df$Class)

## Confusion Matrix
table(p, test_df$Class, dnn=c("Prediction", "Actual"))

confusionMatrix(p, test_df$Class, mode="prec_recall")


















