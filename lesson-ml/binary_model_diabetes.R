
# Pima Indian Diabetes

data("PimaIndiansDiabetes")

## complete?
## target/ label?

df <- PimaIndiansDiabetes

check_complete <- function(df) mean(complete.cases(df))

check_complete(df)

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
set.seed(42)

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  #summaryFunction = twoClassSummary,
  summaryFunction = prSummary,
  verboseIter = TRUE
)

rf_model <- train(
  diabetes ~ .,
  data = train_df,
  method = "rf", 
  metric = "Recall",
  preProcess = c("center", "scale"),
  trControl = ctrl
)

## 3. test model
p <- predict(rf_model, newdata=test_df)
mean(p == test_df$diabetes)

confusionMatrix(p, test_df$diabetes, 
                positive = "pos",
                mode = "prec_recall")





