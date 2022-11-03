## ridge/ lasso regression

df %>% glimpse()

## train model
## help with overfitting
set.seed(42)

# alpha=0 ridge
# alpha=1 lasso
myGrid <- expand.grid(alpha = 0:1,
    lambda = seq(0.001, 1, length=20))

regularized_model <- train(
    diabetes ~ .,
    data = train_df,
    method = "glmnet",
    tuneGrid = myGrid,
    trControl = trainControl(
      method = "cv",
      number = 5,
      verboseIter = TRUE
    )
)

## test model
p <- predict(regularized_model, newdata=test_df)

confusionMatrix(p, test_df$diabetes,
                mode = "prec_recall",
                positive = "pos")


