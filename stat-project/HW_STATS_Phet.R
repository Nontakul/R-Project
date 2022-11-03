# install packages
install.packages(c("dplyr", "nnet", "NeuralNetTools"))
install.packages("mlbench")
library(mlbench)
library(tidyverse)
library(dplyr)
library(nnet)
library(NeuralNetTools)
data("BreastCancer")
data("BostonHousing")


## 1. Build Model linear Regression
head(BostonHousing)

lm_BH <- lm(medv ~ ., data = BostonHousing)
BostonHousing$predicted <- predict(lm_BH)

## Train RMSE
squared_error <- (BostonHousing$medv - BostonHousing$predicted) ** 2
(rmse <- sqrt(mean(squared_error)) )

## split Data
set.seed(18)
n <- nrow(BostonHousing)
id <- sample(1:n, size = n*0.7)
train_data <- BostonHousing[id, ]
test_data <- BostonHousing[-id, ] 

## Train Model
model_BH <- lm(medv ~ age + ptratio, data = train_data)
p_train <- predict(model_BH)
error_train <- train_data$medv - p_train
(rmse_train <- sqrt(mean((error_train) ** 2)))

## Test Model
p_test <- predict(model_BH, newdata = test_data)
error_test <- test_data$medv - p_test
(rmse_test <- sqrt(mean(error_test** 2)))

## Print Result
cat("Result:",
    "\n","RMSE Train:" ,rmse_train,
    "\n","RMSE Test:", rmse_test)

------------------------------------------------------------------
## 2. Build Model Logistic Regression    
##BreastCancer Data Transformation:
head(BreastCancer_Dna)
        
BreastCancer <- BreastCancer %>%
mutate(Class_label = ifelse(BreastCancer$Class == "benign",1,0))
glimpse(BreastCancer)

## Droup NA (missing values)
BreastCancer_Dna <- na.omit(BreastCancer)
nrow(BreastCancer_Dna)
glimpse(BreastCancer_Dna)

## Split Data
set.seed(18)
n <- nrow(BreastCancer_train)
id <- sample(1:n, size = n*0.7)
train_data <- BreastCancer_Dna[id, ]
test_data <- BreastCancer_Dna[-id, ]


## Train Model
logit_model <- glm(Class_label ~  Cell.size , data = train_data, 
                   family = "binomial")
p_train <- predict(logit_model, type ="response")
train_data$pred <- ifelse(p_train >= 0.5, 1,0)
mean(train_data$Class_label == train_data$pred)


## Test Model
p_test <- predict(logit_model,newdata = test_data, type ="response")
test_data$pred <- ifelse(p_test >= 0.5, 1,0)
mean(test_data$Class_label == test_data$pred)


## Confusion Matrix
conM <- table(test_data$pred,test_data$Class_label, 
              dnn = c("Predicted","Actual"))


## Model Evaluation
Acc <-  (conM[1,1] + conM[2,2]) / sum(conM)
Prec <-  conM[2,2]/ (conM[2,1] + conM[2,2])
Recall <- conM[2,2]/ (conM[1,2] + conM[2,2])
F1 <- 2*Prec*Recall/(Prec+Recall)
cat("Result:",
    "\n","Accuracy:",Acc,
    "\n","Precision:",Prec,
    "\n","Recall:",Recall,
    "\n","F1:",F1)    

---------------------------------------------------------------------
# 3. Build Model Neural Network
## Droup NA (missing values)
head(BreastCancer_Dna)
    
BreastCancer_Dna <- na.omit(BreastCancer)
nrow(BreastCancer_Dna)
glimpse(BreastCancer_Dna)    

# train test split
set.seed(2)
n <- nrow(BreastCancer_Dna)
id <- sample(1:n3,size = n3*0.7)

BreastCancer_Dna_Train <- BreastCancer_Dna[id, -1 ]
BreastCancer_Dna_Test <-  BreastCancer_Dna[-id, ]

# model training
nn_model <- nnet(Class ~ .,
                 data = BreastCancer_Dna_Train,
                 size = 3)

# plot networks
plotnet(nn_model)

# model evaluation
p <- predict(nn_model, newdata = BreastCancer_Dna_Test, type = "class")
mean(p == BreastCancer_Dna_Test$Class)
