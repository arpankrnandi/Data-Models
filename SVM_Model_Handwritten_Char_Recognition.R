library(kernlab)
library(readr)
library(caret)
library(caTools)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)

#code starts here
#Loading Data
test_data <- read.csv("mnist_test.csv",stringsAsFactors = F,header = F)
train_data <- read.csv("mnist_train.csv",stringsAsFactors = F,header = F)
#Understanding Dimensions
dim(train_data)

#Structure of the dataset
str(train_data)

#printing first few rows
head(train_data)

#Exploring the data
summary(train_data)

# Changing output variable "V1" to factor type 

train_data$V1 <- as.factor(train_data$V1)

# Checking missing value
sapply(train_data, function(x) sum(is.na(x))) # No missing values found. Data set is correct

# Linear model - SVM  at Cost(C) = 1

set.seed(100)

#take 30% of entier train data for faster calculation and manage calculation load

indices = sample.split(train_data$V1, SplitRatio = 0.30)

train = train_data[indices,]

#####################################################################
# We will be using 30% of provided data to train the model. 
#####################################################################

# Model with C =1
model_1<- ksvm(V1 ~ ., data = train,scale = FALSE,C=1)

# Predicting the model results and store them in factor form
evaluate_1<- as.factor(predict(model_1, test_data))

test_data$V1 <- as.factor(test_data$V1)

# Confusion Matrix - Finding accuracy, Sensitivity and specificity

confusionMatrix(evaluate_1, test_data$V1)

#accuracy is 96.6%

#####################################################################
# Hyperparameter tuning and Cross Validation  - Linear - SVM 
######################################################################

# We will use the train function from caret package to perform crossvalidation

trainControl <- trainControl(method="cv", number=5)
# Number - Number of folds 
# Method - cross validation

metric <- "Accuracy"

set.seed(100)

######################################################################
# Checking overfitting - Linear - SVM
######################################################################
# making a grid of C values. 
grid <- expand.grid(C=seq(1, 25, by=5))

trainControl <- trainControl(method="cv", number=5,allowParallel = TRUE)
# Performing 5-fold cross validation
fit.svm <- train(V1~., data=train, method="svmLinear", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

#ccuracy was used to select the optimal model using the largest value.
#The final value used for the model was C = 1.
#   C  Accuracy   Kappa    
#   1  0.9077788  0.8974945
# Printing cross validation result
print(fit.svm)

# Plotting "fit.svm" results
plot(fit.svm)

###############################################################################


# We will use the train function from caret package to perform crossvalidation

# Making grid of "sigma" and C values. 
grid <- expand.grid(.sigma=seq(0.01, 0.05, by=0.01), .C=seq(1, 5, by=1))


# Performing 5-fold cross validation

trainControl <- trainControl(method="cv", number=5,allowParallel = TRUE)
fit.svm_radial <- train(V1~., data=train, method="svmRadial", metric=metric, 
                        tuneGrid=grid, trControl=trainControl)

# Printing cross validation result
print(fit.svm_radial)
# Best tune at sigma = 0.01 & C=2, Accuracy - 0.935

# Plotting model results
plot(fit.svm_radial)

######################################################################
# Checking overfitting - Non-Linear - SVM
######################################################################

# Validating the model results on test data
evaluate_non_linear<- predict(fit.svm_radial, test)
confusionMatrix(evaluate_non_linear, test$V1)

#final model with updated sigma and c#######################

evaluate_non_linear



######################################################################

