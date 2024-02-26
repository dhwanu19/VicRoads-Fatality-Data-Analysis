# Load necessary packages
library(ROSE)
library(caret)
library(class)
library(glmnet)
library(rpart)
library(randomForest)
library(ipred)
library(gbm)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(grid)
library(magick)
library(lubridate)
library(stringr)
library(MASS)
library(pROC)
library(class)

# LOAD DATA #
setwd("C:/Users/dhwan/OneDrive/Desktop/ACTL3142")

data <- read_csv("VicRoadFatalData.csv")

## TASK 2 ## ###################################################################
# Cleaning Data
# Drop unnecessary variables

data <- dplyr::select(data, -DRIVER_ID, -`Age Group`, -VEHICLE_ID, -ACCIDENT_NO)
# data <- data %>% filter(SEX != "U")

# Make sure all predictors are numeric or factors
data <- data.frame(lapply(data, function(x) if(is.character(x)) as.factor(x) else x))

# Extract year from ACCIDENTDATE
data$ACCIDENTDATE <- year(data$ACCIDENTDATE)

# Extract hour from ACCIDENTTIME
data$ACCIDENTTIME <- hour(data$ACCIDENTTIME)

# Define the breaks for the post code categories
breaks <- seq(3000, 4000, by = 50)

# Add the max value of OWNER_POSTCODE to account for all other post codes
breaks <- c(breaks, max(data$OWNER_POSTCODE))

# Create labels for the categories
labels <- paste0(rep(seq(3000, 3950, by = 50), each = 1), "-", rep(seq(3050, 4000, by = 50), each = 1), collapse = NULL)
labels <- c(labels, "other")

# Convert OWNER_POSTCODE to a factor with the specified breaks and labels
data$OWNER_POSTCODE_CAT <- cut(data$OWNER_POSTCODE, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)

# Change any NA values in the new variable to "other"
data$OWNER_POSTCODE_CAT[is.na(data$OWNER_POSTCODE_CAT)] <- "other"

# Convert the new variable to a factor
data$OWNER_POSTCODE_CAT <- as.factor(data$OWNER_POSTCODE_CAT)

# REMOVE SOME VARIABLES DEEMED INSIGNIFICANT IN PART 2:
data <- dplyr::select(data, - LICENCE_STATE, - VEHICLE_MAKE, - VEHICLE_COLOUR, - OWNER_POSTCODE_CAT, - OWNER_POSTCODE)
# REMOVE VARIABLES ON ACCIDENT_CONDITION:
data <- dplyr::select(data, - ACCIDENTDATE, - ACCIDENTTIME, - DAY_OF_WEEK, - ACCIDENT_TYPE, - LIGHT_CONDITION, - ROAD_GEOMETRY, - SPEED_ZONE, - SURFACE_COND, - ATMOSPH_COND, - ROAD_SURFACE_TYPE)
################################################################################

# Check for missing values in the 'fatal' column
sum(is.na(data$fatal))

# Check the number of levels in 'fatal'
length(levels(data$fatal))

# Convert 'fatal' to factor
data$fatal <- as.factor(data$fatal)
# Check the structure of your data
str(data)
################################################################################
# Split the data into training and validation sets
set.seed(12346)
trainIndex <- createDataPartition(data$fatal, p = 0.75, 
                                  list = FALSE, 
                                  times = 1)

train <- data[trainIndex,]
valid <- data[-trainIndex,]

# Balance data
balanced.train <- ovun.sample(fatal ~ ., data = train, method = "both", N = 200000)$data

balanced.train$fatal <- ifelse(balanced.train$fatal == 'TRUE', 1, 0)
valid$fatal <- ifelse(valid$fatal == 'TRUE', 1, 0)

# Convert the predictors to a matrix
x.train <- model.matrix(fatal ~ .-1, data = balanced.train)
y.train <- balanced.train$fatal
x.valid <- model.matrix(fatal ~ .-1, data = valid)
y.valid <- valid$fatal

# LASSO / RIDGE ################################################################
# Set up cv
cvfit <- cv.glmnet(x.train, y.train, alpha = 1, family = "binomial")
pred.lasso <- predict(cvfit, s = cvfit$lambda.min, newx = x.valid, type = "response")
cvfit.ridge <- cv.glmnet(x.train, y.train, alpha = 0, family = "binomial")
pred.ridge <- predict(cvfit.ridge, s = cvfit.ridge$lambda.min, newx = x.valid, type = "response")

# Compute ROC curve and AUC for Lasso
ROCLasso <- roc(valid$fatal, pred.lasso)
cat('AUC for Lasso:', auc(ROCLasso), '\n')
plot(ROCLasso, main="ROC Curve for Lasso")

# Confusion matrix for Lasso
confusionMatrix(table(round(pred.lasso), valid$fatal), positive = "1")

# Set up cv
cvfit <- cv.glmnet(x.train, y.train, alpha = 1, family = "binomial")
pred.lasso <- predict(cvfit, s = cvfit$lambda.min, newx = x.valid, type = "response")
cvfit.ridge <- cv.glmnet(x.train, y.train, alpha = 0, family = "binomial")
pred.ridge <- predict(cvfit.ridge, s = cvfit.ridge$lambda.min, newx = x.valid, type = "response")

# Compute ROC curve and AUC for Ridge
ROCRidge <- roc(valid$fatal, pred.ridge)
cat('AUC for Ridge:', auc(ROCRidge), '\n')
plot(ROCRidge, main="ROC Curve for Ridge")

# Confusion matrix for Ridge
confusionMatrix(table(round(pred.ridge), valid$fatal), positive = "1")

# KNN ##########################################################################
# K-Nearest Neighbors (KNN)

# Run KNN
knn.pred <- knn(train = x.train, test = x.valid, cl = y.train, k = 5)

# Convert predictions to numeric
knn.pred.numeric <- as.numeric(as.character(knn.pred))

# ROC curve and AUC
ROCKNN <- roc(valid$fatal, knn.pred.numeric)
cat('AUC for KNN:', auc(ROCKNN), '\n')
plot(ROCKNN, main="ROC Curve for KNN")

# Confusion matrix 
confusionMatrix(table(knn.pred.numeric, valid$fatal), positive = "1")

################################################################################
# Gradient Boosting Machine (GBM)
# Set up GBM parameters
gbm.fit <- gbm(formula = fatal ~ .,
               distribution = "bernoulli",
               data = balanced.train,
               n.trees = 1000,
               interaction.depth = 4,
               n.minobsinnode = 10,
               shrinkage = 0.01,
               bag.fraction = 0.5,
               train.fraction = nrow(train) / (nrow(train) + nrow(valid)),
               keep.data = TRUE,
               verbose = FALSE)

summary(gbm.fit)

# predictions
gbm.pred <- predict(gbm.fit, newdata = valid, n.trees = 1000, type = "response")

# ROC curve and AUC 
ROCGbm <- roc(valid$fatal, gbm.pred)
cat('AUC for GBM:', auc(ROCGbm), '\n')
plot(ROCGbm, main="ROC Curve for GBM")

# Confusion matrix for GBM
confusionMatrix(rf.pred, valid$fatal, positive = "1")

################################################################################
# Random Forest
levels(valid$fatal)
str(valid)
unique(valid$fatal)
valid$fatal <- as.factor(valid$fatal)
unique(valid$fatal)
table(train$fatal)
table(valid$fatal)

is.factor(valid$fatal)

# Train 
valid$fatal <- as.factor(valid$fatal)

rf.fit <- randomForest(fatal ~ ., data = balanced.train, ntree = 10, importance = TRUE)

print(rf.fit)

# Predictions
rf.pred <- predict(rf.fit, newdata = valid, type = "class")
table(rf.pred)
is.factor(rf.pred)

# ROC curve and AUC
ROCRf <- roc(valid$fatal, factor(rf.pred, ordered = TRUE))

cat('AUC for Random Forest:', auc(ROCRf), '\n')
plot(ROCRf, main="ROC Curve for Random Forest")

# Confusion matrix 
confusionMatrix(table(as.numeric(as.character(rf.pred)), valid$fatal), positive = "1")
ROCRf$auc
################################################################################
# GLM Model
glmModel <- glm(fatal ~ ., data = balanced.train, family = "binomial")

summary(glmModel)

# Predictions
glm.pred <- predict(glmModel, newdata = valid, type = "response")

# Convert predictions to binary 
glm.pred.binary <- ifelse(glm.pred > 0.5, 1, 0)

# ROC curve and AUC
ROCGLM <- roc(valid$fatal, glm.pred)
cat('AUC for GLM:', auc(ROCGLM), '\n')
plot(ROCGLM, main="ROC Curve for GLM")

# Confusion matrix 
confusionMatrix(table(glm.pred.binary, valid$fatal), positive = "1")
###############################################################################
# Combine cuves
ROCGLM <- roc(valid$fatal, glm.pred)

ROCLasso <- roc(valid$fatal, pred.lasso)

ROCRidge <- roc(valid$fatal, pred.ridge)

# Plot the ROC curve for all three models
plot(ROCGLM, col="red", main="ROC Curve for GLM, Lasso, Ridge")
lines(ROCLasso, col="blue")
lines(ROCRidge, col="green")
legend("bottomright", legend=c("GLM", "Lasso", "Ridge"),
       col=c("red", "blue", "green"), lwd=2)

################################################################################
# LOAD DATA #
setwd("C:/Users/dhwan/OneDrive/Desktop/ACTL3142")

evalData  <- read_csv("Drivers_Eval.csv")

################################################################################
# Cleaning evalData
# Drop unnecessary variables

evalData <- dplyr::select(evalData, -DRIVER_ID, -`Age Group`, -VEHICLE_ID)

evalData <- data.frame(lapply(evalData, function(x) if(is.character(x)) as.factor(x) else x))
evalData$fatal <as.factor(evalData$fatal)
# Define the breaks for the post code categories
breaks <- seq(3000, 4000, by = 50)

# Add the max value of OWNER_POSTCODE to account for all other post codes
breaks <- c(breaks, max(evalData$OWNER_POSTCODE))

# Create labels for the categories
labels <- paste0(rep(seq(3000, 3950, by = 50), each = 1), "-", rep(seq(3050, 4000, by = 50), each = 1), collapse = NULL)
labels <- c(labels, "other")

# Convert OWNER_POSTCODE to a factor with the specified breaks and labels
evalData$OWNER_POSTCODE_CAT <- cut(evalData$OWNER_POSTCODE, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)

# Change any NA values in the new variable to "other"
evalData$OWNER_POSTCODE_CAT[is.na(evalData$OWNER_POSTCODE_CAT)] <- "other"

# Convert the new variable to a factor
evalData$OWNER_POSTCODE_CAT <- as.factor(evalData$OWNER_POSTCODE_CAT)

################################################################################
# Read the evaluation dataset
evalData <- read_csv("Drivers_Eval.csv")

# Use the GBM model to make predictions on evaluation data
gbm.pred.eval <- predict(gbm.fit, newdata = evalData, n.trees = 1000, type = "response")

# Adding prediction to the evalData dataframe
evalData$pred.fatal <- gbm.pred.eval

# Sort evalData by the predicted probabilities in descending order
evalData <- evalData[order(evalData$pred.fatal, decreasing = TRUE), ]

# Select the top 2500 drivers
selected.drivers <- head(evalData, 2500)

# Export the DRIVER_ID of the selected drivers to a csv file
write.csv(selected.drivers$DRIVER_ID, file = "selected_drivers.csv", row.names = FALSE)
# Export all variables of the selected drivers to a csv file
write.csv(selected.drivers, file = "temp.csv", row.names = FALSE)

################################################################################
# Quick data on predicted drivers
predData <- read_csv("temp.csv")

sex_counts <- predData %>% 
  count(SEX)

print(sex_counts)

sex_counts <- evalData %>% 
  count(SEX)

print(sex_counts)

vehicle_type_counts <- predData %>% 
  count(VEHICLE_TYPE)

print(vehicle_type_counts)

vehicle_type_counts <- evalData %>% 
  count(VEHICLE_TYPE)

print(vehicle_type_counts)

age_counts <- predData %>% 
  count(AGE)

print(age_counts)

age_counts <- evalData %>% 
  count(AGE)

print(age_counts)

