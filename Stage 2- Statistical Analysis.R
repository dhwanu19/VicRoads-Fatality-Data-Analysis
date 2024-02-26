library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(grid)
library(magick)
library(lubridate)
library(stringr)
library(MASS)
library(glmnet) 
library(pROC)
library(ROSE)
library(caret)

# LOAD DATA #
setwd("C:/Users/dhwan/OneDrive/Desktop/ACTL3142")

data <- read_csv("VicRoadFatalData.csv")

## TASK 2 ## ###################################################################
# Cleaning Data
# Drop/ filter variables
data <- dplyr::select(data, -DRIVER_ID, -`Age Group`, -VEHICLE_ID, -ACCIDENT_NO, -VEHICLE_YEAR_MANUF )
# data <- data %>% filter(SEX != "U")

# Make sure all predictors are numeric or factors
data <- data.frame(lapply(data, function(x) if(is.character(x)) as.factor(x) else x))

# Only consider year for accident data
data$ACCIDENTDATE <- year(data$ACCIDENTDATE)

# Only consider hour for accident time
data$ACCIDENTTIME <- hour(data$ACCIDENTTIME)

# Separate postcodes into categories
breaks <- seq(3000, 4000, by = 50)
breaks <- c(breaks, max(data$OWNER_POSTCODE))
labels <- paste0(rep(seq(3000, 3950, by = 50), each = 1), "-", rep(seq(3050, 4000, by = 50), each = 1), collapse = NULL)
labels <- c(labels, "other")

# Convert the new variable to a factor
data$OWNER_POSTCODE_CAT <- cut(data$OWNER_POSTCODE, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)

# Change any NA values "other"
data$OWNER_POSTCODE_CAT[is.na(data$OWNER_POSTCODE_CAT)] <- "other"

# Convert the new variable to a factor
data$OWNER_POSTCODE <- as.factor(data$OWNER_POSTCODE_CAT)

# Convert SPEED_ZONE back to numeric
data$SPEED_ZONE <- as.numeric(as.character(data$SPEED_ZONE))

str(data)
# TASK 2 - STATISTICAL MODELLING ###############################################
# Subset selection #############################################################
fullModel <- glm(fatal ~ ., data = data, family = binomial)
# Perform backward selection using BIC
backwardModelBIC <- step(fullModel, 
                         scope = list(lower = ~1, upper = ~.), 
                         direction = "both", 
                         trace = FALSE, 
                         k = log(nrow(data)))

# Print the final model
print(backwardModelBIC)
summary(backwardModelBIC)
# Create GLM ###################################################################
# Check for missing values in the 'fatal' column
sum(is.na(data$fatal))

# Check the number of levels in 'fatal'
length(levels(data$fatal))

# Convert 'fatal' to factor
data$fatal <- as.factor(data$fatal)
# Check the structure of your data
str(data)

set.seed(123)
# Fitting model
glmModel <- glm(fatal ~ SEX + AGE + HELMET_BELT_WORN +  VEHICLE_TYPE + 
                  TOTAL_NO_OCCUPANTS + ACCIDENTTIME + ACCIDENT_TYPE + 
                  LIGHT_CONDITION + ROAD_GEOMETRY + SPEED_ZONE + SURFACE_COND, data = data, family = "binomial")

summary(glmModel)

# Perform Shrinkage ############################################################