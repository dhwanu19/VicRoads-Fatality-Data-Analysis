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

# LOAD DATA ####################################################################
setwd("C:/Users/dhwan/OneDrive/Desktop/ACTL3142")

data <- read_csv("VicRoadFatalData.csv")
# DATA CLEANING & FORMATTING ###################################################
# Drop/ filter variables
data <- dplyr::select(data, -DRIVER_ID, -`Age Group`, -VEHICLE_ID, -ACCIDENT_NO)
#data <- data %>% filter(SEX != "U")

# Make sure all predictors are numeric or factors
data <- data.frame(lapply(data, function(x) if(is.character(x)) as.factor(x) else x))

# Only consider year for accident data
data$ACCIDENTDATE <- year(data$ACCIDENTDATE)

# Only consider hour for accident time
# data$ACCIDENTTIME <- hour(data$ACCIDENTTIME)
data$ACCIDENTTIME <- as.numeric(format(strptime(data$ACCIDENTTIME, format = "%H:%M:%S"), "%H"))

# Separate postcodes into categories
breaks <- seq(3000, 4000, by = 50)
breaks <- c(breaks, max(data$OWNER_POSTCODE))
labels <- paste0(rep(seq(3000, 3950, by = 50), each = 1), "-", rep(seq(3050, 4000, by = 50), each = 1), collapse = NULL)
labels <- c(labels, "other")
data$OWNER_POSTCODE_CAT <- cut(data$OWNER_POSTCODE, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)
data$OWNER_POSTCODE_CAT[is.na(data$OWNER_POSTCODE_CAT)] <- "other"

# Convert the new variable to a factor
data$OWNER_POSTCODE <- as.factor(data$OWNER_POSTCODE_CAT)

# Convert SPEED_ZONE back to numeric
data$SPEED_ZONE <- as.numeric(as.character(data$SPEED_ZONE))

str(data)

# EDA - DRIVER'S PROFILE - AGE #################################################
# Create age groups
ageBins <- c(0, 17, 21, 25, 29, 39, 49, 59, 64, 69, Inf)
ageBinLabels <- c("0-17", "18-21", "22-25", "26-29", "30-39", "40-49", "50-59", "60-64", "64-69", "70+")
data$AgeGroup <- cut(data$AGE, breaks = ageBins, labels = ageBinLabels, include.lowest = TRUE, right = FALSE)

# Group data by age groups
groupedData <- data %>%
  group_by(AgeGroup) %>%
  summarise(fatalAccidents = sum(fatal), totalAccidents = n()) %>%
  mutate(proportionFatal = fatalAccidents / totalAccidents)

# Plot proportion of fatal accidents by age group
ggplot(groupedData, aes(x = AgeGroup, y = proportionFatal)) +
  geom_bar(stat = "identity", fill = "lightgrey") +
  labs(x = "Age Group", y = "Proportion of Fatal Accidents") +
  theme_light()

# EDA - DRIVER'S PROFILE - SEX #################################################
grid.newpage()

# Group data by sex
groupedData <- data %>%
  group_by(SEX) %>%
  summarise(totalAccidents = n(), fatalAccidents = sum(fatal),
            proportionFatal = fatalAccidents / totalAccidents)

# Convert to table and print
accidentsBySexTable <- tableGrob(groupedData)
grid.draw(accidentsBySexTable)

# EDA - ACCIDENT CONDITIONS - HELMET_BELT_WORN #################################
groupedData <- data %>%
  group_by(HELMET_BELT_WORN) %>%
  summarise(fatalAccidents = sum(fatal), totalAccidents = n()) %>%
  mutate(proportionFatal = fatalAccidents / totalAccidents)

# Plot proportion of fatal accidents by HELMET_BELT_WORN
ggplot(groupedData, aes(x = HELMET_BELT_WORN, y = proportionFatal)) +
  geom_bar(stat = "identity", fill = "lightgrey") +
  labs(x = "HELMET_BELT_WORN", y = "Proportion of Fatal Accidents") +
  theme_light()
# EDA - ACCODENT CONDITIONS - TIME OF DAY ######################################
# Create time bins
timeBins <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)
timeBinLabels <- c("12am", "1am", "2am", "3am", "4am", "5am", "6am", "7am", "8am", "9am", "10am", "11am", 
                   "12pm", "1pm", "2pm", "3pm", "4pm", "5pm", "6pm", "7pm", "8pm", "9pm", "10pm", "11pm")

data$AccidentTimeGroup <- cut(data$ACCIDENTTIME, breaks = timeBins, labels = timeBinLabels, include.lowest = TRUE)

# Group data by time bins
groupedData <- data %>%
  group_by(AccidentTimeGroup) %>%
  summarise(fatalAccidents = sum(fatal), totalAccidents = n()) %>%
  mutate(proportionFatal = fatalAccidents / totalAccidents)

# Plot line graph
ggplot(groupedData, aes(x = AccidentTimeGroup, y = proportionFatal, group = 1)) +
  geom_line(colour = "black") +
  geom_point(colour = "grey") +
  labs(x = "Hour of Day", y = "Proportion of Fatal Accidents") +
  theme_light()

# Reorder to start at 5pm
data$AccidentTimeGroup <- factor(data$AccidentTimeGroup, levels = c(timeBinLabels[17:24], timeBinLabels[1:16]))

# Regroup
groupedAccidentTimeData <- data %>%
  group_by(AccidentTimeGroup) %>%
  summarise(fatalAccidents = sum(fatal), totalAccidents = n()) %>%
  mutate(proportionFatal = fatalAccidents / totalAccidents)

# Plot line graph
ggplot(groupedAccidentTimeData, aes(x = AccidentTimeGroup, y = proportionFatal, group = 1)) +
  geom_line(colour = "black") +
  geom_point(colour = "grey") +
  labs(x = "Hour of Day", y = "Proportion of Fatal Accidents") +
  theme_light()

# EDA - VEHICLE CHARACTERISTICS - VEHICLE AGE ##################################
# Compute vehicle age column
# Calculate vehicle age
data$VehicleAge <- data$ACCIDENTDATE - data$VEHICLE_YEAR_MANUF

# Create age bins
vehicleAgeBins <- c(0, 5, 10, 15, 20, 25, Inf)
vehicleAgeBinLabels <- c("0-5", "5-10", "10-15", "15-20", "20-25", "26+")

data$VehicleAgeGroup <- cut(data$VehicleAge, breaks = vehicleAgeBins, labels = vehicleAgeBinLabels, include.lowest = TRUE)

# Remove rows where VehicleAgeGroup is na
data <- data[!is.na(data$VehicleAgeGroup), ]

# Group data
groupedVehicleAgeData <- data %>%
  group_by(VehicleAgeGroup) %>%
  summarise(fatalAccidents = sum(fatal), totalAccidents = n()) %>%
  mutate(proportionFatal = fatalAccidents / totalAccidents)

# Plot
ggplot(groupedVehicleAgeData, aes(x = VehicleAgeGroup, y = proportionFatal, group = 1)) +
  geom_line(colour = "black") +
  geom_point(colour = "grey") +
  labs(x = "Vehicle Age at Accident (years)", y = "Proportion of Fatal Accidents") +
  theme_light()
# EDA - VEHICLE CHARACTERISTICS - VEHICLE TYPE #################################
# Group data by vehicle type
dataGroupedByVehicleType <- data %>%
  group_by(VEHICLE_TYPE) %>%
  summarise(fatalAccidents = sum(fatal), totalAccidents = n()) %>%
  mutate(proportionFatal = fatalAccidents / totalAccidents)

dataGroupedByVehicleType$VEHICLE_TYPE_ABBR <- str_sub(dataGroupedByVehicleType$VEHICLE_TYPE, 1, 7)

# Plot
ggplot(dataGroupedByVehicleType, aes(x = VEHICLE_TYPE_ABBR, y = proportionFatal)) +
  geom_bar(stat = "identity", fill = "lightgrey") +
  labs(x = "Vehicle Type", y = "Proportion of Fatal Accidents") +
  theme_light()