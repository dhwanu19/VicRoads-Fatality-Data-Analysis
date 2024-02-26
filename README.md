# VicRoads Fatality Data Analysis

## Project Overview

This project undertakes a detailed analysis of road crash data in Victoria from 2006 to 2020, with the goal of identifying key factors influencing fatality rates. Through this analysis, we aim to contribute valuable insights towards enhancing road safety measures.

## Data Analysis Stages

### Exploratory Data Analysis (EDA)

- **Objective**: To explore a dataset encompassing 27 variables, identifying initial patterns that correlate with fatality rates.
- **Outcome**: Identification of potential predictors for more detailed statistical analysis.

### Statistical Modelling

- **Approach**: Application of logistic regression to ascertain significant predictors of fatal accidents, using Generalized Linear Models (GLM) and employing backward selection based on the Bayesian Information Criterion (BIC) for model optimization.

### Predictive Modelling

- **Methodology**: A range of predictive models were tested, including Cross-Validation (CV), Lasso, Ridge, K-Nearest Neighbors (KNN), Gradient Boosting Machine (GBM), Random Forest, and GLM.
- **Evaluation**: The GBM model was selected due to its superior AUC scores, indicating high accuracy in predicting scenarios with a high risk of fatalities. The model achieved a prediction accuracy of **67%**.

## Key Achievements

### Academic Recognition

- **High Praise**: The project received commendation from Dr. Andres Villegas and Dr. Patrick Laub, acknowledging its contribution to the field of road safety.
- **Grade Awarded**: A high grade of **94/100** was awarded, reflecting the project's excellence and thoroughness.

### Project Ranking

- **Top 5%**: This analysis was ranked in the top 5% of all submissions, distinguished by its accuracy and comprehensive approach.

## Key Findings

Significant predictors of fatality rates include driver age, vehicle type, and the use of safety gear. The GBM model, with a prediction accuracy of 67%, demonstrates high reliability in forecasting fatal accidents.

## Methodology

Comprehensive methodologies were documented for each stage of the project, from data cleaning and preparation to statistical analysis and model evaluation. The project successfully overcame challenges such as data imbalance and model selection through rigorous testing and performance tuning.

## Conclusion and Implications

The findings underscore the critical role of data-driven analysis in identifying risk factors and informing road safety strategies. The project's data-driven approach provides a foundation for informed decision-making and policy development aimed at reducing road fatalities.

## Technical Details

- **Tools Used**: The analysis was primarily conducted using R, leveraging its statistical and data analysis capabilities.
- **Challenges Overcome**: The project addressed challenges including data imbalance and model selection, focusing on optimizing model performance to enhance predictive accuracy.
