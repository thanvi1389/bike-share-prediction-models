# Bike Rental Demand Analysis

This project analyzes how environmental and seasonal factors affect bike rental demand using a real-world dataset.

## Objectives
- Model total bike rentals and user subgroups (registered, casual)
- Identify key predictors such as temperature, humidity, season, and working day
- Compare linear regression and regression tree models

## Methods
- **Data Preprocessing**: Checked missing values, transformed skewed counts using square root
- **Modeling**:
  - Linear Regression: Fit separate models for total, registered, and casual users
  - Regression Trees: Pruned trees to evaluate non-linear effects
- **Model Comparison**: RMSE, MSE, and R² used for performance evaluation

## Key Findings
- Temperature positively influences rentals; humidity and holidays reduce them
- Casual users show greater sensitivity to environmental factors and working days
- Regression trees captured complex splits but underperformed linear models overall

## Tools Used
- R (base, `rpart`, `car`, `ggplot2`)

## Dataset
Bike sharing dataset with variables like `temp`, `hum`, `season`, `cnt`, `registered`, `casual`

