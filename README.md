# E-commerce Sales Time Series Analysis and Forecasting

This project focuses on analyzing and forecasting e-commerce sales across various product categories. Utilizing a comprehensive dataset, this study leverages time series analysis techniques to uncover trends, seasonal patterns, and volatility in sales data, aiming to predict future sales and understand the interdependence among different product categories.

## Project Overview

### Objectives
- To perform time series analysis on e-commerce sales data to identify underlying patterns and trends.
- To forecast future sales using ARMA and GARCH models.
- To explore the interdependence among sales of different product categories through copula models.

### Dataset Description
The dataset spans from February 1, 2013, to April 8, 2022, comprising 25,261 observations across 30 product categories. The data include daily sales figures, which are aggregated to monthly frequency for analysis, focusing on the period from January 2014 to December 2021.

### Methodology

#### Preprocessing
- Conversion of daily sales data into a monthly frequency time series to facilitate analysis.
- Exclusion of incomplete data for the years 2013 and 2022.

#### Data Exploration
- Decomposition of the time series into trend, seasonality, and residual components using a multiplicative model.
- Investigation of sales data volatility and clustering using log differences and kernel density estimation.

#### Forecasting
- Implementation of an ARMA model for sales forecasting, with parameter selection guided by the Bayesian Information Criterion (BIC).
- Volatility modeling and forecasting using a GARCH model to capture time-varying volatility patterns in the sales data.

#### Dependency Analysis Among Categories
- Examination of the relationship among sales in three selected categories (soccer, casual, and fitness) using copula models to capture the dependency structure.

## Results
- The time series analysis highlighted a non-stationary sales trend with significant seasonality and periods of varying volatility.
- The GARCH model provided a statistically significant fit for modeling sales volatility, suggesting its adequacy for forecasting future sales volatility.
- Dependency analysis using copula models revealed a T-student dependency structure among the selected categories, indicating interrelated sales patterns.

## Tools and Libraries Used
- R programming language for time series analysis, modeling, and visualization.
- Specific R packages such as `forecast`, `rugarch`, and `copula` for implementing ARMA, GARCH models, and copula methods respectively.

## Conclusion
The study underscores the complexity of e-commerce sales dynamics, characterized by trends, seasonality, and inter-category dependencies. Through meticulous time series analysis and the application of advanced statistical models, the project offers valuable insights for forecasting sales and understanding the intricate relationships among different product categories.

For further information, methodologies, and detailed findings, please refer to the full project documentation.
