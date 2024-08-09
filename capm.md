---
Title: "ACTL1101 Assignment Part B"
Author: "Trong Hieu LE"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(quantmod)
library(ggplot2)
library(tidyverse)
```

# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM prlovides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```{r load-data}
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```{r data}
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

\[ E(R_i) = R_f + \beta_i (E(R_m) - R_f) \]

Where:

- \( E(R_i) \) is the expected return on the capital asset,
- \( R_f \) is the risk-free rate,
- \( \beta_i \) is the beta of the security, which represents the systematic risk of the security,
- \( E(R_m) \) is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```{r return}
#fill the code
df <- df %>% 
    mutate(dr_amd = NA , dr_gspc = NA)

for (i in 2:nrow(df))
{
    df$dr_amd[i] <- (df$AMD[i] - df$AMD[i-1])/df$AMD[i-1]
    df$dr_gspc[i] <- (df$GSPC[i] - df$GSPC[i-1])/df$GSPC[i-1]
}
```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```{r riskfree}
#fill the code
df <- df %>% 
    mutate(daily_risk_free_rate = NA)

for (i in 1:nrow(df))
{
    df$daily_risk_free_rate[i] <- ((df$RF[i]/100) + 1)^(1/360) - 1
}
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.


```{r excess return}
#fill the code
df <- df %>% mutate(er_amd = NA , er_gspc = NA)
for (i in 1:nrow(df))
{
    df$er_amd[i] <- df$dr_amd[i] - df$daily_risk_free_rate[i]
    df$er_gspc[i] <- df$dr_gspc[i] - df$daily_risk_free_rate[i]
    
}

```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```{r lm}
#fill the code
model1 <- lm(er_amd ~ er_gspc, data = df)
summary(model1)

```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:**
\(\beta\) is approximately 1.57 which is greater than 1 therefore we can say that AMD share is more volatile than the S&P500

#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```{r plot}

ggplot(df, aes(x = er_gspc, y = er_amd)) + geom_point(fill = "white", shape=21) + 
  stat_smooth(method = "lm", se = TRUE)


#fill the code
```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.

*Hint: Calculate the daily standard error of the forecast ($s_f$), and assume that the annual standard error for prediction is $s_f \times \sqrt{252}$. Use the simple return average method to convert daily stock returns to annual returns if needed.*


**Answer:**
Using the formula of CAPM model

\[ E(R_i) = R_f + \beta_i (E(R_m) - R_f) \]

\[ E(R_i) - R_f = \beta_i (E(R_m) - R_f) \]

From Step 2, the prediction model become:

\[ (E(R_i) - R_f) =\widehat{Y} = 1.5699987 * (E(R_m) - R_f) + 0.0011041 = 1.5699987 * \widehat{X} + 0.0011041 \]

For Step 3, with $R_f = 0.05$ and $E(R_m) = 0.133$. Then

\[ \widehat{Y} = 1.5699987 * (0.133-0.05) +0.0011041 = 0.131414 \] and this is the prediction of excessive return of AMD

```{r pi}

#fill the code
#Using the model, the estimated of excessive return of AMD is 
prediction_er_amd <- 0.131414

#estimated of excessive return of S&P500 is 
x_hat <- 0.083

#Our linear model ignores the first row of data as it contains NA value. Therefore, for Step 3 we can also ignore it

new_df <- df[-1,]

#number of observation 
n <- length(new_df$er_gspc) 
  
#mean of excessive return of S&P500
mean_of_er_gspc <- mean(new_df$er_gspc)
  
#Standard error of our linear model  
se <- sqrt(sum(residuals(model1)^2) / (n-1-1))

#Sum of squares of the es_gspc values
SSX <- sum((new_df$er_gspc - mean_of_er_gspc)^2)

# Calculate the standard error of the forecast
daily_sf <- se * sqrt(1 + 1/n + (x_hat - mean_of_er_gspc)^2 / SSX)
sf <- daily_sf*sqrt(252)

# 90% confidence interval
alpha <- 0.1

# t value for 90% confidence level with n-1-1 degrees of freedom
t_value <- qt(1 - alpha/2, df = n - 2)

# Prediction interval of Excessive return of AMD
lower_bound_er_amd <- prediction_er_amd - t_value * sf
upper_bound_er_amd <- prediction_er_amd + t_value * sf

# Prediction interval of Annual expected return of AMD

lower_bound_amd <-lower_bound_er_amd + 0.05
upper_bound_amd <- upper_bound_er_amd + 0.05

```
**Conclusion: The 90% prediction interval for the annual expected return of AMD given the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3% , is [-0.499659, 0.862487]**
