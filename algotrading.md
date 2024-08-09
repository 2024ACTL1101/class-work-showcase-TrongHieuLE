---
title: "ACTL1101 Assignment Part A"
author: "Trong Hieu Le"
---


## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```{r load-data}

# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

amd_df <- amd_df[, c("date", "close")]
```


##Plotting the Data
Plot the closing prices over time to visualize the price movement.
```{r plot}
plot(amd_df$date, amd_df$close,'l')
```

## Step 2: Customize Trading Period
- Define a trading period you wanted in the past five years
```{r period}

# We define a function that returns a data frame corresponding to the trading period 
# that user wants to analyse. It takes 2 arguments <start_date> and <end_date> 
# and they should be between 20-May-2019 and 17-May-2024
# Format of these 2 arguments must be "yyyy-mm-dd" , for example "2019-05-20"



return_data_frame_in_a_period <- function()
{
    
    start_date = '2010-05-20'
    end_date = '2024-05-17'
    
    # Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
    amd_df$trade_type <- NA
    amd_df$costs_proceeds <- NA  # Corrected column name
    amd_df$accumulated_shares <- 0  # Initialize if needed for tracking
    
    if (start_date < amd_df$date[1]) {start_date =  amd_df$date[1] }
    if (end_date > amd_df$date[nrow(amd_df)] ) {end_date =  amd_df$date[nrow(amd_df)]}
    
    df <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date,]
    return (df)
}
```


## Step 3: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.

```{r trading}


# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0


amd_df_in_period <- return_data_frame_in_a_period()
last_row <- nrow(amd_df_in_period)

#Assume that we always buy on the first day of trading period
amd_df_in_period$trade_type[1] <- 'buy'
amd_df_in_period$costs_proceeds[1] <- 100 * amd_df_in_period$close[1]* (-1)
total_capital_invested <- 100 * amd_df_in_period$close[1] *(-1)
amd_df_in_period$accumulated_shares[1] <-100

#Applying the strategy from day 2 of the period until its second last day
for (i in 2: (last_row - 1)) {
  
  # Fill your code here
  if (amd_df_in_period$close[i] < amd_df_in_period$close[i-1]) 
  {
    amd_df_in_period$trade_type[i] <- "buy"
    amd_df_in_period$costs_proceeds[i] <- 100 * amd_df_in_period$close[i]* (-1)
    amd_df_in_period$accumulated_shares[i] <- amd_df_in_period$accumulated_shares[i-1] + 100
    total_capital_invested <- total_capital_invested + 100 * amd_df_in_period$close[i] * (-1)
  }
  else
  {
    amd_df_in_period$trade_type[i] <- ""
    amd_df_in_period$costs_proceeds[i] <- 0
    amd_df_in_period$accumulated_shares[i] <- amd_df_in_period$accumulated_shares[i-1]
  }
}

# Last day of the period
amd_df_in_period$trade_type[last_row] <- "sell"
amd_df_in_period$accumulated_shares[last_row] <- amd_df_in_period$accumulated_shares[last_row - 1]

amd_df_in_period$costs_proceeds[last_row] <- 
      amd_df_in_period$close[last_row] * amd_df_in_period$accumulated_shares[last_row]

```




## Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```{r}
# Fill your code here
total_profit_or_loss <- sum(amd_df_in_period$costs_proceeds)
ROI <- (-1)*(total_profit_or_loss/total_capital_invested)


```

## Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)

- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- The Profit-Taking strategy (PTS) is:
  If the price of current day is less than previous price , we buy 100 shares.
  If the price of current day is greater than previous price then we consider 2 cases:
    -If the price of current day is increase by a certain percentage (e.g., 20%) from the average, 
    then we sell half of our share holdings, 
    -otherwise we don't trade i.e. no buy or sell. 

```{r option}

      

# Fill your code here


amd_df_PTS <- return_data_frame_in_a_period()
last_row_PTS <- nrow(amd_df_PTS)
amd_df_PTS$avg_purc_price <- 0
amd_df_PTS$price_perc_incr <- 0

#Assume that we always buy on the first day of trading period
amd_df_PTS$trade_type[1] <- 'buy'
amd_df_PTS$costs_proceeds[1] <- 100 * amd_df_PTS$close[1]* (-1)
amd_df_PTS$accumulated_shares[1] <-100
amd_df_PTS$avg_purc_price[1] <-amd_df_PTS$close[1]
amd_df_PTS$price_perc_incr[1] <-0
percentage_threshold <- 0.2

for (i in 2:(last_row_PTS-1))
{
      
      
  if (amd_df_PTS$close[i] < amd_df_PTS$close[i-1])
  {
    amd_df_PTS$trade_type[i] <- "buy"
    amd_df_PTS$costs_proceeds[i] <- amd_df_PTS$close[i] * 100 * (-1)
    amd_df_PTS$accumulated_shares[i] <- amd_df_PTS$accumulated_shares[i-1] + 100
    amd_df_PTS$avg_purc_price [i] <- (amd_df_PTS$avg_purc_price [i-1] + amd_df_PTS$close[i])/2
  }
  
  if (amd_df_PTS$close[i] >= amd_df_PTS$close[i-1])
  {
    amd_df_PTS$price_perc_incr[i] <- 
        (amd_df_PTS$close[i] - amd_df_PTS$avg_purc_price[i-1])/amd_df_PTS$avg_purc_price[i-1]
    
    if (amd_df_PTS$price_perc_incr[i] < percentage_threshold)
    {
      amd_df_PTS$trade_type[i] <- ""
      amd_df_PTS$costs_proceeds[i] <-0
      amd_df_PTS$accumulated_shares[i] <- amd_df_PTS$accumulated_shares[i-1]
      amd_df_PTS$avg_purc_price [i] <-amd_df_PTS$avg_purc_price [i-1]
      
    }
    
    else
    {
      amd_df_PTS$trade_type[i] <- "sell"
      amd_df_PTS$costs_proceeds[i] <- 
            (amd_df_PTS$close[i])* round((amd_df_PTS$accumulated_shares[i-1])/2)
      
      amd_df_PTS$accumulated_shares[i] <- round((amd_df_PTS$accumulated_shares[i-1])/2)
      amd_df_PTS$avg_purc_price [i] <-amd_df_PTS$avg_purc_price [i-1]
    }  
    
  }
  
}

# Last day of the period
amd_df_PTS$trade_type[last_row_PTS] <- "sell"
amd_df_PTS$accumulated_shares[last_row_PTS] <- amd_df_PTS$accumulated_shares[last_row_PTS - 1]
amd_df_PTS$avg_purc_price[last_row_PTS]<- amd_df_PTS$avg_purc_price[last_row_PTS - 1]

amd_df_PTS$price_perc_incr[last_row_PTS] <- 
      (amd_df_PTS$close[last_row_PTS] - amd_df_PTS$avg_purc_price[i-1])/amd_df_PTS$avg_purc_price[i-1]

amd_df_PTS$costs_proceeds[last_row_PTS] <- 
      amd_df_PTS$close[last_row_PTS] * amd_df_PTS$accumulated_shares[last_row_PTS]

total_profit_or_loss_PTS <- sum(amd_df_PTS$costs_proceeds)
total_capital_invested_PTS <- sum(amd_df_PTS[amd_df_PTS$trade_type == "buy",]$costs_proceeds)
ROI_PTS <- (-1)*(total_profit_or_loss_PTS/total_capital_invested_PTS)






```


## Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```{r}
# Fill your code here and Discuss

comparison_table <- data.frame(
  names = c("P/L", "Total Invested", "ROI"),
no_PTS = c(total_profit_or_loss, total_capital_invested, ROI),
with_PTS = c(total_profit_or_loss_PTS,total_capital_invested_PTS,ROI_PTS)
)

#The table below is to compare P/L, Total Invested and ROI of this trading algorithm within same period 
#in 2 cases: uses and not uses Profit Taking Strategy"). 
#Below is showing stats for period from 20-May-2019 to 17-May-2024

comparison_table
```

Discussion: We can see that if we consider the whole 5-year period then the Profit Taking Strategy did not help us to improve the ROI.
Now, let's consider 1-year period and analyse on any full year data that we have i.e. from 1-Jan to 31-Dec, then we have the ROI as below
	    ROI	    ROI_PTS
2020	40%	    24.90%
2021	43.25%	26.90%
2022	-27.90%	-20.70%
2023	45%	    33.60%

The worst year in this period is 2022.
These number suggest that we should not hold and sell this share within 1 year period due to the its volatile. In fact, we should hold it a bit longer.

Now let's consider 2 year period.
	        ROI	    ROI_PTS
2020-2021	73.70%	33.92%
2022-2023	54.94%	11.31%

We can see that if we consider holding the shares for 2 years then sell then our ROI is positive although it's still not an optimal number compare to the ROI without using PTS.
What if we hold it for 3 years.(Two periods we are analysing is buying in 2020 and sell in 2022 OR buying in 2021 then sell in 2023)

	        ROI	  ROI_PTS
2020-2022	-24%	-0.01%
2021-2023	52%	  0.05%

We can see that holding it for 3 years does not help to improve ROI.

We can conclude that the conservative strategy to invest in this share is to keep it for 2 years then sell it. We can apply PTS just in case the market is too bad so it can help to reduce the loss or we can even develop a Stop-Loss mechanism and used it along with PTS.
Given we use the PTS for this strategy on 2-year period, we can see on average, our ROI is approximately 11.2% per annum which is quite good compare to term deposit interest.






