# BDA400 Technical Analysis Dashboard

## Assignment 6 – Visualization Phase

This project was created for BDA400 Data Science Tools and Techniques. 
I developed an interactive technical analysis dashboard in R using Shiny.

The dashboard downloads historical stock market data and allows the user
to explore different stocks, date ranges, time frames, chart types, and
technical indicators.

## Main Features

- Historical stock data retrieved using quantmod
- Selection of different stocks
- Custom date range
- Daily, weekly, and monthly analysis
- Line, area, and candlestick charts
- Simple Moving Average (SMA)
- Exponential Moving Average (EMA)
- Relative Strength Index (RSI)
- MACD
- Custom short and long moving-average periods
- Buy and Sell signals based on moving-average crossovers
- Latest market information summary

## R Packages Used

The application uses the following R packages:

- shiny
- ggplot2
- quantmod
- TTR

## Trading Rule

I used a moving-average crossover strategy. A Buy signal is generated when
the short moving average crosses above the long moving average. A Sell
signal is generated when the short moving average crosses below the long
moving average. When neither crossover occurs, the current signal is Hold.

## Files

`app.R` contains the complete Shiny dashboard code.

## Author

Rajwinder Kaur
