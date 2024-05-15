# Load required libraries
library(ggplot2)
library(fpp2)
library(tseries)
library(forecast)
library(zoo)
library(lmtest)

# ARMA lecture 8

# Set working directory (ensure this is the directory containing the CSV file)
setwd("C:/Users/ASUS/Downloads")

# Read the data from the CSV file
data <- read.csv("Inflation.csv")

# Create a time series object
ts_data <- ts(data$Inflation.Rate, start = 1991, frequency = 1)

# Display the time series data
ggtsdisplay(ts_data)
autoplot(ts_data)
ggAcf(ts_data)
ggPacf(ts_data)

# Check if the series is stationary
adf_test_result <- adf.test(ts_data)
print(adf_test_result)

# Differencing the time series data if it's not stationary
if(adf_test_result$p.value > 0.05) {
  diff_ts_data <- diff(ts_data)
  adf_test_result_diff <- adf.test(diff_ts_data)
  print(adf_test_result_diff)
  
  # Fit an ARMA model to the differenced time series data
  arma_fit <- Arima(diff_ts_data, order = c(2, 0, 2))
  ggtsdisplay(diff_ts_data)
} else {
  # Fit an ARMA model to the original time series data
  arma_fit <- Arima(ts_data, order = c(2, 0, 2))
}

# Summary of the fitted ARMA model
summary(arma_fit)

# Check residuals of the fitted model
checkresiduals(arma_fit)

# Forecasting with the fitted ARMA model
fc_arma <- forecast(arma_fit, h = 10)
print(fc_arma)
plot(fc_arma)
