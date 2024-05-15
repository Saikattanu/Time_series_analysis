# Reinstall the required packages
install.packages("fpp2")
install.packages("forecast")
install.packages("fma")
install.packages("expsmooth")

# Reinstall the ggplot2 package
install.packages("ggplot2")

# Load the library again
library(ggplot2)

# Load the libraries again
library(ggplot2)
library(fpp2)
library(tseries)
library(forecast)
library(zoo)
library(lmtest)

# ARIMA lecture 8

# Set working directory (ensure this is the directory containing the CSV file)
setwd("C:/Users/ASUS/Downloads")

# Read the data from the CSV file
data <- read.csv("Inflation.csv")

# Create a time series object
ts_data <- ts(data$Inflation.Rate, start = 1991, frequency = 1)

# Load required libraries
library(ggplot2)
library(fpp2)
library(tseries)
library(forecast)
library(zoo)
library(lmtest)

# Display the time series data
ggtsdisplay(ts_data)
autoplot(ts_data)
ggAcf(ts_data)
ggPacf(ts_data)
adf.test(ts_data)

# Differencing the time series data
diff1 <- diff(ts_data)
adf.test(diff1)
ggtsdisplay(diff1)

diff2 <- diff(diff1)
adf.test(diff2)
ggtsdisplay(diff2)

# Fit the ARIMA model
fitdata <- Arima(diff2, order = c(2, 2, 0))
coeftest(fitdata)
summary(fitdata)
checkresiduals(fitdata)

# Forecasting
fc <- forecast(fitdata, h = 10)
print(fc)
plot(fc)
