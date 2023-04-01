
library('ggplot2')
library(tseries)
library(forecast)
library(vars)

#Importing the data in R
x=read.csv("C:/Users/shraddha nand/Desktop/time_series_data_annual_survey_of_industries_2018.csv")
View(x)
plot(x)

#Converting it into TimeSeries
datats<-ts(x$Net.Value.added..Rs.in.Crores.)

#plotting the time series
plot.ts(datats)

#plotting the ACF and PACF of the time series
acf<-acf(datats,lag.max = 24)
pacf<-pacf(datats,lag.max = 24)

#ADF test to check data is stationary
adf.test(datats)

datats2=diff(diff(diff(diff(datats))))
adf.test(datats2)

#inorder to find out best possible ARIMA fit to the time series model
fitARIMA2 <- auto.arima(datats2)

#Arima(4,1,1)model
fitARIMA <- arima(datats2, order=c(4,1,1))

accuracy(fitARIMA)
accuracy(fitARIMA2)

#forecasting 
fc<-forecast(fitARIMA,h=10)

#plotting forcasting curve
plot(fc)

