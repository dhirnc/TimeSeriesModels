# Name: Dhir Nilesh Chandan
# Version - 0.1

# Loading packages
library(readxl)
library(tidyverse)
library(forecast)
library(ggplot2)
library(gridExtra)
library(fpp2)

getwd()
# setwd("C:/dhir/Monash MDS/ETF5231 (Business forecasting)")

# Read Data
myts <- read_xlsx("RetailDataIndividual.xlsx", skip=3) %>%
  pull("29853583") %>%
  ts(start = c(1982,4), frequency = 12)

# Plotting data
myts %>% 
  autoplot() + xlab("Year") +
  ylab("Turnover (in million dollars)") +
  ggtitle("Department Stores Turnover in WA from 1982 to 2016")

# Seasonal plot
myts %>%
  ggseasonplot(year.labels = TRUE,year.labels.left = TRUE) + 
  ylab("Turnover (in million dollars)") +
  ggtitle("Department Stores Turnover in WA from 1982 to 2016")

# Subseries plot
myts %>% 
  ggsubseriesplot() + ylab("Turnover (in million dollars)") +
  ggtitle("Department Stores Turnover in WA from 1982 to 2016")

# ACF lag plot
myts %>%
  gglagplot(do.lines = FALSE)
myts %>%
  ggAcf() + ggtitle("ACF Plot")

# Plotting log transformed series
autoplot(log(myts))

# Square root transformed series
autoplot(sqrt(myts))

# Cube root transformed series
autoplot((myts)^(1/3))

# Calculating optimum value for lambda
lambda <- BoxCox.lambda(myts)

# Using optimum lambda value for transformation
myts %>% BoxCox(lambda = lambda) %>% autoplot()


# Trying out different transformations
autoplot(BoxCox(myts,lambda=1/2))+ ylab("Turnover (Square root transformed)") +
  ggtitle("Department Stores Turnover in WA from 1982 to 2016")
autoplot(BoxCox(myts,lambda=1/3))+ ylab("Turnover (Cube root transformed)") +
  ggtitle("Department Stores Turnover in WA from 1982 to 2016")
autoplot(BoxCox(myts,lambda=-1))+ ylab("Turnover (Inverse transformed)") +
  ggtitle("Department Stores Turnover in WA from 1982 to 2016")
autoplot(BoxCox(myts,lambda=0))+ ylab("Turnover (Log transformed)") +
  ggtitle("Department Stores Turnover in WA from 1982 to 2016")
autoplot(BoxCox(myts,lambda=BoxCox.lambda(myts)))+ ylab("Turnover (Log Tranformed)") +
  ggtitle("Department Stores Turnover in WA from 1982 to 2016")

# Creating test and train set
training <- window(myts, end = c(2014,12))
testSet <- window(myts, start = c(2014, 12))

# Plotting test and train data in one plot
autoplot(myts) +
  autolayer(training, series = "Training") +
  autolayer(testSet, series = "Test") + ylab("Turnover") +
  ggtitle("Department Stores Turnover in WA from 1982 to 2016")


# Forecasts using different methods
rwfForecast <- rwf(training, h = length(testSet), lambda = lambda)
rwfWithDriftForecast = rwf(training, drift = TRUE, h = length(testSet), lambda = lambda)
meanfForecast = meanf(training, h = length(testSet),lambda = lambda)
snaiveForecast = snaive(training, h = length(testSet), lambda = lambda)

# Plotting two benchmark methods on one plot for comparision
autoplot(myts) +
  autolayer(rwfForecast, series = "Naive", PI = FALSE) +
  autolayer(snaiveForecast, series = "Seasonal Naive", PI = FALSE) + 
  ylab("Turnover (in million dollars)") +
  ggtitle("Department Store Turnover in WA from 1983 to 2016")

# Calculating accuracy of forecast
accuracy(rwfForecast, testSet)
accuracy(rwfWithDriftForecast, testSet)
accuracy(meanfForecast, testSet)
accuracy(snaiveForecast, testSet)

# Creating a table for performance test
tab <- matrix(NA,ncol=4,nrow=2)
tab[1,] <- accuracy(rwfForecast, testSet)[2,c(2,3,5,6)]
tab[2,] <- accuracy(snaiveForecast, testSet)[2,c(2,3,5,6)]
colnames(tab) <- c("RMSE","MAE","MAPE","MASE")
rownames(tab) <- c("Naive method", 
                   "Seasonal naive method")
knitr::kable(tab, digits=2, booktabs=TRUE)

# Checking residuals
checkresiduals(snaiveForecast)

# Plotting forecasts for next 2 years
snaiveForecast1718 = snaive(myts, h = 24, lambda = lambda)
autoplot(myts) +
  autolayer(snaiveForecast1718, series = "Seasonal Naive Prediction 2017, 2018", PI = FALSE) + 
  ylab("Turnover (in million dollars)") +
  ggtitle("Department Store Turnover in WA from 1983 to 2018")


###### Assignment 3 ######

# check for trend
autoplot(myts) + xlab("Year") + ylab("$ Millions") +
  ggtitle("Department Stores Turnover in WA from 1982 to 2016")

# check for seasonality
ggseasonplot(myts, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("$ Millions") +
  ggtitle("Seasonal plot: Department Stores Turnover in WA from 1982 to 2016")

#Subseries Plot
ggsubseriesplot(myts) + ylab("$ Millions") +
  ggtitle("Subseries plot: Department Stores Turnover in WA from 1982 to 2016")

# STL decomposition
myts %>% stl(s.window="periodic") %>%
  autoplot() + 
  ggtitle("STL plot: Department Stores Turnover in WA from 1982 to 2016 (s.window=periodic, t.window=automated)")


# Creating ETS model - ETS (M,Ad,M)
fit1_ets <- ets(myts, model = "MAM", damped = T)

# Q2
fit1_ets
fit1_ets %>% autoplot

# Q3
fit1_ets %>% checkresiduals()

# Q4

fit2_ets <- ets(myts)
fit2_ets

fit2_ets %>% autoplot
fit2_ets %>% checkresiduals()


# Q6

fit2_ets %>% forecast(h=24) %>%
  autoplot() +
  ylab("Department Stores Turnover in WA from 1982 to 2018")


# Q7

myfc <- fit1_ets %>% forecast(h=24)
myfc

etsfc <- fit2_ets %>% forecast(h=24)
etsfc

# plotting forecasts by both method
myplot<-autoplot(myfc) + ylab("$ Millions")+xlab("Time")
etsplot<-autoplot(etsfc) + ylab("$ Millions")+xlab("Time")
grid.arrange(myplot, etsplot, nrow=2)


########## Assignment 4 ##################

# Loading packages
library(readxl)
library(tidyverse)
library(forecast)
library(ggplot2)
library(gridExtra)
library(urca)
library(fpp2)

getwd()
# setwd("C:/dhir/Monash MDS/ETF5231 (Business forecasting)")

# Read Data
myts <- read_xlsx("RetailDataIndividual.xlsx", skip=3) %>% 
  pull("29853583") %>% 
  ts(start = c(1982,4), frequency = 12)

# Plotting the series
myts %>% 
  autoplot() + xlab("Year") +
  ylab("Turnover (in million dollars)") +
  ggtitle("Department Stores Turnover in WA from 1982 to 2016")


lambda = BoxCox.lambda(myts)

# Taking log transformation
myts %>% BoxCox(lambda = lambda) %>% autoplot() +
  ggtitle("Log Transformed")

# Doing a seasonal difference
myts %>% BoxCox(lambda = lambda) %>% diff(lag=12) %>% autoplot() + ggtitle("Seasonal difference")
  
# taking first difference as well
myts %>% BoxCox(lambda = lambda) %>% diff(lag=12) %>%  diff(lag=1)  %>% autoplot() + ggtitle("Seasonal difference + First order Difference")


# Checking ACF and PACF plots for ARIMA model
myts %>% BoxCox(lambda = lambda) %>% diff(lag=12) %>% diff() %>% ggtsdisplay()

## Q2
# Creating ARIMA(0,1,2)(0,1,1)[12] model 
(fit1_arima <- Arima(myts, c(0,1,2), seasonal = c(0,1,1), lambda = lambda))
## Q3
checkresiduals(fit1_arima)

## Q4
# Creating alternate models

# ARIMA(0,1,2)(1,1,1)[12] model
(fit2_arima <- Arima(myts, c(0,1,2), seasonal = c(1,1,1), lambda = lambda))
checkresiduals(fit2_arima)



# ARIMA(6,1,0)(1,1,1)[12] model
(fit3_arima <- Arima(myts, c(6,1,0), seasonal = c(1,1,1), lambda = lambda))
checkresiduals(fit3_arima)

# ARIMA(6,1,2)(1,1,1)[12] model
(fit4_arima <- Arima(myts, c(6,1,2), seasonal = c(1,1,1), lambda = lambda))
checkresiduals(fit4_arima)

## Q5
# Creating model using default auto.arima
(fit.auto <- auto.arima(myts, lambda = lambda, trace = T))
checkresiduals(fit.auto)

## Q6
# Creating model using auto.arima, stepwise = FALSE
(fit.auto1 <- auto.arima(myts, lambda = lambda, trace = T, stepwise = F, approximation = F))
checkresiduals(fit.auto1)

## Q7
# selecting model with least AICc
fit_arima <- Arima(myts, c(0,1,2), seasonal = c(1,1,1), lambda = lambda)

## Q8
# Forecasting using selected model
fit_arima %>% forecast(h=24) %>% autoplot() + 
  ylab("Turnover") + xlab("Years")

## Q9

# Read Data - full timeseries
myts_full <- read_xlsx("RetailDataIndividualFull.xlsx", skip=3) %>% 
  pull("29853583") %>% 
  ts(start = c(1982,4), frequency = 12)

# Plotting the series - full timeseries
myts_full %>% 
  autoplot() + xlab("Year") +
  ylab("Turnover (in million dollars)") +
  ggtitle("Department Stores Turnover in WA from 1982 to 2018")

## Q10
# Predictions from 3 methos
# benchmark method - snaive
snaive.fcast <- snaive(myts, h = 24, lambda = lambda)
# ETS model
fit_ets <- ets(myts)
ets.fcast <-  fit_ets %>% forecast(h=24)
# arima model
fit_arima <- Arima(myts, c(0,1,2), seasonal = c(1,1,1), lambda = lambda) 
arima.fcast <- fit_arima %>% forecast(h=24)

# Plotting all together
autoplot(myts_full,include = 50) +
  autolayer(snaive.fcast, series = "SNaive Prediction", PI = FALSE) + 
  autolayer(ets.fcast, series = "ETS Prediction", PI = FALSE) + 
  autolayer(arima.fcast, series = "Arima Prediction", PI = FALSE) + 
  ylab("Turnover (in million dollars)") +
  ggtitle("Department Store Turnover in WA from 1983 to 2018")

# Q11.
# Comparing performances
train.end <- c(2016,12)
test.start <- c(2017,1)
train <- window(myts_full,end=train.end)
test <- window(myts_full,start=test.start)

tab <- matrix(NA,ncol=3,nrow=3)
tab[1,] <- accuracy(snaive.fcast, test)[2,c(2,5,6)]
tab[2,] <- accuracy(ets.fcast, test)[2,c(2,5,6)]
tab[3,] <- accuracy(arima.fcast, test)[2,c(2,5,6)]

colnames(tab) <- c("RMSE","MAPE","MASE")
rownames(tab) <- c("Seasonal naive method", "ARIMA(0,1,2)(1,1,1)[12]", "ETS(M,A,M)")
knitr::kable(tab, digits=2, booktabs=TRUE)


# Q12.
# forecasts using top model ARIMA(0,1,2)(1,1,1)[12]
fit_arima_new  <- Arima(myts, c(0,1,2), seasonal = c(1,1,1), lambda = lambda) 
fit_arima_new
fit_arima_new %>% forecast(h=24,level = c(80)) %>% autoplot() +
  ggtitle("Department Store Turnover Forecast for 2019-2020 with ARIMA(0,1,2)(1,1,1)[12]") + 
  ylab("Turnover ($ Millions)") + 
  xlab('Year')

