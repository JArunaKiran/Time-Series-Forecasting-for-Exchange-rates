#--------------------------------------------------------------------------------------#
#BF Project - Forecasting Exchange Rates  of 4 major currencies vs INR
#Importing data set ExchangeRate_Data.csv
exch_data=read.csv("ExchangeRate_Data.csv")
r=c(1:nrow(exch_data))
#plotting data for USD|INR, GBP|INR, EURO|INR, YEN|INR 
#Test for stationarity - checking the plots visually
par(mfrow=c(2,2))
plot(r,exch_data$USD, type='l', col="blue", main="Exch. Rate Movement of USD|INR")
plot(r,exch_data$GBP, type='l', col="red", main="Exch. Rate movement of GBP|INR")
plot(r,exch_data$EURO, type='l', col="green", main="Exch. Rate movement of EURO|INR")
plot(r,exch_data$YEN, type='l', col="orange", main="Exch. Rate movement of YEN|INR")
#As can be observed from the visualization, all the 4 graphs show some seasonality and trend behaviour

#We'll first begin with some exploratory analysis. 
#We'll convert the raw data into time series data
install.packages("tsoutliers")
library(tsoutliers)
#Adjusting the data for outliers. 
usd_data=ts(exch_data$USD, start= 1, frequency = 7)
usd.tso=tso(usd_data)
usd_data_adj= usd.tso$yadj

gbp_data=ts(exch_data$GBP, start=1, frequency = 7)
gbp.tso= tso(gbp_data)
gbp_data_adj= gbp.tso$yadj

euro_data=ts(exch_data$EURO, start=1, frequency = 7)
euro.tso= tso(euro_data)
euro_data_adj= euro.tso$yadj

yen_data=ts(exch_data$YEN, start= 1, frequency = 7)
yen.tso= tso(yen_data)
yen_data_adj= yen.tso$yadj

plot(usd_data_adj)
plot(gbp_data_adj)
plot(euro_data_adj)
plot(yen_data_adj)

#Decomposing the time series into its components
usd_component=decompose(usd_data_adj)
gbp_component=decompose(gbp_data_adj)
euro_component=decompose(euro_data_adj)
yen_component=decompose(yen_data_adj)
#plotting the components
plot(usd_component)
plot(gbp_component)
plot(euro_component)
plot(yen_component)

#We'll now check whether the time series is stationary or not so that we can proceed with further analysis
#We'll check for stationarity using acf and pacf plots
par(mfrow=c(4,4))
acf(usd_data_adj)
pacf(usd_data_adj)
acf(gbp_data_adj)
pacf(gbp_data_adj)
acf(euro_data_adj)
pacf(euro_data_adj)
acf(yen_data_adj)
pacf(yen_data_adj)
#From the acf plots, we can observe that the acf plots are degrading very slowly towards zero
#This means that the time series is non-stationary

#We will also conduct Unit root tests to check for stationarity
library("fUnitRoots")
library("forecast")
adfTest(usd_data_adj)
adfTest(gbp_data_adj)
adfTest(euro_data_adj)
adfTest(yen_data_adj)

#The p-values are greater than 0.05, this means that the time series is non-stationary

# The next step would be to convert the non-stationary time series into stationary so that we can proceed with further analysis

#Converting non=stationary time series into stationary
#We used differencing to stabilize the mean of the time series. 
usd_stationary= diff(usd_data_adj,1)
gbp_stationary= diff(gbp_data_adj,1)
euro_stationary= diff(euro_data_adj,1)
yen_stationary= diff(yen_data_adj,1)
par(mfrow=c(2,2))
plot(usd_stationary, type='l')
plot(gbp_stationary, type='l')
plot(euro_stationary, type='l')
plot(yen_stationary, type='l')


#Checking for stationarity again using Augemented-Dickey Fuller Test
#For a series to be stationary the p-value should be <= 0.05
adfTest(usd_stationary)
adfTest(gbp_stationary)
adfTest(euro_stationary)
adfTest(yen_stationary)
#All p-values are less than 0.05, so the time series is now stationary

#We will now split the data set into 2 parts- training and testing
bp = floor(length(usd_stationary)*(2.9/3))

#Training Data
usd_train= usd_stationary[1:bp]
gbp_train= gbp_stationary[1:bp] 
euro_train= euro_stationary[1:bp]
yen_train= yen_stationary[1:bp]

#Now we'll fit a model to this training data using auto.arima

usd_fit= auto.arima(usd_train, max.D =3,max.P=5, max.Q=5, trace=TRUE)
#Best model is ARIMA(3,0,2)
gbp_fit= auto.arima(gbp_train, max.D =3,max.P=5, max.Q=5, trace=TRUE)
#best model is ARIMA(0,0,0)
euro_fit= auto.arima(euro_train, max.D =3,max.P=5, max.Q=5, trace=TRUE)
#best model is ARIMA(1,0,1)
yen_fit= auto.arima(yen_train, max.D =3,max.P=5, max.Q=5, trace=TRUE)
#best model is ARIMA(0,0,0)
library(FitAR)
#After fitting model, we will check residuals for white-noise. 
#If there is a pattern observed in the residuals, the entire series would need to be refit. 
#We'll use acf plots for residuals and Ljung BoxTest result to check for white noise
#If the acf quickly drops to zero, this would mean there is no significant correlation, thus no pattern
#And if p-values of Box-Test are more than 0.05, this would mean no significant correlation, thus no pattern
acf(usd_fit$residuals)
boxresult1=LjungBoxTest(usd_fit$residuals,k=2,StartLag=1)
plot(boxresult1[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(usd_fit$residuals)
qqline(usd_fit$residuals)

acf(gbp_fit$residuals)
boxresult2=LjungBoxTest (gbp_fit$residuals,k=2,StartLag=1)
plot(boxresult2[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(gbp_fit$residuals)
qqline(gbp_fit$residuals)

acf(euro_fit$residuals)
boxresult3=LjungBoxTest (euro_fit$residuals,k=2,StartLag=1)
plot(boxresult3[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(euro_fit$residuals)
qqline(euro_fit$residuals)

acf(yen_fit$residuals)
boxresult4=LjungBoxTest (yen_fit$residuals,k=2,StartLag=1)
plot(boxresult4[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(yen_fit$residuals)
qqline(yen_fit$residuals)

#All the plots show that the residuals have no pattern and thus the data can now be used for forecasting



#The purpose of breaking the data set into 2 parts was to check the forecasting accuracy
#We would forecast the entire series from the breakpoint bp onwards
#So we will use 2 data frames to store actual and forecasted values and compare the two for accuracy

#Creating an actual series data set from breakpoint onwards <--- these data sets will be used for comparisons 
usd_actual= usd_stationary[(bp+1):length(usd_stationary)]
gbp_actual= gbp_stationary[(bp+1):length(gbp_stationary)]
euro_actual= euro_stationary[(bp+1):length(euro_stationary)]
yen_actual= yen_stationary[(bp+1):length(yen_stationary)]


#forecasting for test data
usd_forecast= forecast(usd_fit, h=38)
plot(usd_forecast)

gbp_forecast= forecast(gbp_fit, h=38)
plot(gbp_forecast)

euro_forecast= forecast(euro_fit, h=38)
plot(euro_forecast)

yen_forecast= forecast(yen_fit, h=38)
plot(yen_forecast)

#All the 4 plots show flat forecasts. 
#This means the series doesn't have enough trend or seasonality or enough temporal dynamics to make predictions for the future values. 
#This could also mean that the series is a random walk series with ARIMA(0,0,0)

#------------------------------------------------------------------------------#
#Forecasting for the next 31 days
#Fitting the model for the entire series
usd_fit_1= auto.arima(usd_stationary, max.D=3, max.P = 5, max.Q = 5, trace = TRUE)
#Best model is ARIMA(1,0,1)(4,0,1)
gbp_fit_1= auto.arima(gbp_stationary, max.D=3, max.P = 5, max.Q = 5, trace = TRUE)
#Best model is ARIMA(0,0,0)
euro_fit_1= auto.arima(euro_stationary, max.D=3, max.P = 5, max.Q = 5, trace = TRUE)
#Best model is ARIMA(1,0,1)(1,0,0)
yen_fit_1= auto.arima(yen_stationary, max.D=3, max.P = 5, max.Q = 5, trace = TRUE)
#Best model is ARIMA(0,0,0)(2,0,0)

#Forecasting
usd_f_31days= forecast(usd_fit_1, h=31)
plot(usd_f_31days)

gbp_f_31days= forecast(gbp_fit_1, h=31)
plot(gbp_f_31days)

euro_f_31days= forecast(euro_fit_1, h=31)
plot(euro_f_31days)

yen_f_31days= forecast(yen_fit_1, h=31)
plot(yen_f_31days)
