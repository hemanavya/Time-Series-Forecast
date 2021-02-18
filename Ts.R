### View time series 

mydata= read.csv(file.choose(), header = FALSE)

mydata.ts=ts(mydata[,1],start=c(1987,1),frequency = 12)

plot(mydata.ts)
abline(reg = lm(mydata.ts~time(mydata.ts)))
### Decomposition
tsdecompose <- decompose(mydata.ts) ### increasing trend present, seasonality is present 
plot(tsdecompose)

## stationarity
install.packages("tseries")
library(tseries)
library(forecast)
adf.test(mydata.ts)
## pvalue = 0.54 -->AcceptNull--> Non stationary

#Splitting data
library(TSstudio)
split_fancy<- ts_split(mydata.ts, sample.out = 24)
training <- split_fancy$train
testing <- split_fancy$test

#Fitting Holt-winter's model
hw_fit <- hw(training, seasonal = "m")
fitted(hw_fit)
summary(hw_fit)

plot.ts(training, main = "Smoothed timeseries of Souveneir Sales", col="blue")
lines(fitted(hw_fit), col="red")

plot(forecast(hw_fit,24))

df_fancy <- as.data.frame(hw_fit)
df_testing<- as.data.frame(testing)
df_testing$hw <- df_fancy$`Point Forecast`


hw_fit$model
#Validating HW using MAPE
library(Metrics)
mape(df_testing$x,df_testing$hw)


##make ARIMA model
##series is stationary or not
library(tseries)
adf.test(mydata.ts) ##series is non stationary

#Log transformation of series
plot(log(mydata.ts))
plot(diff(log(mydata.ts)))
adf.test(diff(log(mydata.ts))) ##now be omes stationary as P<0.5
##1st difference is statioanry, d=1

split_fancy_log<- ts_split(log(mydata.ts), sample.out = 24)
log_training <- split_fancy_log$train
log_testing <- split_fancy_log$test
##acf can be done for a stationary series
acf(ts(diff(log_training)), main="ACF Souvenir Sales") #q=0
pacf(ts(diff(log_training)), main="PACF Souvenir Sales") #p=1

#Fitting the ARIMA model
ARIMAfit = auto.arima(log_training, approximation=FALSE,trace=FALSE)
summary(ARIMAfit)

Fancy_Arima=forecast(ARIMAfit, h=24)
plot(Fancy_Arima)


df_arima = as.data.frame(Fancy_Arima)
Forecasted <- exp(df_arima$`Point Forecast`)
df_testing$arima = Forecasted
df_arima$`Point Forecast`
mape(df_testing$x, df_testing$arima)
#MAPE = 0.20


#Forecasting for next 5 years using HW model
fct_hw_fit <- hw(mydata.ts, seasonal = "m", h=60)
fitted(fct_hw_fit)
summary(fct_hw_fit)
plot(fct_hw_fit, main="Forecast for next 5 years using HW")

#Forecasting for next 5 years using ARIMA model

fancy.model=arima(mydata.ts, period=60)
predict(fancy.model, n.ahead = 60)

fct_ARIMAfit = auto.arima(mydata.ts, approximation=FALSE,trace=TRUE)

summary(fct_ARIMAfit)

fct_Arima=forecast(fct_ARIMAfit, h=60)
plot(fct_Arima)
summary(fct_Arima)
