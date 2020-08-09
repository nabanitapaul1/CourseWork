# Importing libraries
#install.packages("rlang")
library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)
library(xts)
library(moments)
library(forecast)
library(fpp)
library(smooth) 
library(tseries)
library(readxl)
library(MLmetrics) 
library(TSstudio)
library(TTR)
library(rlang)

# Modelling for Domestic Datasets

# Importing data sets

airlines_inter <- read.csv("C:/EXCELR/DataSciencePoject/Project_ Air Fare prediction/FinalSubmissionProject/dataset for Modelling_international.csv")
airlines_inter$InvoiceDate <- as.character(airlines_dom$InvoiceDate)
airlines_inter$InvoiceDate <- as.Date(airlines_dom$InvoiceDate)
airlines_inter <-airlines_inter[,c(1,2)]
View(airlines_inter)
str(airlines_inter)
sum(is.na(airlines_inter$InvoiceDate))
names(airlines_inter)

# Graphical Representation

airlines_inter%>%ggplot(aes(InvoiceDate,AvgNetFare ))+geom_line(col="Red", size=0.5)+
  geom_point(alpha = 0.5,  shape=20,size=2) +
  labs(title = "NetFare Vs Time", x = "Time", y = "Netfare",
       subtitle = "Data from April, 2018 to June, 2019") 
boxplot(airlines_inter$AvgNetFare)

# Converting to time series 

# #tsNetFare<-ts(domestic_air_date_time_day$Avg_Netfare,start=c(2018,4,1),end=c(2019,6,10),frequency = 365.25)
# #tsNetFare <- msts(domestic_air_date_time_day$Avg_Netfare, frequency = 365.25,start = decimal_date(as.Date("2018-04-01")),end=decimal_date(as.Date("2019-06-10")))
# 
# tsNetFare <- msts(airlines_dom$AvgNetFare,seasonal.periods = c(7,365.25),start = decimal_date(as.Date("2018-04-01")),end=decimal_date(as.Date("2019-06-10")))
# tsNetFare
# start(tsNetFare)
# end(tsNetFare)
# str(tsNetFare)
# class(tsNetFare)
# frequency(tsNetFare)
# cycle(tsNetFare)
# 
# # Visualization
# plot(tsNetFare)
# abline(reg=lm(tsNetFare~time(tsNetFare)))
# #abline(reg=lm(tsNetFare~airlines_dom$InvoiceDate))
# 
# plot(aggregate(tsNetFare, FUN=mean)) 
# boxplot(tsNetFare~cycle(tsNetFare))
#  time(tsNetFare)
# 1:406
# 407:436

airlines_inter_seq <- seq(as.Date("2018-04-01"), as.Date("2019-06-10"), by = "day")
length(airlines_inter_seq)
class(airlines_inter_seq)
str(airlines_inter_seq)
## Create a time series object
set.seed(25)
airlines_inter_ts <- ts(airlines_inter$AvgNetFare,     # random data
                      start = c(2018, as.numeric(format(airlines_inter_seq[1], "%j"))),
                      frequency = 365)

ts_info(airlines_inter_ts)

View(airlines_inter_ts)

#ts1 <- ts(airlines_dom$AvgNetFare,start=c(year("2018-04-01"),yday("2018-04-01")), 
#          end =c(year("2019-06-10"),yday("2019-06-10")), frequency = 365 )

plot(airlines_inter_ts, main="Time series plot,Time Vs Net Fare", ylab="Net Fare", xlab="Time")
points(airlines_inter_ts, cex = .5, col = "dark red")

abline(reg=lm(airlines_inter_ts~time(airlines_inter_ts)))

start(airlines_inter_ts)
end(airlines_inter_ts)
frequency(airlines_inter_ts)

#Splitting the data into tarin and test

## Set the last 30 days as a testing partition 

## and the rest as a training partition

split_airlines <- ts_split(ts.obj = airlines_inter_ts, sample.out = 30)

training_airlines <- split_airlines$train
testing_airlines <- split_airlines$test

length(split_airlines)
length(training_airlines)
length(testing_airlines)
class(testing_airlines)

#myzoo <- zoo(airlines_dom$, airlines_dom_seq)
class(airlines_inter_ts)
class(airlines_inter_ts)
start(airlines_inter_ts)
end(airlines_inter_ts)

autoplot(training_airlines, main= "Training and Testing datasets", ylab = "Net Fare") + autolayer(testing_airlines)
# Decompostion
#install.packages("TTR")


#for non- seasonal data
airlines_inter_sma <- SMA(airlines_inter_ts, n=10) # n= 1,2 ,3, 4,5,6,......
plot.ts(airlines_inter_sma)

# for seasonal data
#airlines_dom_decom <- decompose(airlines_dom_ts)

# Model Building
#1. Model 1 

#Arima Model 1

#Make the data stationary(homogenizing variance )
# It will make the variance equal
plot(log(airlines_inter_ts))

# Making the data stationary(homoogenizing mean)
#It will make the mean equal
plot(diff(log(airlines_inter_ts)))

acf(airlines_inter_ts)
acf(diff(log(airlines_inter_ts))) # determines the value of q
# q= 1

pacf(diff(log(airlines_dom_ts))) # determines the value of p
# p=2 

# d=1 no. of time differetion have done

arima_1 =  arima(log(training_airlines), c(2,1,1), seasonal = list(order=c(2,1,1), period=7))
summary(arima_1)
pred_val= predict(arima_1,n.ahead = 30)
pred_val = round(2.718^pred_val$pred,0 )
ts.plot(testing_airlines, pred_val, log="y", lty=c(1,3), col=c("blue","red"))
#plot(pred_val)
#lines(testing_airlines,col="red")

accuracy(testing_airlines, pred_val)
class(pred_val)
RMSE_arima1 <- RMSE(testing_airlines, pred_val)
MAPE_arima1<- MAPE(testing_airlines,pred_val) 

# 2. Model 2

# Arima Model 2

?arima
arima_2 =  arima(log(training_airlines), c(2,1,3), seasonal = list(order=c(2,1,3), period=7),method = 'CSS')
pred_val= predict(arima_2,n.ahead = 30)
pred_val = round(2.718^pred_val$pred,0 )
ts.plot(testing_airlines, pred_val, log="y", lty=c(1,3), col=c("blue","red"))
#plot(pred_val)
#lines(testing_airlines,col="red")

accuracy(testing_airlines, pred_val)
class(pred_val)
RMSE_arima2 <- RMSE(testing_airlines, pred_val)
MAPE_arima2<- MAPE(testing_airlines,pred_val) 

# Using Auto ARIMA
# Model 3 
# With log transform 

plot(training_airlines)
plot(log(training_airlines))

autoarima_model_log <- auto.arima(log(training_airlines), stepwise=FALSE, approximation=FALSE)
#autoarima_model <- auto.arima(training_airlines, stepwise=FALSE, approximation=FALSE)

summary(autoarima_model_log)
attributes(autoarima_model_log)
# Forecasting for next 30 days.
fore_auto_arima = forecast(autoarima_model_log, h=30)
class(fore_auto_arima)
forecast_value<- data.frame(fore_auto_arima)
forecast_value <-exp(forecast_value)
View(forecast_value)

# Graphical Representation
plot(fore_auto_arima)
lines(testing_airlines,col="red")
#legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("testData","ARIMAPred"))
autoplot(fore_auto_arima)
# Accuracy
accuracy(fore_auto_arima,testing_airlines)

RMSE_autoArima_log <- RMSE(forecast_value$Point.Forecast,testing_airlines)
MAPE_autoArima_log<- MAPE(forecast_value$Point.Forecast,testing_airlines)

# plot of residuals
plot(fore_auto_arima$residuals)
qqnorm(fore_auto_arima$residuals)
# ACF and PACF plots
acf(autoarima_model_log$residuals, main="Correlogram")
pacf(autoarima_model_log$residuals, main="Partial Correlogram")

# Ljung-Box test
Box.test(autoarima_model_log$residuals,lag=25, type="Ljung-Box")
# Residua plot
hist(autoarima_model_log$residuals, col='red', xlab = "Error", main = "Histogram of Residuals",freq=FALSE)
lines(density(autoarima_model_log$residuals))
#airlines_dom_seq <-  zoo(airlines_dom, seq(from = as.Date("2018-04-01"), to = as.Date("2019-06-10"), by = 1))

# Using Auto Arima
# Model 4
# Without log transform

autoarima_model <- auto.arima(training_airlines, stepwise=FALSE, approximation=FALSE)
#autoarima_model <- auto.arima(training_airlines, stepwise=FALSE, approximation=FALSE)

summary(autoarima_model)
attributes(autoarima_model)
# Forecasting for next 30 days.
fore_auto_arima = forecast(autoarima_model, h=30)
class(fore_auto_arima)

# Graphical Representation
plot(fore_auto_arima)
lines(testing_airlines,col="red")
#legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("testData","ARIMAPred"))
autoplot(fore_auto_arima)

ts.plot(testing_airlines, pred_val, log="y", lty=c(1,3), col=c("blue","red"))
# Accuracy
accuracy(fore_auto_arima,testing_airlines)

forecast_value<- data.frame(fore_auto_arima)
View(forecast_value)

RMSE_autoArima <- RMSE(forecast_value$Point.Forecast,testing_airlines)
MAPE_autoArima<- MAPE(forecast_value$Point.Forecast,testing_airlines)

# plot of residuals
plot(fore_auto_arima$residuals)
qqnorm(fore_auto_arima$residuals)
# ACF and PACF plots
acf(autoarima_model$residuals, main="Correlogram")
pacf(autoarima_model$residuals, main="Partial Correlogram")

# Ljung-Box test
Box.test(autoarima_model$residuals,lag=15, type="Ljung-Box")

# Residua plot
hist(autoarima_model$residuals, col='red', xlab = "Error", main = "Histogram of Residuals",freq=FALSE)
lines(density(autoarima_model$residuals))
#airlines_dom_seq <-  zoo(airlines_dom, seq(from = as.Date("2018-04-01"), to = as.Date("2019-06-10"), by = 1))


# Model 5
# Naive Forecasting method
naive_model<- naive(training_airlines, h= 30)
summary(naive_model)
fore_naive <- forecast(naive_model, h=30)
#Visual Representation
plot(fore_naive)
lines(testing_airlines,col="red")
ts.plot(testing_airlines, fore_naive, log="y", lty=c(1,3), col=c("blue","red"))

# Model Evaluation
accuracy(fore_naive, testing_airlines)

forecast_value<- data.frame(fore_naive)
View(forecast_value)

RMSE_naive <- RMSE(forecast_value$Point.Forecast,testing_airlines)
MAPE_naive<- MAPE(forecast_value$Point.Forecast,testing_airlines)

# Model 6
# Simple Exponential Smoothing

ses_model <- ses(training_airlines, h= 30)
summary(ses_model)
fore_ses<- forecast(ses_model, h= 30)
# Visualization
plot(fore_ses)
ts.plot(testing_airlines, fore_ses, log="y", lty=c(1,3), col=c("blue","red"))

# Model Evaluation
accuracy(fore_ses, testing_airlines)
forecast_value<- data.frame(fore_ses)
View(forecast_value)

RMSE_ses <- RMSE(forecast_value$Point.Forecast,testing_airlines)
MAPE_ses<- MAPE(forecast_value$Point.Forecast,testing_airlines)

# Model 7

# Holt's trend Model

holt_model <- holt(training_airlines, h = 30)
summary(holt_model)
fore_holt <- forecast(holt_model, h=30)

# Visualization
plot(fore_holt)
ts.plot(testing_airlines, fore_holt, log="y", lty=c(1,3), col=c("blue","red"))

# Model Evaluation
accuracy(fore_holt, testing_airlines)
forecast_value<- data.frame(fore_holt)
View(forecast_value)
RMSE_holt <- RMSE(forecast_value$Point.Forecast,testing_airlines)
MAPE_holt<- MAPE(forecast_value$Point.Forecast,testing_airlines)

# MOdel 8
# TBTS Model

mstsNetFare <- msts(airlines_inter$AvgNetFare,seasonal.periods = c(7,365.25),start = decimal_date(as.Date("2018-04-01")),end=decimal_date(as.Date("2019-06-10")))
start(mstsNetFare)
end(mstsNetFare)
str(mstsNetFare)
class(mstsNetFare)
frequency(mstsNetFare)
cycle(mstsNetFare)

# # Visualization
plot(mstsNetFare)
abline(reg=lm(mstsNetFare~time(mstsNetFare)))
# #abline(reg=lm(mstsNetFare~airlines_dom$InvoiceDate))

# plot(aggregate(mstsNetFare, FUN=mean)) 
time(mstsNetFare)

# Data Partitioning

#Splitting data into train and test

split_airlines <- ts_split(ts.obj = mstsNetFare, sample.out = 30)
training_airlines <- split_airlines$train
testing_airlines <- split_airlines$test

# Model Building 

tbts_model <- tbats(training_airlines) 
summary(tbts_model)
fore_tbts <- forecast(tbts_model, h=30)

# Visualization
plot(fore_tbts, ylab="NetFare" )
ts.plot(testing_airlines, fore_tbts, log="y", lty=c(1,3), col=c("blue","red"))

accuracy(fore_tbts,testing_airlines)

forecast_value<- data.frame(fore_tbts)
View(forecast_value)

RMSE_tbts <- RMSE(forecast_value$Point.Forecast,testing_airlines)
MAPE_tbts<- MAPE(forecast_value$Point.Forecast,testing_airlines)


# Just to compare values

fore_orig <- data.frame(testing_airlines,fore_tbts)
colnames(fore_orig) <- c("original Netfare","Forecasted Netfare","Lo 80","Hi 80","Lo 95","Hi 95")
View(fore_orig)

# Model 9 
# Neural Net

neural_net_Model <- nnetar(training_airlines)
fore_neuralnet <- forecast(neural_net_Model, h= 30)

# Visualization
plot(fore_neuralnet)

ts.plot(testing_airlines, fore_neuralnet, log="y", lty=c(1,3), col=c("blue","red"))

# Model Evaluation
accuracy(fore_neuralnet, testing_airlines)
forecast_value<- data.frame(fore_neuralnet)
View(forecast_value)


RMSE_neuralnet <- RMSE(forecast_value$Point.Forecast,testing_airlines)
MAPE_neuralnet<- MAPE(forecast_value$Point.Forecast,testing_airlines)


# Model 10 
# Neural Net 2
sequence = alist(0.0,0.01,0.1,0.5,1,0.025,0.075,0.085,0.095,0.05, 0.25, 0.75,0.9,1)

for (a_decay in sequence)
{
  a <- paste("The Decay is: ", a_decay)
  print(a)
  neural_net_Model2 <- nnetar(training_airlines, decay=a_decay, maxit=100, lambda=0.5)
  fore_neuralnet <- forecast(neural_net_Model2, h= 30)
  
  print(accuracy(fore_neuralnet, testing_airlines))
}
set.seed(123)
neural_net_Model2 <- nnetar(training_airlines, decay=1, maxit=100, lambda=0.5)
summary(neural_net_Model2)
fore_neuralnet <- forecast(neural_net_Model2, h= 30)
? nnetar
# Visualization
autoplot(fore_neuralnet)
plot(fore_neuralnet)
lines(testing_airlines, col="red")

ts.plot(testing_airlines, fore_neuralnet, log="y", lty=c(1,3), col=c("blue","red"))

# Model Evaluation
accuracy(fore_neuralnet, testing_airlines)
forecast_value<- data.frame(fore_neuralnet)
View(forecast_value)

RMSE_neuralnet2 <- RMSE(forecast_value$Point.Forecast,testing_airlines)
MAPE_neuralnet2<- MAPE(forecast_value$Point.Forecast,testing_airlines)

# Model 11
# Neural Network model 3
set.seed(123)
neural_net_Model3 <- nnetar(training_airlines, decay=1, maxit=100, lambda=0.5, scale.inputs = TRUE)
fore_neuralnet <- forecast(neural_net_Model3, h= 30,PI=TRUE)
? nnetar
# Visualization
autoplot(fore_neuralnet)
plot(fore_neuralnet)
lines(testing_airlines, col="red")

ts.plot(testing_airlines, fore_neuralnet, log="y", lty=c(1,3), col=c("blue","red"))

# Model Evaluation
accuracy(fore_neuralnet, testing_airlines)
forecast_value<- data.frame(fore_neuralnet)
View(forecast_value)

RMSE_neuralnet3 <- RMSE(forecast_value$Point.Forecast,testing_airlines)
MAPE_neuralnet3<- MAPE(forecast_value$Point.Forecast,testing_airlines)

# Model 12
#### USING HoltWinters function ################
# Optimum values
# with alpha = 0.2 which is default value
# Assuming time series data has only level parameter
set.seed(123)
holtWinter_model<-HoltWinters(training_airlines,alpha = 0.2,beta = F,gamma = F)
holtWinter_model
hwa_pred<-data.frame(predict(holtWinter_model,n.ahead=30))
# By looking at the plot the forecasted values are not showing any characters of train data 
plot(forecast(holtWinter_model,h=30))
accuracy(hwa_pred$fit,testing_airlines)
RMSE_holtWinter<- RMSE(hwa_pred$fit,testing_airlines)
MAPE_holtWinter<-MAPE(hwa_pred$fit,testing_airlines)

#Model 13

# with alpha = 0.2, beta = 0.1
# Assuming time series data has level and trend parameter 
set.seed(123)
hw_ab<-HoltWinters(training_airlines,alpha = 0.2,beta = 0.15,gamma = F)
hw_ab
hwab_pred<-data.frame(predict(hw_ab,n.ahead = 30))
# by looking at the plot the forecasted values are still missing some characters exhibited by train data
plot(forecast(hw_ab,h=30))
accuracy(hwab_pred$fit,testing_airlines)
RMSE_hwab <-RMSE(hwab_pred$fit,testing_airlines)
MAPE_hwab<-MAPE(hwab_pred$fit,testing_airlines)

#Model 14
# With out optimum values 
set.seed(123)
hw_na<-HoltWinters(training_airlines,beta = F,gamma = F)
hw_na
hwna_pred<-data.frame(predict(hw_na,n.ahead = 30))
hwna_pred
plot(forecast(hw_na,h=30))

accuracy(hwna_pred$fit,testing_airlines)
RMSE_hwna <-RMSE(hwna_pred$fit, testing_airlines)
MAPE_hwna<-MAPE(hwna_pred$fit,testing_airlines)

# Model 15
set.seed(123)
hw_nab<-HoltWinters(training_airlines,gamma=F)
hw_nab
hwnab_pred<-data.frame(predict(hw_nab,n.ahead=30))
hwnab_pred
#Visualization
plot(forecast(hw_nab,h=30))
autoplot(forecast(hw_nab,h=30))
plot(forecast(hw_nab,h=30))
lines(testing_airlines, col="red")

ts.plot(testing_airlines, forecast(hw_nab,h=30), log="y", lty=c(1,3), col=c("blue","red"))

#Model Evauation
accuracy(forecast(hw_nab,h=30),testing_airlines)
RMSE_hwnab <- RMSE(hwnab_pred$fit,testing_airlines)
MAPE_hwnab<-MAPE(hwnab_pred$fit,testing_airlines)

########################################################################
# The following data frame/ table  will show the RMSE and MAPE value for all the model

table_rmse_MAPE <- data.frame(c("arima_1","arima_2","autoarima_model_log","autoarima_model","naive_model","ses_model","holt_model","tbts_model","neural_net_Model","neural_net_Model2", "neural_net_Model3","holtWinter_model","hw_ab","hw_na","hw_nab" ),
                              c(RMSE_arima1, RMSE_arima2, RMSE_autoArima_log, RMSE_autoArima, RMSE_naive, RMSE_ses, RMSE_holt, RMSE_tbts, RMSE_neuralnet, RMSE_neuralnet2, RMSE_neuralnet3, RMSE_holtWinter,RMSE_hwab,RMSE_hwna,RMSE_hwnab),
                              c(MAPE_arima1, MAPE_arima2, MAPE_autoArima_log, MAPE_autoArima, MAPE_naive, MAPE_ses, MAPE_holt, MAPE_tbts, MAPE_neuralnet, MAPE_neuralnet2, MAPE_neuralnet3, MAPE_holtWinter, MAPE_hwab, MAPE_hwna,MAPE_hwnab))
colnames(table_rmse_MAPE) <- c("Model Names","RMSE","MAPE")
View(table_rmse_MAPE)

# New Model for overall data



new_model <- arima(log(airlines_dom_ts), c(2,1,1), seasonal = list(order=c(2,1,1), period=7))
new_model

autoplot(forecast(new_model,30))

forecast_value <- forecast(new_model,h = 30, ts=TRUE)

plot(forecast_value)
class(forecast_value)

# Forecasted values for the next 4 quarters
forecast_new <- data.frame(forecast_value)
View(forecast_new)
class(forecast_new)
names(forecast_new)
