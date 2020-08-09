
############# Importing libraries ################

library(dplyr)  # data manipulation
library(lubridate) # to deal with dates and formatting
library(ggplot2)

############# Importing Dataset ################

airlines <- read.csv("C:/EXCELR/DataSciencePoject/Project_ Air Fare prediction/FinalSubmissionProject/Concatenate_B2C_B2E.csv")
summary(airlines)
str(airlines)
View(airlines)

############# EDA  ################

# Converting   Net fare into numeric format
airlines$NetFare <- as.numeric(as.character(airlines$NetFare))

#convert date into proper format
airlines$InvoiceDate <- dmy_hm(airlines$InvoiceDate)
head(airlines$InvoiceDate)
View(airlines)
str(airlines)
summary(airlines)
names(airlines)

# Arranging data 

airlines <- arrange(airlines, airlines$InvoiceDate)
View(airlines)

#####################################################################################################

# Analysis for Domestic data

# Filtering out doemstic data from total data  and choosing only  AIR from all product type
#average analysis for hours for NETFARE for domestic air

domestic <- filter(airlines,ProductType=="Air",ItineraryType=="Domestic")
domestic$NetFare <- as.numeric(as.character(domestic$NetFare))
View(domestic)
summary(domestic)
summary(domestic$NetFare)

sum(is.na(domestic)) # There is no NA values

# Arranging the dataset in the ascending order of Net Fare so that we can remove negative values or zero , as per domain knowledge netfare can not be negative.

domestic <- arrange(domestic, domestic$NetFare)
summary(domestic)
View(domestic)
domestic <- domestic[7234:141234,]
summary(domestic)
View(domestic)

# i.)NetFare
# satatistical analysis
mean(domestic$NetFare)
median(domestic$NetFare)
var(domestic$NetFare)
sd(domestic$NetFare)

# ii.) Product Type
table(domestic$ProductType)

# barplot(table(airlines_domestic$ProductType),border="red",
#         col="blue",
#         density=10, ylab = "Frequency", xlab="Product_Type", main="Bar plot of the Different type of product during April 2018 to June 2019") 

ggplot(airlines_domestic) +
  aes(x = ProductType, fill=ProductType) + 
  geom_bar(col="red") + 
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2))+
  labs(x="Product Type",y="Frequency" )+theme_minimal()

boxplot(airlines_domestic_trail$NetFare~airlines_domestic_trail$ProductType, ylab = "Net Fare", xlab = "Product Type",main="Net fare of different Products",col="#8a0303")


# Graphical Representation
hist(domestic$NetFare)

# Trend Analysis for NetFare of AIR
dom_hour <- domestic %>% 
  mutate( year= year(domestic$InvoiceDate),
          month=month(domestic$InvoiceDate,label = TRUE),
          weekday=wday(domestic$InvoiceDate,label = TRUE),
          day=day(domestic$InvoiceDate),
          hour=hour(domestic$InvoiceDate)) %>%
  group_by(year,hour) %>%
  summarise(total=mean(NetFare),max=max(NetFare),min=min(NetFare))

View(dom_hour)

#average Netfare during different hours of day for the year 2018 , 2019 seperately 

ggplot(data = dom_hour) +  
  geom_line(aes(dom_hour$hour,dom_hour$total),col="black",size=1)+ 
  geom_smooth(aes(dom_hour$hour,dom_hour$total),size=1) +
  facet_wrap(~year) 

ggplot(data = dom_hour, aes(hour,total)) +
  geom_bar(stat = "identity", fill="orange", color="black") + 
  facet_wrap(~year)  + coord_flip()

#Hour Wise trend for each day of the week

dom_hour_weekday <- domestic %>% 
  mutate( year= year(domestic$InvoiceDate),
          month=month(domestic$InvoiceDate,label = TRUE),
          weekday=wday(domestic$InvoiceDate,label = TRUE),
          day=day(domestic$InvoiceDate),
          hour=hour(domestic$InvoiceDate)) %>%
  group_by(weekday,hour) %>%
  summarise(total=mean(NetFare))
View(dom_hour_weekday)

ggplot(data = dom_hour_weekday,aes(hour,total), group=1 ) + 
  geom_point()+
  geom_line(col="black",size=1)+ 
  facet_wrap(~weekday) 

#compare Avaerge NetFare  vs min NetFare values for different hours of day

ggplot(data = dom_hour,aes(x=hour))+ 
  geom_line(aes(y=total, color="Total netfare"), size=1)+
  geom_line(aes(y=min,color="Min NetFare"),size=1) +
  labs(x="Hours",y="Net Fare",color="Legend")

##################################################################################################

#avg analysis for months

#dom_month <- domestic %>%
#  mutate( year= year(domestic$InvoiceDate),
#          month=month(domestic$InvoiceDate,label = TRUE),
#          week=wday(domestic$InvoiceDate,label = TRUE),
#          day=day(domestic$InvoiceDate),
#          hour=hour(domestic$InvoiceDate)) %>% 
#  group_by(year,month,day) %>% 
#  summarise(total = mean(NetFare))


dom_month <- domestic %>%
  mutate( year= year(domestic$InvoiceDate),
          month=month(domestic$InvoiceDate,label = TRUE),
          week=wday(domestic$InvoiceDate,label = TRUE),
          day=day(domestic$InvoiceDate),
          hour=hour(domestic$InvoiceDate)) %>% 
  group_by(year,month) %>% 
  summarise(total=mean(NetFare))

View(dom_month) 

# avg vs months

ggplot(data = dom_month, aes(month,total, group=1)) + 
  geom_line(color="blue",size=1)+
  geom_point(color="red", size=3) +
   facet_wrap(~year)+
  labs(x= "Months",
       y= "Net Fare")+
  ggtitle("Months Vs Average Net Fare")

###############################################################################################

#avg analysis for weekday 
#for different for different months
dom_weekday <-  domestic %>%
  mutate( year= year(domestic$InvoiceDate),
          month=month(domestic$InvoiceDate,label = TRUE),
          weekday=wday(domestic$InvoiceDate,label = TRUE)) %>%
  group_by(year,month,weekday) %>%
  summarise(total=mean(NetFare))
View(dom_weekday) 

ggplot(data = dom_weekday,aes(x=weekday,y=total, group=1)) +
  geom_point(stat='identity',size=2) +
  geom_line(color="#008000")+
  facet_wrap(vars(year,month),scales = "free") +
  labs(x="weekday",y = "Net fare")

#considering only weekday without month

dom_week <-  domestic %>%
  mutate( year= year(domestic$InvoiceDate),
          month=month(domestic$InvoiceDate,label = TRUE),
          weekday=wday(domestic$InvoiceDate,label = TRUE)) %>%
  group_by(year,weekday) %>%
  summarise(total=median(NetFare))
View(dom_week) 

ggplot(data = dom_week,aes(weekday,total, group=1)) + 
  geom_point(stat='identity',size=5) + 
  geom_line(color= "cyan", size=2)+
  facet_wrap(~year)+ 
  theme(plot.title=element_text(size=15))+ theme_minimal()


#mostly sundays are cheap and and tuesday, friday are expensive

#Month wise trend for each day for the year 2018 and 2019
dom_month <-  domestic %>%
  mutate( year= year(domestic$InvoiceDate),
          month=month(domestic$InvoiceDate,label = TRUE),
          weekday=wday(domestic$InvoiceDate,label = TRUE),
          day=day(domestic$InvoiceDate)) %>%
  group_by(year,month,day) %>%
  summarise(total=mean(NetFare))
View(dom_month) 

ggplot(data = dom_month,aes(day,total, group=1)) + 
  geom_point(stat='identity',size=3) + 
  geom_line(color= "cyan", size=2)+
  facet_wrap(vars(year,month))
  
#############################################################

#Analysis For Total data

invoice <- filter(airlines, airlines$ProductType=="Air")
View(invoice)
invoice$NetFare <- as.numeric(as.character(invoice$NetFare))
invoice$InvoiceDate <- as.Date(invoice$InvoiceDate)
invoice <- arrange(invoice, invoice$NetFare)
invoice <- invoice[7734:151995,] # considering only positive values and excluding 0 and negative values
invoice1<- invoice %>%
  group_by(InvoiceDate,ItineraryType) %>% 
  summarise(max= max(NetFare),AvgNetFare=mean(NetFare),
            min=min(NetFare))
View(invoice1)
#consist of maximum , mean and minimum value for that day! 
invoice1_2018 <- invoice1[1:508,]
summary(invoice1_2018$ItineraryType)

#comparision of average values for domestic and international 
ggplot(data = invoice1_2018) + stat_smooth(aes(InvoiceDate,AvgNetFare,col=ItineraryType)) + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal() 

#boxplots 
invoice_box <- invoice %>% 
  mutate(year=year(InvoiceDate),
         month=month(InvoiceDate,label = TRUE),
         weekday=wday(InvoiceDate,label = TRUE))

View(invoice_box)
#distribution of fare based on different months for domestic and international
ggplot(data = invoice_box) + 
  geom_boxplot(aes(month,col=ItineraryType)) +
  facet_wrap(~year)
summary(invoice_box$NetFare)
#international vs domestic 
#how the fare varies of the period for domestic and international flight fare
ggplot(data = invoice_box) + stat_smooth(aes(InvoiceDate,NetFare,col=ItineraryType),size=1) 

#Filtering Domestic and internatiaonal data

invo_dom <- filter(invoice_box, invoice_box$ItineraryType=="Domestic")
invo_int <- filter(invoice_box, invoice_box$ItineraryType=="International")

#Domestic 
#how the fare varies on different day of the week over the period
ggplot(data = invo_dom) + stat_smooth(aes(InvoiceDate,NetFare,col=weekday),size=1)
ggplot(data = invo_dom) + geom_boxplot(aes(weekday,NetFare,col=year),size=1) + facet_wrap(~year)

#international
#how the fare varies on different day of the week over the period
ggplot(data = invo_int) + geom_boxplot(aes(weekday,NetFare,col=year),size=1) + facet_wrap(~year)
ggplot(data = invo_int) + stat_smooth(aes(InvoiceDate,NetFare,col=weekday))

########################################################################################

View(domestic)
domestic$year <- as.factor(year(domestic$InvoiceDate))
domestic$month <- as.factor(month(domestic$InvoiceDate))
domestic$day <- as.factor(day(domestic$InvoiceDate))
domestic$weekday <- as.factor(weekdays(domestic$InvoiceDate))
domestic$hour <- as.factor(hour(domestic$InvoiceDate))
total_netfare <- domestic %>% group_by(year, month) %>% summarise(totalSales =sum(NetFare))%>%ungroup() %>%arrange(desc(totalSales))
total_netfare %>% distinct(month,.keep_all=TRUE)
View(total_netfare)

#months which had most profit = sum of total netfare for that month
ggplot(data = total_netfare, aes(month,total_netfare$totalSales),col="green")  + 
  geom_bar(stat = 'identity',col="green") + 
  theme(legend.position = "none")+ coord_flip() +
  labs(y = 'total price', x = 'months', title = 'Avg price for different months of the year') +
  facet_wrap(~year) +
  theme(plot.title=element_text(size=15))+
  theme_minimal()
class(domestic$month)
domestic$month <- as.factor(domestic$month)
ggplot(data = domestic, aes(month, NetFare)) + geom_boxplot() + facet_wrap(~year)

#variation 

ggplot(data = domestic, aes(weekday, NetFare)) + geom_boxplot() + facet_wrap(~month)

ggplot(data = domestic, aes(year, NetFare)) + geom_boxplot()

hist(domestic$NetFare)
plot(density(domestic$NetFare))

#months having highest sales of tickets 
class(domestic$month)
domestic$month <- as.numeric(domestic$month)
count <- count(domestic,domestic$year,domestic$month)
View(count)
count$`domestic$month` <- as.factor(count$`domestic$month`)
#this indicates the no. of tickets sold in that month,
ggplot(data = count) +
  geom_point(aes(`domestic$month`,n,col=`domestic$year`),size=3) +
  theme(plot.title=element_text(size=15))+ theme_minimal()

#may-19,april-19,march19 had higher sales of tickets 
###################################################################################################

airlines$InvoiceDate <- as.Date(airlines$InvoiceDate)
compare <- filter(airlines, airlines$ProductType=="Air" | airlines$ProductType=="Other Product" | airlines$ProductType=="Hotel"| airlines$ProductType=="payment" , airlines$ItineraryType=="Domestic")
View(compare)
compare$NetFare <- as.numeric(as.character(compare$NetFare))
summary(compare)
compare <- arrange(compare, NetFare)

#remove negative values
compare <- compare[7391:214785,]

compare <- compare %>% group_by(InvoiceDate,ProductType,ItineraryType)
View(compare)
summary(compare)

compare$year <- as.factor(year(compare$InvoiceDate))
compare$month <- as.factor(month(compare$InvoiceDate,label = TRUE))
View(compare)
compare$InvoiceDate <- as.Date(compare$InvoiceDate)
table(compare$ProductType)
ggplot(data = compare) + geom_smooth(aes(InvoiceDate,NetFare,col=compare$ProductType)) +
  theme(plot.title=element_text(size=15))+ theme_minimal()

####################################################################################################

# International Data Analysis

international <- filter(airlines,ProductType=="Air",ItineraryType=="International")
View(international)
summary(international)
summary(international$NetFare)
international$NetFare <- as.numeric(as.character(international$NetFare))
international$InvoiceDate <- as.Date(international$InvoiceDate)
str(international)
summary(international)
international <- arrange(international, NetFare)
international <- international[501:10761,] # excluding  negative and 0 values
summary(international)
View(international)

########################################################################################################

# i.)NetFare
# satatistical analysis
mean(international$NetFare)
median(international$NetFare)
var(international$NetFare)
sd(international$NetFare)

# ii.) Product Type
international_trail <- filter(airlines,ItineraryType=="International")
table(international_trail$ProductType)

barplot(table(international_trail$ProductType),border="red",
         col="blue",
         density=10, ylab = "Frequency", xlab="Product_Type", main="Bar plot of the different Types of product during April 2018 to June 2019") 

ggplot(international_trail) +
  aes(x = ProductType, fill=ProductType) + 
  geom_bar(col="red") + 
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2))+
  labs(x="Product Type",y="Frequency" )+theme_minimal()

boxplot(international_trail$NetFare~international_trail$ProductType, ylab = "Net Fare", xlab = "Product Type",main="Net fare of different Products",col="#8a0303")

#avg analysis for months

int_month <- international %>% 
  mutate( year= year(international$InvoiceDate),
          month=month(international$InvoiceDate,label = TRUE),
          weekday=wday(international$InvoiceDate,label = TRUE),
          day=day(international$InvoiceDate),
          hour=hour(international$InvoiceDate)) %>%
  group_by(year,month) %>%
  summarise(total=mean(NetFare))
View(int_month)

ggplot(data = int_month, aes(month,total,fill=as.factor(year)))  +
  geom_bar(stat = 'identity',position = "dodge") + 
  theme(legend.position = "none")+
  coord_flip() +
  labs(y = 'avg price', x = 'months', title = 'Avg price for different months of the year') +
  theme_bw()

ggplot(data = int_month, ) + geom_point(aes(as.factor(month),total))+ 
  theme(legend.position = "none")+
  labs(y = 'avg price', x = 'month', title = 'Highest Selling Items') +
  facet_wrap(~year) + 
  theme(plot.title=element_text(size=15))+ 
  theme_minimal()
#2018 - may,august and dec had high prices , 2019- may and June had high prices

################################################################################################

#avg analysis for weekday 
#for different for different months

int_week <- international %>% 
  mutate( year= year(international$InvoiceDate),
          month=month(international$InvoiceDate,label = TRUE),
          week=wday(international$InvoiceDate,label = TRUE),
          day=day(international$InvoiceDate),
          hour=hour(international$InvoiceDate)) %>%
  group_by(year,week) %>%
  summarise(total=mean(NetFare))
View(int_week)
ggplot(data = int_week, aes(x=(week),y=total,fill=as.factor(year)))  +
  geom_bar(stat = 'identity',position = "dodge") + 
  theme(legend.position = "none")+ 
  coord_flip() +
  labs(y = 'avg price', x = 'weekday', title = 'Avg price for different day of the week')+
  theme_bw()

ggplot(data = int_week) + geom_point(aes(week,total))+ facet_wrap(~year)


################################################################################################

#we can also find out maximum and minimum NetFare for different periods

int_month <- international %>% 
  mutate( year= year(international$InvoiceDate),
          month=month(international$InvoiceDate,label = TRUE),
          week=wday(international$InvoiceDate,label = TRUE),
          day=day(international$InvoiceDate),
          hour=hour(international$InvoiceDate)) %>%
  group_by(year,month) %>%
  summarise(total=mean(NetFare))
View(int_month)

ggplot(data = int_month, aes(month,total))  + 
  geom_bar(stat = 'identity') + 
  theme(legend.position = "none")+
  coord_flip() +
  labs( x = 'Month', y = 'Average price', title = 'Avg price for different months of the year') + 
  facet_wrap(~year)

##################################################

# compute average based on each day 

int_day <- international %>% 
  mutate( year= year(international$InvoiceDate),
          month=month(international$InvoiceDate,label = TRUE),
          weekday=wday(international$InvoiceDate,label = TRUE),
          day=day(international$InvoiceDate),
          hour=hour(international$InvoiceDate)) %>%
  group_by(year,month,day) %>%
  summarise(total=mean(NetFare))
View(int_day)

ggplot(data = int_day, aes(day,total))  + 
  geom_bar(stat = 'identity') + 
  theme(legend.position = "none")+ coord_flip() +
  labs( x = 'Day', y = 'Average price',title = 'Avg price for each day of every month of both the year') +
    facet_wrap(vars(year,month))


ggplot(data = int_day, aes(day,total),group=1)  + 
  geom_point(size= 1)+geom_line(col="red")+
  labs( x = 'Day', y = 'Average price',title = 'Avg price for each day of every month of both the year') +
  facet_wrap(vars(year,month))

international <- filter(airlines,ProductType=="Air",ItineraryType=="International")
international$date <- date(international$InvoiceDate)
View(international)
summary(international$NetFare)
international <- arrange(international, NetFare)

international <- international[501:10761,]
View(inter)
boxplot(international$NetFare)

int_avg <- international %>% 
  group_by(date) %>%
  summarise(Avg=mean(NetFare))
View(int_avg)
summary(int_avg)
hist(int_avg$Avg)







########################### ###############################################
########### Model Building #############

# Importng Library
library(forecast)
library(xts)
library(ggplot2)
library(tseries)

domestic_time <- read.csv("C:/EXCELR/DataSciencePoject/Project_ Air Fare prediction/FinalSubmissionProject/dataset_for_Modelling_domestic.csv")
View(domestic_time)
summary(domestic_time)
domestic_time <- domestic_time[,1:2]
class(domestic_time$InvoiceDate)
domestic_time$InvoiceDate <- as.Date(as.character(domestic_time$InvoiceDate))
class(domestic_time$AvgNetFare)

# Converting to time series data

dom_air <- xts(domestic_time$AvgNetFare, order.by = domestic_time$InvoiceDate,frequency = 7)
plot(dom_air)
View(dom_air)
summary(dom_air)
hist(domestic_time$AvgNetFare)
shapiro.test(domestic_time$AvgNetFare)

# Data partioning into train and test

dom_air_train <- dom_air[1:406]
dom_air_test <- dom_air[407:436]



ggAcf(dom_air)
pacf(dom_air)

adf.test(dom_air)

# Model 1
##################### arima / auto arima ############################
dom_arima <- auto.arima(dom_air_train,d=1,D=1,stepwise = FALSE,approximation = FALSE, trace = TRUE)
checkresiduals(dom_arima)
forecast_dom <- forecast(dom_air, h=30)
autoplot(forecast_dom)
data_dom <- data.frame(forecast_dom$mean,dom_air_test)
View(data_dom)
rmse_dom <- sqrt(mean((data_dom$forecast_dom.mean-data_dom$dom_air_test)^2))
rmse_dom
#293.683

# diff order
dom_arima1 <- arima(dom_air_train, order = c(0,1,1))
pacf(dom_arima1$residuals)
checkresiduals(dom_arima1)
forecast_dom <- forecast(dom_arima1, h=30)
data_dom <- data.frame(forecast_dom$mean,dom_air_test)
View(data_dom)
plot(forecast_dom)
rmse_dom1 <- sqrt(mean((data_dom$forecast_dom.mean-data_dom$dom_air_test)^2))
rmse_dom1
#198.12 for c(0,1,0)
#266 for 0,1,1
adf.test(dom_air)
acf(dom_air)

# Model 2
####################### ses ###############################

dom_ses <- ses(dom_air_train,h=30)
data_dom3 <- data.frame(dom_ses$mean,dom_air_test)
rmse_dom2 <- sqrt(mean((data_dom3$dom_ses.mean-data_dom3$dom_air_test)^2))
rmse_dom2
# 266.42

# Model 3
################### neural network ####################

netforc <- nnetar(dom_air_train,scale.inputs = TRUE)
forcast_neural <- forecast(netforc, PI=TRUE, h=30)
plot(forcast_neural)
View(data_dom4)
forcast_neural
data_dom4 <- data.frame(forcast_neural$mean,dom_air_test)
rmse_dom3 <- sqrt(mean((data_dom4$forcast_neural.mean-data_dom4$dom_air_test)^2))
rmse_dom3
#195.548

# Model 4
#################### facebook data ####################
ds <- domestic_time$InvoiceDate
y <- domestic_time$AvgNetFare
data_facebook <- data.frame(ds,y)
data_facebook_train <- data_facebook[1:406,]
data_facebook_test <- data_facebook[407:436,]
install.packages("prophet")
library(prophet)
m <- prophet(data_facebook,daily.seasonality = TRUE)
future <- make_future_dataframe(m, periods = 180 , freq="day")
View(future)
predict <- predict(m,future)
View(predict)
plot(m,predict)
pred.test <- predict[407:436,]
data.dom5 <- data.frame(pred.test$yhat,data_facebook_test)
View(data.dom5)
rmse_dom5 <- sqrt(mean((data.dom5$pred.test.yhat-data.dom5$y)^2))
rmse_dom5
#588.504


# Model 5
################# ets ####################
dm_etc <- ets(dom_air_train,model="AAN")
forecast_ets <- forecast(dm_etc,h=30)
plot(forecast_ets)
data_dom5 <- data.frame(forecast_ets$mean,dom_air_test)
rmse_dom4 <- sqrt(mean((data_dom5$forecast_ets.mean-data_dom$dom_air_test)^2))
rmse_dom4
#253.449
View(dom_air_test)

# Model  6
################ naive model ####################
library(TTR)
dom_naive <- naive(dom_air_train ,h=30)
autoplot(dom_naive)
data_dom6 <- data.frame(dom_naive$mean,dom_air_test)
rmse_dom5 <- sqrt(mean((data_dom6$dom_naive.mean-data_dom6$dom_air_test)^2))
rmse_dom5
#198.3123

############ model based ####################

dom.data <- domestic_time
dom.data["t"] <- 1:436
dom.data$tsq <- dom.data$t*dom.data$t
View(dom.data)
dom.data.train <- dom.data[1:406,]
dom.data.test <- dom.data[407:436,]

dom.mod1 <- lm(dom.data.train$AvgNetFare~t+tsq , data = dom.data.train)
dom.mod1
predict.mod1 <- predict(dom.mod1,dom.data.test)
sqrt(mean((predict.mod1-dom.data.test$AvgNetFare)^2))
#261.65

#################### #####################

# Model 7

random <- dom.data  
random$day <- day(random$InvoiceDate)
random$week <- wday(random$InvoiceDate)
random$month <- month(random$InvoiceDate)
head(random)
library(randomForest)
forest <- randomForest(random$AvgNetFare ~ random$week+ random$day + random$month)
forest
varImpPlot(forest)

################## ######################
dom_air_train <- ts(dom_air_train, start = c(2018,4),frequency = 365)
autoplot(dom_air_train) +
  autolayer(meanf(dom_air_train, h=30),
            series="Mean", PI=FALSE) +
  autolayer(naive(dom_air_train, h=30),
            series="Naïve", PI=FALSE) 
autolayer(snaive(dom_air_train, h=30),
          series="Seasonal naïve", PI=FALSE) +
  ggtitle("Seasonal Naive Method") +
  xlab("Date") + ylab("Price") + 
  guides(colour=guide_legend(title="Forecast"))  + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7"))

smod <- snaive(dom_air_train,h=2*15,drift=FALSE)
plot(smod)

sqrt(mean((smod$mean-dom_air_test)^2))
####################################### ###########################################


# International

international_time <- read.csv("C:\\Users\\USER\\Downloads\\FinalInternatioalDataset.csv")
View(international_time)
international_time <- international_time[,1:2]
class(domestic_time$InvoiceDate)
international_time$InvoiceDate <- as.Date(as.character(international_time$InvoiceDate))

int_air <- xts(international_time$AvgNetFare, order.by = international_time$InvoiceDate,frequency = 365)
View(int_air)
summary(int_air)
plot(int_air)

int_air_train <- int_air[1:380]
int_air_test <- int_air[381:436]

adf.test(int_air)

# Model 1
#stationary 
##################### arima / auto arima ############################

int_arima <- auto.arima(int_air_train)
int_arima
int_forecast <- forecast(int_arima,h=56)
int_data <- data.frame(int_forecast$mean, int_air_test)
int_rmse <- sqrt(mean((int_data$int_forecast.mean-int_data$int_air_test)^2))
int_rmse
#2575

checkresiduals(int_arima)
autoplot(int_forecast)
summary(int_arima)


# Model 2
################# neural network ################################
neural_int_air <- nnetar(int_air,scale.inputs = TRUE)
int_forecast1 <- forecast(neural_int_air,PI=TRUE, h=205)
int_data1 <- data.frame(int_forecast1$mean,int_air_test)
int_rmse1 <- sqrt(mean((int_data1$int_forecast1.mean-int_data1$int_air_test)^2))
int_rmse1
#2406.02

plot(log(int_air_train))
################### manual arima #########################

acf(int_air_train)
pacf(int_air_train)
int_arima_1 <- arima(int_air_train, order = c(15,1,0))
checkresiduals(int_arima_1)
int_forecast2 <- forecast(int_arima_1,h=30)
int_data2 <- data.frame(int_forecast2$mean,int_air_test)
int_rmse2 <- sqrt(mean((int_data2$int_forecast2.mean-int_data2$int_air_test)^2))
int_rmse2
#2368.333
################## #######################

# Model 3

int_ses <- ses(int_air_train,h=30)
plot(int_ses)

plot(diff(int_air_train,lag = 15))
################# ################################# 

