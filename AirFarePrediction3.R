library(lubridate)
library(dplyr)
library(ggplot2)
library(zoo)
library(xts)
library(moments)

# Importing the datasets

airlines_data <-read.csv("C:/EXCELR/DataSciencePoject/Project_ Air Fare prediction/FinalSubmissionProject/Concatenate_B2C_B2E.csv")
View(airlines_data)
sum(is.na(airlines_data$NetFare)) # 0

# Exploratory data analysis
str(airlines_data)
levels(airlines_data$ProductType)

# Converting the datatype to date format for InvoiceDate 

airlines_data$InvoiceDate <- dmy_hm(airlines_data$InvoiceDate)
sum(is.na(airlines_data$NetFare))
#airlines_data$NetFare <- as.numeric(airlines_data$NetFare)
#airlines_data$NetFare <- as.numeric(as.character(airlines_data$NetFare))
head(airlines_data$InvoiceDate)
str(airlines_data)
View(airlines_data)
summary(airlines_data)

# Sorting the data in ascending format

airlines_data <- arrange(airlines_data, airlines_data$InvoiceDate)  # This contains entire datasets  both  domestic and international arranged in ascending order
View(airlines_data)
airlines_domestic <- filter(airlines_data,ItineraryType=="Domestic") # This  will filter out only the domestic data
airlines_domestic <- airlines_domestic[,c(1,2,3)]
View(airlines_domestic)
levels(airlines_domestic$ProductType)
str(airlines_domestic)

############# Statistical Analysis ##############

# Excluding product type like refund and payment as this contains empty values.
names(airlines_domestic_trail)
table(airlines_domestic$ProductType)
airlines_domestic_trail <- airlines_domestic
airlines_domestic_trail <- filter(airlines_domestic_trail, ProductType != "payment" & ProductType !="refund")
View(airlines_domestic_trail)
levels(airlines_domestic_trail$ProductType)
table(airlines_domestic_trail$ProductType)

# i.) Netfare
str(airlines_domestic_trail)
summary(airlines_domestic_trail)
airlines_domestic_trail$NetFare <- as.numeric(as.character(airlines_domestic_trail$NetFare))
mean(airlines_domestic_trail$NetFare)
median(airlines_domestic_trail$NetFare)
skewness(airlines_domestic_trail$NetFare)
kurtosis(airlines_domestic_trail$NetFare)

#Graphical Representation

hist(airlines_domestic_trail$NetFare, xlab="Net_Fare", ylab= "Frequency",  main="Histogram of doemstic airlines netfare",col="blue")
boxplot(airlines_domestic_trail$NetFare, main="Barplot of doemstic airlines netfare", col="#FF7F50")


# ii.) Product Type
table(airlines_domestic_trail$ProductType)

# barplot(table(airlines_domestic$ProductType),border="red",
#         col="blue",
#         density=10, ylab = "Frequency", xlab="Product_Type", main="Bar plot of the Different type of product during April 2018 to June 2019") 
 
ggplot(airlines_domestic) +
   aes(x = ProductType, fill=ProductType) + 
   geom_bar(col="red") + 
   geom_text(stat = 'count',aes(label =..count.., vjust = -0.2))+
 labs(x="Product Type",y="Frequency" )+theme_minimal()

boxplot(airlines_domestic_trail$NetFare~airlines_domestic_trail$ProductType, ylab = "Net Fare", xlab = "Product Type",main="Net fare of different Products",col="#8a0303")

####################  Analyisis of trend of NetFare ##################

# Sorting out only air 
domestic_air <- filter(airlines_domestic,ProductType=="Air") # 141234
View(domestic_air)
domestic_air$NetFare<-as.numeric(as.character(domestic_air$NetFare))
str(domestic_air)
mean(domestic_air$NetFare)
ggplot(data=domestic_air, aes(x=InvoiceDate,y= NetFare))+geom_point()
# ggplot(data=domestic_air, aes(x=ProductType, y= NetFare, col=ProductType))+ geom_point()

# Seperatting the Time stamp into diferent columns namely, year,
# month,day,wday,hours,minute

domestic_air2 <- mutate(domestic_air,year= year(domestic_air$InvoiceDate),
                              month=month(domestic_air$InvoiceDate,label = TRUE),
                              day=day(domestic_air$InvoiceDate),
                              wday= wday(domestic_air$InvoiceDate,label = TRUE),
                              hours=hour(domestic_air$InvoiceDate),
                              minute=minute(domestic_air$InvoiceDate))


View(domestic_air2)
# Air  NetFare trend for a particular year

domestic_air2_year<- domestic_air2 %>%
  group_by(month)%>%
  summarise(mean_netfare =mean(NetFare))
View(domestic_air2_year)

 
ggplot(data=domestic_air2_year, aes(x=month,y= mean_netfare, group=1))+
  geom_point(stat='identity')+ geom_line(color="#008000")+labs(x="Months",y = "Net fare")

# Month Wise trend for average NetFare of Air for the year 2018 and 2019

domestic_air2_month <- domestic_air2 %>%
  group_by(year,month)%>%
  summarise(mean_netfare =mean(NetFare))
View(domestic_air2_month)
ggplot(data=domestic_air2_month, aes(x=month,y= mean_netfare, group=1))+
  geom_point(stat='identity')+ geom_line(color="#008000")+facet_grid(~year)+labs(x="Months",y = "Net fare")

# Average  weekday Netfare  for every month for 2018 and 2019 

domestic_air2_day <- domestic_air2 %>%
  group_by(year,wday,month)%>%
  summarise(mean_netfare =mean(NetFare))
View(domestic_air2_day)
ggplot(data=domestic_air2_day, aes(x=wday,y= mean_netfare, group=1))+
  geom_point(stat='identity')+ geom_line(color="#008000")+facet_grid(month~year)+labs(x="Months",y = "Net fare")

# Average weekday sale for both the year 
domestic_air2_day <- domestic_air2 %>%
  group_by(year,wday)%>%
  summarise(mean_netfare =mean(NetFare))
View(domestic_air2_day)
ggplot(data=domestic_air2_day, aes(x=wday,y= mean_netfare, group=1))+
  geom_point(stat='identity')+ geom_line(color="#008000")+facet_grid(~year)+labs(x="weekday",y = "Net fare")

# Average wekday sale for a year
domestic_air2_day <- domestic_air2 %>%
  group_by(wday)%>%
  summarise(mean_netfare =mean(NetFare))
View(domestic_air2_day)
ggplot(data=domestic_air2_day, aes(x=wday,y= mean_netfare, group=1))+
  geom_point(stat='identity')+ geom_line(color="#008000")+labs(x="weekday",y = "Net fare")

# Hour Wise trend for each day of a week

domestic_air2_hour <- domestic_air2 %>%
  group_by(wday, hours)%>%
  summarise(mean_netfare =mean(NetFare))
View(domestic_air2_hour)
ggplot(data=domestic_air2_hour, aes(x=hours,y= mean_netfare, group=1))+
  geom_point(stat='identity')+ geom_line(color="#008000")+facet_wrap(~wday)+labs(x="Hours",y = "Net fare")

# Average Hour Wise trend for a week
domestic_air2_hour <- domestic_air2 %>%
  group_by(hours)%>%
  summarise(mean_netfare =mean(NetFare))
View(domestic_air2_hour)
ggplot(data=domestic_air2_hour, aes(x=hours,y= mean_netfare, group=1))+
  geom_point(stat='identity')+ geom_line(color="#008000")+labs(x="Hours",y = "Net fare")

# Average Hour Wise trend for both the  year
domestic_air2_hour <- domestic_air2 %>%
  group_by(year, hours, )%>%
  summarise(mean_netfare =mean(NetFare))
View(domestic_air2_hour)
ggplot(data=domestic_air2_hour, aes(x=hours,y= mean_netfare, group=1))+
  geom_point(stat='identity')+ geom_line(color="#008000")+facet_grid(~year)+labs(x="Hours",y = "Net fare")

# Month Wise trend for each day of every month for both the year 2018 and 2019
domestic_air2_month <- domestic_air2 %>%
  group_by(year,month,day)%>%
  summarise(mean_netfare =mean(NetFare))
View(domestic_air2_month)
ggplot(data=domestic_air2_month, aes(x=day,y= mean_netfare, group=1))+
  geom_point(stat='identity')+ geom_line(color="#008000")+
  facet_wrap(vars(month,year))+
  labs(x="Hours",y = "Net fare")

# Month Wise trend for each day for a particular year

domestic_air2_month <- domestic_air2 %>%
  group_by(month,day)%>%
  summarise(mean_netfare =mean(NetFare))
View(domestic_air2_month)
ggplot(data=domestic_air2_month, aes(x=day,y= mean_netfare, group=1))+
  geom_point(stat='identity')+ geom_line(color="#008000")+
  facet_wrap(~month)+
  labs(x="Hours",y = "Net fare")

#### Filtering data based on  Product type #########
# Filtering out only Air Cancellation 
domestic_air_cancellation <- filter(airlines_domestic,ProductType=="Air Cancellation") #4099
View(domestic_air_cancellation)
# Filtering out only Air Debit Note
domestic_air_debitnote <- filter(airlines_domestic,ProductType=="Air Debit Note") #446
View(domestic_air_debitnote)
# Filtering out only Air Loss 
domestic_airloss <- filter(airlines_domestic,ProductType=="Air Loss") #195
View(domestic_airloss)
# Filtering out only Hotel
domestic_hotel <- filter(airlines_domestic,ProductType=="Hotel") #7444
View(domestic_hotel)
# Filtering out only Hotel Cancellation
domestic_hotelcancel <- filter(airlines_domestic,ProductType=="Hotel Cancellation") # 264
View(domestic_hotelcancel)
# Filtering out only Other Product
domestic_otherproduct <- filter(airlines_domestic,ProductType=="Other Product") #16250
View(domestic_otherproduct)
# Filtering out only Other Product Cancellation
domestic_otherproductcancel <- filter(airlines_domestic,ProductType=="Other Product Cancellation") # 440
View(domestic_otherproductcancel)
# Filtering out only payment
domestic_payment <- filter(airlines_domestic,ProductType=="payment") # 49857
View(domestic_payment)
# Filtering out only refund
domestic_refund <- filter(airlines_domestic, ProductType=="refund") # 4987
View(domestic_refund) #Empty values in NetFare

# the other Product Type  Hotel Debit Note (3) , Hotel Loss(4) and Other Product Debit Note(8) is ignored because of very low count

# Analysis of  air netfare   vs air cancellation

# View(domestic_air)
# names(domestic_air)
# colnames(domestic_air)[colnames(domestic_air) == "InvoiceDate"] <- "InvoiceDate_Air"
# colnames(domestic_air)[colnames(domestic_air) == "NetFare"] <- "NetFare_Air"
# View(domestic_air_cancellation)
# View(domestic_air)
# 
# colnames(domestic_air_cancellation)[colnames(domestic_air_cancellation) == "InvoiceDate"] <- "InvoiceDate_Air_cancel"
# colnames(domestic_air_cancellation)[colnames(domestic_air_cancellation) == "NetFare"] <- "NetFare_Air_cancel"
# View(domestic_air_cancellation)
# combo_air_cancel2<-  merge(domestic_air,domestic_air_cancellation)
# names(combo_air_cancel2)
# merge(authors, books, by.x="surname", by.y="name")
# 
# View(combo_air_cancel2)
# plot(as.numeric(domestic_air$NetFare_Air),as.numeric(domestic_air_cancellation$NetFare_Air_cancel))



######## Comparison of trend among different Product###########

########## Comaparing Trends of netfare of AIR and net fare of HOTEL #############
levels(airlines_domestic$ProductType)

domestic_air_hotel <- filter(airlines_domestic,ProductType=="Air" | ProductType=="Hotel") # 148678
View(domestic_air_hotel)
domestic_air_hotel$NetFare<- as.numeric(as.character(domestic_air_hotel$NetFare))
str(domestic_air_hotel)
ggplot(data=domestic_air_hotel, aes(x=InvoiceDate,y= NetFare, col=ProductType))+geom_point()
#ggplot(data=domestic_air_hotel, aes(x=ProductType, y= NetFare, col=ProductType))+ geom_point()

# Seperatting the Time stamp into diferent columns namely, year,
# month,day,wday,hours,minute

domestic_air_hotel2 <- mutate(domestic_air_hotel,year= year(domestic_air_hotel$InvoiceDate),
                              month=month(domestic_air_hotel$InvoiceDate,label = TRUE),
                              day=day(domestic_air_hotel$InvoiceDate),
                              wday= wday(domestic_air_hotel$InvoiceDate,label = TRUE),
                              hours=hour(domestic_air_hotel$InvoiceDate),
                              minute=minute(domestic_air_hotel$InvoiceDate))

View(domestic_air_hotel2)
# Using groupBy function
domestic_air_hotel2g <- domestic_air_hotel2 %>%
            group_by(ProductType,year,month,day)%>%
            summarise(mean_netfare =mean(NetFare))

View(domestic_air_hotel2g)

# Visualization

ggplot(data=domestic_air_hotel2g, aes(x=day,y= mean_netfare, col=ProductType))+
  geom_point(stat='identity')+ geom_line(stat='identity')+ facet_wrap(month~year)+
  labs(x="Day",y = "Net fare")
ggplot(data=domestic_air_hotel2g, aes(x=day,y= mean_netfare, col=ProductType))+
   geom_line(stat='identity')+ facet_wrap(month~year)+
  labs(x="Day",y = "Net fare")

########## Comparing Trends of netfare of AIR and net fare of PAYMENT #############

domestic_air_payment<- filter(airlines_domestic,ProductType=="Air" | ProductType=="payment") # 191091
View(domestic_air_payment)
domestic_air_payment$NetFare<- as.numeric(as.character(domestic_air_payment$NetFare))
str(domestic_air_payment)
mean(domestic_air_payment$NetFare)
#ggplot(data=domestic_air_payment, aes(x=InvoiceDate,y= NetFare, col=ProductType))+geom_point()

# Seperatting the Time stamp into diferent columns namely, year,
# month,day,wday,hours,minute

domestic_air_payment2 <- mutate(domestic_air_payment,year= year(domestic_air_payment$InvoiceDate),
                              month=month(domestic_air_payment$InvoiceDate,label = TRUE),
                              day=day(domestic_air_payment$InvoiceDate),
                              wday= wday(domestic_air_payment$InvoiceDate,label = TRUE),
                              hours=hour(domestic_air_payment$InvoiceDate),
                              minute=minute(domestic_air_payment$InvoiceDate))


View(domestic_air_payment2)
# Using groupBy function
domestic_air_payment2g <- domestic_air_payment2 %>%
  group_by(year,ProductType,month,day)%>%
  summarise(mean_netfare =mean(NetFare))

View(domestic_air_payment2g)

# Visualization

ggplot(data=domestic_air_payment2g, aes(x=day,y= mean_netfare, col=ProductType))+
  geom_point(stat='identity')+ geom_line(stat='identity')+ facet_wrap(month~year)+
  labs(x="Day",y = "Net fare")
ggplot(data=domestic_air_payment2g, aes(x=day,y= mean_netfare, col=ProductType))+
   geom_line(stat='identity')+ facet_wrap(month~year)+
  labs(x="Day",y = "Net fare")

########## Comparing Trends of netfare of AIR and net fare of Other Product #############

domestic_air_otherPro<- filter(airlines_domestic,ProductType=="Air" | ProductType=="Other Product") # 157484
View(domestic_air_otherPro)
domestic_air_otherPro$NetFare<- as.numeric(as.character(domestic_air_otherPro$NetFare))
str(domestic_air_otherPro)

#ggplot(data=domestic_air_payment, aes(x=InvoiceDate,y= NetFare, col=ProductType))+geom_point()

# Seperatting the Time stamp into diferent columns namely, year,
# month,day,wday,hours,minute

domestic_air_otherPro2 <- mutate(domestic_air_otherPro,year= year(domestic_air_otherPro$InvoiceDate),
                                month=month(domestic_air_otherPro$InvoiceDate,label = TRUE),
                                day=day(domestic_air_otherPro$InvoiceDate),
                                wday= wday(domestic_air_otherPro$InvoiceDate,label = TRUE),
                                hours=hour(domestic_air_otherPro$InvoiceDate),
                                minute=minute(domestic_air_otherPro$InvoiceDate))


View(domestic_air_otherPro2)
# Using groupBy function
domestic_air_otherPro2g <- domestic_air_otherPro2 %>%
  group_by(year,ProductType,month,day)%>%
  summarise(mean_netfare =mean(NetFare))

View(domestic_air_otherPro2g)

# Visualization

ggplot(data=domestic_air_otherPro2g, aes(x=day,y= mean_netfare, col=ProductType))+
  geom_point(stat='identity')+ geom_line(stat='identity')+ facet_wrap(month~year)+
  labs(x="Day",y = "Net fare")
ggplot(data=domestic_air_otherPro2g, aes(x=day,y= mean_netfare, col=ProductType))+
  geom_line(stat='identity')+ facet_wrap(month~year)+
  labs(x="Day",y = "Net fare")

########## Comparing Trends of netfare of AIR and net fare of Refund #############


domestic_air_refund<- filter(airlines_domestic,ProductType=="Air" | ProductType=="refund") # 146221
View(domestic_air_refund)
domestic_air_refund$NetFare<- as.numeric(as.character(domestic_air_refund$NetFare))
str(domestic_air_refund)

#ggplot(data=domestic_air_payment, aes(x=InvoiceDate,y= NetFare, col=ProductType))+geom_point()

# Seperatting the Time stamp into diferent columns namely, year,
# month,day,wday,hours,minute

domestic_air_refund2 <- mutate(domestic_air_refund,year= year(domestic_air_refund$InvoiceDate),
                                 month=month(domestic_air_refund$InvoiceDate,label = TRUE),
                                 day=day(domestic_air_refund$InvoiceDate),
                                 wday= wday(domestic_air_refund$InvoiceDate,label = TRUE),
                                 hours=hour(domestic_air_refund$InvoiceDate),
                                 minute=minute(domestic_air_refund$InvoiceDate))


View(domestic_air_refund2)
# Using groupBy function
domestic_air_refund2g <- domestic_air_refund2 %>%
  group_by(year,ProductType,month,day)%>%
  summarise(mean_netfare =mean(NetFare))

View(domestic_air_refund2g)

# Visualization

ggplot(data=domestic_air_refund2g, aes(x=day,y= mean_netfare, col=ProductType))+
  geom_point(stat='identity')+ geom_line(stat='identity')+ facet_wrap(month~year)+
  labs(x="Day",y = "Net fare")
ggplot(data=domestic_air_refund2g, aes(x=day,y= mean_netfare, col=ProductType))+
  geom_line(stat='identity')+ facet_wrap(month~year)+
  labs(x="Day",y = "Net fare")

########## Comparing Trends of netfare of AIR and net fare of Air Cancellation #############

domestic_air_airCancel<- filter(airlines_domestic,ProductType=="Air" | ProductType=="Air Cancellation") # 145333
View(domestic_air_airCancel)
domestic_air_airCancel$NetFare<- as.numeric(as.character(domestic_air_airCancel$NetFare))
str(domestic_air_airCancel)

#ggplot(data=domestic_air_payment, aes(x=InvoiceDate,y= NetFare, col=ProductType))+geom_point()

# Seperatting the Time stamp into diferent columns namely, year,
# month,day,wday,hours,minute

domestic_air_airCancel2 <- mutate(domestic_air_airCancel,year= year(domestic_air_airCancel$InvoiceDate),
                               month=month(domestic_air_airCancel$InvoiceDate,label = TRUE),
                               day=day(domestic_air_airCancel$InvoiceDate),
                               wday= wday(domestic_air_airCancel$InvoiceDate,label = TRUE),
                               hours=hour(domestic_air_airCancel$InvoiceDate),
                               minute=minute(domestic_air_airCancel$InvoiceDate))


View(domestic_air_airCancel2)
# Using groupBy function
domestic_air_airCancel2g <- domestic_air_airCancel2 %>%
  group_by(year,ProductType,month,day)%>%
  summarise(mean_netfare =mean(NetFare))

View(domestic_air_airCancel2g)

# Visualization

ggplot(data=domestic_air_airCancel2g, aes(x=day,y= mean_netfare, col=ProductType))+
  geom_point(stat='identity')+ geom_line(stat='identity')+ facet_wrap(month~year)+
  labs(x="Day",y = "Net fare")
ggplot(data=domestic_air_airCancel2g, aes(x=day,y= mean_netfare, col=ProductType))+
  geom_line(stat='identity')+ facet_wrap(month~year)+
  labs(x="Day",y = "Net fare")

########## Comparing Trends of netfare of AIR,  OTHER PRODUCT CANCELLATION, HOTEL CANCELLATION, AIR DEBIT NOTE,AIR LOSS  #############

domestic_air_variPrdct<- filter(airlines_domestic,ProductType=="Air" | ProductType=="Other Product Cancellation"|
                                  ProductType=="Hotel Cancellation"|ProductType=="Air Debit Note"|
                                  ProductType=="Air Loss") # 142579

View(domestic_air_variPrdct)
domestic_air_variPrdct$NetFare<- as.numeric(as.character(domestic_air_variPrdct$NetFare))
str(domestic_air_variPrdct)

#ggplot(data=domestic_air_payment, aes(x=InvoiceDate,y= NetFare, col=ProductType))+geom_point()

# Seperatting the Time stamp into diferent columns namely, year,
# month,day,wday,hours,minute

domestic_air_variPrdct2 <- mutate(domestic_air_variPrdct,year= year(domestic_air_variPrdct$InvoiceDate),
                                  month=month(domestic_air_variPrdct$InvoiceDate,label = TRUE),
                                  day=day(domestic_air_variPrdct$InvoiceDate),
                                  wday= wday(domestic_air_variPrdct$InvoiceDate,label = TRUE),
                                  hours=hour(domestic_air_variPrdct$InvoiceDate),
                                  minute=minute(domestic_air_variPrdct$InvoiceDate))


View(domestic_air_variPrdct2)
# Using groupBy function
domestic_air_variPrdct2g <- domestic_air_variPrdct2 %>%
  group_by(year,ProductType,month,day)%>%
  summarise(mean_netfare =mean(NetFare))

View(domestic_air_variPrdct2g)

# Visualization

ggplot(data=domestic_air_variPrdct2g, aes(x=day,y= mean_netfare, col=ProductType))+
  geom_point(stat='identity')+ geom_line(stat='identity')+ facet_wrap(month~year)+
  labs(x="Day",y = "Net fare")
ggplot(data=domestic_air_variPrdct2g, aes(x=day,y= mean_netfare, col=ProductType))+
  geom_line(stat='identity')+ facet_wrap(month~year)+
  labs(x="Day",y = "Net fare")

#create  Dummy Variables of ProductType 

library(dummies)
domestic_air_dummy <- cbind(airlines_domestic,dummy(airlines_domestic$ProductType,sep = "_"))
View(domestic_air_dummy)
hist(count(domestic_air$ProductType))
names(domestic_air_dummy)
dom_air <- domestic_air_dummy[,c(2,5:16)]
View(dom_air)
cor(dom_air)
plot(dom_air)

library(corpcor)
cor2pcor(cor(dom_air))
library(car)
plot(dom_air)

####### Scatter plot matrix with Correlations inserted in graph
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}
?pairs
pairs(dom_air, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")


## Exploratory Data Analysis

# Filtering  the data based on the criteria---  ProductType= "Air", IterneraryType= "Domestic"

domestic_air <- filter(airlines_data,ProductType=="Air",ItineraryType=="Domestic")
View(domestic_air)
length(domestic_air)
str(domestic_air)
domestic_air$InvoiceDate

# Converting the datatype to date format for InvoiceDate

domestic_air$InvoiceDate <- dmy_hm(domestic_air$InvoiceDate)
head(domestic_air$InvoiceDate)
str(domestic_air)
View(domestic_air)




# #Corelation
# 
# domestic_air
# names(domestic_air)
# domestic_air_cancellation
# names(domestic_air_cancellation)
# 
# domestic_air <- mutate(domestic_air,year= year(domestic_air$InvoiceDate),
#                                   month=month(domestic_air$InvoiceDate,label = TRUE),
#                                   day=day(domestic_air$InvoiceDate),
#                                   wday= wday(domestic_air$InvoiceDate,label = TRUE),
#                                   hours=hour(domestic_air$InvoiceDate),
#                                   minute=minute(domestic_air$InvoiceDate))
# 
# 
# View(domestic_air)
# # Using groupBy function
# domestic_airg <- domestic_air %>%
#   group_by(year,month,day)%>%
#   summarise(mean_netfare =mean(NetFare))
# View(domestic_airg)
# 
# domestic_air_cancellation <- mutate(domestic_air_cancellation,year= year(domestic_air_cancellation$InvoiceDate),
#                                     month=month(domestic_air_cancellation$InvoiceDate,label = TRUE),
#                                     day=day(domestic_air_cancellation$InvoiceDate),
#                                     wday= wday(domestic_air_cancellation$InvoiceDate,label = TRUE),
#                                     hours=hour(domestic_air_cancellation$InvoiceDate),
#                                     minute=minute(domestic_air_cancellation$InvoiceDate))
# View(domestic_air_cancellation)
# domestic_air_cancellationg <- domestic_air_cancellation %>%
#   group_by(year,month,day)%>%
#   summarise(mean_netfare =mean(NetFare))
# View(domestic_air_cancellationg)

############## Model Building ######################
# 1.) For domestic dataset

domestic_air <- filter(airlines_domestic,ProductType=="Air") # 141234
View(domestic_air)
domestic_air$NetFare<-as.numeric(as.character(domestic_air$NetFare))
str(domestic_air)
mean(domestic_air$NetFare)
hist(domestic_air$NetFare)
boxplot(domestic_air$NetFare)
ggplot(data=domestic_air, aes(x=InvoiceDate,y= NetFare))+geom_point()
# ggplot(data=domestic_air, aes(x=ProductType, y= NetFare, col=ProductType))+ geom_point()

domestic_air_date_time <- tidyr::separate(domestic_air, InvoiceDate, c("InvoiceDate", "Invoicetime"), sep = " ")
View(domestic_air_date_time)
str(domestic_air_date_time)
domestic_air_date_time$NetFare <- as.numeric(as.character(domestic_air_date_time$NetFare))
domestic_air_date_time$InvoiceDate <- ymd(domestic_air_date_time$InvoiceDate)

dom<- domestic_air_date_time[1:28,c(1,3)] 
View(dom)
mean(dom$NetFare)
sum(dom$NetFare)
domestic_air_date_time_day <- domestic_air_date_time %>% 
  group_by(InvoiceDate) %>% 
  summarize(Avg_Netfare = mean(NetFare), Total_Netfare= sum(NetFare))

View(domestic_air_date_time_day)
str(domestic_air_date_time_day)
plot(domestic_air_date_time_day$Avg_Netfare,type = 'o')

library(forecast)
library(fpp)
library(smooth) # forsmoothing and MAPE
library(tseries)
library(readxl)
library(MLmetrics) # For Mape

plot( domestic_air_date_time_day$InvoiceDate,domestic_air_date_time_day $Avg_Netfare)
abline(reg=lm(domestic_air_date_time_day$Avg_Netfare~domestic_air_date_time_day$InvoiceDate))


tsNetFare<-ts(domestic_air_date_time_day$Avg_Netfare,start=c(2018,4,1),end=c(2019,6,10),frequency = 365.25)
tsNetFare <- msts(domestic_air_date_time_day$Avg_Netfare,seasonal.periods = c(7,365.25),start = decimal_date(as.Date("2018-04-01")),end=decimal_date(as.Date("2019-06-10")))
tsNetFare <- msts(domestic_air_date_time_day$Avg_Netfare, frequency = 365.25,start = decimal_date(as.Date("2018-04-01")),end=decimal_date(as.Date("2019-06-10")))

ggseasonplot(tsNetFare, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")


#polar seasonal plot
ggseasonplot(tsNetFare, polar=TRUE) +
  ylab("$ million") +
  ggtitle("Polar seasonal plot: antidiabetic drug sales")

ggplot(domestic_air_date_time_day, aes(InvoiceDate, Avg_Netfare)) + geom_line() + scale_x_date('month')  + ylab("Daily Netfare") +
  xlab("")

count_ts = ts(domestic_air_date_time_day[, c('Avg_Netfare')])
View(count_ts)

domestic_air_date_time_day$clean_cnt = tsclean(count_ts)
View(domestic_air_date_time_day)
ggplot() + 
  geom_line(data = domestic_air_date_time_day, aes(x = InvoiceDate, y = clean_cnt)) + ylab('Cleaned Net Fare') 


View(tsNetFare)
start(tsNetFare)
end(tsNetFare)
plot(tsNetFare, main="Time Vs NetFare", xlab="Year", ylab="Daily NetFare")
seasonplot(tsNetFare, main="Time Vs NetFare", xlab="Year", ylab="Daily NetFare")
Seasonality(tsNetFare, return.freq = FALSE, plot = FALSE)
install.packages("greenbrown")

# Decomposition
#To analyse the seasonality of the data we can extract the components.
#To do this with multiple seasonal components we can use TBATS.
?tbats
tbats <- tbats(tsNetFare)
plot(tbats, main="Multiple Season Decomposition")
fit <- tbats(tsNetFare, use.box.cox=NULL, use.parallel=TRUE, num.cores = NULL, 
             use.trend=NULL, use.damped.trend=NULL, use.arma.errors=TRUE,                           
             model=NULL)

components <- tbats.components(fit)
plot(components)

checkresiduals(fit)

fc <- forecast(fit)
plot(fc)
accuracy(fc)

sp<- predict(tbats,h=30)
plot(forecast(tbats))
plot(sp, main = "TBATS Forecast", include=30)


# Arima
arima_model <- auto.arima(tsNetFare)
summary(arima_model)
fore_arima = forecast::forecast(arima_model, h=30)
plot(fore_arima, main = " Forecast", include=30)
df_arima = as.data.frame(fore_arima)
dat_test$arima = df_arima$`Point Forecast`
mape(dat_test$unemploy, dat_test$arima)  ## 2.1


# For everyday model

dom_month_april<- domestic_air_date_time[1:2471,c(1,3)] 
View(dom_month_april)
mean(dom_month_april$NetFare)
sum(dom_month_april$NetFare)
dom_month_april1 <- dom_month %>% 
  group_by(InvoiceDate) %>% 
  summarize(Avg_Netfare = mean(NetFare), Total_Netfare= sum(NetFare))

View(dom_month_april1)
str(dom_month_april1)
plot(dom_month_april1$Avg_Netfare,type = 'o')

#  For Month 
dom_month_may<- domestic_air_date_time[1:2472,c(1,3)] 
View(dom_month_may)
mean(dom_month_may$NetFare)
sum(dom_month_may$NetFare)
dom_month_may1 <- dom_month_may %>% 
  group_by(InvoiceDate) %>% 
  summarize(Avg_Netfare = mean(NetFare), Total_Netfare= sum(NetFare))

View(dom_month_may1)
str(dom_month_may1)
plot(dom_month_may1$Avg_Netfare,type = 'o')