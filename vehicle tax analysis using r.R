data<-read.csv("C:/Users/KARTHIKA/OneDrive/Desktop/GetVehicleData.csv")
data


typeof(data)

data1<-data.frame(data)
data1

head(data1)
summary(data1)

mean()
max(data1$Count)

barplot(data1)



hist(data1$VehicleTypeId,main = "HISTOGRAM OF VEHICLE TYPE ID WITH ITS COUNT",
     xlab = "VEHICLE TYPE ID",
     ylab = "COUNT",
     col = "black")


data1[is.na(data1)]=0

library(modeest)
mfv(data1$VehicleTypeId)
mfv(data1$Year)
mfv(data1$Month)
mfv(data1$Amount)

#linear regression
#as count increases,amount increases,
#so amount is dependent on count

x<-data1$Count
y<-data1$Amount
relation<-lm(y~x)
relation

summary(relation)

#ml model
plot(y,x,col="red",main="regression",
     abline(lm(x~y)),cex=1.3,pch=16,xlab="count",ylab="amount")

#predict
a<-data.frame(Year=2024)
res<-predict(relation,a)
print(res)
mean(res)

#time series
library(lubridate)
x<-data1$Amount
#creating time series object,from date 22 jan 2020
mts<-ts(x,start = decimal_date(ymd("1989-01-01")),end=decimal_date(ymd("2022-01-01")),frequency = 12)
#plotting the graph
plot(mts,xlab="YEAR",ylab="AMOUNT",main="YEARLY TAX COLLECTION AMOUNT",col.main="black")


#using ggplot
library(ggplot2)
ggplot(data1,aes(x=VehicleTypeId,y=Count))+geom_bar(stat="identity",width=0.8)

mean(data1$Amount)
