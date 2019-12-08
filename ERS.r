#Author: ECON452 - team Charlie
#Date: Fall 2019
#Subject: ERS
setwd("C:/Users/6o8an/OneDrive/Documents/ECON452")
daily <- read.csv("ers/daily.csv", header = TRUE, stringsAsFactors = FALSE)
weekly <- read.csv("ers/weekly.csv", header = TRUE, stringsAsFactors = FALSE)
monthly <- read.csv("ers/monthly.csv", header = TRUE, stringsAsFactors = FALSE)
monthly <- monthly[1:238,1:7]
colnames(weekly)[2] <- "Mortage_Rate_30Year"

y.ts<- ts(daily$WILLREITIND, start = c(2000,1,3), frequency = 252.75)
S5UTIL.ts <- ts(daily$S5UTIL, start = c(2000,1,3), frequency = 252.75)
S5CONS.ts <- ts(daily$S5CONS, start = c(2000,1,3), frequency = 252.75)
S5COND.ts <- ts(daily$S5COND, start = c(2000,1,3),frequency = 252.75)
Gold.ts <- ts(daily$Gold_Price, start = c(2000,1,3),frequency = 252.75)

MortgageRate.ts <- ts(weekly$Mortage_Rate_30Year, start = c(2000,1), frequency = 52.15)

unemployment.ts <- ts(monthly$US_unemployment_rate, start = c(2000,1),frequency = 12)
PPI.petroleum.ts <- ts(monthly$PPI_Petroleun_and_Coal, start = c(2000,1),frequency = 12)
PPI.Lumber.ts <- ts(monthly$PPI_Lumber_Wood, start = c(2000,1),frequency = 12)
PPI.Rubber.ts <- ts(monthly$PPI_rubber_plastic, start = c(2000,1),frequency = 12)
PPI.Tranportation.ts <- ts(monthly$PPI_Transportation_Equipment, start = c(2000,1),frequency = 12)
PPI.Machinery.ts <- ts(monthly$PPI_Machinery_Equipment, start = c(2000,1),frequency = 12)
