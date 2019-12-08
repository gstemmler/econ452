#Author: ECON452 - team Charlie
#Date: Fall 2019
#Subject: ERS

# LOAD PACKAGES
library(xts)
library(zoo)
library(dplyr)
library(leaps)
# -------------------------------------------------------------------------------------------
# LOAD and CLEAN DATA
setwd("C:/Users/6o8an/OneDrive/Documents/ECON452")

daily <- read.csv("ers/daily.csv", header = TRUE, stringsAsFactors = FALSE)
daily$date <- as.Date(daily$date, "%Y-%m-%d")

weekly <- read.csv("ers/weekly.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(weekly)[1] <- "date"
colnames(weekly)[2] <- "Mortage_Rate_30Year"
weekly$date <- as.Date(weekly$date, "%Y-%m-%d")

monthly <- read.csv("ers/monthly.csv", header = TRUE, stringsAsFactors = FALSE)
monthly <- monthly[1:238,1:7]
monthly$date <- as.Date(monthly$date , "%Y-%m-%d")

# -------------------------------------------------------------------------------------------
# CONVERT W/ TS

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

# -------------------------------------------------------------------------------------------
# CONVERT W/ XTS
y.xts <- xts(daily$WILLREITIND,order.by = daily$date)
S5UTIL.xts <- xts(daily$S5UTIL,order.by = daily$date)
S5CONS.xts <- xts(daily$S5CONS,order.by = daily$date)
S5COND.xts <- xts(daily$S5COND,order.by = daily$date)
Gold.xts <- xts(daily$Gold_Price,order.by = daily$date)

MortgageRate.xts <- xts(weekly$Mortage_Rate_30Year,order.by = weekly$date)

unemployment.xts <- xts(monthly$US_unemployment_rate,order.by = monthly$date)
PPI.petroleum.xts <- xts(monthly$PPI_Petroleun_and_Coal,order.by = monthly$date)
PPI.Lumber.xts <- xts(monthly$PPI_Lumber_Wood,order.by = monthly$date)
PPI.Rubber.xts <- xts(monthly$PPI_rubber_plastic,order.by = monthly$date)
PPI.Tranportation.xts <- xts(monthly$PPI_Transportation_Equipment,order.by = monthly$date)
PPI.Machinery.xts <- xts(monthly$PPI_Machinery_Equipment,order.by = monthly$date)

MortgageRate.365 <- na.locf(merge(MortgageRate.xts, foo=zoo(NA, order.by=seq(start(MortgageRate.xts), end(MortgageRate.xts),"day",drop=F)))[, 1])

unemployment.365 <- na.locf(merge(unemployment.xts, foo=zoo(NA, order.by=seq(start(unemployment.xts), end(unemployment.xts),"day",drop=F)))[, 1])
PPI.petroleum.365 <- na.locf(merge(PPI.petroleum.xts, foo=zoo(NA, order.by=seq(start(PPI.petroleum.xts), end(PPI.petroleum.xts),"day",drop=F)))[, 1])
PPI.Lumber.365 <- na.locf(merge(PPI.Lumber.xts, foo=zoo(NA, order.by=seq(start(PPI.Lumber.xts), end(PPI.Lumber.xts),"day",drop=F)))[, 1])
PPI.Rubber.365 <- na.locf(merge(PPI.Rubber.xts, foo=zoo(NA, order.by=seq(start(PPI.Rubber.xts), end(PPI.Rubber.xts),"day",drop=F)))[, 1])
PPI.Tranportation.365 <- na.locf(merge(PPI.Tranportation.xts, foo=zoo(NA, order.by=seq(start(PPI.Tranportation.xts), end(PPI.Tranportation.xts),"day",drop=F)))[, 1])
PPI.Machinery.365 <- na.locf(merge(PPI.Machinery.xts, foo=zoo(NA, order.by=seq(start(PPI.Machinery.xts), end(PPI.Machinery.xts),"day",drop=F)))[, 1])

# -------------------------------------------------------------------------------------------
# LEFT JOIN ALL DATA

xts.df <- data.frame(merge(unemployment.365,MortgageRate.365, join='left') %>% 
                       merge(.,PPI.petroleum.365,join ='left') %>%
                       merge(.,PPI.Lumber.365,join ='left') %>%
                       merge(.,PPI.Tranportation.365,join ='left') %>%
                       merge(.,PPI.Machinery.365,join ='left'))

write.csv(xts.df,"C:/Users/6o8an/OneDrive/Documents/ECON452/ers/xts.csv", row.names = FALSE)

# -------------------------------------------------------------------------------------------
# Manually combined all data into daily.csv
data <- read.csv("ers/daily.csv", header = TRUE, stringsAsFactors = FALSE)
data <- data[1:4968,]

reg_all <- regsubsets(data$WILLREITIND ~ ., data = data[,2:13], method=c("forward"))
regall_coef <- names(coef(reg_all, scale="adjr2",5))[-1] #get best variables without intercept

print(paste("selected variables:",list(regall_coef)))
