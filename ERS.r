#Author: ECON452 - team Charlie
#Date: Fall 2019
#Subject: ERS

# LOAD PACKAGES
library(xts)
library(zoo)
library(dplyr)
library(tseries)
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
monthly <- monthly[1:238,1:9]
monthly$date <- as.Date(monthly$date , "%Y-%m-%d")

quarterly <- read.csv("ers/quarterly.csv", header = TRUE, stringsAsFactors = FALSE)
quarterly <- quarterly[1:76,1:8]
colnames(quarterly)[1] <- "date"
quarterly$date <- as.Date(quarterly$date , "%Y-%m-%d")

# -------------------------------------------------------------------------------------------
# CONVERT W/ TS

y.ts<- ts(daily$WILLREITIND, start = c(2000,1,3), frequency = 252.75)
S5UTIL.ts <- ts(daily$S5UTIL, start = c(2000,1,3), frequency = 252.75)
S5CONS.ts <- ts(daily$S5CONS, start = c(2000,1,3), frequency = 252.75)
S5COND.ts <- ts(daily$S5COND, start = c(2000,1,3),frequency = 252.75)
Gold.ts <- ts(daily$Gold_Price, start = c(2000,1,3),frequency = 252.75)
TreasuryBill.ts <- ts(daily$Treasury_Bill_3M, start = c(2000,1,3), frequency = 252.75)

MortgageRate.ts <- ts(weekly$Mortage_Rate_30Year, start = c(1999,52), frequency = 52.15)

unemployment.ts <- ts(monthly$US_unemployment_rate, start = c(2000,1),frequency = 12)
PPI.petroleum.ts <- ts(monthly$PPI_Petroleun_and_Coal, start = c(2000,1),frequency = 12)
PPI.Lumber.ts <- ts(monthly$PPI_Lumber_Wood, start = c(2000,1),frequency = 12)
PPI.Tranportation.ts <- ts(monthly$PPI_Transportation_Equipment, start = c(2000,1),frequency = 12)
PPI.Machinery.ts <- ts(monthly$PPI_Machinery_Equipment, start = c(2000,1),frequency = 12)
Gov_Bond.ts <- ts(monthly$LongTerm_Gov_Bond_Yields10Year, start = c(2000,1), frequency = 12)
CPI.ts <- ts(monthly$CPIAUCSL, start = c(2000,1),frequency = 12)
FederalDebt.ts <- ts(monthly$Gross_Federal_Debt_Market_Value, start = c(2000,1), frequency=12)

# -------------------------------------------------------------------------------------------
# CONVERT W/ XTS
y.xts <- xts(daily$WILLREITIND,order.by = daily$date)
S5UTIL.xts <- xts(daily$S5UTIL,order.by = daily$date)
S5CONS.xts <- xts(daily$S5CONS,order.by = daily$date)
S5COND.xts <- xts(daily$S5COND,order.by = daily$date)
Gold.xts <- xts(daily$Gold_Price,order.by = daily$date)
TreasuryBill.xts <- xts(daily$Treasury_Bill_3M,order.by = daily$date) 

MortgageRate.xts <- xts(weekly$Mortage_Rate_30Year,order.by = weekly$date)

unemployment.xts <- xts(monthly$US_unemployment_rate,order.by = monthly$date)
PPI.petroleum.xts <- xts(monthly$PPI_Petroleun_and_Coal,order.by = monthly$date)
PPI.Lumber.xts <- xts(monthly$PPI_Lumber_Wood,order.by = monthly$date)
PPI.Tranportation.xts <- xts(monthly$PPI_Transportation_Equipment,order.by = monthly$date)
PPI.Machinery.xts <- xts(monthly$PPI_Machinery_Equipment,order.by = monthly$date)
Gov_Bond.xts <- xts(monthly$LongTerm_Gov_Bond_Yields10Year,order.by = monthly$date)
CPI.xts <- xts(monthly$CPIAUCSL,order.by = monthly$date)
FederalDebt.xts <- xts(monthly$Gross_Federal_Debt_Market_Value,order.by = monthly$date)

MortgageRate.365 <- na.locf(merge(MortgageRate.xts, foo=zoo(NA, order.by=seq(start(MortgageRate.xts), end(MortgageRate.xts),"day",drop=F)))[, 1])

unemployment.365 <- na.locf(merge(unemployment.xts, foo=zoo(NA, order.by=seq(start(unemployment.xts), end(unemployment.xts),"day",drop=F)))[, 1])
PPI.petroleum.365 <- na.locf(merge(PPI.petroleum.xts, foo=zoo(NA, order.by=seq(start(PPI.petroleum.xts), end(PPI.petroleum.xts),"day",drop=F)))[, 1])
PPI.Lumber.365 <- na.locf(merge(PPI.Lumber.xts, foo=zoo(NA, order.by=seq(start(PPI.Lumber.xts), end(PPI.Lumber.xts),"day",drop=F)))[, 1])
PPI.Tranportation.365 <- na.locf(merge(PPI.Tranportation.xts, foo=zoo(NA, order.by=seq(start(PPI.Tranportation.xts), end(PPI.Tranportation.xts),"day",drop=F)))[, 1])
PPI.Machinery.365 <- na.locf(merge(PPI.Machinery.xts, foo=zoo(NA, order.by=seq(start(PPI.Machinery.xts), end(PPI.Machinery.xts),"day",drop=F)))[, 1])
Gov_Bond.365 <- na.locf(merge(Gov_Bond.xts, foo=zoo(NA, order.by=seq(start(Gov_Bond.xts), end(Gov_Bond.xts),"day",drop=F)))[, 1])
CPI.365 <- na.locf(merge(CPI.xts, foo=zoo(NA, order.by=seq(start(CPI.xts), end(CPI.xts),"day",drop=F)))[, 1])
FederalDebt.365 <- na.locf(merge(FederalDebt.xts, foo=zoo(NA, order.by=seq(start(FederalDebt.xts), end(FederalDebt.xts),"day",drop=F)))[, 1])

# -------------------------------------------------------------------------------------------
# LEFT JOIN ALL DATA
# an issue went up here when data in xts format have their date as indexes but not as a column within the data table,
# I could not left join these data with daily data by 'date', so I ended up exporting the CSV, added the dates and 
# merged all variables with vlookup function in Excel, and read it in again.

xts.df <- data.frame(merge(MortgageRate.365,unemployment.365, join='left') %>% 
                       merge(.,PPI.petroleum.365,join ='left') %>%
                       merge(.,PPI.Lumber.365,join ='left') %>%
                       merge(.,PPI.Tranportation.365,join ='left') %>%
                       merge(.,PPI.Machinery.365,join ='left') %>% 
                       merge(.,Gov_Bond.365,join ='left') %>% 
                       merge(.,CPI.365,join ='left') %>% 
                       merge(.,FederalDebt.365,join ='left'))
                      

#This could be downloaded from my Github page
#write.csv(xts.df,"C:/Users/6o8an/OneDrive/Documents/ECON452/ers/xts.csv", row.names = FALSE)

# -------------------------------------------------------------------------------------------
# Manually combined all data into merged.csv
data <- read.csv("ers/merged.csv", header = TRUE, stringsAsFactors = FALSE)
data <- data[1:4968,]
colnames(data) <- c("date","Y","S5UTIL","S5CONS","S5COND","GoldPrice","TreasuryBill3m","MortgageRate30y",
                    "UnemploymentRate","PPI.Petroleum","PPI.Lumber","PPI.Tranportation","PPI.Machinery",
                    "GovernmentBond","CPI","FederalDebt")

# -------------------------------------------------------------------------------------------
# STATIONARITY OF DATA
#TsNames <- list()
#for(i in 1:ncol(data)) { 
#  variable_name <- paste(colnames(data[i]), "ts" ,sep = ".")
#  TsNames[[i]] <- variable_name
#}
#TsList = list()

## Adf test 
# S5COND had
data_no_S5COND <- data[,-5]
for(i in 2:ncol(data_no_S5COND)){
    print(colnames(data_no_S5COND[i]))
    TS = ts(data_no_S5COND[,i], start = c(2000,1),frequency = 252.75)
    print(adf.test(TS))
}

## Adf test of loged variables
for(i in 2:ncol(data_no_S5COND)){
  print(colnames(data_no_S5COND[i]))
  TS_Log = ts(log(data_no_S5COND[,i]), start = c(2000,1),frequency = 252.75)
  print(adf.test(TS_Log))
}

## Adf test of diff of log
for(i in 2:ncol(data_no_S5COND)){
  print(colnames(data_no_S5COND[i]))
  TS_LogDiff = ts(diff(log(data_no_S5COND[,i])), start = c(2000,1),frequency = 252.75)
  print(adf.test(TS_LogDiff))
}


# -------------------------------------------------------------------------------------------
