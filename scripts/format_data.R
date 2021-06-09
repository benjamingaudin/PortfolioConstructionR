#-------------------
# Loading the data :
#-------------------
# This script loads loads the data that has been provided to us, transforms it and stores it in a standard convenient way.
#
# The data for the bitcoin had to be somehow truncated as several exchange rate exist during the same timestamp.

library(zoo)
library(tseries)

# Dataset A : swiss stocks
#-------------------------
load('data/swiss_stocks.rda')

# Dataset B : US stocks
#----------------------
load('data/us_stocks.rda')
# Creating US_stocks without the S&P500
SP500 <- us_stocks[,1]
us_stocks <- us_stocks[,2:dim(us_stocks)[2]]

# Dataset C : etf.csv
#--------------------
ETF <- read.csv("data/etf.csv", header=FALSE)
size <- dim(ETF)
index <- as.character(ETF[2:size[1],1])
index <- as.Date(index, format = "%Y-%m-%d")
values <- as.matrix(ETF[2:size[1],2:size[2]])
class(values) <- "numeric"

colnames(values) <- as.character(unlist(ETF[1,2:size[2]]))
ETF <- zoo(x = values, order.by = index);

# Dataset D : Mt. Gox
#--------------------
# Getting rid of transactions occuring in the same second is necessary since they may not be differentiated.
mtgoxUSD <- read.csv("data/mtgoxUSD.csv", header=FALSE)
mtgoxUSD$V3 <- NULL

# Extracting dates and taking only the last value of the day.
dates <- as.Date(as.POSIXct(mtgoxUSD[,1], origin="1970-01-01"))
index_dates <- c(diff(dates),1)
index_dates <- index_dates != 0

mtgoxUSD <- zoo(x=mtgoxUSD[index_dates,2], order.by = dates[index_dates])

# Dataset E : Data acquired from yahoo finance
#------------
# Reading tickers
data <- read.csv("data/sp500_constituents.csv")
tickers <- as.character(data$Symbol)
tickers <- tickers[sample(1:length(tickers), 120,
                          replace=FALSE)]
rm(data)

# Getting the data : do not download the close (There is jumps due to splits of the stock price.). We use adjusted close.
time_series <- sapply(tickers, 
                     function(x) tryCatch(
                       get.hist.quote(x, start = "1900-01-01",quote = "AdjClose",  compression = "m"), 
                       error = function(e)NULL
                     )
)
US_stocks_monthly <- do.call("cbind",time_series[!sapply(time_series,is.null)])

# Saving the data :
#------------------
save(swiss_stocks,us_stocks,ETF,mtgoxUSD,US_stocks_monthly,SP500,file = "data/formatted_data.RData")
rm(list = ls())
