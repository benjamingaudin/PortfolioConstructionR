#------------------------
#   Main script
#------------------------

# Setting up :
#-------------
# Please run the seven following lines in order to have the whole framework running smoothly
sapply(list.files(pattern="[.]R$", path="functions/", full.names=TRUE), source)
library(tseries)
library(zoo)
library(stats)
library(igraph)
library(moments)
load("data/formatted_data.RData")


# Selection of the stocks :
#--------------------------
beg_whole_sample <- as.Date("1000-01-01")
end_whole_sample <- as.Date("2016-01-01")

# Cleaning returns : deleting outliers / getting rid of stocks that don't have enough data
ETF_returns <- data_returns(ETF, interpolate = FALSE, extrapolate = FALSE, beg_whole_sample, end_whole_sample)
swiss_returns <- data_returns(swiss_stocks, interpolate = FALSE, extrapolate = FALSE, beg_whole_sample, end_whole_sample)
us_stocks_returns <- data_returns(us_stocks, interpolate = FALSE, extrapolate = FALSE, beg_whole_sample, end_whole_sample)

# First : selection with correlation networks
selected_ETF <- correlation_network(ETF_returns)
selected_swiss_stocks <- correlation_network(swiss_returns)
selected_us_stocks <- correlation_network(us_stocks_returns)

names_swiss_stocks <- colnames(swiss_returns[,selected_swiss_stocks])
names_ETF <- colnames(ETF[,selected_ETF])
names_us_stocks <- colnames(us_stocks[,selected_us_stocks])

# Using selected assets :
asset_prices <- merge(us_stocks[, names_us_stocks], swiss_stocks[,names_swiss_stocks], all = TRUE)

# Backtesting the portfolio on these assets :
#--------------------------------------------
# Specify when it starts (Month and year), its duration in months (integer), and the method for computing the correlation matrix.
# The portfolio will work only if every asset prices are defined at the start time.
start_date <- as.yearmon("2005-01")
duration <- 40

strategy_worth_eigenvalue_clipping <- portfolio(asset_prices, start_date,duration,"eigenvalue_clipping")

strategy_worth_shrinkCorr <- portfolio(asset_prices, start_date,duration,"shrinkCorr")

strategy_worth_cor <- portfolio(asset_prices, start_date,duration,"cor")

# Computing the sharp ratios :
sharp_ratio_eigenvalue_clipping <- mean(diff(log(strategy_worth_eigenvalue_clipping)))/sd(diff(log(strategy_worth_eigenvalue_clipping)))
sharp_ratio_shrinkCorr <- mean(diff(log(strategy_worth_shrinkCorr)))/sd(diff(log(strategy_worth_shrinkCorr)))
sharp_ratio_cor <- mean(diff(log(strategy_worth_cor)))/sd(diff(log(strategy_worth_cor)))

# Improvement of sharp ratio
print(100*c((sharp_ratio_eigenvalue_clipping-sharp_ratio_cor)/sharp_ratio_cor, (sharp_ratio_shrinkCorr-sharp_ratio_cor)/sharp_ratio_cor))

