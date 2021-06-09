#------------
# PORTFOLIO :
#------------
# This function creates and backtests a portfolio according to the selected asset chosen through the correlation network
#
# INPUT : Asset prices, start date of the portfolio, duration in months and the method to compute the correlation matrix
# OUTPUT : The portfolio worth evolution
#
# The portfolio is backtested on a rolling time window of 5 years and re-weighted each months.
# Important : the price of every asset we want to invest in must be available at the inception of the portfolio.

portfolio <- function(asset_prices,start_date,duration,correlation_method){
  # Construct the porftolio :
  #--------------------------
  # Calibrate the model on a 5-years rolling period
  start_date <- as.integer(format(start_date,'%Y%m'))
  duration_months <- duration
  number_months <- 60 # rolling window to build correlation matrix with historical data
  portfolio_worth <- 100
  
  
  # Each beginning of month date : Portfolio is rebalanced at the beginning of each month
  dates <- time(asset_prices)
  beginning_of_month <- as.logical(c(1,diff(as.integer(format(dates,'%Y%m')))))
  dates_beg_month <- dates[beginning_of_month]
  
  # Start date of the portfolio
  start_date_index <- format(dates_beg_month,'%Y%m') == start_date
  start_date_index <- which.max(start_date_index)
  
  # Returns :
  returns <- data_returns(asset_prices, interpolate = TRUE, extrapolate = TRUE, dates_beg_month[start_date_index - number_months], dates_beg_month[start_date_index + duration_months])
  
  # Price of the assets at the start of the portfolio :
  x <- na.approx(asset_prices)
  price <- x[dates_beg_month[start_date_index],colnames(returns)]

  for (ii in 1:number_months) {
    # Computation of correlation matrix :
    in_sample_index <- dates_beg_month[start_date_index + ii - 61] <= time(returns) & time(returns) <= dates_beg_month[start_date_index + ii -1]
    returns_in_sample <- returns[in_sample_index,]
    
    # Correlation matrix :
    if (correlation_method =="eigenvalue_clipping") {
      correlation_matrix <- eigenvalue_clipping(returns_in_sample, optim = TRUE)
    }
    else if (correlation_method == "shrinkCorr") {
      C <- cor(returns_in_sample,use="complete.obs")
      correlation_matrix <- shrinkCorr(returns_in_sample, C)
      correlation_matrix <- correlation_matrix$delta
    }
    else {
      correlation_matrix <- cor(returns_in_sample)
    }
    
    # Portfolio weights :
    portfolio_weights <- portfolio.optim(returns_in_sample, pm = 0.01, riskless = TRUE,shorts = FALSE, rf = 0, reslow = rep(0.1,dim(returns)[2]), reshigh = rep(1.8,dim(returns)[2]),covmat = correlation_matrix)
    weights <- portfolio_weights$pw
    weights <- weights/sum(weights)

    # Computation of the evolution of the portfolio :
    out_sample_index <- dates_beg_month[start_date_index + ii - 1] <= time(returns) & time(returns) <= dates_beg_month[start_date_index + ii]-1
    returns_out_sample <- returns[out_sample_index,]
    prices_out_sample <- t(price %x% rep(1,dim(returns_out_sample)[1])) * t(exp(cumsum(returns_out_sample)))
    portfolio_worth <- c(portfolio_worth, tail(portfolio_worth,n=1)/(price %*% weights) * as.vector(weights %*% prices_out_sample))
  }
  # Returning the portfolio worth with the correct dates :
  dates_returns <- time(returns)
  index_date_returns <- dates_beg_month[start_date_index]-1 <= dates_returns
  portfolio_worth <- zoo(x = portfolio_worth, order.by = dates_returns[index_date_returns]);
  
  # Plotting the portfolio worth :
  plot(portfolio_worth, xlab = "Time", ylab = "Portfolio Worth")
  abline(v = dates_beg_month)
  
  return(portfolio_worth)
}