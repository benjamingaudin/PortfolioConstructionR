#----------------
# DATA_CLEANING :
#----------------
# This function takes out assets for which not enough data is available for correlation computation.
# It also removes outliers by winsorizing.
# Extrapolation and interpolation is available : particularly useful for computing the portfolio returns.
#
# INPUT : asset prices of stocks
# OUTPUT : returns of selected assets
#
# Make sure the Zoo library is loaded
#
# Criteria for stock selection :
#   - At least 30% of the whole sample must be available
#   - Winsorizing the top 0.5 % and the lowest 0.5%
# Interpolate and extrapolate :
#   - Interpolate linearly interpolates prices.
#   - Extrapolate : additional data for returns is generated with same variance, same mean and a t-student distribution.

library(zoo)

data_returns <- function(x,interpolate,extrapolate, beg, end){
  # Interpolation : helps to get smooth returns
  if (interpolate) {
    x <- na.approx(x)
  }
  # Taking only the data in the given interval :
  dates <- time(x)
  index_dates <- dates > beg & dates < end
  x <- x[index_dates,]
  # Returns :
  returns<-diff(log(x))
  #ones<-rep(1,dim(x_)[2])
  #ones<-t(ones)
  #d<-diff(time(x))/365
  #returns <- x_/kronecker(d,ones)
  number_assets <- ncol(returns)
  number_observations <- nrow(returns)
  
  # Stock selection :
  #------------------
  threshold <- 0.3 # Stocks that have less than 30% of data to compute correlation are not selected
  returns_isNA <- is.na(returns)
  keep_asset <- colSums(!returns_isNA) >  threshold * number_observations
  returns <- returns[,keep_asset]
  number_assets <- ncol(returns)
  
  # Outliers selection :
  #---------------------
  tolerance <- 0.01
  for (ii in 1:number_assets) {
    # Winsorize the data :
    top_quantile <- quantile(returns[,ii], 1-tolerance/2, na.rm = TRUE)
    low_quantile <- quantile(returns[,ii], tolerance/2, na.rm = TRUE)
    index_top_quantile <- returns[,ii] > top_quantile
    index_low_quantile <- returns[,ii] < low_quantile
    returns[index_top_quantile,ii] <- top_quantile
    returns[index_low_quantile,ii] <- low_quantile
  }
  
  # Extrapolating returns :
  #------------------------
  if (extrapolate){
    # Simulate for each asset a fitted t-student distribution :
    for (ii in 1:number_assets){
      index <- is.na(returns[,ii])
      # Fit the distribution on existing returns :
      variance <- var(returns[!index,ii])
      mean_returns <- mean(returns[,ii], na.rm = TRUE)
      returns[index,ii] <- rt(sum(index), 3, 0) * (3-2)/2 * 2 * variance^0.5 + mean_returns
    }
  }
  
  # Returning cleaned returns for the good assets :
  return(returns)
}