library(zoo)

#-------------------------
# DIVERSE USEFUL FUNCTIONS 
#-------------------------


#--------------------------------------------------
# This function turns prices to log returns.
# INPUT : matrix or vector of prices + frequency
# OUTPUT : log returns
#--------------------------------------------------

price2lret <- function(asset_prices, freq="day",interpolate=TRUE){ 
  ## Converts prices into log-returns
  ## Validate input
  if (!is.element(freq, c("day", "week", "month", "year"))) { 
    stop("Invalid 'freq' argument!")
  }
  prices<-asset_prices
  if (interpolate) {
     prices<- na.approx(prices)
  }
  
  ## Compute returns
    if(freq == "week") {
      ret <- diff(log(prices), lag=7)
    }else if(freq == "month"){
      ret <- diff(log(prices), lag=30)
    }else if(freq == "year"){
      ret <- diff(log(prices), lag=365)
    }else {
      ret <- diff(log(prices))
    }
  
  return(ret)
}

#--------------------------------------------------
# This function sets outliers to NA. 
# INPUT : logreturns + their frequency
# OUTPUT : log returns with outliers set to NA
#--------------------------------------------------
no.outliers<-function(lreturn, freq){
  
  if (!is.element(freq, c("day", "week", "month", "year"))) { 
    stop("Invalid 'ret.type' argument!")
  }
  
  #remove outliers (threshold chosen arbitrarily)
  if(freq == "week") {
    threshold<-3
  }else if(freq == "month"){
    threshold<-4
  }else if(freq == "year"){
    threshold<-6
  }else {
    threshold<-2
  }
  lret<-lreturn
  
  outliers<-which(abs(lret) > threshold, arr.ind=T)
  lret[outliers]<-NA;
  
  
  
  return(lret)
}

##################################################################
# Remove stocks where data miss (NA) for 75% of the time.
##################################################################

#--------------------------------------------------
# This function remove stocks where data miss (NA) for 75% of the time.
# INPUT : prices or returns + desired threshold
# OUTPUT : matrix of returns (prices) without the stocks with scarce data.
#--------------------------------------------------

enough.data<-function(x,na.threshold=.75){
  
  #remove stocks with not enough data
  remove.index<-c()
  nr<-nrow(x)
  for(i in 1:ncol(x))
  {
    s<-sum(is.na(x[,i]))
    if(s/nr > na.threshold){remove.index <- c(remove.index, i)}
  }
  
  y<-x[,-remove.index];
  
  
  return(y)
}
