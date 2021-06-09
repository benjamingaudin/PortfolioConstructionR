#------------------
# STYLIZED FACTS
#------------------


#-----------------------------------------------------------------------------------------------------
# There are two functions for stylized facts:
# One focuses on a single time series and checks extensively the stylized facts.
# The second one takes several assets and and checks them less extensively.
#-----------------------------------------------------------------------------------------------------


#setwd("/Users/Benji/Desktop/Big Data/project")

library(moments)
library(zoo)


source('functions/diverse_functions.R')


############################################################################################
#### Stylized Facts for one time series
#### Input: asset class (e.g. swiss_stocks) and position of the stock in the asset class
#### Output: Correlation between volatilty and returns (leverage effect).
#### Plot: main stylized facts for an asset:
####       autocorrelation of returns, volatility, absolute returns
############################################################################################


stylizedFacts_for_one_time_series<-function(asset_class, position=1,interpolate=FALSE){
  
  # get the name of the instrument
  #options(warn=-1)
  #if (asset_class==mtgoxUSD){
  #  name<-'mtgoxUSD'
  #}
  #else if(asset_class==portfolio){
  #  name<-'portfolio'
  #}
  #else{
  #  name <- as.character(colnames(asset_class)[position])
  #}
  #options(warn=0)
  
  if(is.null(dim(asset_class))){
      name<-'portfolio'
    }
    else{
      name <- as.character(colnames(asset_class)[position])
  }
  #options(warn=0)
  #name <- as.character(colnames(asset_class)[position])
  
  # get the daily logreturns
  freq <- 'day'
  if(is.null(dim(asset_class))){
    price <- asset_class
  }
  else{
    price <- asset_class[,position]
  }
  daily <- price2lret(price,freq,interpolate)
  
  
  # number of lags shown in the autocorrelation functions
  lag_max <- 20
    
  
  #---------------- Stylized Facts ----------------#
  
  #1 Absence of autocorrelations 
  title <- paste("Returns -", name) 
  autocorr <- acf(daily, lag.max=lag_max, main=title, na.action=na.pass, plot=FALSE)
   
    
  #2 Heavy tails
  #3 Asymmetry
  #4 Aggregational Gaussianity
  stats <- list()
  for (freq in c("day","week", "month", "year")) {
    ret <- price2lret(price,freq = freq,interpolate = interpolate)
    
    
    stats[[freq]] <- list(
      excess_kurtosis = kurtosis(ret,na.rm=TRUE),
      skewness = skewness(ret,na.rm=TRUE)
    )
      
  }
 
  #6 Volatility
  title <- paste("Volatility -", name) 
  volatility<-daily^2
  volatility <- acf(volatility,lag.max=lag_max, main=title, na.action=na.pass, plot=FALSE)

  
  #8 Slow decay of autocorrelation in absolute returns
  title <- paste("Absolute returns -", name) 
  abs_returns<-abs(daily)
  abs_returns<- acf(abs_returns, lag.max =lag_max, main=title, na.action=na.pass, plot=FALSE)
   

  old.par <-par( mfrow=c(2, 2), oma = c( 0, 0, 2, 0 ) , mar=c(2.15,2,1.35,.4)*2)
  
  plot(daily,xlab='year',ylab='log returns', main='Daily returns')
  plot(autocorr, main='Autocorrelation')
  plot(volatility,main='Volatility')
  plot(abs_returns,main='Absolute returns')
  
  mtext( name, outer = TRUE )
  
  par(old.par)

  
  stats_day <- c(stats$day$skewness,stats$day$excess_kurtosis)
  stats_week <- c(stats$week$skewness,stats$week$excess_kurtosis)
  stats_month <- c(stats$month$skewness,stats$month$excess_kurtosis)
  stats_year <- c(stats$year$skewness,stats$year$excess_kurtosis)
  normal<-t(matrix(as.numeric(c(stats$day,stats$week,stats$month,stats$year)),2,4))

  
  colnames(normal) <- c('excess kurtosis','skewness')
  rownames(normal) <- c('day','week','month','year')
  

  barplot(normal, beside=TRUE, legend = rownames(normal), col=c("darkblue","red","turquoise","darkgrey"),main=name)
  
  #9 leverage effect
   leverage<-cor(daily,daily^2,use="complete.obs")
    
  
  return(leverage_effect=leverage)#list(leverage_effect=leverage,normality=stats))
}





############################################################################################
#### Stylized Facts for several assets
#### Input: Matrix of prices
#### Output: Average leverage effect
#### Plot: Distribution of skewness and kurtosis for daily and yearly returns
####       p-value of the test H0=not autocorrelated for daily returns and absolute returns
############################################################################################

stylizedFacts_for_several_assets<-function(prices){
  
  # compute the skewness and the excess kurtosis for daily returns and yearly returns
  sum.stat <- list()
  for (freq in c("day", "month")) {
    i.ret <- price2lret(prices,freq = freq)
    
    sum.stat[[freq]] <- list(
      skew = apply(i.ret, 2, skewness, na.rm=TRUE), #Skewness
      kurt = apply(i.ret, 2, kurtosis, na.rm=TRUE)  #Excess Kurtosis
    ) 
    
  }
  
  # plot it
  old.par <-par( mfrow=c(2, 2), oma = c( 0, 0, 2, 0 ) , mar=c(2.15,1,1.35,.4)*2)
  
  hist(sum.stat$day$kurt,50,xlab='daily kurtosis',main='')
  hist(sum.stat$month$kurt,50,xlab='monthly kurtosis',main='')
  
  hist(sum.stat$day$skew,50,xlab='daily skewness',main='')
  hist(sum.stat$month$skew,50,xlab='monthly skewness',main='')
  
  mtext('Aggregate Gaussianity', outer = TRUE )
  
  par(old.par)
  
  
  
  daily <- price2lret(prices)
  volatility<-daily*daily
  abs_returns<-abs(daily)
  
  # compute the average leverage effect
  leverage <- 0
  for(i in dim(daily)[2]){
    l<-cor(daily[,i],(daily^2)[,i],use="complete.obs")
    leverage <- leverage + l
  }
  leverage <- leverage/dim(daily)[2]
  
  
  # check the autocorrelation of daily returns and plot the p-value
  LB1<-apply(daily,2,function(x) Box.test(x, type="Ljung-Box", lag=1))
  LB_pvalue1<-sapply(LB1, function(x){as.numeric(x[3])})
  
  # check the autocorrelation of daily absolute returns and plot the p-value
  LB2<-apply(abs_returns,2,function(x) Box.test(x, type="Ljung-Box", lag=1))
  LB_pvalue2<-sapply(LB2, function(x){as.numeric(x[3])})
  
  old.par <-par( mfrow=c(2,1), mar=c(1.15,5,1.35,.4))
  
  barplot(LB_pvalue1,xaxt='n',ylab='p-value',main='returns, H0 = not-autocorrelated')
  abline(a = 0.05, b = 0,col='red')
  
  barplot(LB_pvalue2,xaxt='n',ylab='p-value',main='absolute returns, H0 = not-autocorrelated')
  abline(a = 0.05, b = 0,col='red')
  
  mtext('Aggregate Gaussianity', outer = TRUE )
  
  par(old.par)
  
  return(leverage)
}


