
#Shrinkage estimator as explained in "Honey, I shrunk the sample
#covariance matrix" by Ledoit and Wolf.
#Computation of rho-hat depends on the prior, or shrinkage target,
#which in this paper is taken to be the constant correlation model of Elton & Gruber.
#x is the t times n matrix of returns.
#By default, the covariance matrix 'sample' is the (biased) estimator x^tx/t.
#Another estimator can be supplied instead, but further checked might be required...

"shrinkCorr" <- function(x,shrink=NULL,sample=NULL)
{
  t <- nrow(x)
  n <- ncol(x)
  
  Tmat  <- function(x){
    t <- nrow(x)
    n <- ncol(x)
    a <- apply(x,2,function(x)sum(!is.na(x)))
    Tmat <- matrix(0,n,n)
    for(i in 1:n)
      for(j in 1:n) Tmat[i,j] <- min(a[i],a[j])
    return(Tmat)
  }
  T <- Tmat(x)
  
  x <- scale(x,center = TRUE,scale = FALSE)
  x[is.na(x)] <- 0
  
  #sample covariance matrix
  if(is.null(sample))
    sample <- crossprod(x)/T
  #compute the prior
  var <- diag(sample)
  sd <- sqrt(var)
  x.cor <- sample/tcrossprod(sd)
  rBar <- mean(x.cor[upper.tri(x.cor)])
  prior <- rBar*tcrossprod(sd)
  diag(prior) <- var
  
  if(is.null(shrink)) #compute shrinkage parameters
  {
    #pi-hat
    y <- x^2
    piMat <- crossprod(y)/T - 2*crossprod(x)*sample/T + sample^2
    pi <- sum(piMat) 
    
    #rho-hat
    term1 <- crossprod(x^3,x)/T
    #terms 2,3 and 4 in the original matlab code are in fact the same and given by
    term2 <- var * sample
    thetaMat <- term1-term2
    diag(thetaMat) <- 0
    rho <- sum(diag(piMat)) + rBar*sum( ((1/sd) %*% t(sd)) * thetaMat)
    
    #gamma-hat
    gamma <- sum( (sample-prior)^2 )
    
    #shrinkage constant
    kappa <- (pi - rho)/gamma;
    shrinkage <- max(0,min(1,kappa/mean(T)))
  }
  else
    shrinkage <- shrink
  return(list(cov= shrinkage*prior + (1-shrinkage)*sample, delta=shrinkage))
}