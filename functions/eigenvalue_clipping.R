#----------------------
# Eigenvalue clipping :
#----------------------
# This function creates a plot of the correlation between assets. The optimization parameter fits the eigenvalues if it is set to TRUE.
# If it is not the case, Q=T/N is taken.
#
# INPUT : asset prices of stocks, optimization parameter
# OUTPUT : covariance matrix
#
# The function optim() is used to find the value that best fits the eigenvalues distribution. This helps get lambda_max which can help us
# differentiate between random noise and actual information.

eigenvalue_clipping <- function(returns, optim) {
  # Asset returns
  T_assets <- dim(returns)[1] + 1
  N <- dim(returns)[2]
  # Correlation matrix
  corr_returns <-  cor(returns, use="pairwise.complete.obs")
  eigen_computation <- eigen(corr_returns)
  eigen_values <- eigen_computation$values
  eigen_vectors <- eigen_computation$vectors
  
  # Two solutions depending on Q : optimize it or take Q=T/N (asymptotic value)
  if (optim) {
    # Optimizing Q :
    func <- function(Q){
      lambda_max <- (1+ 1/Q + 2*(1/Q)^0.5)
      lambda_min <- (1+ 1/Q - 2*(1/Q)^0.5)
      
      # Fitting only the eigenvalues below lambda_max :
      index <- eigen_values<lambda_max & eigen_values > lambda_min
      x <- eigen_values[index]
      # Empirical density :
      empirical_density <- density(eigen_values)
      density_x <- empirical_density$x
      density_y <- empirical_density$y
      empirical_density <- NULL
      # Empirical density for each eigenvalue
      for (ii in 1:N){
        closest_value_index <- which(abs(density_x-eigen_values[ii])==min(abs(density_x-eigen_values[ii])))
        empirical_density[ii] <- density_y[closest_value_index]
      }
      # Taking into account the values before lambda_min for the fit (setting them to 0 for the analytical solution):
      x <- eigen_values
      analytical_solution <- Q/(2*pi) * ((lambda_max-x)*(x-lambda_min))^0.5 / x
      analytical_solution[eigen_values < lambda_min] <- 0
      index <- eigen_values<lambda_max
      analytical_solution <- analytical_solution[index]
      # Returning the squared error that should be minimized.
      errors <-  analytical_solution - empirical_density[index]
      return(sum(errors*errors))
    }
    Q <- optim(1.5,func)
    Q <- Q$par
  }
  else {
    Q <- T_assets/N
  }
  
  # Modifying the correlation matrix :
  #-----------------------------------
  lambda_max <- (1+ 1/Q + 2*(1/Q)^0.5)
  eigen_values[eigen_values<lambda_max] <- sum(eigen_values[eigen_values<lambda_max])/sum(eigen_values<lambda_max)
  
  return(eigen_vectors %*% diag(eigen_values) %*% t(eigen_vectors))
}