#' Computes covariance matrix from correlation matrix, sigma and time to expiry
#' Instead of full correlation matrix, a schalar rho can be used.
#' 
#' @examples 
#' sigma <- c(0.2, 0.1, 0.3, 0.2)
#' GetCovMat( sigma, rho = 0.5 )
#' 
#' corr.mat <- matrix( c(1, 0.35, 0.1, 0.27, 0.04, 0.17, 0.71,
#' 0.35, 1, 0.39, 0.27, 0.5, -0.08, 0.15,
#' 0.1, 0.39, 1, 0.53, 0.7, -0.23, 0.09,
#' 0.27, 0.27, 0.53, 1, 0.46, -0.22, 0.32,
#' 0.04, 0.5, 0.7, 0.46, 1, -0.29, 0.13,
#' 0.17, -0.08, -0.23, -0.22, -0.29, 1, -0.03,
#' 0.71, 0.15, 0.09, 0.32, 0.13, -0.03, 1), 7)
#' sigma <- c(11.55, 20.68, 14.53, 17.99, 15.59, 14.62, 15.68)/100
#' GetCovMat( sigma, corr.mat = corr.mat )

GetCovMat <- function( sigma, t.exp = 1, rho = NA, corr.mat = NA, chol = F ){
  
  n.asset <- length(sigma)
  
  if(is.numeric(corr.mat)) {
    stopifnot( n.asset == ncol(corr.mat), n.asset == nrow(corr.mat) )
  } else {
    corr.mat <- diag(n.asset)*(1-rho) + rho
  }
  
  cov.mat <- ( sigma %o% sigma ) * corr.mat * t.exp
  
  if( chol ) {
    chol.mat <- t(chol(cov.mat))
    return( chol.mat )
  } else {
    return( cov.mat )
  }
}