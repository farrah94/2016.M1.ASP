n.asset <- 4
rho <- 0.5
sigma <- 5    # sigma*sqrt(T) in case T != 1

( cov.mat <- sigma*( diag(n.asset) + (1-rho) ) )

#### Be careful: Cholesky decomposition in R is upper triangular part
cov.chol <- chol(cov.mat)
cov.chol %*% t(cov.chol)  # Not equal to the original cov.mat

#### So make sure to transpose the result
cov.chol <- t(chol(cov.mat)) 
cov.chol %*% t(cov.chol)  # Same as the original cov.mat

# Generation of the correlated normal RNs
n.sample <- 1e5
rn <- matrix( rnorm(n.asset*n.sample), nrow=n.asset )
dim(rn)

rn.corr <- cov.chol %*% rn

# The covariance of the generated samples
( rn.corr %*%t(rn.corr) ) / n.sample

# ATM option pricing
weights <- rep(1/n.asset, n.asset)
price.basket <- as.vector( weights %*% rn.corr )

## Price under MC
( price.mc <- sum( pmax(price.basket,0) )/n.sample )

## Analytic Pricing
var.basket <- as.vector( t(weights) %*% cov.mat %*% weights )
price.bs <- sqrt(var.basket) * sqrt(1/2/pi)

price.mc - price.bs
