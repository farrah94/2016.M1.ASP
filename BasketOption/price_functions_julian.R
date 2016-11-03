##########################################
##### Basket Options Pricing Formula #####
##########################################

## Monte Carlo Pricing under the ABM
price.basket.gbm <- function( spot, sigma, rho, n.asset, t.exp=1, r=0, n ){
  cov.mat <- sigma*(diag(n.asset)+(1-rho))
  cov.chol <- t(chol(cov.mat)) 
  rn <- matrix(rnorm(n*n.asset), nrow=n.asset)
  rn.corr <- cov.chol %*% rn
  path.gbm <- spot * exp(sigma*sqrt(t.exp)*rn.corr - 0.5*(sigma^2)*t.exp)
  basket.gbm <- t(weights %*% path.gbm)
  price.gbm <- sum(exp(-r*t.exp)*pmax(basket.gbm-spot,0))/n
  return ( price.gbm )
}

## Monte Carlo Pricing under the ABM
price.basket.nm <- function( spot, sigma, rho, n.asset, t.exp=1, r=0, n ){
  cov.mat <- sigma*(diag(n.asset)+(1-rho))
  cov.chol <- t(chol(cov.mat)) 
  rn <- matrix(rnorm(n*n.asset), nrow=n.asset)
  rn.corr <- cov.chol %*% rn
  sigma.n <- sigma.bs * spot
  path.nm <- spot + sigma.n*sqrt(t.exp)*rn.corr
  basket.nm <- t(weights %*% path.nm)
  price.nm <- sum(exp(-r*t.exp)*pmax(basket.nm-spot,0))/n
  return ( price.nm )
}


## Analytic Pricing
price.basket.bs <- function (
  type = 'call', spot, forward = spot*exp((r-div)*t.exp),
  strike = forward, t.exp = 1, r = 0, div = 0, sigma, rho){
  cov.mat <- GetCovMat( sigma = sigma, rho = rho )
  var.basket <- as.vector( t(weights) %*% cov.mat %*% weights )
  spot.bs <- t(weight) %*% spot
  price.bs <- CalcBsmPrice( type = 'call', spot = spot.bs, forward = spot *exp((r-div)*t.exp), strike = strike, t.exp = t.exp, r = r, div = div, sigma = var.basket)
  return ( price.bs )
}

#########################################
##### Control Variance option price #####
#########################################

price.cv<- price.basket.gbm +( price.basket.bs - price.basket.nm) 
