##########################################
##### Basket Options Pricing Formula #####
##########################################

#set seed explicitly for the MC simulation
set.seed(9)

## Monte Carlo Pricing under the ABM
price.basket.gbm <- function( spot, sigma, rho, n.asset, t.exp=1, r=0, n, weights = rep(1/n.asset, n.asset) ){
  cov.mat <- sigma*(diag(n.asset)+(1-rho))
  cov.chol <- t(chol(cov.mat)) 
  rn <- matrix(rnorm(n*n.asset), nrow=n.asset)
  rn.corr <- cov.chol %*% rn
  path.gbm <- spot * exp(sigma*sqrt(t.exp)*rn.corr - 0.5*(sigma^2)*t.exp)
  basket.gbm <- t(weights %*% path.gbm)
  price.gbm <- sum(exp(-r*t.exp)*pmax(basket.gbm-spot,0))/n
  return ( price.gbm )
}

set.seed(9)
## Monte Carlo Pricing under the ABM
price.basket.nm <- function( spot, sigma, rho, n.asset, t.exp=1, r=0, n, weights = rep(1/n.asset, n.asset) ){
  sigma.n <- sigma * spot #sigma.bs
  cov.mat <- sigma*(diag(n.asset)+(1-rho))
  cov.chol <- t(chol(cov.mat)) 
  rn <- matrix(rnorm(n*n.asset), nrow=n.asset)
  rn.corr <- cov.chol %*% rn
  path.nm <- spot + sigma.n*sqrt(t.exp)*rn.corr
  basket.nm <- t(weights %*% path.nm)
  price.nm <- sum(exp(-r*t.exp)*pmax(basket.nm-spot,0))/n
  return ( price.nm )
}


## Analytic Pricing
price.basket.bs <- function (
  type = 'call', spot, forward = spot*exp((r-div)*t.exp),
  strike = forward, t.exp = 1, r = 0, div = 0, sigma, rho,
  n.asset,  weights = rep(1/n.asset, n.asset)){
  cov.mat <- sigma*(diag(n.asset)+(1-rho))
  var.basket <- as.vector( t(weights) %*% cov.mat %*% weights )
  sd.basket <- sqrt(var.basket)
  spot.bs <- as.vector(t(weights) %*% spot)
  forward.bs <- as.vector(t(weights) %*% forward)
  price.bs <- CalcBsmPrice( type = 'call', spot = spot.bs, forward = forward.bs, strike = as.vector(t(weights) %*% strike), t.exp = t.exp, r = r, div = div, sigma = sd.basket)
  return ( price.bs )
}

#########################################
##### Control Variance option price #####
#########################################

#calc CV
set.seed(4)
p.gbm  <- price.basket.gbm(spot=c(100,110,120,90),sigma=0.5,rho=0.5,n.asset = 4, n=1e5)
set.seed(4)
p.nm <- price.basket.nm(spot=c(100,110,120,90),sigma=0.5,rho=0.5,n.asset = 4, n=1e5)
p.bs <- price.basket.bs(spot=c(100,110,120,90),sigma=0.5,rho=0.5,n.asset = 4)

price.cv<- p.gbm +( p.bs - p.nm) 
