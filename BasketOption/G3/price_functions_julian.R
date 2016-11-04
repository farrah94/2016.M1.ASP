##########################################
##### Basket Options Pricing Formula #####
##########################################

#make sure to load 'phbsasp' library and define 'GetCovMat' function

## Monte Carlo Pricing under the GBM
price.basket.gbm <- function( spot, sigma, rho, n.asset, t.exp=1, r=0, n, weights = rep(1/n.asset, n.asset) ){
  
#make sure the sigma is a vector of each asset's volatility
   if(length(sigma)!=n.asset){
     sigma <- rep(sigma,n.asset)
   }  
  
#Cholesky decomposition; explicit function GetCovMat
  #cov.chol   is a lower triangular matrix used for Cholesky decomposition
  cov.chol <- GetCovMat(sigma=sigma, rho=rho, t.exp=t.exp, chol=T)
  
#sanity check
#all(round(cov.chol %*% t(cov.chol), 10) == round(cov.mat, 10))

#generation of the (n)x(n.asset) correlated normal RNs using a normal distibution N(0,1)
  #rn         is a (n)x(n.asset) matrix containing random numbers; rn~N(0,1)
  rn <- matrix(rnorm(n*n.asset), nrow=n.asset)

  #rn.corr    is a (n)x(n.asset) matrix containing correlated random numbers  
  rn.corr <- cov.chol %*% rn

#path generation
  #path.gbm   is the simulated path under the GBM
  path.gbm <- spot * exp(rn.corr - 0.5*(sigma^2)*t.exp)
  
#option pricing
  basket.gbm <- t(weights %*% path.gbm)
  
#price of the option following the GBM
  price.gbm <- sum(exp(-r*t.exp)*pmax(basket.gbm-spot,0))/n
  return ( price.gbm )
}

## Monte Carlo Pricing under the NM
price.basket.nm <- function( spot, sigma, rho, n.asset, t.exp=1, r=0, n, weights = rep(1/n.asset, n.asset) ){
  
#make sure the sigma is a vector of each asset's volatility 
  if(length(sigma)!=n.asset){
    sigma <- rep(sigma,n.asset)
  }
  
  #sigma.n    is a volatility under NM
  sigma.n <- sigma * spot

#Cholesky decomposition; explicit function GetCovMat
  #cov.chol   is a lower triangular matrix used for Cholesky decomposition  
  cov.chol <- GetCovMat(sigma=sigma.n, rho=rho, t.exp=t.exp, chol=T)
  
#generation of the (n)x(n.asset) correlated normal RNs using a normal distibution N(0,1)
  #rn         is a (n)x(n.asset) matrix containing random numbers; rn~N(0,1)
  rn <- matrix(rnorm(n*n.asset), nrow=n.asset)
  
  #rn.corr    is a (n)x(n.asset) matrix containing correlated random numbers
  rn.corr <- cov.chol %*% rn
  
#path generation
  #path.nm   is the simulated path under the NM  
  path.nm <- spot + rn.corr
  
#option pricing  
  basket.nm <- t(weights %*% path.nm)
  
#price of the option following the GBM
  price.nm <- sum(exp(-r*t.exp)*pmax(basket.nm-spot,0))/n
  return ( price.nm )
}

## Analytic Pricing - BS
price.basket.bs <- function (
  type = 'call', spot, forward = spot*exp((r-div)*t.exp),
  strike = forward, t.exp = 1, r = 0, div = 0, sigma, rho,
  n.asset,  weights = rep(1/n.asset, n.asset)){

#make sure the sigma is a vector of each asset's volatility   
  if(length(sigma)!=n.asset){
    sigma <- rep(sigma,n.asset)
  }
#make sure the spot price is a vector of length(n.asset)
  if(length(spot)!=n.asset){
    spot <- rep(spot,n.asset)
  }  

#Covariance matrix; explicit function GetCovMat
  #cov.mat    is a covariance matrix     
  cov.mat <- GetCovMat(sigma=sigma, rho=rho, t.exp=t.exp, chol=F)

#option pricing
  
  #var.basket is an option variance
  var.basket <- as.vector( t(weights) %*% cov.mat %*% weights )
  
  #sd.basket  is an option SD; sigma passed to the pricing function
  sd.basket <- sqrt(var.basket/t.exp)

  #spot.bs    is spot price under the BS
  spot.bs <- as.vector(t(weights) %*% spot)

#price of the option under the BS
  price.bs <- phbsasp::CalcBsmPrice( type = 'call', spot = spot.bs, forward = forward, strike = strike, t.exp = t.exp, r = r, div = div, sigma = sd.basket)
  return ( price.bs[1] )
}

#########################################
##### Control Variance option price #####
#########################################

#calc CV
set.seed(4)
p.gbm  <- price.basket.gbm(spot=100,sigma=0.4,rho=0.5,n.asset = 4, n=1e5, t.exp=1)
set.seed(4)
p.nm <- price.basket.nm(spot=100,sigma=0.4,rho=0.5,n.asset = 4, n=1e5, t.exp=1)
p.bs <- price.basket.bs(spot=100,sigma=0.4,rho=0.5,n.asset = 4, t.exp=1)

price.cv <- p.gbm + (p.bs - p.nm) 
