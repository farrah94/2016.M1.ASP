## Basket Presentation

library(ggplot2)
library(statmod)
library(phbsasp)

t.exp <- 5
rho <- 0.5
strike <- 100
spot <- 100
sigma <- 0.4
n.asset <- 4
n.sample <- 1e5
r=0
weights = rep(1/n.asset, n.asset)

# functions             
GetCovMat <- function( sigma, t.exp = t.exp, rho = NA, corr.mat = NA, chol = F ){
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

price.basket.gbm <- function( spot, sigma, rho, n.asset, t.exp=1, r=0, n.sample, weights = rep(1/n.asset, n.asset) ){
  if(length(sigma)!=n.asset){
    sigma <- rep(sigma,n.asset)
  }  
  cov.chol <- GetCovMat(sigma=sigma, rho=rho, t.exp=t.exp, chol=T)
  rn1 <- matrix(rnorm(n.sample*n.asset), nrow=n.asset)
  rn <- cbind(rn1, -rn1)
  rn.corr <- cov.chol %*% rn
  path.gbm <- spot * exp(rn.corr - 0.5*(sigma^2)*t.exp)
  basket.gbm <- t(weights %*% path.gbm)
  price.gbm <- sum(exp(-r*t.exp)*pmax(basket.gbm-spot,0))/(2*n.sample)
  return ( price.gbm )
}

price.basket.nm <- function( spot, sigma, rho, n.asset, t.exp=1, r=0, n.sample, weights = rep(1/n.asset, n.asset) ){
  if(length(sigma)!=n.asset){
    sigma <- rep(sigma,n.asset)
  }
  sigma.n <- sigma * spot
  cov.chol <- GetCovMat(sigma=sigma.n, rho=rho, t.exp=t.exp, chol=T)
  rn1 <- matrix(rnorm(n.sample*n.asset), nrow=n.asset)
  rn <- cbind(rn1, -rn1)
  rn.corr <- cov.chol %*% rn
  path.nm <- spot + rn.corr
  basket.nm <- t(weights %*% path.nm)
  price.nm <- sum(exp(-r*t.exp)*pmax(basket.nm-spot,0))/(2*n.sample)
  return ( price.nm )
}
price.basket.bs <- function (
  type = 'call', spot, forward = spot*exp((r-div)*t.exp),
  strike = forward, t.exp = 1, r = 0, div = 0, sigma, rho,
  n.asset,  weights = rep(1/n.asset, n.asset)){  
  if(length(sigma)!=n.asset){
    sigma <- rep(sigma,n.asset)
  }
  if(length(spot)!=n.asset){
    spot <- rep(spot,n.asset)
  }  
  cov.mat <- GetCovMat(sigma=sigma, rho=rho, t.exp=t.exp, chol=F)
  var.basket <- as.vector( t(weights) %*% cov.mat %*% weights )
  sd.basket <- sqrt(var.basket/t.exp)
  spot.bs <- as.vector(t(weights) %*% spot)
  price.bs <- phbsasp::CalcBsmPrice( type = 'call', spot = spot.bs, forward = forward, strike = strike, t.exp = t.exp, r = r, div = div, sigma = sd.basket)
  return ( price.bs[1] )
}

# get prices
set.seed(4)
( p.gbm  <- price.basket.gbm(spot=spot,sigma=sigma,rho=rho,n.asset=n.asset, n.sample=n.sample, t.exp=t.exp) )
set.seed(4)
( p.nm <- price.basket.nm(spot=spot,sigma=sigma,rho=rho,n.asset=n.asset, n.sample=n.sample, t.exp=t.exp) )
( p.bs <- price.basket.bs(spot=spot,sigma=sigma,rho=rho,n.asset=n.asset, t.exp=t.exp) )
( p.cv <- p.gbm + (p.bs - p.nm) )

# generate price paths for plots
if(length(sigma)!=n.asset){
  sigma <- rep(sigma,n.asset)
}
cov.chol <- GetCovMat(sigma=sigma, rho=rho, t.exp=t.exp, chol=T)
rn1 <- matrix(rnorm(n.sample*n.asset), nrow=n.asset)
rn <- cbind(rn1, -rn1)
rn.corr <- cov.chol %*% rn
path.gbm <- spot * exp(rn.corr - 0.5*(sigma^2)*t.exp)
basket.gbm <- t(weights %*% path.gbm)
price.path.gbm <- exp(-r*t.exp)*pmax(basket.gbm-spot,0)
sigma.n <- sigma * spot
cov.chol <- GetCovMat(sigma=sigma.n, rho=rho, t.exp=t.exp, chol=T)
rn1 <- matrix(rnorm(n.sample*n.asset), nrow=n.asset)
rn <- cbind(rn1, -rn1)
rn.corr <- cov.chol %*% rn
path.nm <- spot + rn.corr
basket.nm <- t(weights %*% path.nm)
price.path.nm <- exp(-r*t.exp)*pmax(basket.nm-spot,0)
price.path.cv <- price.path.gbm + (p.bs - price.path.nm)
summary(price.path.nm)
summary(price.path.gbm)
summary(price.path.cv)

#normal plots with MC/CV mean and sd
par(mfrow=c(1,1))
x <- seq(-500,500,by=0.01)
norm1 <- dnorm(x,mean = mean(price.path.gbm),sd = sd(price.path.gbm))
norm2 <- dnorm(x,mean = mean(price.path.cv),sd = sd(price.path.cv))
data <- data.frame(x=rep(x,2),price=c(norm1,norm2),Type=rep(c("MC","CV"),c(length(x),length(x))))
ggplot(aes(x=x,y=price,color=Type),data = data) + geom_line()

