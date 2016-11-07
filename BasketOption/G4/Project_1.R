#       sigma is volatility vector
#       corr is the correlation matrix
#       spot is the spot price vector (length equals to sigma)
#       weight is the weight of portfolio(length equals to sigma)
#       k is the strike price
#       t.exp is the time to maturity

source('covfunctions.R')
CalPrice <- function(sigma, corr, r, k,spot, weight, t.exp,type="Normal"){
  #----MC prcing call option price----
  n.sample=100000
  cov.chol <- GetCovMat(sigma=sigma,t.exp=t.exp,corr.mat=corr,chol=T)
  n.asset <- length(spot)
  rn <- matrix( rnorm(n.asset*n.sample), nrow=n.asset )
  rn <-cbind(rn,rn*-1)
  rn.corr <- cov.chol %*% rn
  if (type=="Normal"){
    s.mat <- spot*exp(r*t.exp)+rn.corr
  }else if (type=="GBM"){
    s.mat <- spot*exp((r-0.5*sigma^2)*t.exp+rn.corr)
  }
  portfolio.sample <- t(weight) %*% s.mat
  price <- exp(-r*T)*mean(pmax(portfolio.sample-k,0))
  return(price)
}

CalCV <- function(sigma, corr, r, k,spot, weight, t.exp){
  #------MC prcing call option price with CV-------
  price.GBMMC <- CalPrice(sigma = sigma,corr = corr,r = r,k = k,spot = spot,t.exp = t.exp,weight = weight,type="GBM")
  price.NormalMC <- CalPrice(sigma = sigma,corr = corr,r = r,k = k,spot = spot,t.exp = t.exp,weight = weight,type="Normal")
  cov.mat <- GetCovMat(sigma=sigma,corr.mat=corr)
  portfolio.spot <- t(weight) %*% spot
  portfolio.var <- t(weight) %*%  cov.mat %*% weight
  portfolio.sigma <- sqrt(portfolio.var)
  price.NormalAnalystic <- phbsasp::CalcBsmPrice(type = 'call',spot = portfolio.spot,strike = k,t.exp = t.exp,r = r,div = 0,sigma = portfolio.sigma)
  price <- as.numeric(price.GBMMC - price.NormalMC + price.NormalAnalystic)
  return(price)
}

# example1
# sigma <- c(0.5,0.5)
# spot <- c(20,10)
# k <- 5
# corr <- matrix(c(1,0.4,0.4,1),2,2)
# weight <- c(1,-1)
# r <- 0.05
# t.exp <- 1
# CalPrice(sigma, corr, r, k,spot, weight, t.exp,type="Normal")
# CalPrice(sigma, corr, r, k,spot, weight, t.exp,type="GBM")
# CalCV (sigma, corr, r, k,spot, weight, t.exp)
