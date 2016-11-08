
#example1
source("Project_1.R")
sigma <- c(0.5,0.5)
spot <- c(20,10)
k <- 5
corr <- matrix(c(1,0.4,0.4,1),2,2)
weight <- c(1,-1)
r <- 0.05
t.exp <- 1
CalPrice(sigma, corr, r, k,spot, weight, t.exp,type="Normal")
CalPrice(sigma, corr, r, k,spot, weight, t.exp,type="GBM")
CalCV (sigma, corr, r, k,spot, weight, t.exp)

CalCVtest <- function(sigma, corr, r, k,spot, weight, t.exp){
  #------MC prcing call option price with CV-------
  price.GBMMC <- CalPrice(sigma = sigma,corr = corr,r = r,k = k,spot = spot,t.exp = t.exp,weight = weight,type="GBM")
  price.NormalMC <- CalPrice(sigma = sigma,corr = corr,r = r,k = k,spot = spot,t.exp = t.exp,weight = weight,type="Normal")
  cov.mat <- GetCovMat(sigma=sigma,corr.mat=corr)
  portfolio.spot <- t(weight) %*% spot
  portfolio.var <- t(weight) %*%  cov.mat %*% weight
  portfolio.sigma <- sqrt(portfolio.var)
  price.NormalAnalystic <- phbsasp::CalcBsmPrice(type = 'call',spot = portfolio.spot,strike = k,t.exp = t.exp,r = r,div = 0,sigma = portfolio.sigma)
  price <- as.numeric(price.GBMMC - price.NormalMC + price.NormalAnalystic)
  price2<-c(price.GBMMC,price)
  return(price2)
}
x<-CalCVtest (sigma, corr, r, k,spot, weight, t.exp)
for (j in 2:1000){
  x<-rbind(x,CalCVtest(sigma, corr, r, k,spot, weight, t.exp))
}
(var(x[,1])-var(x[,2]))/var(x[,1])