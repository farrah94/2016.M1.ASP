library(ggplot2)
library(statmod)

#Baisc seting
sigma <- c(0.3,-0.25)
spot <- c(10,8)
k <- 2
corr <- matrix(c(1,0.2,0.2,1),2,2)
weight <- c(1,-1)
r <- 0.05
t.exp <- 1
n.sample <- 100000
cov.mat <- sigma %*% t(sigma) * corr 
cov.chol <- t(chol(cov.mat))
n.asset <- length(spot)
rn <- matrix( rnorm(n.asset*n.sample), nrow=n.asset )
rn.corr <- cov.chol %*% rn

portfolio.spot <- t(weight) %*% spot
portfolio.var <- t(weight) %*%  cov.mat %*% weight
portfolio.sigma <- sqrt(portfolio.var)

s.mat <- spot*exp((r-0.5*sigma^2)*t.exp+sqrt(t.exp)*rn.corr)
s.mat.normal <- spot*exp(r*t.exp)+sqrt(t.exp)*rn.corr
portfolio.sample <- t(weight) %*% s.mat
portfolio.sample.normal <- t(weight) %*% s.mat.normal
payoffs <- pmax(portfolio.sample-k,0)
payoffs.normal <- pmax(portfolio.sample.normal-k,0)

#MC lognormal price,MC normal price, normal analystic price
price.MC <- exp(-r*T)*payoffs
price.normal <- exp(-r*T)*payoffs.normal
price.normal.Analystic <- phbsasp::CalcBsmPrice(type = 'call',spot = portfolio.spot,strike = k,t.exp = t.exp,r = r,div = 0,sigma = portfolio.sigma)
price.CV <- price.MC -  price.normal + price.normal.Analystic[1,1]

#hist of MS,CV price
par(mfrow=c(2,1))
hist(price.MC,breaks = 1000,xlim = c(0,10),probability = T,ylim = c(0,0.5))
hist(price.CV,breaks = 1000,xlim = c(0,10),probability = T,ylim = c(0,0.5))
data1 <- data.frame(price = c(price.MC,price.CV),type=rep(c("MC","CV"),c(n.sample,n.sample)))
ggplot(aes(x=price,fill=type),data=data1)+geom_density(alpha=0.5)+xlim(c(0,10))    
#normal plots with MC/CV mean and sd
par(mfrow=c(1,1))
x <- seq(-7,10,by=0.01)
norm1 <- dnorm(x,mean = mean(price.MC),sd = sd(price.MC))
norm2 <- dnorm(x,mean = mean(price.CV),sd = sd(price.CV))
data <- data.frame(x=rep(x,2),price=c(norm1,norm2),Type=rep(c("MC","CV"),c(length(x),length(x))))
ggplot(aes(x=x,y=price,color=Type),data = data) + geom_line()
   
