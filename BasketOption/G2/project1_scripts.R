library(phbsasp)
library(ggplot2)
source('project1_functions.R')

## spread option MC pricing
sigma = c(0.3,0.3)
corr = matrix(c(1,0.2,0.2,1),2,2)
r = 0
k = 10
spot = c(110,100)
weight = c(1,-1)
t.exp = 1

loops = 100
price.MC <- sapply(1:loops,function(i){CalcBasketGBMMC(sigma = sigma,corr = corr,r = r,k = k,spot = spot,weight = weight,t.exp = t.exp)})
price.CV <- sapply(1:loops,function(i){CalcBasketCV(sigma = sigma,corr = corr,r = r,k = k,spot = spot,weight = weight,t.exp = t.exp)})

 ##[0] Reduction of variance
hist(price.MC, col='blue',breaks = 30)
hist(price.CV,col = "red",breaks = 30,add=T)
data1 <- data.frame(price = c(price.MC,price.CV),type=rep(c("MC","CV"),c(loops,loops)))
ggplot(aes(x=price,fill=type),data=data1)+geom_density(alpha=0.5)+xlim(c(1.58,1.63))

x <- seq(1.58,1.63,by=0.0001)
norm1 <- dnorm(x,mean = mean(price.MC),sd = sd(price.MC))
norm2 <- dnorm(x,mean = mean(price.CV),sd = sd(price.CV))
data <- data.frame(x=rep(x,2),price=c(norm1,norm2),Type=rep(c("MC","CV"),c(length(x),length(x))))
ggplot(aes(x=x,y=price,color=Type),data = data) + geom_line()

1-var(price.CV)/var(price.MC)

##[1] Normal exact VS normal MC
price.Normal.MC <- sapply(1:loops,function(i){CalcBasketNormalMC(sigma = sigma*spot,corr = corr,r = r,k = k,spot = spot,weight = weight,t.exp = t.exp)})

cov.mat <- sigma %*% t(sigma) * corr 
portfolio.spot <- as.vector(t(weight) %*% spot)
portfolio.var <- as.vector(t(weight) %*%  cov.mat %*% weight)
portfolio.sigma <- sqrt(portfolio.var)
price.NormalAnalystic <- phbsasp::CalcNormalPrice(type = 'call',spot = portfolio.spot,strike = k,t.exp = t.exp,r = r,div = 0,sigma = portfolio.sigma*portfolio.spot)
qplot(x = 1:loops,y = price.Normal.MC,geom = "point")+geom_hline(yintercept = price.NormalAnalystic,color="red")

##[2] Lognormal MC VS Normal exact ATM
qplot(x = 1:loops,y = price.MC,geom = "point")+geom_hline(yintercept = price.NormalAnalystic,color="red")

##[3] Normal exact of single asset
weight_single <- c(1,0)
cov.mat <- sigma %*% t(sigma) * corr 
portfolio.spot <- as.vector(t(weight_single) %*% spot)
portfolio.var <- as.vector(t(weight_single) %*%  cov.mat %*% weight_single)
portfolio.sigma <- sqrt(portfolio.var)
(price.NormalAnalystic <- phbsasp::CalcNormalPrice(type = 'call',spot = portfolio.spot,strike = k,t.exp = t.exp,r = r,div = 0,sigma = portfolio.sigma))
(price.NormalAnalystic <- phbsasp::CalcNormalPrice(type = 'call',spot = spot[1],strike = k[1],t.exp = t.exp,r = r,div = 0,sigma = sigma[1]))
