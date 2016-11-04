#bootstrap for Monte Carlo spread pricing
#use functions from phbs.asp.2016/BasketOption/price_functions_julian.R

#create empty matrix for storing GBM & NM prices
bootstrap.price <- matrix(nrow=100,ncol=3)    #play with the nrow
colnames(bootstrap.price) = c("gbm", "nm", "cv")

#get BS price
price.bs <- price.basket.bs(spot=100,sigma=0.4,rho=0.5,n.asset = 4)

for (j in 1:nrow(bootstrap.price)) {
  i <- floor(runif(1, min=0, max=10001))  #generate RN from uniform distribution
  set.seed(i)
  bootstrap.price[j,1] <- price.basket.gbm(spot=100,sigma=0.4,rho=0.5,n.asset = 4, n=1e5)   #store GBM price
  set.seed(i)
  bootstrap.price[j,2] <- price.basket.nm(spot=100,sigma=0.4,rho=0.5,n.asset = 4, n=1e5)    #store NM price
  bootstrap.price[j,3] <- bootstrap.price[j,1] +( price.bs - bootstrap.price[j,2]) 
}

#check out summary statistics
summary(bootstrap.price[,1])
summary(bootstrap.price[,2])
summary(bootstrap.price[,3])

#density plots
plot(density(bootstrap.price[,1]), 
     xlim=c(min(bootstrap.price[,1:2]),max(bootstrap.price[,1:2])),
     ylim=c(0,10),
     main="Density plots")
lines(density(bootstrap.price[,2]))

#plot the bootstrap
plot(bootstrap.price[,1], type="l", col=2, lwd=2,
     ylim=c(min(bootstrap.price[,1:2]),max(bootstrap.price[,1:2])), 
     main="Price simulation",
     ylab="Option price")
lines(bootstrap.price[,2], type="l", col=3, lwd=2)

plot(density(bootstrap.price[,3]),
     main="Density plot for CV")


#test!
#kirk pricing model
price.kirk <- function(spot1, spot2, sigma1, sigma2, strike,
                       t.exp=1, rho=0.5, r=0, corr.kirk){
  
#inputs
  #spot1 - spot price on contract 1,
  #spot2 - spot price on contract 2,
  #sigma1 - volatility 1,
  #sigma2 - volatility 2,
  #strike - strike price
  #t.exp - time to expiry
  #r - risk-free rate
  #rho - sensitivity of an option
  #corr.kirk - correlation matrix between the two futures contracts   
  
  #Greek; beta
  beta <- (sigma2*spot2)/(spot2+strike)
  
  #portfolio sigma
  sigma.kirk <- sqrt( sigma2^2 + beta^2 -
                        (2*corr.kirk*sigma1*beta))
  
  #d <- log(spot1/(spot2+strike))/(sigma.kirk*sqrt(t.exp))
  d1 <- (log(spot1/(spot2+strike))+(t.exp*0.5*(sigma.kirk)^2))/
    (sigma.kirk*sqrt(t.exp))
  d2 <- d1 - sigma.kirk*sqrt(t.exp)
  
  call.kirk <- exp(-r*t.exp)*(spot1*dnorm(d1) - ((spot2+strike)*dnorm(d2)))
  #put.kirk <- call.kirk + exp(-r*t.exp)*(spot2+strike-spot1)
  
  return(call.kirk)
}
  
#check result
price.kirk(spot1=100, spot2=100,
           sigma1=0.5, sigma2=0.5, 
           t.exp=1, rho=0.5, r=0,
           strike=100, corr.kirk=0.5)
