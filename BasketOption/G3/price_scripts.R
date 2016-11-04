#bootstrap for Monte Carlo spread pricing
#use functions from phbs.asp.2016/BasketOption/price_functions_julian.R

#create empty matrix for storing GBM & NM prices
bootstrap.price <- matrix(nrow=100,ncol=3)    #play with the nrow
colnames(bootstrap.price) = c("gbm", "nm", "cv")

#get BS price
price.bs <- price.basket.bs(spot=c(100,110,120,90),sigma=0.5,rho=0.5,n.asset = 4)

for (j in 1:nrow(bootstrap.price)) {
  i <- floor(runif(1, min=0, max=10001))  #generate RN from uniform distribution
  set.seed(i)
  bootstrap.price[j,1] <- price.basket.gbm(spot=c(100,110,120,90),sigma=0.5,rho=0.5,n.asset = 4, n=1e5)   #store GBM price
  set.seed(i)
  bootstrap.price[j,2] <- price.basket.nm(spot=c(100,110,120,90),sigma=0.5,rho=0.5,n.asset = 4, n=1e5)    #store NM price
  bootstrap.price[j,3] <- bootstrap.price[j,1] +( price.bs - bootstrap.price[j,2]) 
}

#check out summary statistics
summary(bootstrap.price[,1])
summary(bootstrap.price[,2])
summary(bootstrap.price[,3])

#plot the bootstrap
plot(bootstrap.price[,1], type="l", col=2, lwd=3,
     ylim=c(min(bootstrap.price,price.bs),max(bootstrap.price,price.bs)), 
     main="Price simulation",
     ylab="Option price")
lines(bootstrap.price[,2], type="l", col=3, lwd=3)
lines(rep(price.bs, nrow(bootstrap.price)), col=5, lwd=3)

#density plots
plot(density(bootstrap.price[,1]), 
     xlim=c(min(bootstrap.price,price.bs),max(bootstrap.price,price.bs)),
     main="Density plots")
lines(density(bootstrap.price[,2]))

plot(density(bootstrap.price[,3]),
     main="Density plot for CV")

#kirk approx

# # #  
#input

#spot1 - spot price on contract 1,
#spot2 - spot price on contract 2,
#sigma1 - volatility 1,
#sigma2 - volatility 2,
#corr.kirk - correlation between the two contracts
#strike - strike price of an option

beta <- (sigma2*spot2)/(spot2+strike)
sigma.kirk <- sqrt( sigma2^2 + beta^2 -
                    (2*corr.kirk*sigma1*beta))

# d <- log(spot1/(spot2+strike))/(sigma.kirk*sqrt(t.exp))
d1 <- (log(spot1/(spot2+strike))+(t.exp*0.5*(sigma.kirk)^2))/
        (sigma.kirk*sqrt(t.exp))
d2 <- d1 - sigma.kirk*sqrt(t.exp)

call.kirk <- exp(-r*t.exp)*(spot1*dnorm(d1) - ((spot2+strike)*dnorm(d2))) 
put.kirk <- call.kirk + exp(-r*t.exp)*(spot2+strike-spot1)
