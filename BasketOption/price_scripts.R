#bootstrap for Monte Carlo spread pricing
#use functions from phbs.asp.2016/BasketOption/price_functions_julian.R

#create empty matrix for storing GBM & NM prices
bootstrap.price <- matrix(nrow=100,ncol=2)    #play with the nrow
colnames(bootstrap.price) = c("gbm", "nm")

for (j in 1:nrow(bootstrap.price)) {
  i <- floor(runif(1, min=0, max=10001))  #generate RN from uniform distribution
  set.seed(i)
  bootstrap.price[j,1] <- price.basket.gbm(spot=c(100,110,120,90),sigma=0.5,rho=0.5,n.asset = 4, n=1e5)   #store GBM price
  set.seed(i)
  bootstrap.price[j,2] <- price.basket.nm(spot=c(100,110,120,90),sigma=0.5,rho=0.5,n.asset = 4, n=1e5)    #store NM price
  }

#check out summary statistics
summary(bootstrap.price[,1])
summary(bootstrap.price[,2])

#plot the bootstrap
plot(bootstrap.price[,1], type="l", col=2, lwd=3,
     ylim=c(min(bootstrap.price),max(bootstrap.price)), 
     main="Price simulation",
     ylab="Option price")
lines(bootstrap.price[,2], type="l", col=3, lwd=3)
