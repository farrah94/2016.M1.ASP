numPrices=1e3
strike <- 15
type <- 'call'
spot <- c(10, 15 , 30)
t.exp <- 1
r <- 0
sigma <- c(0.3, 0.4, 0.5)
rho <- 0.5


price.MC.nm <- vector(mode = "numeric", length = numPrices)
price.MC.gbm <- vector(mode = "numeric", length = numPrices)
price.cv <- vector(mode = "numeric", length = numPrices)

for(i in 1:numPrices) {
    seed <- i
    price.MC.nm[i] <- CalcBasket(type = type, spot = spot, strike = strike, t.exp= t.exp, sigma = sigma, rho = rho, 
                                 r = r, price = 1, model=0)
    price.MC.gbm[i] <- CalcBasket(type = type, spot = spot, strike = strike, t.exp= t.exp, sigma = sigma, rho = rho, 
                                  r = r, price = 1, model=1)
    price.cv[i] <- CalcBasket(type = type, spot = spot, strike = strike, t.exp= t.exp, sigma = sigma, rho = rho, 
                              r = r, price = 3, model=0)
  
} 
sd.MC.nm <- sd(price.MC.nm)
sd.MC.gbm <- sd(price.MC.gbm)
sd.cv <- sd(price.cv)


hist(price.MC.nm, col='blue', breaks = 25)
hist(price.MC.gbm, col='blue', breaks = 25)
hist(price.cv, col='red', breaks = 25,add=T)
