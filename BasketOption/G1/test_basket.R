#Basket options

#Volatile assets
strike <- 15
type <- 'call'
spot <- c(10, 15 , 30)
t.exp <- 1.5
r <- 0
sigma <- c(0.3, 0.4, 0.5)
rho <- 0.5
seed = 0

# Less volatile assets
strike <- 15
type <- 'call'
spot <- c(10, 15, 30)
t.exp <- 1.5
r <- 0
sigma <- c(0.05, 0.10, 0.15)
rho <- 0.5



# Normal model
CalcBasket(type = type, spot = spot, strike = strike, t.exp= t.exp, sigma = sigma, rho = rho, 
           r = r, price = 1, model=0)

CalcBasket(type = type, spot = spot, strike = strike, t.exp= t.exp, sigma = sigma, rho = rho, 
           r = r, price = 2, model=0)

CalcBasket(type = type, spot = spot, strike = strike, t.exp= t.exp, sigma = sigma, rho = rho, 
           r = r, price = 3, model=0)

# GBM model
CalcBasket(type = type, spot = spot, strike = strike, t.exp= t.exp, sigma = sigma, rho = rho, 
           r = r, price = 1, model=1)

CalcBasket(type = type, spot = spot, strike = strike, t.exp= t.exp, sigma = sigma, rho = rho, 
           r = r, price = 2, model=1)

CalcBasket(type = type, spot = spot, strike = strike, t.exp= t.exp, sigma = sigma, rho = rho, 
           r = r, price = 3, model=1)


#Spread Options
# Volatile Assets
strike <- 0
type <- 'call'
spot <- c(20, 10)
t.exp <- 1.5
r <- 0
sigma <- c(0.50, 0.50)
rho <- 0.5
weights=c(1,-1)
seed = 0

# Less volatile assets
strike <- 0
type <- 'call'
spot <- c(20, 10)
t.exp <- 1.5
r <- 0
sigma <- c(0.50, 0.10)
rho <- 0.5
weights=c(1,-1)#spread option
seed = 0

# Normal model
CalcBasket(type = type, spot = spot, strike = strike, t.exp= t.exp, sigma = sigma, rho = rho, 
           r = r, price = 1, model=0,weights = weights)

CalcBasket(type = type, spot = spot, strike = strike, t.exp= t.exp, sigma = sigma, rho = rho, 
           r = r, price = 2, model=0,weights = weights)

CalcBasket(type = type, spot = spot, strike = strike, t.exp= t.exp, sigma = sigma, rho = rho, 
           r = r, price = 3, model=0,weights = weights)

# GBM model
CalcBasket(type = type, spot = spot, strike = strike, t.exp= t.exp, sigma = sigma, rho = rho, 
           r = r, price = 1, model=1,weights = weights)

CalcBasket(type = type, spot = spot, strike = strike, t.exp= t.exp, sigma = sigma, rho = rho, 
           r = r, price = 2, model=1,weights = weights)

CalcBasket(type = type, spot = spot, strike = strike, t.exp= t.exp, sigma = sigma, rho = rho, 
           r = r, price = 3, model=1,weights = weights)
