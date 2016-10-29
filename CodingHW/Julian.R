
#Assignment 1

spot <- 100
strike <- seq(80,125,5)
t.exp <- 1.2
sigma <- 0.2
r <- 0.05

( price <- CalcBsmPrice(spot=spot, t.exp = t.exp, sigma=sigma, strike=strike, r=r) )
( impvol <- CalcNormalImpvol(price=price, spot=spot, strike=strike, t.exp=t.exp, r=r) )

plot(price, impvol, col='blue')
plot(strike, impvol, col='green')

# slope
d <- 0.00001
( sigma1 <- ( impvol <- CalcNormalImpvol(price=price, spot=spot, strike=spot + d, t.exp=t.exp, r=r) )
( sigma2 <- ( impvol <- CalcNormalImpvol(price=price, spot=spot, strike=spot - d, t.exp=t.exp, r=r) )
( slope <- (sigma1/sigma2)/(2*d) )


#Assignment 2

L <- 1
spot <- 100 + L
strike <- seq(80 + L,125 + L,5)
t.exp <- 1.2
sigma <- 0.2
r <- 0.05

( price <- CalcBsmPrice(spot=spot, t.exp = t.exp, sigma=sigma, strike=strike, r=r) )
( impvol <- CalcBsmImpvol(price=price, spot=spot, strike=strike, t.exp=t.exp, r=r) )
plot(strike, impvol)