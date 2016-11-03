
#Assignment 1

spot <- 100
strike <- seq(80,125,5)
t.exp <- 1.2
sigma <- 0.2
r <- 0.05

( price <- phbsasp::CalcBsmPrice(spot=spot, t.exp = t.exp, sigma=sigma, strike=strike, r=r) )
( impvol <- phbsasp::CalcNormalImpvol(price=price, spot=spot, strike=strike, t.exp=t.exp, r=r) )

plot(price, impvol, col='blue') # [JC] what's the point of this?
plot(strike, impvol, col='green')

# slope
d <- 0.00001
# [JC] you need to give the price first
( price <- phbsasp::CalcBsmPrice(spot=spot, t.exp = t.exp, sigma=sigma, strike=spot + d, r=r) )
( sigma1 <- phbsasp::CalcNormalImpvol(price=price, spot=spot, strike=spot + d, t.exp=t.exp, r=r) )

( price <- phbsasp::CalcBsmPrice(spot=spot, t.exp = t.exp, sigma=sigma, strike=spot - d, r=r) )
( sigma2 <- phbsasp::CalcNormalImpvol(price=price, spot=spot, strike=spot - d, t.exp=t.exp, r=r) )
( slope <- (sigma1-sigma2)/(2*d) )
# [JC] How is the slope compared with sigma?


#Assignment 2

L <- 10 # a big bigger value
spot <- 100
strike <- seq(80,125,5)
t.exp <- 1.2
sigma <- 0.2
r <- 0.05

sigma_L=(sigma*spot)/(spot+L)
( price <- phbsasp::CalcBsmPrice(spot=spot+L, t.exp = t.exp, sigma=sigma_L, strike=strike+L, r=r) )
( impvol <- phbsasp::CalcBsmImpvol(price=price, spot=spot, strike=strike, t.exp=t.exp, r=r) )
plot(strike, impvol)
