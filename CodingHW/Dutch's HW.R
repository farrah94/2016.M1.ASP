#Name:Dutch Li   Number:1601214216
#Parameter set
spot <- 100
strike <- seq(60,140,1)
t.exp <- 1
sigma <- 0.4
r <- 0.05
#1.1 plot normal implied volatility
price <- phbsasp::CalcBsmPrice(spot=spot, t.exp = t.exp, sigma=sigma, strike=strike, r=r)
impvol <- phbsasp::CalcNormalImpvol(price=price, spot=spot, strike=strike, t.exp=t.exp, r=r)
impvol[impvol==0]<-NaN
plot(strike,impvol, type="l", col="blue")
#1.2 Estimate the ATM slope
dk=0.001
sigma_left=CalcNormalImpvol(price=phbsasp::CalcBsmPrice(spot=spot, t.exp = t.exp, sigma=sigma, strike=spot-dk, r=r) , spot=spot, strike=spot-dk, t.exp=t.exp, r=r)
sigma_right=CalcNormalImpvol(price=phbsasp::CalcBsmPrice(spot=spot, t.exp = t.exp, sigma=sigma, strike=spot+dk, r=r) , spot=spot, strike=spot+dk, t.exp=t.exp, r=r)
estimated_slope <- (sigma_right - sigma_left)/(2*dk)
# [JC] How is the slope compared with sigma?

#Assignment2
L=20
spot <- 100
strike <- seq(80,120,1)
t.exp <- 1
sigma <- 0.3
r <- 0.05

# [JC] the meaning of sigma is different in a displaced BS model, so you need to solve for a new sigma
sigma_L=(sigma*spot)/(spot+L)

#plot BS implied volatility
price <- phbsasp::CalcBsmPrice(spot=spot+L, t.exp = t.exp, sigma=sigma, strike=strike+L, r=r)
impvol <- phbsasp::CalcBsmImpvol(price=price, spot=spot, strike=strike, t.exp=t.exp, r=r)
plot(strike,impvol, type="l", col="blue",ylim=c(0.25,0.50))
par(new=TRUE)
abline(h=sigma,col="black")
