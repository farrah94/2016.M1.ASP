Name:Kevin Lu   Number: 1601214237
Parameter set
spot <- 100
strike <- seq(150,450,2)
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
sigma_left=phbsasp::CalcNormalImpvol(price=phbsasp::CalcBsmPrice(spot=spot, t.exp = t.exp, sigma=sigma, strike=spot-dk, r=r) , spot=spot, strike=spot-dk, t.exp=t.exp, r=r)
sigma_right=phbsasp::CalcNormalImpvol(price=phbsasp::CalcBsmPrice(spot=spot, t.exp = t.exp, sigma=sigma, strike=spot+dk, r=r) , spot=spot, strike=spot+dk, t.exp=t.exp, r=r)
estimated_slope <- (sigma_right - sigma_left)/(2*dk)

#Assignment2
L=20
spot <- 100
strike <- seq(50,150,1)
t.exp <- 1
sigma <- 0.3
r <- 0.05

#plot BS implied volatility
price <- phbsasp::CalcBsmPrice(spot=spot+L, t.exp = t.exp, sigma=sigma, strike=strike+L, r=r)
impvol <- phbsasp::CalcBsmImpvol(price=price, spot=spot, strike=strike, t.exp=t.exp, r=r)
plot(strike,impvol, type="l", col="blue",ylim=c(0.28,0.53))
par(new=TRUE)
abline(h=sigma,col="black")