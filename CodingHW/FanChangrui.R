##Assignment1

spot <- 50
strike <- seq(10,100,1)
t.exp <- 1
sigma <- 0.3
r <- 0.02

price <- phbsasp::CalcBsmPrice(spot=spot, t.exp = t.exp, sigma=sigma, strike=strike, r=r) 
impvol <- phbsasp::CalcNormalImpvol(price=price, spot=spot, strike=strike, t.exp=t.exp, r=r)
plot(strike,impvol, type="l", col="red")

d=0.001
sigma1=phbsasp::CalcNormalImpvol(price=phbsasp::CalcBsmPrice(spot=spot, t.exp = t.exp, sigma=sigma, strike=spot-d, r=r) , spot=spot, strike=spot-d, t.exp=t.exp, r=r)
sigma2=phbsasp::CalcNormalImpvol(price=phbsasp::CalcBsmPrice(spot=spot, t.exp = t.exp, sigma=sigma, strike=spot+d, r=r) , spot=spot, strike=spot+d, t.exp=t.exp, r=r)
slope <- (sigma2 - sigma1)/(2*d)

#reverse comparation
#price <- phbsasp::CalcNormalPrice(spot=spot, t.exp = t.exp, sigma=sigma, strike=strike, r=r) 
#impvol <- phbsasp::CalcBsmImpvol(price=price, spot=spot, strike=strike, t.exp=t.exp, r=r)
#plot(strike,impvol, type="l", col="blue")

##Assignment2

spot <- 50
strike <- seq(10,100,1)
t.exp <- 1
sigma <- 0.3
r <- 0.02
L <- 10

price <- phbsasp::CalcBsmPrice(spot=spot+L,t.exp=t.exp,sigma=sigma,strike=strike+L,r=r)
impvol <- phbsasp::CalcBsmImpvol(price=price,spot=spot,strike=strike,t.exp=t.exp,r=r)
plot(strike,impvol, type="l", col="red")
sigma_L=(impvol*spot)/(spot+L)
#plot(strike,sigma_L, type="l", col="green")
