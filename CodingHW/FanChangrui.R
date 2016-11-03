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
# [JC] How is the slope compared with sigma?

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

# [JC] You need to estimate sigma_L or exactly compute (see the solution)
sigma_L=(sigma*spot)/(spot+L)
price <- phbsasp::CalcBsmPrice(spot=spot+L,t.exp=t.exp,sigma=sigma_L,strike=strike+L,r=r)
impvol <- phbsasp::CalcBsmImpvol(price=price,spot=spot,strike=strike,t.exp=t.exp,r=r)
plot(strike,impvol, type="l", col="red")
#plot(strike,sigma_L, type="l", col="green")
