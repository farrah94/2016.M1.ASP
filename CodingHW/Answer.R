spot <- 100
strike <- seq(10,150,by=5)
t.exp <- 1
sigma <- 0.4
r <- 0.0

#### 1.1 Plot normal implied vol of GBM prices
price <- phbsasp::CalcBsmPrice(spot=spot, t.exp = t.exp, sigma=sigma, strike=strike, r=r)
impvol <- phbsasp::CalcNormalImpvol(price=price, spot=spot, strike=strike, t.exp=t.exp, r=r)

plot(strike,impvol, type="b", col="blue", lwd=3, pch="x", xlim=c(0,150), ylim=c(0,50))
lines(strike, strike*sigma, type="b", col="red", lwd=2, pch="x")
grid(lw=2)

#### 1.2 Estimate the ATM slope of the implied vol
eps <- 0.001
strike.slope <- spot + eps*c(-1,1)
price.slope <- phbsasp::CalcBsmPrice(spot=spot, t.exp=t.exp, sigma=sigma, strike=strike.slope, r=r)
impvol <- phbsasp::CalcNormalImpvol( price=price.slope, spot=spot, strike=strike.slope, t.exp=t.exp, r=r)
( slope <- diff(impvol)/(2*dk) ) # Slope is approximately 0.5*sigma


#### 2. BS implied vol of displaced GBM prices

# displacement = 50
disp <- 50  # displacement, L
# first get the ATM price from BS with given sigma. Then convert the sigma_L to match the ATM price
price.atm <- phbsasp::CalcBsmPrice(spot=spot, strike=spot, t.exp=t.exp, sigma=sigma, r=r)
sigma.disp <- phbsasp::CalcBsmImpvol(price=price.atm, spot=spot+disp, strike=spot+disp, t.exp=t.exp, r=r)
c(sigma.disp, sigma*spot/(spot+disp)) # two numbers should be similar

price.disp <- phbsasp::CalcBsmPrice(spot=spot+disp, strike=strike+disp, t.exp=t.exp, sigma=sigma.disp, r=r)
impvol.disp1 <- phbsasp::CalcBsmImpvol(price=price.disp, spot=spot, strike=strike, t.exp=t.exp, r=r)

# displacement = 50
disp <- 200  # displacement, L
price.atm <- phbsasp::CalcBsmPrice(spot=spot, strike=spot, t.exp=t.exp, sigma=sigma, r=r)
sigma.disp <- phbsasp::CalcBsmImpvol(price=price.atm, spot=spot+disp, strike=spot+disp, t.exp=t.exp, r=r)
c(sigma.disp, sigma*spot/(spot+disp)) # two numbers should be similar

price.disp <- phbsasp::CalcBsmPrice(spot=spot+disp, strike=strike+disp, t.exp=t.exp, sigma=sigma.disp, r=r)
impvol.disp2 <- phbsasp::CalcBsmImpvol(price=price.disp, spot=spot, strike=strike, t.exp=t.exp, r=r)

# plot the two BS impled vol
plot(strike, impvol.disp1, type="b", col="blue", lwd=3, pch="x", xlim=c(40,150), ylim=c(0,0.8))
lines(strike, impvol.disp2, type="b", col="red", lwd=3, pch="x")
grid(lw=2)
