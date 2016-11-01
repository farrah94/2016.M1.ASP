####Assignment 1 : computing BS call-option prices with different strikes####
-----------------------------------------------------------------------------


  spot <- 100
  strike <- seq(80,125,5)
  t.exp <- 1.2
  sigma <- 0.2
  r <- 0.05

  price <- phbsasp::CalcBsmPrice(spot=spot, t.exp = t.exp, sigma=sigma, strike=strike, r=r)
  plot( strike, price, type="l", col ='blue' )

  impvol <- phbsasp::CalcNormalImpvol(price=price, spot=spot, strike=strike, t.exp=t.exp, r=r)
  plot(strike,impvol, type="l", col="blue")

####Assignment 2 : computing implied volatility under displaced-GBM####
-----------------------------------------------------------------------

  l=100
  spot <- 100+l
  strike <- seq(80+l,125+l,5)
  t.exp <- 1.2
  sigma <- 0.2
  r <- 0.05

  price <- phbsasp::CalcBsmPrice(spot=spot, t.exp = t.exp, sigma=sigma, strike=strike, r=r)
  plot( strike, price, type="l", col ='red' )

  price.l=price
  impvol<-phbsasp::CalcNormalImpvol(price=price.l,spot=spot,strike=strike,t.exp=t.exp,r=r)
  plot(strike,impvol, type="l", col="red")

