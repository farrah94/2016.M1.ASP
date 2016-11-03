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

  # [JC] where is the slope part?

####Assignment 2 : computing implied volatility under displaced-GBM####
-----------------------------------------------------------------------

  l=100
  spot <- 100
  strike <- seq(80,125,5)
  t.exp <- 1.2
  sigma <- 0.2
  r <- 0.05

  # [JC] the meaning of sigma is different in a displaced BS model, so you need to solve for a new sigma
  sigma_L=(sigma*spot)/(spot+l)
  price <- phbsasp::CalcBsmPrice(spot=spot+l, t.exp = t.exp, sigma=sigma_L, strike=strike+l, r=r)
  plot( strike, price, type="l", col ='red' )

  price.l=price
  impvol<-phbsasp::CalcBsmImpvol(price=price.l,spot=spot,strike=strike,t.exp=t.exp,r=r)
  plot(strike,impvol, type="l", col="red")

