####Assignment 1 : computing BS call-option prices with different strikes####
-----------------------------------------------------------------------------

  #strike price and implied volatility
  #setwd("F:\学习\研二上\随机过程\hw3\phbs.asp.2016-master\BlackScholes")
  #source("bsm_price.R")
  #source("normal_impvol.R")
  spot <- 100
  strike <- seq(30,150,1)
  t.exp <- 2
  sigma <- 0.2
  r <- 0.05

  price <- phbsasp::CalcBsmPrice(spot=spot, t.exp = t.exp, sigma=sigma, strike=strike, r=r)
  impvol <- phbsasp::CalcNormalImpvol(price=price, spot=spot, strike=strike, t.exp=t.exp, r=r)
  plot(strike,impvol, type="l", col="blue")

  #the slope of d(sigma(k))/d(k) at the money.
  delta=0.0001
  strike<-c(spot-delta,spot+delta)
  price <- phbsasp::CalcBsmPrice(spot=spot, t.exp = t.exp, sigma=sigma, strike=strike, r=r)
  impvol <- phbsasp::CalcNormalImpvol(price=price, spot=spot, strike=strike, t.exp=t.exp, r=r)
  slope<-(impvol[2]-impvol[1])/(2*delta)
  cat(slope)
  # [JC] How is the slope compared with sigma?


####Assignment 2 : computing implied volatility under displaced-GBM####
-----------------------------------------------------------------------

  #strike price and implied B-S volatility
  #setwd("F:\学习\研二上\随机过程\hw3\phbs.asp.2016-master\BlackScholes")
  #source("bsm_price.R")
  #source("bsm_impvol.R")

  #calibrate sigma_l [JC] you need to calibrate first
  l<-10
  spot <- 100
  t.exp <- 2
  sigma_bs <- 0.2
  r <- 0.05

  price<-phbsasp::CalcBsmPrice(spot=spot, t.exp = t.exp, sigma=sigma_bs, strike=spot, r=r)
  sigma_l<-phbsasp::CalcBsmImpvol(price=price,spot=spot+l,strike=spot+l,t.exp=t.exp,r=r)
  cat(sigma_l)


  l<-10
  spot <- 100+l
  strike <- seq(30,150,1)+l
  t.exp <- 2
  #sigma_l <- 0.2 #[JC] use the sigma_l calibrated from above
  r <- 0.05

  price <- phbsasp::CalcBsmPrice(spot=spot, t.exp = t.exp, sigma=sigma_l, strike=strike, r=r)
  impvol<- phbsasp::CalcBsmImpvol(price=price,spot=spot-l,strike=strike-l,t.exp=t.exp,r=r)
  plot(strike-l, impvol, type="l", col="red") # [JC] should use the original strike
  grid(lw=2)










