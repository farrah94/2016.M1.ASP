#HW Assignment 1&2
#Sylwester Fronczak
#version control: https://github.com/sylwesterf/scripts/commits/master/assignment.R

#parameter sets
spot <- 100
strike <- seq(80,125,5)
t.exp <- 1.2
sigma <- 0.2
r <- 0.05
#parameter for displaced model
L <- 1.5
spotD <- spot+L
strikeD <- strike+L

#load required libraries
#library(devtools)
#source_url('https://github.com/jaehyukchoi/phbs.asp.2016/blob/master/BlackScholes/bsm_price.R')
#source_url('https://github.com/jaehyukchoi/phbs.asp.2016/blob/master/BlackScholes/normal_impvol.R')
#source_url('https://github.com/jaehyukchoi/phbs.asp.2016/blob/master/BlackScholes/bsm_impvol.R')
  #or
#source('bsm_price.R')
#source('normal_impvol.R')
#source('bsm_impvol.R')

#Assignment 1
price <- phbsasp::CalcBsmPrice(spot=spot, t.exp = t.exp, sigma=sigma, strike=strike, r=r)
impvol <- phbsasp::CalcNormalImpvol(price=price, spot=spot, strike=strike, t.exp=t.exp, r=r)

plot( price, impvol,
      type="b", xlab = "price", ylab = "implied volatility",
      col="blue", lwd = 3,
      main = paste("normal implied volatility of BS prices"))
lines(impvol, col="red", lwd=3) # [SF] changed the plot
slope <- lm(impvol ~ price)$coefficients[2] # [JC] This is pretty nice, but how is the slope compared to sigma
cat("Slope:    ", slope, "\nSigma BS: ", sigma)  # [SF] comparison

#Assignment 2
# [JC] the meaning of sigma is different in a displaced BS model, so you need to solve for a new sigma
sigmaD <- (sigma*spot)/spotD
priceD <- phbsasp::CalcBsmPrice(spot=spotD, t.exp = t.exp, sigma=sigmaD, strike=strikeD, r=r) # [SF] new sigma for CalcBsmPrice; that's it?
impvol <- phbsasp::CalcBsmImpvol(price=priceD, spot=spot, strike=strike, t.exp=t.exp, r=r)

plot( price, impvol,
       type="b", xlab = "price", ylab = "implied volatility (displaced)",
       col="blue", lwd = 3,
       main = paste("implied volatility of displaced BS prices"))

sigmaD <- impvol*(spot/spotD) # [JC] I wanted this earlier

plot(rep(sigma,length(sigmaD)),
     col="red", type="b")
lines(sigmaD, col="blue")
