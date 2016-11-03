#author Chen Zhiyun
#id     1501213406

#Assignment1
spot <- 100
strike <- seq(50,150,1)
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
# [JC] How is the slope compared with sigma?


#Assignment2
L=20
spot <- 100
strike <- seq(50,150,1)
t.exp <- 1
sigma <- 0.3
r <- 0.05

#plot BS implied volatility
# [JC] the meaning of sigma is different in a displaced BS model, so you need to solve for a new sigma
sigma_L=(sigma*spot)/(spot+L)
price <- phbsasp::CalcBsmPrice(spot=spot+L, t.exp = t.exp, sigma=sigma_L, strike=strike+L, r=r)
impvol <- phbsasp::CalcBsmImpvol(price=price, spot=spot, strike=strike, t.exp=t.exp, r=r)
plot(strike,impvol, type="l", col="blue",ylim=c(0.28,0.53))
par(new=TRUE)
abline(h=sigma,col="black")


#calibration of sigma_L
# As is known that there are two parameters (L and sigma)
# to be decided in the DL model, which means we need to
# provied to points on the implied volatility curve deriving
# from the BS model(k1,k2,sigma1,sigma2)

library(rootSolve)
DL_calibration <- function(type='call', s=100, k,sigma, r=0.05, T=1){
  #------------------------------------------------
  #------------------------------------------------
  #Imput: type("call","put","straddle","digit")
  #       s is current price
  #       k is the strike price vector (length is 2)
  #       T is the time to maturity
  #       r is the risk-free rate
  #       sigma is the implied volatility vector(length is 2)
  #Return: the option price
  #       The calibration parameter sigmaL and L in DL model
  #------------------------------------------------
  #------------------------------------------------
  subfunction <- function(x){
    bsprice_1 <- phbsasp::CalcBsmPrice(type = type,spot=s,strike=k[1],sigma = sigma[1],r = r,t.exp = T)
    bsprice_2 <- phbsasp::CalcBsmPrice(type = type,spot=s,strike=k[2],sigma = sigma[2],r = r,t.exp = T)
    delta1 <- phbsasp::CalcBsmPrice(type = type,spot = s+x[2],strike = k[1]+x[2],sigma = x[1],r = r,t.exp = T) - bsprice_1
    delta2 <- phbsasp::CalcBsmPrice(type = type,spot = s+x[2],strike = k[2]+x[2],sigma = x[1],r = r,t.exp = T) - bsprice_2
    return(c(delta1,delta2))
  }
  solver <- multiroot(f = subfunction,start = c(0.1,1))
  root <- solver$root
  names(root) <- c('sigmaL',"L")
  return(root)
}

#example
DL_calibration(s=spot,k=c(80,90),sigma=c(0.398,0.384),r=r,T=t.exp)
