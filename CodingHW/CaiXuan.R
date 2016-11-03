# encode:  utf-8
# title:   ASP homework
# author:  Cai Xuan
# date:    2016-10-29

# Description: Based on the Micheal's work, I rewrite his code by using professor's package


#---------------------Assignment1---------------------
k <-seq(0,1.2,by=0.01)
price.bs <- phbsasp::CalcBsmPrice(type='call',spot=1,strike=k,t.exp=1,r=0.05,sigma=0.4)
plot(k,price.bs,"l")
imvol.normal <- vector(length=length(k))
for(i in 1:length(k)){
  try <- try(phbsasp::CalcNormalImpvol(type='call',price=price.bs[i],spot=1,strike=k[i],r=0.05,t.exp=1),silent=T)
  if(class(try)=="try-error"){
    imvol.normal[i]=(NA)
  }else{
    imvol.normal[i]=(try)
  }
}

#------------------------plot 1-----------------------
plot(k,imvol.normal,"l",main="Normal implied volatility",ylab = "Implied volatility")


#---------------Estimate the ATM slope----------------
dk=0.0001
sigma_upper <- phbsasp::CalcNormalImpvol(type = 'call',price = phbsasp::CalcBsmPrice(type = 'call',t.exp= 1,spot = 1,strike= 1+dk,sigma = 0.3,r = 0.05),spot = 1,strike = 1+dk,r = 0.05,t.exp= 1)
sigma_lower <- phbsasp::CalcNormalImpvol(type = 'call',price = phbsasp::CalcBsmPrice(type = 'call',t.exp= 1,spot = 1,strike= 1-dk,sigma = 0.3,r = 0.05),spot = 1,strike = 1-dk,r = 0.05,t.exp = 1)
estimated_slope <- (sigma_upper - sigma_lower)/(2*dk)

# [JC] How is the slope compared with sigma?


#---------------------Assignment2---------------------

#----------------Displaced BS price -----------------
bs.displaced <- function(type, s, k, T, r, sigma, L){
  price <- phbsasp::CalcBsmPrice(type=type,spot=s+L,strike=k+L,t.exp=T,r=r,sigma=sigma)
  return(price)
}

k <- seq(60,150,by=0.1)
price_positiveL <- bs.displaced(type = 'call',s = 100,k = k,r = 0.05,sigma = 0.3,T = 1,L = 1)
price_negativeL <- bs.displaced(type = 'call',s = 100,k = k,r = 0.05,sigma = 0.3,T = 1,L = -1)
imvol.positiveL <- vector(length=length(k))
imvol.negativeL <- vector(length=length(k))
# [JC] the meaning of sigma is different in a displaced BS model, so you need to solve for a new sigma



for(i in 1:length(k)){
  try <- try(phbsasp::CalcBsmImpvol(type = 'call',price = price_positiveL[i],spot=100,strike=k[i],r = 0.05,t.exp= 1),silent=T)
  if(class(try)=="try-error"){
    imvol.positiveL[i]=(NA)
  }else{
    imvol.positiveL[i]=(try)
  }
}

for(i in 1:length(k)){
  try <- try(phbsasp::CalcBsmImpvol(type = 'call',price = price_negativeL[i],spot=100,strike=k[i],r = 0.05,t.exp = 1),silent=T)
  if(class(try)=="try-error"){
    imvol.negativeL[i]=(NA)
  }else{
    imvol.negativeL[i]=(try)
  }
}

#------------------------plot 2-----------------------
plot(k,imvol.positiveL,"l",main = "Implied volatility from displaced BS",ylab="Implied volatility",ylim = c(0.285,0.315))
lines(k,imvol.negativeL,col='red')
text(x = c(70,70),y = c(0.295,0.310),labels = c('L=-1','L=1'))


#---------------Calibration of DL BS model-------------
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
  solver <- multiroot(f = subfunction,start = c(0.3,1))
  root <- solver$root
  names(root) <- c('sigmaL',"L")
  return(root)
}
