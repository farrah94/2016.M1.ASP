# encode:  utf-8
# title:   ASP homework
# author:  Micheal
# date:    2016-10-27


# Description: In this file I only consider the call
# option for simplification.


#---------------------BS price ----------------------
bs.price <- function(type, s, k, T, r, sigma){
    #------------------------------------------------
    #------------------------------------------------
    #Imput: type("call","put","straddle","digit")
    #       s is current price
    #       k is the strike price
    #       T is the time to maturity
    #       r is the risk-free rate
    #       sigma is the colatility
    #Return: the option price
    #        delta of the option
    #------------------------------------------------
    #------------------------------------------------
    
    d1 <- (log(s/k)+(r+0.5*sigma^2)*T)/(sigma*sqrt(T))
    d2 <- d1 - sigma*sqrt(T)
    price <- s*pnorm(d1)-k*exp(-r*T)*pnorm(d2)
    return(price)
}


#----------------Displaced BS price -----------------
bs.displaced <- function(type, s, k, T, r, sigma, L){
    price <- bs.price(type = type,s = s+L,k = k+L,T = T,r = r,sigma = sigma)
    return(price)
}
    

#-----------------Normal option price----------------
normal.price <- function(type, s, k,r, T, sigma){
    #------------------------------------------------
    #------------------------------------------------
    #Imput: type("call","put","staddle","digit")
    #       s is current stock price
    #       k is the strike price ( s by default)
    #       T is the time to maturity
    #       r is the risk-free rate (0 by default)
    #       sigma is the volatility
    #Return: the option price
    #------------------------------------------------
    #------------------------------------------------
    
    price <- exp(-r*T)*sigma*sqrt(T/2/pi)*exp(-(k-s-r*T)^2/(2*sigma^2*T))+exp(-r*T)*(s+r*T-k)*pnorm((s+r*T-k)/(sigma*sqrt(T)))
    return(price)
}    


#------------------BS implied volatility--------------
bs.iv <- function(type, price, s, k, r, T){
    #-------------------------------------------------
    #-------------------------------------------------
    #Inputs: type thesame as in function optionprice
    #        price: the observed option price
    #Return: implied volatility
    #-------------------------------------------------
    #-------------------------------------------------
    
    sub <- function(sigma){
        f <- bs.price(type = type,s = s,k = k,r = r, T = T,sigma = sigma)[1]-price
        return(f)
    }
    iv <- uniroot(f = sub,interval = c(0,1))$root
    return(iv)
}


#---------------Normal implied volatility-------------

normal.iv <- function(type, price, s, k=s,r=0, T){
    #-------------------------------------------------
    #-------------------------------------------------
    #Inputs: type thesame as in function optionprice
    #        price: the observed option price
    #Return: implied volatility
    #-------------------------------------------------
    #-------------------------------------------------
    
    sub <- function(sigma){
        f <- normal.price(type = type,s =  s,k =  k,r = r,T = T,sigma = sigma)-price
        return(f)
    }
    iv <- uniroot(f = sub,interval = c(0,1))$root
    return(iv)
}


#---------------------Assignment1---------------------
# To get the implied volatility,we should make sure that
# the price you input is appropriate,otherwise the volatility
# will exceed the range 0-1 we have demanded. Therefore,we 
# should try to choose nice numbers.Hhhhh....
# Here, I use function try to control errors. 

k <-seq(0,1.2,by=0.01)
price.bs <- bs.price(type = 'call',s = 1,k = k,T = 1,r = 0.05,sigma = 0.4)
imvol.normal <- vector(length=length(k))
for(i in 1:length(k)){
    try <- try(normal.iv(type = 'call',price = price.bs[i],s = 1,k = k[i],r = 0.05,T = 1),silent=T)
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
sigma_upper <- normal.iv(type = 'call',price = bs.price(type = 'call',T = 1,s = 1,k = 1+dk,sigma = 0.3,r = 0.05),s = 1,k = 1+dk,r = 0.05,T = 1)
sigma_lower <- normal.iv(type = 'call',price = bs.price(type = 'call',T = 1,s = 1,k = 1-dk,sigma = 0.3,r = 0.05),s = 1,k = 1-dk,r = 0.05,T = 1)
estimated_slope <- (sigma_upper - sigma_lower)/(2*dk)



#---------------------Assignment2---------------------

k <- seq(60,150,by=0.1)
price_positiveL <- bs.displaced(type = 'call',s = 100,k = k,r = 0.05,sigma = 0.3,T = 1,L = 1)
price_negativeL <- bs.displaced(type = 'call',s = 100,k = k,r = 0.05,sigma = 0.3,T = 1,L = -1)
imvol.positiveL <- vector(length=length(k))
imvol.negativeL <- vector(length=length(k))

for(i in 1:length(k)){
    try <- try(bs.iv(type = 'call',price = price_positiveL[i],s = 100,k = k[i],r = 0.05,T = 1),silent=T)
    if(class(try)=="try-error"){
        imvol.positiveL[i]=(NA)
    }else{
        imvol.positiveL[i]=(try)
    }
}

for(i in 1:length(k)){
    try <- try(bs.iv(type = 'call',price = price_negativeL[i],s = 100,k = k[i],r = 0.05,T = 1),silent=T)
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
        bsprice_1 <- bs.price(type = type,s = s,k = k[1], sigma = sigma[1],r = r,T = T)
        bsprice_2 <- bs.price(type = type,s = s,k = k[2], sigma = sigma[2],r = r,T = T)
        delta1 <- bs.price(type = type,s = s+x[2],k = k[1]+x[2],sigma = x[1],r = r,T = T) - bsprice_1
        delta2 <- bs.price(type = type,s = s+x[2],k = k[2]+x[2],sigma = x[1],r = r,T = T) - bsprice_2
        return(c(delta1,delta2))
    }
    solver <- multiroot(f = subfunction,start = c(0.3,1))
    root <- solver$root
    names(root) <- c('sigmaL',"L")
    return(root)
}