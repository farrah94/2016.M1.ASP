
#-------------------GBM option pricing --------------------
bsprice <- function(type = 'call', spot, strike = spot, t.exp = 1, r = 0, b = 0, sigma){
    #------------------------------------------------
    #------------------------------------------------
    #Imput: type("call","put","straddle","digit")
    #       s is current price
    #       k is the strike price
    #       T is the time to maturity
    #       r is the risk-free rate
    #       b is the cost of carry (b eauals 0 for a future)
    #       sigma is the colatility
    #Return: the option price
    #        delta of the option
    #------------------------------------------------
    #------------------------------------------------
    
    stdev <- sigma*sqrt(T)
    d1 <- (log(s/k)+(b+0.5*sigma^2)*T) / stdev
    d2 <- d1 - stdev
    disc.factor <- exp(-r*t.exp)
  
    pnorm.d1 <- pnorm(d1)
    pnorm.d2 <- pnorm(d2)
    
    if (type == "call" ){
        price <- spot*exp(b*t.exp)*pnorm.d1 - strike*pnorm.d2
        delta <- exp(b*t.exp) * pnorm.d1
    }else if (type == "put"){
        price <- strike*(1-pnorm.d2) - spot*exp(b*t.exp)*(1-pnorm.d1)
        delta <- exp(b*t.exp) * pnorm.d1 - 1
    }else if (type == "straddle"){
        price <- exp(b*t.exp)*spot*pnorm.d1 - strike*pnorm.d2 + strike*(1-pnorm.d2) - exp(b*t.exp)*spot*(1-pnorm.d1)
        delta <- 2*exp(b*t.exp)*pnorm.d1 - 1
    }else if (type == "digit"){
        price <- pnorm.d2
        delta <- 1/sqrt(2*pi)*exp(-d2^2/2)/(s*stdev)
    }
    
    return( disc.fac * c(price=price,delta=delta) )
}



