#------------- European Normal Black-Sholes ------------
bsnormal <- function(type, s, k, t, r,sigma ){
  
  #------------------------------------------------
  #------------------------------------------------
  #Inputs: 
  #       type: "call","put","straddle","digital"
  #       s:  spot price of the underlying asset
  #       k: strike price
  #       t: time to maturity
  #       r: risk-free rate
  #       sigma: volatility
  #Outputs:  
  #       price: option price
  #       delta: option delta
  #       gamma: option gamma
  #       vega:  option vega
  #------------------------------------------------
  #------------------------------------------------
  
  
  d1=(s-k)/(sigma*sqrt(t))
  if(type=="call"){
    #Option Price
    price=exp(-r*t)*((s-k)*pnorm(d1)+sigma*exp(-d1^2/2)*sqrt(t/(2*pi)))
    #Greeks
    delta=exp(-r*t)*pnorm(d1)#Delta
    gamma=exp(-r*t)/(sigma*t)/sqrt(2*pi)*exp(-d1^2/2)#Gamma
    vega=exp(-r*t)*sqrt(t)/sqrt(2*pi)*exp(-d1^2/2)#Vega

  }else if (type=="put"){
    #Option Price
    price=exp(-r*t)*((k-s)*pnorm(-d1)+sigma*exp(-d1^2/2)*sqrt(t/(2*pi)))
    #Greeks
    delta=-exp(-r*t)*pnorm(-d1)#Delta
    gamma=exp(-r*t)/(sigma*t)/sqrt(2*pi)*exp(-d1^2/2)#Gamma
    vega=exp(-r*t)*sqrt(t)/sqrt(2*pi)*exp(-d1^2/2)#Vega
    
    
  } else if (type=="straddle"){
    #Straddle price
    price=exp(-r*t)*((s-k)*pnorm(d1)+sigma*exp(-d1^2/2)*sqrt(t/(2*pi)))+exp(-r*t)*((k-s)*pnorm(-d1)+sigma*exp(-d1^2/2)*sqrt(t/(2*pi)))
    delta=exp(-r*t)*pnorm(d1)-exp(-r*t)*pnorm(-d1)
    gamma=2*exp(-r*t)/(sigma*t)/sqrt(2*pi)*exp(-d1^2/2)
    vega=2*exp(-r*t)*sqrt(t)/sqrt(2*pi)*exp(-d1^2/2)#Vega
    
  } else if (type== "digital call"){
    #Digital call price
    price=exp(-r*t)*pnorm(d1)
    delta=exp(-r*t)/sqrt(2*pi)*exp(-d1^2/2)/(sigma*sqrt(t))
    gamma=exp(-r*t)/sqrt(2*pi)*exp(-d1^2/2)/(sigma^2*t)
    vega=-exp(-r*t)/sqrt(2*pi)*exp(-d1^2/2)*(s-k)/(sigma^2*sqrt(t))
      
  } else if (type== "digital put"){
    #Digital put price
    price=exp(-r*t)*pnorm(-d1)
    delta=-exp(-r*t)/sqrt(2*pi)*exp(-d1^2/2)/(sigma*sqrt(t))
    gamma=exp(-r*t)/sqrt(2*pi)*exp(-d1^2/2)/(sigma^2*t)
    vega=-exp(-r*t)/sqrt(2*pi)*exp(-d1^2/2)*(s-k)/(sigma^2*sqrt(t))
    
  } else {
    cat("Error! Please input: call/put/straddle/digital call/digital put")
  }
  return(c(price=price,delta=delta, gamma=gamma,vega=vega))
}

