#//////////////////////////////////////////////////////////
#///      European Option Price Under Normal Model      ///
#/////////////////////////////////////////////////////////


rm(list=ls())
cat("\014") 
cat("This calculates the European option price under the normal model")

#INPUTS
#Type of the option
type=winDialogString(message="Please input type of the option: call/put/straddle/digital call/digital put", default="call")
#Spot price of the underlying asset
s=as.numeric(winDialogString(message="Please input the spot price of the underlying asset", default=""))
#Annual risk-free rates in decimal
r=as.numeric(winDialogString(message="Please input the annual risk-free interest rate in decimal", default=""))
#Maturity of the option contract in years
t=as.numeric(winDialogString(message="Please input the maturity of the option in years", default=""))
#Time the option has existed in years
t0=as.numeric(winDialogString(message="Please input the time in years that the option has existed", default=""))
#Strike price
k=as.numeric(winDialogString(message="Please input the strike price", default=""))
#Annual volatility in decimal
sigma=as.numeric(winDialogString(message="Please input the annual volatility of underlying asset", default=""))


d1=(s-k)/(sigma*sqrt(t-t0))
if(type=="call"){
  #Option Price
  price=exp(-r*(t-t0))*((s-k)*pnorm(d1)+sigma*exp(-d1^2/2)*sqrt((t-t0)/(2*pi)))
  #Greeks
  delta=exp(-r*(t-t0))*pnorm(d1)#Delta
  gamma=exp(-r*(t-t0))/(sigma*(t-t0))/sqrt(2*pi)*exp(-d1^2/2)#Gamma
  vega=exp(-r*(t-t0))*sqrt(t-t0)/sqrt(2*pi)*exp(-d1^2/2)#Vega
  theta=-r*exp(-r*(t-t0))*((s-k)*pnorm(d1)+sigma*sqrt((t-t0)/(2*pi))*exp(-d1^2/2))-exp(-r*(t-t0))*sigma/(2*sqrt(2*pi*t0))*exp(-d1^2/2)#Theta
  
  #Output
  cat("\014") 
  cat("Under the normal model, the price of the call option is: ",price, "\n")
  #Greeks
  cat("Delta: ",delta,"\n")
  cat("Gamma: ",gamma,"\n")
  cat("Vega: ",vega,"\n")
  cat("Theta: ",theta,"\n")
  
}else if (type=="put"){
  #Option Price
  price=exp(-r*(t-t0))*((k-s)*pnorm(-d1)+sigma*exp(-d1^2/2)*sqrt((t-t0)/(2*pi)))
  #Greeks
  delta=-exp(-r*(t-t0))*pnorm(-d1)#Delta
  gamma=exp(-r*(t-t0))/(sigma*(t-t0))/sqrt(2*pi)*exp(-d1^2/2)#Gamma
  vega=exp(-r*(t-t0))*sqrt(t-t0)/sqrt(2*pi)*exp(-d1^2/2)#Vega
  theta=-r*exp(-r*(t-t0))*((k-s)*pnorm(-d1)+sigma*sqrt((t-t0)/(2*pi))*exp(-d1^2/2))-exp(-r*(t-t0))*sigma/(2*sqrt(2*pi*t0))*exp(-d1^2/2)#Theta
  
  #Output
  cat("\014") 
  cat("Under the normal model, the price of the put option is: ",price, "\n")
  #Greeks
  cat("Delta: ",delta,"\n")
  cat("Gamma: ",gamma,"\n")
  cat("Vega: ",vega,"\n")
  cat("Theta: ",theta,"\n")

  
} else if (type=="straddle"){
  #Straddle price
  straddle.price=exp(-r*(t-t0))*((s-k)*pnorm(d1)+sigma*exp(-d1^2/2)*sqrt(t/(2*pi)))+exp(-r*(t-t0))*((k-s)*pnorm(-d1)+sigma*exp(-d1^2/2)*sqrt((t-t0)/(2*pi)))
  #output
  cat("\014") 
  cat("Under the normal model, the price of the straddle is: ",straddle.price,"\n")
  
} else if (type== "digital call"){
  #Digital call price
  payoff=as.numeric(winDialogString(message="Please input the amount of money paid when option expires in the money", default="0"))
  digital.price=exp(-r*(t-t0))*payoff*pnorm(d1)
  #Output
  cat("\014") 
  cat("Under the normal model, the price of the digital call is: ",digital.price,"\n")

} else if (type== "digital put"){
  #Digital put price
  payoff=as.numeric(winDialogString(message="Please input the amount of money paid when option expires in the money", default="0"))
  digital.price=exp(-r*(t-t0))*payoff*pnorm(-d1)
  #Output
  cat("\014") 
  cat("Under the normal model, the price of the digital put is: ",digital.price,"\n")
  
} else {
  cat("\014") 
  cat("Error! Please input: call/put/straddle/digital call/digital put")
}

