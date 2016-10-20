      #-----------------Numerical approximation of the implied volatility-----------------
      #-------------------------under arithmetic Brownian motion--------------------------
      
ImpliedVola <- function(price, S=F/exp(r*T), F, K=S, r=0, T=1) {
      #------------------------------------------------
      #------------------------------------------------
      #Input: price is option price
      #       S is current stock price (can be derived from the future price)
      #       F is the future price
      #       K is the strike price ( S by default)
      #       T is the time to maturity (1 by default)
      #       r is the risk-free rate (0 by default)
      #       
      #Return: sigma is the volatility
      #------------------------------------------------
      #------------------------------------------------  
  
  if (S == K){
      #a case when current stock price 
      #is equal to the strike price
    
      #implied volatility
    sigma <- price/(0.4*sqrt(T))
    
    return(sigma)
    
  }else{
      #implied volatility when current stock price 
      #is different from the strike price
  
      #variable v which is bounded in the range [-1, 1],
      #since the straddle price is always worth more 
      #than the intrinsic value |F-K|.
        v <- (S-K)/(price)
     
      #transformation of v used for better 
      #approximation of h
        n <- v/atanh(v)

      #vectors a and b used for rational Chebyshev approximation
  a <- c(3.994961687345134*exp(-1), 
       2.100960795068497*exp(1), 
       4.980340217855084*exp(1), 
       5.988761102690991*exp(2), 
       1.848489695437094*exp(3), 
       6.106322407867059*exp(3),
       2.493415285349361*exp(4), 
       1.266458051348246*exp(4))
  
  b <- c(1.000000000000000,
       4.990534153589422*exp(1),
       3.093573936743112*exp(1),
       1.495105008310999*exp(3),
       1.323614537899738*exp(3),
       1.598919697679745*exp(4),
       2.392008891720782*exp(4),
       3.608817108375034*exp(3),
      -2.067719486400926*exp(2),
       1.174240599306013*exp(1))
  
      #summands sum.a and sum.b used to calculate h(n)
  for (i in 1:8){
    sum.a <- sum(a[i]*(n^(i-1)))
  }
  
  for (i in 1:10){
    sum.b <- sum(b[i]*(n^(i-1)))
  }
  
      #approximation of h(n)
  h <-  sqrt(n) * (sum.a/sum.b)
  
      #implied volatility
  sigma <-  sqrt(pi/(2*T)) * price * h
  
  return(sigma)

  }
}
