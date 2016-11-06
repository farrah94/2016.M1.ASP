CalcSabrPriceMC<-function(spot = spot, strike = strike, alpha = alpha, beta=0, rho = rho, 
     sigma0 = sigma0, timestep=120, n.sample = 100000, t.exp = t.exp, r = 0){
  ##use MC method to calculate option price in Sabr model
    
  ##Create Random Number
  rn <- matrix(rnorm(timestep*n.sample),ncol=n.sample)
  w <- cbind(rn, -rn)
  rn <- matrix(rnorm(timestep*n.sample),ncol=n.sample)
  rn <- cbind(rn, -rn)
  z<-rho*w+sqrt(1-rho^2)*rn

  ##initialization
  sigma <-  matrix(numeric((timestep+1)*2*n.sample), ncol=2*n.sample)
  sigma[1,] <-  sigma0
  pricepath<-matrix(numeric((timestep+1)*2*n.sample), ncol=2*n.sample)
  pricepath[1,] <-spot
  dt<-t.exp/timestep
  
  #iteration
  for (k in 1:timestep){
    sigma[k+1, ] <- sigma[k,]*exp(alpha*sqrt(dt)*z[k,]-0.5*alpha^2*dt)
    pricepath[k+1, ] <- pricepath[k, ] + sigma[k, ]*w[k, ]*sqrt(dt)
  }
  
  price<-rep(0,length(strike))
  for(k in 1:length(strike)){
    price[k]<-mean(pmax(pricepath[timestep+1,]-strike[k],0))
  }
  return(price)
}
# example
# beta <- 0
# sigma0 <- 0.68
# alpha <- 0.3691
# rho <- -0.0286
# spot <- 4.35/100
# t.exp <- 10
# r <- 0
# strike <- c( 4, seq(4.05,4.95,0.01), 5)/100
# timestep<-120
# price<-CalcSabrPriceMC(spot=spot,strike=strike,alpha=alpha,rho=rho,sigma0=sigma0,t.exp=t.exp)
# plot(strike,price, type="l", col="blue")