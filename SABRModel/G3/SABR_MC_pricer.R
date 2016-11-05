###################################
##### SABR Monte Carlo Pricer #####
###################################
##### Returns MC Price & Vola #####
###################################
library("phbsasp", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")

sabr.mc <- function(spot = spot, strike = strike, alpha = alpha, rho = rho, sigma0 = sigma0, n.time = n.time, n.sample = n.sample, t.exp = t.exp, r = 0) { 
  
  d.t.exp <- (t.exp/n.time)
  
  rn1 <- matrix(rnorm(n.sample*n.time), nrow=n.time)
  z1 <- cbind(rn1, -rn1)
  rn2 <- matrix(rnorm(n.sample*n.time), nrow=n.time)
  z2.norho <- cbind(rn2, -rn2)
  z2<-rho*z1+sqrt(1-rho^2)*z2.norho
  
  sigma <-  matrix(numeric(length(z1)), nrow = n.time)
  sigma[ ,1] <-  sigma0
  for (k in 2:n.time){
    sigma[ k,] <- sigma[k-1,]*exp(alpha*sqrt(d.t.exp)*z2[k,]-0.5*alpha^2*d.t.exp)
  }
  
  path.beta.zero <- matrix(numeric(length(z1)), nrow=n.time)
  path.beta.zero[1, ] <- z1[1, ]
  for (k in 2:n.time){
    path.beta.zero[k, ] <- path.beta.zero[k-1, ] + sigma[k-1, ]*z1[k, ]*sqrt(d.t.exp)
  }
  
 price.mc <- for (i in 1:length(strike)){
   sum(pmax(path.beta.zero - strike[i]),0)/(n.sample*2)
 }
 
}

