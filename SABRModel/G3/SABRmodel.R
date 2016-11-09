###################### SABR ######################
################################################## 
########### Run Code for Presentation ############
################### Contains: ####################
############ - MC Method             #############
############ - Kennedy Method   n    #############
############ - Parameter Calibration #############
############ - Plots for Variation   #############
##################################################


library(phbsasp)
library(rootSolve)
library(statmod)

################## Implement Montecarlo Pricing ###############

sabr.mc <- function(spot = spot, strike = strike, alpha = alpha, rho = rho, sigma0 = sigma0, n.time = n.time, n.sample = n.sample, t.exp = t.exp, r = 0) { 
  d.t.exp <- (t.exp/n.time)
  rn1 <- matrix(rnorm(n.sample), nrow=n.time)
  z1 <- cbind(rn1, -rn1)
  rn2 <- matrix(rnorm(n.sample), nrow=n.time)
  z2.norho <- cbind(rn2, -rn2)
  z2<-rho*z1+sqrt(1-rho^2)*z2.norho
  
  sigma <-  matrix(numeric(length(z1)), nrow = n.time)
  sigma[1, ] <-  rep(sigma0, ncol(sigma))
  path.beta.zero <- matrix(numeric(length(z1)), nrow=n.time)
  path.beta.zero[1, ] <- rep(spot, ncol(path.beta.zero) )
  for (k in 2:n.time){
    sigma[k, ] <- sigma[k-1,]*exp(alpha*sqrt(d.t.exp)*z2[k,]-0.5*alpha^2*d.t.exp)
    path.beta.zero[k, ] <- path.beta.zero[k-1, ] + sigma[k-1, ]*z1[k, ]*sqrt(d.t.exp)
  }
  
  price.mc <- matrix(numeric(length(strike)))
  for (i in 1:length(strike)){
    price.mc[i] <- sum(pmax(path.beta.zero[n.time, ] - strike[i],0))/ncol(path.beta.zero)
  }
  return(price.mc)
}

############### Implement Kennedy-Model Pricing #################

CalcSabrPriceKennedy <- function(forward,spot=forward*exp(-r*t.exp),strike,sigma0,alpha,beta=0,rho,t.exp,r,nodes=50){
  ghq <- statmod::gauss.quad.prob(nodes, dist='normal')
  z <- ghq$nodes
  w <- ghq$weights
  sigmaT <- sigma0*exp(-0.5*alpha^2*t.exp+alpha*sqrt(t.exp)*z)
  d = log(sigmaT/sigma0)/(alpha*sqrt(t.exp))
  EVt.cond <- sigma0^2*sqrt(t.exp)/(2*alpha) * (pnorm(d+alpha*sqrt(t.exp))-pnorm(d-alpha*sqrt(t.exp)))/dnorm(d+alpha*sqrt(t.exp))
  yita <- EVt.cond/sqrt(t.exp)
  CN.cond <- phbsasp::CalcNormalPrice(type = 'call',spot  =  spot+rho/alpha*(sigmaT-sigma0), strike = strike, t.exp = t.exp,sigma = sqrt(1-rho^2)*yita,r = r,div = 0)
  return(sum(w*CN.cond))
}

############### Implement a Calibration process #################

sigma.alpha.rho <- function(forward, sigma, strike, t.exp){
  sub <- function(x){
    
    f1 <- phbsasp::CalcNormalImpvolSabrHagan(forward=forward, strike = strike[1], t.exp = t.exp, sigma0 = x[1], alpha = x[2], rho = x[3])
    f2 <- phbsasp::CalcNormalImpvolSabrHagan(forward=forward, strike = strike[2], t.exp = t.exp, sigma0 = x[1], alpha = x[2], rho = x[3]) 
    f3 <- phbsasp::CalcNormalImpvolSabrHagan(forward=forward, strike = strike[3], t.exp = t.exp, sigma0 = x[1], alpha = x[2], rho = x[3]) 
    variation1 <- f1 - sigma[1]
    variation2 <- f2 - sigma[2]
    variation3 <- f2 - sigma[3]
    
    return(c(variation1, variation2, variation3))
  }
  values <- multiroot(f = sub, start = c(0.05, 0.3, -0.02))
  root <- values$root
  names(root) <- c("sigma0","alpha","rho")
  return(root)
}


##################################### - Example 1

beta <- 0
sigma0 <- 0.68/100
alpha <- 0.3691
rho <- -0.0286
spot <- 4.35/100
t.exp <- 10
r <- 0
strike <- c( 4, seq(4.05,4.95,0.1), 5)/100
n.time <- 100
n.sample <- 1e5


################# MC Method - Exmple 1 ###################

(mc.price <- sabr.mc(spot = spot, strike = strike, alpha = alpha, rho = rho, sigma0 = sigma0, n.time = n.time, n.sample = n.sample, t.exp = t.exp, r = 0))
(mc.imp.vol <- phbsasp::CalcNormalImpvol(type = 'call', price = mc.price,spot = spot, forward = spot, strike = strike, t.exp = t.exp))
plot(strike, mc.imp.vol, type = 'l')

################# Kennedy Method - Example 1 #############

# Kennedy delivers a nice smile only if we increase the alpa parameters by quite a lot

alpha <- 1.5

kennedy.price <- c(rep(0,length(strike)))
for (i in 1:length(strike)){
  (kennedy.price[i] <- CalcSabrPriceKennedy(forward=spot,strike=strike[i],sigma0=sigma0,alpha=alpha,beta=beta,rho=rho,t.exp=t.exp,r=r,nodes=5))
}
(kennedy.price)
(kennedy.price.Implvol <-phbsasp::CalcNormalImpvol(type = 'call', price = kennedy.price,spot = spot, forward = spot, strike = strike, t.exp = t.exp))
plot(strike, kennedy.price.Implvol, type = 'l')

################ Calibration - Example 1 #################

alpha <- 0.3691

calc.values <- sigma.alpha.rho(forward=spot, sigma=sigma, strike=strike, t.exp=t.exp)
(calc.values) 

##################################### - Example 2

beta <- 0
sigma0 <- 1/100
alpha <- 0.5
rho <- 0
spot <- 3.5/100
t.exp <- 30
r <- 0
strike <- seq(3,4,0.1)/100
n.time <- 100
n.sample <- 1e5

################# MC Method - Exmple 2 ###################

(mc.price <- sabr.mc(spot = spot, strike = strike, alpha = alpha, rho = rho, sigma0 = sigma0, n.time = n.time, n.sample = n.sample, t.exp = t.exp, r = 0))
(mc.imp.vol <- phbsasp::CalcNormalImpvol(type = 'call', price = mc.price,spot = spot, forward = spot, strike = strike, t.exp = t.exp))
plot(strike, mc.imp.vol, type = 'l')

################# Kennedy Method - Example 1 #############

##### Kennedy delivers a nice smile only if we increase the alpa parameters by quite a lot

alpha <- 1.5

kennedy.price <- c(rep(0,length(strike)))
for (i in 1:length(strike)){
  (kennedy.price[i] <- CalcSabrPriceKennedy(forward=spot,strike=strike[i],sigma0=sigma0,alpha=alpha,beta=beta,rho=rho,t.exp=t.exp,r=r,nodes=5))
}
(kennedy.price)
(kennedy.price.Implvol <-phbsasp::CalcNormalImpvol(type = 'call', price = kennedy.price,spot = spot, forward = spot, strike = strike, t.exp = t.exp))
plot(strike, kennedy.price.Implvol, type = 'l')

################ Calibration - Example 2 #################

alpha <- 0.3691

calc.values <- sigma.alpha.rho(forward=spot, sigma=mc.imp.vol, strike=strike[c(1,2,3)], t.exp=t.exp)
(calc.values)

######### PLOTS ##########

########### Increasing alpha ############
t.exp <- 0.25

alpha1 <- 0.3
alpha2 <- 0.4
alpha3 <- 0.5

implvol.1 <- phbsasp::CalcNormalImpvolSabrHagan(forward=spot,strike=strike,t.exp=t.exp,sigma0=sigma0,alpha=alpha1,rho=rho)
implvol.2 <- phbsasp::CalcNormalImpvolSabrHagan(forward=spot,strike=strike,t.exp=t.exp,sigma0=sigma0,alpha=alpha2,rho=rho)
implvol.3 <- phbsasp::CalcNormalImpvolSabrHagan(forward=spot,strike=strike,t.exp=t.exp,sigma0=sigma0,alpha=alpha3,rho=rho)

plot(strike,implvol.1,type="l", col="blue", ylim=c(min(implvol.1,implvol.2,implvol.3),max(implvol.1,implvol.2,implvol.3)))
lines(strike, implvol.2, col="red")
lines(strike, implvol.3, col="yellow")

############ Increasing rho #############

rho1 <- -0.03
rho2 <- -0.015
rho3 <- -0.00

implvol.1 <- phbsasp::CalcNormalImpvolSabrHagan(forward=spot,strike=strike,t.exp=t.exp,sigma0=sigma0,alpha=alpha,rho=rho1)
implvol.2 <- phbsasp::CalcNormalImpvolSabrHagan(forward=spot,strike=strike,t.exp=t.exp,sigma0=sigma0,alpha=alpha,rho=rho2)
implvol.3 <- phbsasp::CalcNormalImpvolSabrHagan(forward=spot,strike=strike,t.exp=t.exp,sigma0=sigma0,alpha=alpha,rho=rho3)

plot(strike,implvol.1,type="l", col="blue", ylim=c(min(implvol.1,implvol.2,implvol.3),max(implvol.1,implvol.2,implvol.3)))
lines(strike, implvol.2, col="red")
lines(strike, implvol.3, col="yellow")

############ Increasing sigma0 ##############

alpha <- 2

sigma01 <- 0.0068
sigma02 <- 0.0078
sigma03 <- 0.0088

implvol.1 <- phbsasp::CalcNormalImpvolSabrHagan(forward=spot,strike=strike,t.exp=t.exp,sigma0=sigma01,alpha=alpha,rho=rho)
implvol.2 <- phbsasp::CalcNormalImpvolSabrHagan(forward=spot,strike=strike,t.exp=t.exp,sigma0=sigma02,alpha=alpha,rho=rho)
implvol.3 <- phbsasp::CalcNormalImpvolSabrHagan(forward=spot,strike=strike,t.exp=t.exp,sigma0=sigma03,alpha=alpha,rho=rho)

plot(strike,implvol.1,type="l", col="blue", ylim=c(min(implvol.1,implvol.2,implvol.3),max(implvol.1,implvol.2,implvol.3)))
lines(strike, implvol.2, col="red")
lines(strike, implvol.3, col="yellow")

