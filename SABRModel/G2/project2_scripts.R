#### test the situation when alpha is zero
beta <- 0 
sigma0 <- 1/100 
alpha <- 0.5 
rho <- 0 
spot <- 3.5/100 
t.exp <- 30 
r <- 0 
strike <- seq(3,4,0.1)/100 

CalcSabrPriceMC(spot = spot,strike = strike[1],sigma0 = sigma0,alpha = 0,beta = 0,rho = rho,t.exp = t.exp,r = r,n.periods = 1000,n.sample = 1000)
CalcNormalPrice(type = 'call',spot = spot,strike = strike[1],t.exp = t.exp,r = r,div = 0,sigma = sigma0)

#### SABR case 1 from Korn & Tang (Wilmott) 
beta <- 0 
sigma0 <- 0.68/100 
alpha <- 0.3691 
rho <- -0.0286 
spot <- 4.35/100 
t.exp <- 10 
r <- 0 
strike <- c( 4, seq(4.05,4.95,0.1), 5)/100 
price.case1 <- c(0.011392, 0.0111, 0.010535, 0.009994, 0.009476, 0.008983, 0.008513,0.008068, 0.007646, 0.007247, 0.00687, 0.00669)
price.case1.MC <- sapply(strike,function(i){CalcSabrPriceMC(spot = spot,strike = strike[1],sigma0 = sigma0,alpha = alpha,beta = beta,rho = rho,t.exp = t.exp,r = r,n.periods = 1000,n.sample = 1000)})


#### SABR case 2 from Korn & Tang (Wilmott) 
beta <- 0 
sigma0 <- 1/100 
alpha <- 0.5 
rho <- 0 
spot <- 3.5/100 
t.exp <- 30 
r <- 0 
strike <- seq(3,4,0.1)/100 
price.case2 <- c( 0.034919, 0.034346, 0.033789, 0.033248, 0.032724, 0.032216, 0.031724, 0.031248, 0.030789, 0.030346, 0.029919 )
price.case2.MC <- sapply(strike,function(i){CalcSabrPriceMC(spot = spot,strike = strike[1],sigma0 = sigma0,alpha = alpha,beta = beta,rho = rho,t.exp = t.exp,r = r,n.periods = 1000,n.sample = 1000)})
price.case2.Ken <- sapply(strike,function(i){CalcSabrPriceKennedy(spot = spot,strike = strike[1],sigma0 = sigma0,alpha =alpha,beta = beta,rho = rho,t.exp = t.exp,r = r)})

##Impact of parameters

beta <- 0
sigma0 <- 1/100
alpha <- 0.5
rho <- 0
spot <- 3.5/100
t.exp <- 0.25
r <- 0
strike <- seq(1,5,0.1)/100
price <- c( 0.034919, 0.034346, 0.033789, 0.033248, 0.032724, 0.032216,
            0.031724, 0.031248, 0.030789, 0.030346, 0.029919 )
alpha1 <- CalcNormalImpvolSabrHagan(forward = spot,strike = strike,t.exp = t.exp,sigma0 = sigma0,alpha = 0.3,rho = rho)
alpha2 <- CalcNormalImpvolSabrHagan(forward = spot,strike = strike,t.exp = t.exp,sigma0 = sigma0,alpha = 0.4,rho = rho)
alpha3 <- CalcNormalImpvolSabrHagan(forward = spot,strike = strike,t.exp = t.exp,sigma0 = sigma0,alpha = 0.5,rho = rho)
plot(strike,alpha1,"l",ylim = c(min(c(range(alpha1,alpha2,alpha3))),max(c(range(alpha1,alpha2,alpha3)))))
lines(strike,alpha2,col='red')
lines(strike,alpha3,col='blue')

rho1 <- CalcNormalImpvolSabrHagan(forward = spot,strike = strike,t.exp = t.exp,sigma0 = sigma0,alpha = alpha,rho = 0.1)
rho2 <- CalcNormalImpvolSabrHagan(forward = spot,strike = strike,t.exp = t.exp,sigma0 = sigma0,alpha = alpha,rho = 0.15)
rho3 <- CalcNormalImpvolSabrHagan(forward = spot,strike = strike,t.exp = t.exp,sigma0 = sigma0,alpha = alpha,rho = 0.2)
plot(strike,rho1,"l",ylim = c(min(c(range(rho1,rho2,rho3))),max(c(range(rho1,rho2,rho3)))))
lines(strike,rho2,col='red')
lines(strike,rho3,col='blue')

sigma01 <- CalcNormalImpvolSabrHagan(forward = spot,strike = strike,t.exp = t.exp,sigma0 = 0.2999,alpha = alpha,rho = rho)
sigma02 <- CalcNormalImpvolSabrHagan(forward = spot,strike = strike,t.exp = t.exp,sigma0 = 0.3,alpha = alpha,rho = rho)
sigma03 <- CalcNormalImpvolSabrHagan(forward = spot,strike = strike,t.exp = t.exp,sigma0 = 0.3001,alpha = alpha,rho = rho)
plot(strike,sigma01,"l",ylim = c(min(c(range(sigma01,sigma02,sigma03))),max(c(range(sigma01,sigma02,sigma03)))))
lines(strike,sigma02,col='red')
lines(strike,sigma03,col='blue')