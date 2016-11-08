library("phbsasp")
source("CalcSabrPriceMC.r")

#example 1
beta <- 0
sigma0 <- 0.0068
alpha <- 0.3691
rho <- -0.0286
spot <- 4.35/100
t.exp <- 10
r <- 0
strike <- c( 3.7, seq(3.75,4.95,0.1), 5)/100
timestep<-120

#show the impact of parameters
#1. sigma0
sigma0<-0.00068
vol<-phbsasp::CalcNormalImpvolSabrHagan(forward=spot,strike=strike,t.exp=t.exp,sigma0=sigma0,alpha=alpha,rho=rho)
sigma0<-0.00088
vol2<-phbsasp::CalcNormalImpvolSabrHagan(forward=spot,strike=strike,t.exp=t.exp,sigma0=sigma0,alpha=alpha,rho=rho)
sigma0<-0.00108
vol3<-phbsasp::CalcNormalImpvolSabrHagan(forward=spot,strike=strike,t.exp=t.exp,sigma0=sigma0,alpha=alpha,rho=rho)

plot(strike,vol, ylim=c(0.8*min(vol,vol2,vol3),1.2*max(vol,vol2,vol3)),type="l", col="blue")
points(strike,vol2, type="l", col="red")
points(strike,vol3, type="l", col="green")
legend("topright",cex=0.6,text.width=0.002,inset=0.05,title="sigma0",c("0.00068","0.00088","0.00108"),lty=c(1,1,1),col=c("blue","red","green"))
title("starting vol sigma0")


#2. correlation rho
sigma0<-0.00068
rho<-0.03
vol<-phbsasp::CalcNormalImpvolSabrHagan(forward=spot,strike=strike,t.exp=t.exp,sigma0=sigma0,alpha=alpha,rho=rho)
rho<-0.13
vol2<-phbsasp::CalcNormalImpvolSabrHagan(forward=spot,strike=strike,t.exp=t.exp,sigma0=sigma0,alpha=alpha,rho=rho)
rho<-0.23
vol3<-phbsasp::CalcNormalImpvolSabrHagan(forward=spot,strike=strike,t.exp=t.exp,sigma0=sigma0,alpha=alpha,rho=rho)

plot(strike,vol, ylim=c(0.8*min(vol,vol2,vol3),1.2*max(vol,vol2,vol3)),type="l", col="blue")
points(strike,vol2, type="l", col="red")
points(strike,vol3, type="l", col="green")
legend("topright",cex=0.6,text.width=0.001,inset=0.05,title="rho",c("0.03","0.13","0.23"),lty=c(1,1,1),col=c("blue","red","green"))
title("correlation rho")
rho<--0.0286

#3. 'vol of vol' alpha
sigma0<-0.00068
alpha<-0.36
vol<-phbsasp::CalcNormalImpvolSabrHagan(forward=spot,strike=strike,t.exp=t.exp,sigma0=sigma0,alpha=alpha,rho=rho)
alpha<-0.4
vol2<-phbsasp::CalcNormalImpvolSabrHagan(forward=spot,strike=strike,t.exp=t.exp,sigma0=sigma0,alpha=alpha,rho=rho)
alpha<-0.44
vol3<-phbsasp::CalcNormalImpvolSabrHagan(forward=spot,strike=strike,t.exp=t.exp,sigma0=sigma0,alpha=alpha,rho=rho)

plot(strike,vol, ylim=c(0.8*min(vol,vol2,vol3),1.2*max(vol,vol2,vol3)),type="l", col="blue")
points(strike,vol2, type="l", col="red")
points(strike,vol3, type="l", col="green")
legend("topright",cex=0.6,text.width=0.001,inset=0.05,title="alpha",c("0.36","0.40","0.44"),lty=c(1,1,1),col=c("blue","red","green"))
title("'vol of vol' alpha")
alpha=0.3691

#example 2
beta <- 0
sigma0 <- 1/100
alpha <- 0.5
rho <- 0
spot <- 3.5/100
t.exp <- 30
r <- 0
strike <- seq(3,4,0.1)/100
timestep<-120
price<-CalcSabrPriceMC(spot=spot,strike=strike,alpha=alpha,rho=rho,sigma0=sigma0,t.exp=t.exp)
plot(strike,price, type="l", col="blue")
price
