#example 1
beta <- 0
sigma0 <- 0.0068
alpha <- 0.3691
rho <- -0.0286
spot <- 4.35/100
t.exp <- 10
r <- 0
strike <- c( 4, seq(4.05,4.95,0.1), 5)/100
timestep<-120

#show the impact of parameters
#1. sigma0
sigma0=0.0068
price1<-CalcSabrPriceMC(spot=spot,strike=strike,alpha=alpha,rho=rho,sigma0=sigma0,t.exp=t.exp)
sigma0=0.0088
price2<-CalcSabrPriceMC(spot=spot,strike=strike,alpha=alpha,rho=rho,sigma0=sigma0,t.exp=t.exp)
sigma0=0.0108
price3<-CalcSabrPriceMC(spot=spot,strike=strike,alpha=alpha,rho=rho,sigma0=sigma0,t.exp=t.exp)

plot(strike,price1, ylim=c(0.8*min(price1,price2,price3),1.2*max(price1,price2,price3)),type="l", col="blue")
points(strike,price2, type="l", col="red")
points(strike,price3, type="l", col="green")
legend("topright",cex=0.6,text.width=0.001,inset=0.05,title="sigma0",c("0.0068","0.0088","0.0108"),lty=c(1,1,1),col=c("blue","red","green"))
title("starting vol sigma0")
sigma0=0.0068

#2. correlation rho
rho=0.2
price1<-CalcSabrPriceMC(spot=spot,strike=strike,alpha=alpha,rho=rho,sigma0=sigma0,t.exp=t.exp)
rho=0.4
price2<-CalcSabrPriceMC(spot=spot,strike=strike,alpha=alpha,rho=rho,sigma0=sigma0,t.exp=t.exp)
rho=0.6
price3<-CalcSabrPriceMC(spot=spot,strike=strike,alpha=alpha,rho=rho,sigma0=sigma0,t.exp=t.exp)

plot(strike,price1, ylim=c(0.8*min(price1,price2,price3),1.2*max(price1,price2,price3)),type="l", col="blue")
points(strike,price2, type="l", col="red")
points(strike,price3, type="l", col="green")
legend("topright",cex=0.6,text.width=0.001,inset=0.05,title="rho",c("0.2","0.4","0.6"),lty=c(1,1,1),col=c("blue","red","green"))
title("correlation rho")
rho=0.0286

#3. 'vol of vol' alpha
alpha=0.3
price1<-CalcSabrPriceMC(spot=spot,strike=strike,alpha=alpha,rho=rho,sigma0=sigma0,t.exp=t.exp)
alpha=0.5
price2<-CalcSabrPriceMC(spot=spot,strike=strike,alpha=alpha,rho=rho,sigma0=sigma0,t.exp=t.exp)
alpha=0.7
price3<-CalcSabrPriceMC(spot=spot,strike=strike,alpha=alpha,rho=rho,sigma0=sigma0,t.exp=t.exp)

plot(strike,price1, ylim=c(0.8*min(price1,price2,price3),1.2*max(price1,price2,price3)),type="l", col="blue")
points(strike,price2, type="l", col="red")
points(strike,price3, type="l", col="green")
legend("topright",cex=0.6,text.width=0.001,inset=0.05,title="alpha",c("0.3","0.5","0.7"),lty=c(1,1,1),col=c("blue","red","green"))
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
