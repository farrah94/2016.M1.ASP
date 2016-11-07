##price under hagan##
K<-80
F<-90
beta<-1
rho<-0.6
v<-0.4
a0<-0.3
t<-1.5
r<-0.05

sigmahagan<-function(K,F,beta,rho,v,a0,t){
  z<-v/a0*((F*K)^((1-beta)/2))*log(F/K)
  x<-log((sqrt(1-2*rho*z+z^2)+z-rho)/(1-rho))
  tcoe1<-((1-beta)^2)*(a0^2)/24/((F*K)^(1-beta))
  tcoe2<-rho*beta*v*a0/4/((F*K)^((1-beta)/2))
  tcoe3<-(2-3*rho^2)*(v^2)/24
  coe4<-1+((1-beta)^2)/24*((log(F/K))^2)+((1-beta)^4)/1920*((log(F/K))^4)
  y<-a0/coe4/((F*K)^((1-beta)/2))*(z/x)*(1+(tcoe1+tcoe2+tcoe3)*t)
  y2<-a0*(1+(tcoe1+tcoe2+tcoe3)*t)/(F^(1-beta))
  if(K!=F){
  return(y)} else {
  return(y2)}
}
sigma_hagan<-sigmahagan(K,F,beta,rho,v,a0,t)
cat(sigma_hagan)
#price_hagan<-phbsasp::CalcBsmPrice(spot=F,strike=K,t.exp=t,r=r,sigma=sigmahagan(K,F,beta,rho,v,a0,t))
#cat(price_hagan)

library("rootSolve")
model <- function(x) c(F1=0.324374 - sigmahagan(K=100,F=90,beta=1,rho=x[1],v=x[2],a0=x[3],t=t), 
                       F2=0.3186 - sigmahagan(K=90,F=90,beta=1,rho=x[1],v=x[2],a0=x[3],t=t),
                       F3=0.2968936 - sigmahagan(K=80,F=90,beta=1,rho=x[1],v=x[2],a0=x[3],t=t) )
solver <- multiroot(f = model,start = c(0.5,0.5,0.5))
root <- solver$root
cat(root)







model <- function(x) c(F1=13.04335 - phbsasp::CalcBsmPrice(spot=100,strike=90,t.exp=t,r=r,sigma=sigmahagan(K=100,F=90,beta=1,rho=x[1],v=x[2],a0=x[3],t=t)), 
                       F2=16.59847 - phbsasp::CalcBsmPrice(spot=90,strike=90,t.exp=t,r=r,sigma=sigmahagan(K=90,F=90,beta=1,rho=x[1],v=x[2],a0=x[3],t=t)),
                       F3=21.33894 - phbsasp::CalcBsmPrice(spot=80,strike=90,t.exp=t,r=r,sigma=sigmahagan(K=80,F=90,beta=1,rho=x[1],v=x[2],a0=x[3],t=t)) )
solver <- multiroot(f = model,start = c(0.5,0.5,0.5))
root <- solver$root
cat(root)