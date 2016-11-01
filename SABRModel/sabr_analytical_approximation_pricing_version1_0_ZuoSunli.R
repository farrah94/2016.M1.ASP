# analyticial pricing of SABR model
# set paramaters
r<-0.08 #useless
t<-2

sigma0.normal<-20 #normal sigma0
sigma0.lognormal<-0.2 #log normal
s0<-100
alpha<-0.2
beta<-1
rho<-0.2
k<-105

#ghq
ghq <- statmod::gauss.quad.prob(7, dist='normal')
z<-ghq$nodes
w<-ghq$weights

if(beta==0){
  
  #generate series of sigma.t
  sigmat.normal<-sigma0.normal*exp(-0.5*alpha^2*t+alpha*sqrt(t)*z)
  
  # mean.st and var.st
  
  pnorm1<-pnorm(log(sigmat.normal/sigma0.normal)/alpha/sqrt(t)+alpha*sqrt(t))
  pnorm2<-pnorm(log(sigmat.normal/sigma0.normal)/alpha/sqrt(t)-alpha*sqrt(t))
  dnorm1<-dnorm(log(sigmat.normal/sigma0.normal)/alpha/sqrt(t)+alpha*sqrt(t))
  
  mean.st.normal<-s0+rho/alpha*(sigmat.normal-sigma0.normal)
  var.st.normal<-(1-rho^2)*((sigma0.normal)^2)*sqrt(t)/2/alpha*(pnorm1-pnorm2)/dnorm1

  #analytical price
  integrand.normal<-sqrt(var.st.normal)*dnorm((k-mean.st.normal)/sqrt(var.st.normal))+(mean.st.normal-k)*(1-pnorm((k-mean.st.normal)/sqrt(var.st.normal)))
  call.price.normal.analytical<-sum(w*integrand.normal)
  
  cat(call.price.normal.analytical)
  
} else if(beta==1){
  
  #generate series of sigma.t
  sigmat.lognormal<-sigma0.lognormal*exp(-0.5*alpha^2*t+alpha*sqrt(t)*z)
  
  # mean.st and var.st
  pnorm1<-pnorm(log(sigmat.lognormal/sigma0.lognormal)/alpha/sqrt(t)+alpha*sqrt(t))
  pnorm2<-pnorm(log(sigmat.lognormal/sigma0.lognormal)/alpha/sqrt(t)-alpha*sqrt(t))
  pnorm3<-pnorm(log(sigmat.lognormal/sigma0.lognormal)/alpha/sqrt(t)+2*alpha*sqrt(t))
  pnorm4<-pnorm(log(sigmat.lognormal/sigma0.lognormal)/alpha/sqrt(t)-2*alpha*sqrt(t))
  dnorm1<-dnorm(log(sigmat.lognormal/sigma0.lognormal)/alpha/sqrt(t)+alpha*sqrt(t))
  dnorm2<-dnorm(log(sigmat.lognormal/sigma0.lognormal)/alpha/sqrt(t)+2*alpha*sqrt(t))
  
  part.1 <- (pnorm1-pnorm2)/dnorm1*(sigma0.lognormal)^2*sqrt(t)/2/alpha*(1-rho^2-(sigmat.lognormal^2+sigma0.lognormal^2)/(8*alpha^2))
  part.2 <- (pnorm3-pnorm4)/dnorm2*(sigma0.lognormal)^4*sqrt(t)/16/alpha^3
  part.3 <- -((pnorm1-pnorm2)/dnorm1)^2*(sigma0.lognormal)^4*t/16/alpha^2
  
  mean.st.lognormal<-log(s0)+rho/alpha*(sigmat.lognormal-sigma0.lognormal)-(sigma0.lognormal)^2*sqrt(t)/4/alpha*(pnorm1-pnorm2)/dnorm1
  var.st.lognormal<-part.1+part.2+part.3
  
  #analytical price
  integrand.lognormal<-exp(mean.st.lognormal+0.5*var.st.lognormal)*pnorm((mean.st.lognormal+var.st.lognormal-log(k))/sqrt(var.st.lognormal))-k*pnorm((mean.st.lognormal-log(k))/sqrt(var.st.lognormal))
  call.price.lognormal.analytical<-sum(w*integrand.lognormal)
  
  cat(call.price.lognormal.analytical)
  
} else {
  cat("Fatal error! Cannot calculate when beta is not 0 or 1 !")
}




