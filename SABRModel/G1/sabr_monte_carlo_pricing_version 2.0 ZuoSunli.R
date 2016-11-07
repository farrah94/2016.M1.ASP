# monte carlo pricing of SABR model
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
n.step<- 1e2
n.sample <- 1e4

sabr.monte.carlo<-function(r,t,sigma0.normal,sigma0.lognormal,s0,alpha,beta,rho,k,n.setp,n.sample){
  d.t<-t/n.step
  #generate correlated random variables matrix
  rn1 <- matrix( rnorm(n.step*n.sample), nrow=n.step )
  rn2 <- matrix( rnorm(n.step*n.sample), nrow=n.step )
  
  z1<-rn1
  z2<-rho*rn1+sqrt(1-rho^2)*rn2
  
  if(beta==0){
    
    sigma.normal<-matrix(numeric(n.step*n.sample), nrow=n.step)
    s.normal<-matrix(numeric(n.step*n.sample), nrow=n.step)
    
    sigma.normal[1,]<-sigma0.normal*exp(alpha*sqrt(d.t)*z2[1,]-0.5*alpha^2*d.t)
    for(i in 2:n.step){
      sigma.normal[i,]<-sigma.normal[i-1,]*exp(alpha*sqrt(d.t)*z2[i,]-0.5*alpha^2*d.t)
    }
    
    s.normal[1,]<-s0+sigma0.normal*sqrt(d.t)*z1[1,]
    for(i in 2:n.step){
      s.normal[i,]<-s.normal[i-1,]+sigma.normal[i-1,]*sqrt(d.t)*z1[i,]
    }
    
    # monte carlo price
    call.price.normal.monte.carlo<-sum( pmax(s.normal[n.step,]-k,0) )/n.sample 
    #cat(call.price.normal.monte.carlo)
    return(call.price.normal.monte.carlo)
    
  } else if(beta==1){
    
    sigma.lognormal<-matrix(numeric(n.step*n.sample), nrow=n.step)
    s.lognormal<-matrix(numeric(n.step*n.sample), nrow=n.step)
    
    sigma.lognormal[1,]<-sigma0.lognormal*exp(alpha*sqrt(d.t)*z2[1,]-0.5*alpha^2*d.t)
    for(i in 2:n.step){
      sigma.lognormal[i,]<-sigma.lognormal[i-1,]*exp(alpha*sqrt(d.t)*z2[i,]-0.5*alpha^2*d.t)
    }
    
    s.lognormal[1,]<-s0*exp(sigma0.lognormal*sqrt(d.t)*z1[1,]-0.5*sigma0.lognormal^2*d.t)
    for(i in 2:n.step){
      s.lognormal[i,]<-s.lognormal[i-1,]*exp(sigma.lognormal[i-1,]*sqrt(d.t)*z1[i,]-0.5*sigma.lognormal[i-1,]^2*d.t)
    }
    
    # monte carlo price
    call.price.lognormal.monte.carlo<-sum( pmax(s.lognormal[n.step,]-k,0) )/n.sample 
    #cat(call.price.lognormal.monte.carlo)
    return(call.price.lognormal.monte.carlo)
    
  } else{
    cat("Fatal error! Cannot calculate when beta is not 0 or 1 !")
  }
}

call.price<-numeric(0)

for(k in 50:150){
  call.price[k-50+1]<-sabr.monte.carlo(r,t,sigma0.normal,sigma0.lognormal,s0,alpha,beta,rho,k,n.setp,n.sample)
  cat(k)
}

k<-c(50:150)

plot(k,call.price,main="Monte Carlo")
lines(k,call.price,type="l")



for(i in 1:200){
  call.price[i]<-sabr.monte.carlo(r,t,sigma0.normal,sigma0.lognormal,s0,alpha,beta,rho,k,n.setp,n.sample)
  cat(i)
}

hist(call.price,freq=T,breaks=20,col="white",xlab="Call Price Of Sabr Model")
lines(density(call.price),col="blue")


