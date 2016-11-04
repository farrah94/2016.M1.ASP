#generate risk free rate, time and number of assets in the basket
r<-0.08
t<-2
n<-4

# generate initial price, weight of assets, executive price and initial standard deviation sigma
# notice: s0 is a n*1 matrix
s0<-seq(100,100+2*(n-1),2)
s0.diag<-diag(s0)
w<-rep(1/n, n)
q<-rep(0.01,n)
k<-120
sigma0<-seq(0.1,0.1+0.1*(n-1),0.1)
sigma0.diag<-diag(sigma0)

# generate covariance matrix for correlated standard normal distributed variables
rho <- 0.6
cov.std.normal<-diag(n)*(1-rho)+rho

# calculate analytical normal call price
f<-t(w)%*%(s0+(r-q)*t*s0) #future price of the basket
variance.st.matrix<-t*(s0.diag%*%sigma0.diag%*%cov.std.normal%*%sigma0.diag%*%s0.diag)
sigma.st<-sqrt(t(w)%*%variance.st.matrix%*%w)
d1<-(f-k)/sigma.st
call.analytical<-exp(-r*t)*((f-k)*pnorm(d1)+sigma.st/sqrt(2*pi)*exp(-0.5*d1^2))

# monte carlo simulation for the normal model call price
cov.std.normal.chol <- t(chol(cov.std.normal))
n.sample <- 1e4
rn<-matrix(rnorm(n*n.sample), nrow=n)
rn.corr <- cov.std.normal.chol %*% rn #correlated random variables, used in both model of simulation
st.mc.normal.matrix<-(s0+(r-q)*s0*t)+sigma0*s0*sqrt(t)*rn.corr
st.mc.normal<-t(w)%*%st.mc.normal.matrix
call.mc.normal<-sum(exp(-r*t)*pmax(st.mc.normal-k,0))/n.sample

#control variate
cv<-call.analytical-call.mc.normal

## monte carlo simulation for the log normal model call price
st.mc.lognormal.matrix<-s0*exp((r-q-0.5*sigma0*sigma0)*t+sigma0*sqrt(t)*rn.corr)
st.mc.lognormal<-t(w)%*%st.mc.lognormal.matrix
call.mc.lognormal<-sum(exp(-r*t)*pmax(st.mc.lognormal-k,0))/n.sample

# monte carlo price after control variate
call.mc.lognormal.atercv<-call.mc.lognormal+cv

cat(call.mc.lognormal.atercv)




