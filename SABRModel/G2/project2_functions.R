# title:  Project2 sabr model
# author: Micheal
# date:   2016-11-01


#------------------Calibration SARB-------------------
CalibrationNormalSabr <- function(strike,sigma,forward=100,t.exp=1){
    require(rootSolve)
    require(phbsasp,quietly = TRUE)
    #------------------------------------------------
    #------------------------------------------------
    #Imput: forward is the forward price
    #       strike is the strike price vector (length is 3)
    #       t.exp is the time to maturity
    #       sigma is the implied volatility vector(length is 3)
    #Return: the option price
    #       The calibration parameter sigma0,rho,and alpha in SABR model
    #------------------------------------------------
    #------------------------------------------------
    subfunction <- function(x){
        imvol1 <- CalcNormalImpvolSabrHagan(sigma0 = x[1],rho = x[2],alpha = x[3],forward =  forward,t.exp =  t.exp,strike = strike[1])
        imvol2 <- CalcNormalImpvolSabrHagan(sigma0 = x[1],rho = x[2],alpha = x[3],forward =  forward,t.exp =  t.exp,strike = strike[2])
        imvol3 <- CalcNormalImpvolSabrHagan(sigma0 = x[1],rho = x[2],alpha = x[3],forward =  forward,t.exp =  t.exp,strike = strike[3])
        delta1 <-  imvol1 - sigma[1]
        delta2 <-  imvol2 - sigma[2]
        delta3 <-  imvol3 - sigma[3]
        return(c(delta1,delta2,delta3))
    }
    solver <- multiroot(f = subfunction,start = c(0.4,0.3,0.3))
    root <- solver$root
    names(root) <- c('sigma0',"rho",'alpha')
    return(root)
}



#-------------------Monte Carlo simulation-------------------
CalcSabrPriceMC <- function(forward, spot=forward*exp(-pr*t.exp),strike,sigma0,alpha,
                            beta=0, rho, t.exp, r, n.periods=1000, n.sample=10000){
    #------------------------------------------------
    #------------only support beta=0 so far----------
    #Imput: spot is current price
    #       strike is the strike price vector (length is 3)
    #       t.exp is the time to maturity
    #       sigma0 is the implied volatility vector(length is 3)
    #       alpha,beta,rho
    #       n.sample is the numbers of discrete periods
    #Return: the option price
    dt = t.exp/n.periods
    s = vol =matrix(0,n.periods,n.sample*2)
    s[1,] = spot*exp(r*t.exp)
    vol[1,] = sigma0
    payoffs <- vector(length=n.sample*2)
    index <- 2*(1:n.sample)-1
    for(j in index){
        w1=rnorm(n.periods)
        w2=rnorm(n.periods)
        w=w1
        z=rho*w1+sqrt(1-rho^2)*w2
        for(i in 2:n.periods){
            vol[i,j] = vol[i-1] * exp(-0.5*alpha^2*dt+alpha*sqrt(dt)*w[i-1])
            s[i,j] = s[i-1,j] + s[i-1,j]^beta*vol[i-1,j]*sqrt(dt)*z[i-1]
            vol[i,j+1] = vol[i-1] * exp(-0.5*alpha^2*dt+alpha*sqrt(dt)*(-w[i-1]))
            s[i,j+1] = s[i-1,j+1] + s[i-1,j+1]^beta*vol[i-1,j+1]*sqrt(dt)*(-z[i-1])
        }
        payoffs[j] = max(s[n.periods,j]-strike,0)
        payoffs[j+1] = max(s[n.periods,j+1]-strike,0)
    }
    call.price <- exp(-r*t.exp)*mean(payoffs)
    return(call.price)
}


#------------------Kennedy method   -----------------

CalcSabrPriceKennedy <- function(forward,spot=forward*exp(-r*t.exp),strike,sigma0,alpha,beta=0,rho,t.exp,r,nodes=50){
    #------------------------------------------------
    #-----------only support beta=0 so far-----------
    #Imput: spot is current price
    #       strike is the strike price vector (length is 3)
    #       t.expr is the time to maturity
    #       sigma0 is the implied volatility vector(length is 3)
    #       alpha,beta,rho
    #       n is the numbers of discrete periods
    #       nodes is the number of nodes
    #Return: the option price
    #------------------------------------------------
    #------------------------------------------------
    require(phbsasp,quietly = TRUE)
    require(statmod,quietly = TRUE)
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