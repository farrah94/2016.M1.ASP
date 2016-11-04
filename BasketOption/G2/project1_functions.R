CalBasketGBMMC <- function(sigma, corr, r, k, spot, weight, t.exp,n.sample=100000,seed=rnorm(1)){
    #------------------------------------------------
    #------MC prcing call option price under GBM-----
    #Imput: sigma is volatility vector
    #       corr is the correlation matrix
    #       spot is the spot price vector (length equals to sigma)
    #       weight is the weight of portfolio(length equals to sigma)
    #       k is the strike price
    #       t.exp is the time to maturity
    #       sigma is the implied volatility vector(length is 3)
    #       n.sample is the numbers of loops
    #       seed number to control the random generators(default is random)
    #Return: the option price (call)
    #------------------------------------------------
    #------------------------------------------------
    set.seed(seed)
    cov.mat <- sigma %*% t(sigma) * corr 
    cov.chol <- t(chol(cov.mat))
    n.asset <- length(spot)
    rn <- matrix( rnorm(n.asset*n.sample), nrow=n.asset )
    rn.corr <- cov.chol %*% rn
    s.mat <- spot*exp((r-0.5*sigma^2)*t.exp+sqrt(t.exp)*rn.corr)
    portfolio.sample <- t(weight) %*% s.mat
    payoffs <- pmax(portfolio.sample-k,0)
    price <- exp(-r*t.exp)*mean(payoffs)
    return(price)
}

CalBasketNormalMC <- function(sigma, corr, r, k, spot, weight, t.exp, n.sample=100000, seed=rnorm(1)){
    #------------------------------------------------
    #----MC prcing call option price under Normal----
    #    #Imput: sigma is volatility vector
    #       corr is the correlation matrix
    #       spot is the spot price vector (length equals to sigma)
    #       weight is the weight of portfolio(length equals to sigma)
    #       k is the strike price
    #       t.exp is the time to maturity
    #       sigma is the implied volatility vector(length is 3)
    #       n.sample is the numbers of loops
    #       seed number to control the random generators(default is random)
    #Return: the option price (call)
    #------------------------------------------------
    #------------------------------------------------
    set.seed(seed)
    cov.mat <- sigma %*% t(sigma) * corr 
    cov.chol <- t(chol(cov.mat))
    n.asset <- length(spot)
    rn <- matrix( rnorm(n.asset*n.sample), nrow=n.asset )
    rn.corr <- cov.chol %*% rn
    s.mat <- spot*exp(r*t.exp)+sqrt(t.exp)*rn.corr
    portfolio.sample <- t(weight) %*% s.mat
    payoffs <- pmax(portfolio.sample-k,0)
    price <- exp(-r*t.exp)*mean(payoffs)
    return(price)
}

CalBasketCV <- function(sigma, corr, r, k, spot, weight, t.exp, n.sample=100000, seed=rnorm(1)){
    #------------------------------------------------
    #------MC prcing call option price with CV-------
    #Imput: sigma is volatility vector
    #       corr is the correlation matrix
    #       spot is the spot price vector (length equals to sigma)
    #       weight is the weight of portfolio(length equals to sigma)
    #       k is the strike price
    #       t.exp is the time to maturity
    #       sigma is the implied volatility vector(length is 3)
    #       n.sample is the numbers of loops
    #       seed number to control the random generators(default is random)
    #Return: the option price (call)
    #------------------------------------------------
    #------------------------------------------------
    price.GBMMC <- CalBasketGBMMC(sigma = sigma,corr = corr,r = r,k = k,spot = spot,t.exp = t.exp,weight = weight,seed = seed)
    price.NormalMC <- CalBasketNormalMC(sigma = sigma,corr = corr,r = r,k = k,spot = spot,t.exp = t.exp,weight = weight,seed = seed)
    cov.mat <- sigma %*% t(sigma) * corr
    portfolio.spot <- t(weight) %*% spot
    portfolio.var <- t(weight) %*%  cov.mat %*% weight
    portfolio.sigma <- sqrt(portfolio.var)
    price.NormalAnalystic <- phbsasp::CalcBsmPrice(type = 'call',spot = portfolio.spot,strike = k,t.exp = t.exp,r = r,div = 0,sigma = portfolio.sigma)
    price <- price.GBMMC - price.NormalMC + price.NormalAnalystic
    return(price)
}
