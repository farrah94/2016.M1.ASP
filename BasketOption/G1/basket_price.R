
#----------- Basket Option -----------
CalcBasket <- function(
  type = 'call', spot, forward = spot*exp(r*t.exp),
  strike = forward, t.exp, r, sigma, 
  rho, n.asset = length(spot), weights= rep(1/n.asset, n.asset), n.sample= 1e5, 
  price = 1, model = 0, seed = 0
){
  #--------------------------------------------------------------------------
  #--------------------------------------------------------------------------
  #Inputs: 
  #       type: call or put. The default is call option
  #       spot: a vector of spot prices of the underlying assets in the basket
  #       strike: a scalar of strike price
  #       t.exp: a scalar of time to maturity
  #       r: a scalar of risk-free rate
  #       sigma: a vector of volatility. NOTE THAT SIGMA SHOULD BE IN PERCENTAGE AS INPUT
  #       rho: a scalar of correlation coefficient
  #       n.asset: a scalar of number of underlying assets in the basket
  #       weights: a vector of the underlying's weights in the basket
  #       n.sample: a scalar of number RNs generated
  #       price: Monte Carlo price if price = 1
  #              Analytic price if price = 2
  #              Monte Carlo price with control variate if price = 3
  #              The default price is Monte Carlo price
  #       model: normal model if model =0
  #              geometric Brownian model if model = 1
  #              The default model is normal model
  #       seed : default seed is used if seed = 1
  #              
  #Outputs:  
  #       price and standard deviation of price if MC method is used
  #
  #Author: Yingxiang Li    yingxiangli@sz.pku.edu.cn
  # 
  #--------------------------------------------------------------------------
  #--------------------------------------------------------------------------
  
  


  if (price == 1){
    if (model == 0){
      sigma <- sigma*spot
    }
    #correlation matrix
    corr.mat <- diag(n.asset)*(1-rho) + rho
    cov.mat <- ( sigma %o% sigma ) * corr.mat
    # Cholesky decomposition of the covariance matrix
    cov.chol <- t(chol(cov.mat))
    # Generate correlated random variables
    if (seed){
      set.seed(1)
    }
    rn <- matrix( rnorm(n.asset*n.sample), nrow=n.asset )
    rn.corr <- cov.chol %*% rn
    
    ## The covariance of the generated samples
    #( rn.corr %*%t(rn.corr) ) / n.sample
    
    #generate price path
    if (model){
      path.gbm <- spot * exp(rn.corr - 0.5*(sigma^2)*t.exp)
      price.basket.gbm <- t(weights) %*% path.gbm
      p.gbm <- exp(-r*t.exp)*pmax(price.basket.gbm - strike,0)
      price.MC.gbm <- mean(p.gbm)
      std.MC.gbm <- sd(p.gbm)
      hist(p.gbm, col='blue',breaks = 25)
      return (c(price.MC.gbm =price.MC.gbm, std.price=std.MC.gbm))
    } else {
      path.nm <- spot + rn.corr
      price.basket.nm <- t(weights) %*% path.nm
      p.nm <- exp(-r*t.exp)*pmax(price.basket.nm - strike,0)
      price.MC.nm <- mean(p.nm)
      std.MC.nm <- sd(p.nm)
      hist(p.nm, col='blue',breaks = 25)
      return (c(price.MC.nm =price.MC.nm, std.price=std.MC.nm))
    }

    
  } else if (price == 2){
    if (model == 0){
      sigma <- sigma*spot
    }
    #correlation matrix
    corr.mat <- diag(n.asset)*(1-rho) + rho
    cov.mat <- ( sigma %o% sigma ) * corr.mat
    
    var.basket <- as.vector( t(weights) %*% cov.mat %*% weights )
    sigma.basket <- sqrt(var.basket)
    spot.basket <- t(weights) %*% spot
    
    if (model){
      price.analytic.gbm <- phbsasp::CalcBsmPrice(type=type,spot=spot.basket,strike=strike,
                                            t.exp=t.exp,r=r,sigma=sigma.basket) # analytic price under normal model
      return (price.analytic.gbm[1])
    } else {
      price.analytic.nm <- phbsasp::CalcNormalPrice(type=type,spot=spot.basket,strike=strike,
                                             t.exp=t.exp,r=r,sigma=sigma.basket) # analytic price under GBM model
      return (price.analytic.nm[1])
    }
    
  } else if (price == 3){
    sigma.nm <- sigma*spot
    #correlation matrix
    corr.mat <- diag(n.asset)*(1-rho) + rho
    cov.gbm.mat <- ( sigma %o% sigma ) * corr.mat
    cov.nm.mat <- ( sigma.nm %o% sigma.nm ) * corr.mat
    
    # Cholesky decomposition of the covariance matrix
    cov.chol.gbm <- t(chol(cov.gbm.mat))
    cov.chol.nm <- t(chol(cov.nm.mat))
    
    # Generate correlated random variables
    if (seed){
      set.seed(1)
    }
    rn <- matrix( rnorm(n.asset*n.sample), nrow=n.asset )
    rn.corr.gbm <- cov.chol.gbm %*% rn
    rn.corr.nm <- cov.chol.nm %*% rn
    
    #Price of the basket
    path.gbm <- spot * exp(rn.corr.gbm - 0.5*(sigma^2)*t.exp)
    price.basket.gbm <- t(weights) %*% path.gbm
    path.nm <- spot + rn.corr.nm
    price.basket.nm <- t(weights) %*% path.nm
    
    #Monte Carlo price
    p.nm <- exp(-r*t.exp)*pmax(price.basket.nm - strike,0)
    price.MC.nm <- mean(p.nm)
    p.gbm <- exp(-r*t.exp)*pmax(price.basket.gbm - strike,0)
    price.MC.gbm <- mean(p.gbm)

    spot.basket <- t(weights) %*% spot
    var.basket.gbm <- as.vector( t(weights) %*% cov.gbm.mat %*% weights )
    sigma.basket.gbm <- sqrt(var.basket.gbm)
    var.basket.nm <- as.vector( t(weights) %*% cov.nm.mat %*% weights )
    sigma.basket.nm <- sqrt(var.basket.nm)
    
    
    price.analytic.nm <- phbsasp::CalcNormalPrice(type=type,spot=spot.basket,strike=strike,
                                              t.exp=t.exp,r=r,sigma=sigma.basket.nm)
    price.analytic.gbm <- phbsasp::CalcBsmPrice(type=type,spot=spot.basket,strike=strike,
                                             t.exp=t.exp,r=r,sigma=sigma.basket.gbm)
    if (model){
      price.cv <- price.MC.gbm +(price.analytic.nm - price.MC.nm) # Monte Carlo price with control variate
      hist(p.gbm, col='blue',breaks = 25)
      p <- p.gbm + (price.analytic.nm[1] - p.nm)
      hist(p, col='red',add=T, breaks = 25)
      std.mc <- sd(p.gbm)
      std.cv <- sd(p)
    } else {
      
      price.cv <- price.MC.nm + (price.analytic.gbm - price.MC.gbm) # Monte Carlo price with control variate
      hist(p.nm, col='blue', breaks = 25)
      p <- p.nm +(price.analytic.gbm[1]-p.gbm)
      hist(p, col='red',add=T, breaks = 25)
      std.mc <- sd(p.nm)
      std.cv <- sd(p)
    }
    return (c(price.cv = price.cv, std.mc = std.mc, std.cv = std.cv))
  }
}