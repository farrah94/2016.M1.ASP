
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
    cov.mat <- ( sigma %o% sigma ) * corr.mat * t.exp
    # Cholesky decomposition of the covariance matrix
    cov.chol <- t(chol(cov.mat))
    # Generate correlated random variables
    if (seed){
      set.seed(seed)
    }
    rn <- matrix( rnorm(n.asset*n.sample), nrow=n.asset )
    rn.corr <- cov.chol %*% rn

    
    #generate price path
    if (model){
      path.gbm <- spot * exp(rn.corr - 0.5*(sigma^2)*t.exp)
      price.basket.gbm <- t(weights) %*% path.gbm
      p.gbm <- exp(-r*t.exp)*pmax(price.basket.gbm - strike,0)
      price.MC.gbm <- mean(p.gbm)
      return (price.MC.gbm)
    } else {
      path.nm <- spot + rn.corr
      price.basket.nm <- t(weights) %*% path.nm
      p.nm <- exp(-r*t.exp)*pmax(price.basket.nm - strike,0)
      price.MC.nm <- mean(p.nm)
      return (price.MC.nm )
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
      cat("No analytic solution under geometric Brownian motion model")
      return ()
    } else {
      price.analytic.nm <- phbsasp::CalcNormalPrice(type=type,spot=spot.basket,strike=strike,
                                             t.exp=t.exp,r=r,sigma=sigma.basket) # analytic price under GBM model
      return (price.analytic.nm[1])
    }
    
  } else if (price == 3){
    sigma.nm <- sigma*spot
    #correlation matrix
    corr.mat <- diag(n.asset)*(1-rho) + rho
    cov.gbm.mat <- ( sigma %o% sigma ) * corr.mat * t.exp
    cov.nm.mat <- ( sigma.nm %o% sigma.nm ) * corr.mat * t.exp
    
    # Cholesky decomposition of the covariance matrix
    cov.chol.gbm <- t(chol(cov.gbm.mat))
    cov.chol.nm <- t(chol(cov.nm.mat))
    
    # Generate correlated random variables
    if (seed){
      set.seed(seed)
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
    var.basket.nm <- as.vector( 1/t.exp * t(weights) %*% cov.nm.mat %*% weights )
    sigma.basket.nm <- sqrt(var.basket.nm)
    
    
    price.analytic.nm <- phbsasp::CalcNormalPrice(type=type,spot=spot.basket,strike=strike,
                                              t.exp=t.exp,r=r,sigma=sigma.basket.nm)

    if (model){
      cat("No analytic solution under geometric Brownian motion model")
      return()
    } else {
      price.cv <- price.MC.gbm +(price.analytic.nm - price.MC.nm) # Monte Carlo price with control variate
      return (price.cv)
    }
  }
}