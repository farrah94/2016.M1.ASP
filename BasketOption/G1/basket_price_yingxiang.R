
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
  #       sigma: a vector of volatility
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
  #       price.cv: Monte Carlo price of the basket option with control variate
  #--------------------------------------------------------------------------
  #--------------------------------------------------------------------------
  
  
  #correlation matrix
  corr.mat <- diag(n.asset)*(1-rho) + rho
  cov.mat <- ( sigma %o% sigma ) * corr.mat
  
  if (price == 1){
    # Cholesky decomposition of the covariance matrix
    cov.chol <- t(chol(cov.mat))
    # Generate correlated random variables
    if (seed){
      set.seed(1)
    }
    rn <- matrix( rnorm(n.asset*n.sample), nrow=n.asset )
    rn.corr <- cov.chol %*% rn
    # The covariance of the generated samples
    ( rn.corr %*%t(rn.corr) ) / n.sample
    
    #generate price path
    if (model){
      s.mat <- spot*exp((r-0.5*sigma^2)*t.exp+sqrt(t.exp)*rn.corr)
      price.basket <- t(weights) %*% s.mat
    } else {
      s.mat <- spot*exp(r*t.exp)+sqrt(t.exp)*rn.corr
      price.basket <- t(weights) %*% rn.corr
    }
    
    price.MC <- exp(-r*t.exp)*mean(pmax(price.basket-strike,0))   #Monte Carlo price
    return (price.MC)
  } else if (price == 2){
    spot.basket <- t(weights) %*% spot
    var.basket <- t(weights) %*%  cov.mat %*% weights
    sigma.basket <- sqrt(var.basket)
    if (model){
      price.analytic <- phbsasp::CalcNormalPrice(type=type,spot=spot.basket,strike=strike,
                                            t.exp=t.exp,r=r,sigma=sigma.basket) # analytic price under normal model
    } else {
      price.analytic <- phbsasp::CalcBsmPrice(type=type,spot=spot.basket,strike=strike,
                                             t.exp=t.exp,r=r,sigma=sigma.basket) # analytic price under GBM model
    }
    return (price.analytic)
  } else if (price == 3){
    # Cholesky decomposition of the covariance matrix
    cov.chol <- t(chol(cov.mat))
    # Generate correlated random variables
    if (seed){
      set.seed(1)
    }
    rn <- matrix( rnorm(n.asset*n.sample), nrow=n.asset )
    rn.corr <- cov.chol %*% rn
    # The covariance of the generated samples
    ( rn.corr %*%t(rn.corr) ) / n.sample
    
    #Price of the basket
    if (model){
      s.mat <- spot*exp((r-0.5*sigma^2)*t.exp+sqrt(t.exp)*rn.corr)
      price.basket <- t(weights) %*% s.mat
    } else {
      s.mat <- spot*exp(r*t.exp)+sqrt(t.exp)*rn.corr
      price.basket <- t(weights) %*% rn.corr
    }
    #generate price path
    s.mat <- spot*exp((r-0.5*sigma^2)*t.exp+sqrt(t.exp)*rn.corr)
    price.basketGBM <- t(weights) %*% s.mat
    s.mat <- spot*exp(r*t.exp)+sqrt(t.exp)*rn.corr
    price.basketNormal <- t(weights) %*% rn.corr
    #Monte Carlo price
    price.MCNormal <- exp(-r*t.exp)*mean(pmax(price.basketNormal - strike,0))
    Price.MCGBM <- exp(-r*t.exp)*mean(pmax(price.basketGBM - strike,0))
    
    
    spot.basket <- t(weights) %*% spot
    var.basket <- t(weights) %*%  cov.mat %*% weights
    sigma.basket <- sqrt(var.basket)
    
    price.AnalyticNormal <- phbsasp::CalcNormalPrice(type=type,spot=spot.basket,strike=strike,
                                              t.exp=t.exp,r=r,sigma=sigma.basket)
    price.AnalyticGBM <- phbsasp::CalcBsmPrice(type=type,spot=spot.basket,strike=strike,
                                             t.exp=t.exp,r=r,sigma=sigma.basket)
    if (model){
      price.cv <- Price.MCGBM +(price.AnalyticNormal - price.MCNormal) # Monte Carlo price with control variate 
    } else {
      price.cv <- price.MCNormal + (price.AnalyticGBM - Price.MCGBM) # Monte Carlo price with control variate
    }
    return (price.cv)
  }
}