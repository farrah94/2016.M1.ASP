#' Calculate normal model implied vol
#'
#' @param type option type either 'call' or 'put'
#' @param price Price
#' @param spot current stock price
#' @param forward forward stock price
#' @param strike strike price
#' @param t.exp time to expiry
#' @param r interest rate
#' @param div dividend rate
#' @return implied vol
#' @examples
#' spot <- 100
#' strike <- 100
#' t.exp <- 1.2
#' sigma <- 0.2
#' r <- 0.05
#' price <- 20
#' vol <- phbsasp::CalcNormalImpvol(price=price, spot=spot, strike=strike, t.exp=t.exp, r=r)
#' @export
CalcNormalImpvol <- function(
  type = 'call', price, spot, forward = spot*exp((r-div)*t.exp),
  strike = forward, t.exp = 1, r = 0, div = 0
){
      #------------------------------------------------
      #------------------------------------------------
      #Input: price is option price
      #       S is current stock price (can be derived from the future price)
      #       F is the future price
      #       K is the strike price ( S by default)
      #       T is the time to maturity (1 by default)
      #       r is the risk-free rate (0 by default)
      #
      #Return: sigma is the volatility
      #------------------------------------------------
      #------------------------------------------------

  n.price = length(price)
  n.strike = length(strike)

  if( length(strike) > 1L ) {
    stopifnot(n.price == n.strike )
    strike.vec <- strike
  } else {
    strike.vec <- rep( strike, n.price )
  }

  price.forward = price * exp(r*t.exp)

  if( type == 'call' ) {
    price.straddle <- 2*price.forward - (forward - strike.vec)
  } else if( type == 'put' ) {
    price.straddle <- 2*price.forward + (forward - strike.vec)
  } else if( type == 'straddle') {
    price.straddle <- price.forward
  }

  vol <- rep(NA, n.price )

  #vectors a and b used for rational Chebyshev approximation
  a <- c(3.994961687345134e-1,
       2.100960795068497e1,
       4.980340217855084e1,
       5.988761102690991e2,
       1.848489695437094e3,
       6.106322407867059e3,
       2.493415285349361e4,
       1.266458051348246e4)

  b <- c(1.000000000000000,
       4.990534153589422e1,
       3.093573936743112e1,
       1.495105008310999e3,
       1.323614537899738e3,
       1.598919697679745e4,
       2.392008891720782e4,
       3.608817108375034e3,
      -2.067719486400926e2,
       1.174240599306013e1)

  prefactor <- sqrt(pi/(2*t.exp))

  for(k in 1:n.price) {

    #implied volatility when current stock price
    #is different from the strike price

    #variable v which is bounded in the range [-1, 1],
    #since the straddle price is always worth more
    #than the intrinsic value |F-K|.
    v <- abs( forward - strike.vec[k] ) / price.straddle[k]

    #transformation of v used for better
    #approximation of h

    if( v < 1e-8 ) {
      nu <- 1/(1+v*v/3)
    } else {
      nu <- v/atanh(v)
    }

    #summands sum.a and sum.b used to calculate h(n)

    sum.a <- (((((((a[8]*nu+a[7])*nu+a[6])*nu+a[5]))*nu+a[4])*nu+a[3])*nu+a[2])*nu+a[1]

    sum.b <- (((((((((b[10]*nu+b[9])*nu+b[8])*nu+b[7]))*nu+b[6])*nu+b[5])*nu+b[4])*nu+b[3])*nu+b[2])*nu+b[1]

    #approximation of h(n)
    #implied volatility
    vol[k] <- prefactor * price.straddle[k] * sqrt(nu) * (sum.a/sum.b)
  }

  return(vol)
}
