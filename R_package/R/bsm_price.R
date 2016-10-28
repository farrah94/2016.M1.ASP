#' Calculate Black-Scholes-Merton option price
#'
#' @param type option type either 'call' or 'put'
#' @param spot current stock price
#' @param forward forward stock price
#' @param strike strike price
#' @param t.exp time to expiry
#' @param r interest rate
#' @param div dividend rate
#' @param sigma volatility
#' @return option price
#' @examples
#' spot <- 100
#' strike <- seq(80,125,5)
#' t.exp <- 1.2
#' sigma <- 0.2
#' r <- 0.05
#' price <- CalcBsmPrice(spot=spot, t.exp = t.exp, sigma=sigma, strike=strike, r=r)
#' @export
CalcBsmPrice <- function(
  type = 'call', spot, forward = spot*exp((r-div)*t.exp),
  strike = forward, t.exp = 1, r = 0, div = 0, sigma
){
    #------------------------------------------------
    #------------------------------------------------
    #Imput: type("call","put","straddle","digit")
    #       s is current price
    #       k is the strike price
    #       T is the time to maturity
    #       r is the risk-free rate
    #       b is the cost of carry (b equals 0 for a future)
    #       sigma is the volatility
    #Return: the option price
    #        delta of the option
    #------------------------------------------------
    #------------------------------------------------

    stdev <- sigma*sqrt(t.exp)
    d1 <- log(forward/strike)/stdev +0.5*stdev
    d2 <- d1 - stdev
    disc.factor <- exp(-r*t.exp)

    pnorm.d1 <- pnorm(d1)
    pnorm.d2 <- pnorm(d2)

    if (type == "call" ){
        price <- forward*pnorm.d1 - strike*pnorm.d2
        delta <- pnorm.d1
    }else if (type == "put"){
        price <- strike*(1-pnorm.d2) - forward*(1-pnorm.d1)
        delta <- pnorm.d1 - 1
    }else if (type == "straddle"){
        price <- forward*(2*pnorm.d1 - 1) - strike*(2*pnorm.d2 - 1)
        delta <- 2*pnorm.d1 - 1
    }else if (type == "digit"){
        price <- pnorm.d2
        delta <- 1/sqrt(2*pi)*exp(-d2^2/2)/(s*stdev)
    }
    return( disc.factor * price )
}
