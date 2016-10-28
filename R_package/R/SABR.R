#' Hagan's formula calculating normal model implied vol under SABR model. Currently support only beta=0
#'
#' @param strike strike price
#' @param forward forward stock price
#' @param t.exp time to expiry
#' @param sigma0 sigma0
#' @param alpha alpha vol of vol
#' @param rho rho correlation
#' @return normal implied vol
#' @examples
#' sigma0 <- 0.68/100
#' alpha <- 0.3691
#' rho <- -0.0286
#' forward <- 4.35/100
#' strike <- c( 4, seq(4.05,4.95,0.1), 5)/100
#' t.exp <- 10
#' vol <- phbsasp::CalcNormalImpvolSabrHagan(strike=strike, forward=forward, sigma0=sigma0, t.exp=t.exp, alpha=alpha, rho=rho)
#' price <- phbsasp::CalcNormalPrice(forward=forward, strike=strike, t.exp=t.exp, sigma=vol)
#' @export
CalcNormalImpvolSabrHagan <- function(forward, strike=forward, t.exp=1, sigma0, alpha=0, rho=0){

  zeta <- (forward - strike)*alpha/sigma0
  yy = sqrt(1 + zeta*(zeta-2*rho));
  chi_zeta <- rep(NaN, length(zeta))

  ind <- (abs(zeta) < 1e-5)
  chi_zeta[ind] <- 1 + (rho/2)*zeta[ind] + (1/2*rho^2-1/6)*zeta[ind]^2 + 1/8*(5*rho^2-3)*rho*zeta[ind]^3

  ind <- (zeta >= 1e-5)
  chi_zeta[ind] = log( (yy[ind] + (zeta[ind] - rho))/(1-rho) ) / zeta[ind]

  ind <- (zeta <= -1e-5)
  chi_zeta[ind] = log( (1+rho)/(yy[ind] - (zeta[ind] - rho)) )/ zeta[ind]

  vol <- sigma0 * (1 + (2-3*rho^2)/24*alpha^2*t.exp) / chi_zeta

  return(vol)
}
