#multiroot

sigma.alpha.rho <- function(forward, strike, t.exp){
  library(phbsasp)
  library(rootSolve)
  sub <- function(x){
    sigma <- phbsasp::CalcNormalImpvolSabrHagan(forward=spot, strike = strike, t.exp = t.exp, sigma0 = sigma0, alpha = alpha, rho = rho)
    f1 <- phbsasp:::CalcNormalImpvolSabrHagan(forward=spot, strike = strike[1], t.exp = t.exp, sigma0 = x[1], alpha = x[2], rho = x[3]) - sigma[1]
    f2 <- phbsasp:::CalcNormalImpvolSabrHagan(forward=spot, strike = strike[2], t.exp = t.exp, sigma0 = x[1], alpha = x[2], rho = x[3]) - sigma[2]
    f3 <- phbsasp:::CalcNormalImpvolSabrHagan(forward=spot, strike = strike[3], t.exp = t.exp, sigma0 = x[1], alpha = x[2], rho = x[3]) - sigma[3]
    return(c(f1, f2, f3))
  }
  values <- multiroot(f = sub, c(0.6, 0.3, -0.02))$root
  names(values) <- c('sigma0',"rho",'alpha')
  return(values)
}