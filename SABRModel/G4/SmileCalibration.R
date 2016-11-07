SabrCalibration <- function(strike,sigma,forward,t.exp=1){
  #return the calibration parameter sigma0,rho,and alpha in SABR model
  library(rootSolve)
  library(phbsasp)
  subfunction <- function(x){
    delta1<-sigma[1]-CalcNormalImpvolSabrHagan(sigma0 = x[1],rho = x[2],alpha = x[3],forward =  forward,t.exp =  t.exp,strike = strike[1])
    delta2<-sigma[2]-CalcNormalImpvolSabrHagan(sigma0 = x[1],rho = x[2],alpha = x[3],forward =  forward,t.exp =  t.exp,strike = strike[2])
    delta3<-sigma[3]-CalcNormalImpvolSabrHagan(sigma0 = x[1],rho = x[2],alpha = x[3],forward =  forward,t.exp =  t.exp,strike = strike[3])
    return(c(delta1,delta2,delta3))
  }
  solver <- multiroot(f = subfunction,start = c(0.3,0,0.3))
  root <- solver$root
  names(root) <- c('sigma0',"rho",'alpha')
  return(root)
}
