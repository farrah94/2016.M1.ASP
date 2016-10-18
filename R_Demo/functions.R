
DiffSqrtBad <- function(x=0) {
  return(sqrt(x+1) - sqrt(x))
}

DiffSqrtGood <- function(x=0) {
  return( 1/(sqrt(x+1) + sqrt(x)) )
}

GetPiMC <- function( n.sample = 1e5 ) {
  xy <- matrix(runif(n.sample), ncol=2)
  pi.mc = sum( rowSums(xy*xy)<1 ) / nrow(xy) * 4
  return( pi.mc )
}

GetPiMCAntiThetic <- function( n.sample = 1e5 ) {
  xy <- matrix(runif(n.sample), ncol=2)
  x2y2 <- rowSums(xy*xy)
  pi.mc <- sum( x2y2<1 ) + sum( x2y2-2*xy[,1]<0 ) + sum( x2y2-2*xy[,2]<0 ) + 
    sum( x2y2-2*xy[,1]-2*xy[,2]+1<0 )
  pi.mc = pi.mc / nrow(xy)
  return( pi.mc )
}