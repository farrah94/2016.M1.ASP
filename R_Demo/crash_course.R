### How am I?

# Jaehyuk CHOI
browseURL("http://www.jaehyukchoi.net/teaching")

### Why use R?

# free (vs Matlab)
# open source (lots of volutarily developers distributing 'packages')
# statistics, scientific computation, graphics


## simple calculation

3 + 4

3 * 16

# Ctrl-Enter, Ctrl-Enter on selection

# Ctrl-L: clear screen

# history, partical completion (Ctrl+Up)



## variable assignment

# variables are case-sensitive

x <- 5
X <- 15

(x <- 5)

y = 8

x + y

# specia constants

TRUE 

T # same as TRUE

FALSE

F # same as FALSE

NA # Not Available, or Not Applicable

x.vec <- c(1,2)

Inf

1/0

NaN

0/0


## vector

( x.vec <- 1:10 )  # matlab style a:b:c doesn't work

( x.vec <- c(1.3, 2.4, 3.5, 0.4) )

( y.vec <- c(5,4,3,2) )

x.vec + y.vec

x.vec * y.vec

# seq

( x.vec <- seq(1, 5, 0.1) )
( x.vec <- seq(1, 100, 2) )
( x.vec <- seq(1, 5, by = 0.1) )

length(x.vec)

# rep

( y.vec <- rep(4, 10) )
( y.vec <- rep(c(3,4), 10) )

# indexing

( x.vec <- 1:10 )

x.vec[4]

x.vec[1:5]

x.vec[-1]

x.vec[-(1:5)]

x.vec[ x.vec > 5 ]

x.vec[ x.vec > 5 ] = 9
x.vec


### functions on vector

( x.vec <- rnorm(10) )

sum( x.vec )

mean( x.vec )

var(x.vec)

prod( x.vec )

exp( x.vec )

log( x.vec )


### Probability distribution

( x.vec <- seq(-2,2,by=0.1) )

d.vec <- dnorm( x.vec )

plot( x.vec, d.vec )

p.vec <- pnorm( x.vec )

plot( x.vec, p.vec )

( r.vec <- rnorm(10) )

runif(10)
dunif(c(0, 0.5, 1))
punif(c(0, 0.5, 1))

## Matrix

( x.mat <- matrix(1:20) )

( x.mat <- matrix(1:20, 4) )

( x.mat <- matrix(1:20, nrow=5) )

( x.mat <- matrix(1:20, nrow=5, byrow=T) )

## other way of creating matrix

( identity.mat <- diag( 10 ) )
( x.mat <- matrix( rnorm(25), 5 ))

## functions on matrix

exp( x.mat )

dim( x.mat )

diag(x.mat)


### matrix is also a vector (column-wise). 
### Rule-of-thumb is vector is a column vector by default

length( x.mat )

x.mat
as.vector( x.mat )

### operations between vectors (or matrix) with different sizes

( x.vec <- 1:20 )
( y.vec <- 1:4 ) 

x.vec * y.vec   # vector with smaller size is applied repeatedly.
y.vec * x.vec

( y.vec <- 1:3 ) 
x.vec * y.vec # error

( x.mat <- matrix( 1:20, nrow = 4 ) )
( y.vec <- 1:4 ) 

x.mat * y.vec  # the same applies to the matrix * vector multiplication
y.vec * x.mat


### Matrix operations

( x.mat <- matrix( 1:25, ncol=5 ) )
diag( x.mat )
t(x.mat)

x.mat[2:4,2:4]
x.mat[2:4,2:4] <- 0
x.mat

dim(x.mat)

( i.mat <- diag(5) )
( x.mat <- matrix(seq(1,25), ncol=5) )

i.mat * x.mat
i.mat %*% x.mat

( b.vec <- 1:5 )
( y.mat <- 10*diag(5) + matrix(rnorm(25), 5) )

( a.vec <- solve( y.mat, b.vec ) )   # Solve a such that Y * a = b
y.mat %*% a.vec - b.vec # confirm that Y * a = b

library("MASS")
( yinv.mat <- MASS::ginv(y.mat) )
yinv.mat %*% b.vec - a.vec # Inv(Y) * b = a

( eig <- eigen(y.mat) )
eig$values
eig$vectors

rowSums(x.mat)
rowMeans(x.mat)

colSums(x.mat)
colMeans(x.mat)

### Control sequence
val <- 2
if ( val == 1 ) {
  1
} else if( val == 2) {
  2
} else {
  3
}

### Avoid using loops if possible

i.sum <- 0
for(i in 1:10) {
  print(i)
  i.sum <- i.sum + i
} 
i.sum

( i.sum <- sum(1:10) )


### functions

# sourcing

source('functions.R')

AddTwoNumbers( 2, 5 )

LogBase( 1000, 10 )

LogBase( 1000 )

LogBase( 1000, base = 2 )


# variable scope 

AddTwoNumbersWrong( 3, 4 )

yy <- 10

AddTwoNumbersWrong( 3, 4 ) # shouldn't work, but it works


### Data frame & Linear regression

library("datasets")
browseURL( "https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/00Index.html" )

head( cars )

cars.fitted = lm(dist ~ speed, data = cars)
summary( cars.fitted )

plot( cars$speed, cars$dist, xlab = "Speed", ylab = "Stopping Distance" )
abline(cars.fitted, col="red", lw=2)

### XL, CVS file

journals = read.csv("journals.csv", header = TRUE)
journals


### Execution time

i.sum <- 0;
system.time(
  for(i in 1:1e6) {
    i.sum <- i.sum + i
  } 
)

system.time( sum(as.numeric(1:1e6)) )

system.time( exp( rnorm(1e7) ) )

### avoid resizeing vector size

( x.vec <- 1:10 )
length(x.vec)

x.vec[11] <- 11  ## no good !
length(x.vec)


### Google's R Style guide

browseURL( 'https://cran.r-project.org/manuals.html' )

browseURL( 'https://google.github.io/styleguide/Rguide.xml' )
