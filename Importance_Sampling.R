# Prob( Z > 30 ) for a standard normal Z

pnorm(-30) # correct answer
1-pnorm(30) # 0 due to machine epsilon

Z <- rnorm(1e6)
sum(Z>30)

# Shift mean from 0 t0 30
X <- Z+30
mean( exp(-30*X+0.5*30^2)*(X>30) )

# Shift mean from 0 t0 32
X <- Z+31
mean( exp(-31*X+0.5*31^2)*(X>30) )

