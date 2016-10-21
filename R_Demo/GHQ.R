ghq <- statmod::gauss.quad.prob(7, dist='normal')

z<-ghq$nodes
w<-ghq$weights

plot(z,w)

sum(w*z)

sum(w*z^2)

sum(w*z^4)

sum(w*exp(-0.5+z))
