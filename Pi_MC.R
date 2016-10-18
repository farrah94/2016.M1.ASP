xy <- matrix(runif(1e6),ncol=2)

head(xy)
par(pty="s")
plot(xy, type="p", pch=".")

xy <- matrix(runif(1e6),ncol=2)
sum( rowSums(xy*xy)<1 ) / nrow(xy) * 4


# Performace test: dummy vs anti-thetic method

set1 <-rep(0,100)
set2 <-rep(0,100)

for(k in 1:100) {
  set1[k] <- GetPiMC()
  set2[k] <- GetPiMCAntiThetic()
}

c(mean(set1), var(set1))
c(mean(set2), var(set2))

hist(set1, col='blue')
hist(set2, col='red', add=T)

