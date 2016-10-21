## Testing BSM price & implied vol functions

spot <- 100
strike <- seq(80,125,5)
t.exp <- 1.2
sigma <- 0.2
r <- 0.05

# Compute Price on different strikes
( price <- CalcBsmPrice(spot=spot, t.exp = t.exp, sigma=sigma, strike=strike, r=r) )

# Compute implied volatility
( impvol <- CalcBsmImpvol(price=price, spot=spot, strike=strike, t.exp=t.exp, r=r) )



