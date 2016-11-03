##Assignment 1
spot=80
strike=seq(from=70,to=90,by=2)
nlength=length(strike)
type="call"
t.exp=1
r=0.05
div=0
sigma=0.1
callprice=phbsasp::CalcBsmPrice(spot=spot,t.exp=t.exp,sigma=sigma,strike=strike,r=r)
ImV=phbsasp::CalcNormalImpvol(type,price=callprice,strike=strike,spot=spot,t.exp=t.exp,r=r) # [JC] pass strike
delta=(ImV[2:nlength]-ImV[1:(nlength-1)])/2
plot(strike,ImV)

##Assignment2
L=10
spot=80
strike=seq(from=70,to=90,by=2)
nlength=length(strike)
type="call"
t.exp=1
r=0.05
div=0
sigma=0.1
sigma_L=(sigma*spot)/(spot+L) # [JC] compute the 'estimated' sigma_L to match the price or exactly solve
callprice=phbsasp::CalcBsmPrice(spot=spot+L,t.exp=t.exp,sigma=sigma_L,strike=strike+L,r=r)
ImV=phbsasp::CalcBsmImpvol(type=type,price=callprice,spot=spot,strike=strike,t.exp=t.exp,r=r)
plot(strike,ImV)
grid(lw=2)
