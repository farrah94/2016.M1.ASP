#------Plot Implied Volatility Under GBM and Displaced-GBM Model------
# 
# Author: Yingxiang Li  yingxiangli@sz.pku.edu.cn
# 2016/10/29
#


# Parameter set
spot <- 100
strike <- seq(40,160,5)
t.exp <- 1.2
sigma <- 0.2
r <- 0.05

#Assignment One
#-----Normal Implied Volatility of BS prices-------------
#GetBS price of call options at different strikes
price.bs <- phbsasp::CalcBsmPrice(type="call",spot=spot,strike=strike,
                                  t.exp=t.exp,r=r,sigma=sigma)

(price.bs)
#Get normal implied volatilities of BS prices
impvol.normal <- phbsasp::CalcNormalImpvol(type="call",price=price.bs,spot=spot,
                                           strike=strike,t.exp=t.exp,r=r)
impvol.normal <- impvol.normal/spot   #convert to percentage
(impvol.normal)
#Make plot
plot( strike, impvol.normal, col ='blue',type = 'l' )
#Measure the slope of the volatility smile at the money
slope <- 1/2*(impvol.normal[14]-impvol.normal[12])/(strike[14]-strike[12])
(slope)



#Assignment Two
#----BS Implied volatility of displaced-BS prices----
#Get displaced-BS prices
l <- 10
price.displaced <- phbsasp::CalcBsmPrice(type = "call",spot=spot+l,strike=strike+l,
                                         t.exp=t.exp,r=r,sigma=sigma)
(price.displaced)
#Get BS implied volatilaties of prices under displaced GBM model
impvol.bs <- phbsasp::CalcBsmImpvol(price=price.displaced, spot=spot, 
                                            strike=strike, t.exp=t.exp, r=r)
(impvol.bs)
#Make plot
plot( strike, impvol.bs, col ='red' ,type = 'l')


#Calibrate
impvol.displaced=(impvol.bs*spot)/(spot+l)
(impvol.displaced)


