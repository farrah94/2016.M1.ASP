#-----------------IV normal distribution--------------

normaliv <- function(type, price, s, k=s,r=0, T){
    #-------------------------------------------------
    #-------------------------------------------------
    #Inputs: type the same as in function optionprice
    #        price: the observed option price
    #Return: implied volatility
    #-------------------------------------------------
    #-------------------------------------------------

    sub <- function(sigma){
        f <- normalprice(type = type,s =  s,k =  k,r = r,T = T,sigma = sigma)-price
        return(f)
    }
    iv <- uniroot(f = sub,interval = c(0,1))$root
    return(iv)
}
