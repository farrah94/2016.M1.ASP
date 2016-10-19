#-------------------implied volatility----------------

bsiv <- function(type, price, s, k,r, b, T){
    #-------------------------------------------------
    #-------------------------------------------------
    #Inputs: type the same as in function optionprice
    #        price: the observed option price
    #Return: implied volatility
    #-------------------------------------------------
    #-------------------------------------------------

    sub <- function(sigma){
        f <- bsprice(type = type,s = s,k = k,r = r, b=b, T = T,sigma = sigma)[1]-price
        return(f)
    }
    iv <- uniroot(f = sub,interval = c(0,1))$root
    return(iv)
}
