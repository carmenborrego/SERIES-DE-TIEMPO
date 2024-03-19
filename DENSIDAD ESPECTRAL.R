
library(astsa) 
par(mfrow=c(3,1))
arma.spec(ar=0, ma=0, main="White Noise")
arma.spec(ma=.5, main="Moving Average")
arma.spec(ar=c(1,-.9), main="Autoregression")

arma.spec(ar=c(1,-.9), xlim=c(.15,.151), n.freq=100000)

