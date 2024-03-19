library(astsa)
jj
class(jj)
plot.ts(jj, main="rendimientos", xlab="time", ylab="yield", col="blue")
descompuestajj=decompose(jj)
plot(descompuestajj)

plot.ts(polio)
descompuestapolio=decompose(polio)
plot(descompuestapolio)

plot.ts(gdp)
descompuestagdp=decompose(gdp)
plot(descompuestagdp)
