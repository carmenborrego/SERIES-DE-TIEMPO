library(vars)
data("Canada")
plot(Canada[, "U"], main='Desempleo', xlab='Mes/A?o', ylab='Tasa')
lines(rollmean(Canada[, "U"], 4), col="red", lwd=2)
lines(rollmean(Canada[, "U"], 4,align="right"), col="blue", lwd=2)
legend("bottomleft", c("Original", "Media movil centrada",
                       "Media móvil no centrada"),
       lwd=c(1,2,2), col=c("black", "red", "blue"))
grid()

#Medias M?viles
#Alisado MA centrados
alisado1 = filter(Canada[, "U"], rep(1,4)/4, side=2)
#Alisado MA no centrados
alisado2 = filter(Canada[, "U"], rep(1,4)/4, side=1)
#Hacemos la gr?fica para comparar
plot.ts(Canada[, "U"],main="Desempleo",xlab="Tiempo",ylab="Tasa")
lines(alisado1, col="red")
lines(alisado2, col="blue")
legend("bottomleft", c("Original", "Media m?vil centrada",
                       "Media m?vil no centrada"),
       lwd=c(1,2,2), col=c("black", "red", "blue"))
grid()



library(forecast)

