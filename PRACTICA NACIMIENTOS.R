plot(SERIE_NACIMIENTOS$nacimientos, type="l")
nacimientos=SERIE_NACIMIENTOS$nacimientos
class(nacimientos)
serie_mensual<-ts(nacimientos, 
                 frequency = 12, 
                 start = c(2000,1))
class(serie_mensual)
print(serie_mensual)
descompuesta=decompose(serie_mensual)
plot(descompuesta)
#desestacionalizo (quito componente estacional)
desestacionalizada=serie_mensual-descompuesta$seasonal
plot(desestacionalizada, type="l")
library(tseries)
adf.test(desestacionalizada)
#.51 
#existe raíz unitaria (nula)
#resultado de la prueba: no rechazo que exista raiz unitaria
#no es estacionaria 

kpss.test(desestacionalizada)
#.01 
#nula la serie es estacionaria 
#resultado de la prueba: rechazo h0 y concluyo que la 
#serie es no estacionaria 

diferenciada=diff(desestacionalizada)
plot(diferenciada, type="l")
adf.test(diferenciada)
#resultado rechazo ho y concluyo que no existe raiz unitaria y 
#que la serie es estacionaria 

kpss.test(diferenciada)
#resultado de la prueba: no rechazo ho y concluyo que la serie
# es estacionaria 
