library(readxl)
#DATOS MENSUALES
data<-read_xlsx("E:/INVESTIGACION/SUICIDIO/BASEMENSUAL.xlsx")
#visualizaci?n incial y preprocesamiento de los datos y 
#estad?sticos descriptivos 
plot.default(data$Suicides, type="l", col="red",
             main="Monthly suicides 1998-2021", ylab ="Suicides",
             xlab="Date", ylim=c(0,40)) 

summary(data$Suicides)

library(moments)
kurtosis(data$Suicides)
skewness(data$Suicides)
var(data$Suicides)
sd(data$Suicides)

#se convierte la serie en un objeto tseries
serie_mensual<-ts(data$Suicides, 
                  frequency = 12, 
                  start = c(1998,1))
class(serie_mensual)
print(serie_mensual)

#se prueba la funcion plot.ts y es similar plot default
plot.ts(serie_mensual, main = "Number of suicides by month", 
        ylab="Suicides", xlab="Time", col="black")


#se desea ver los componentes de la serie de tiempo y se 
#procede a su descomposici?n 
descompuesta<-decompose(serie_mensual)
plot(descompuesta)
class(serie_mensual)
seriesintendencia=serie_mensual-descompuesta$trend
seriesinestacionalidad=serie_mensual-descompuesta$seasonal
plot(seriesintendencia)
plot(seriesinestacionalidad)

#es un suavizado exponencial con tendencia y componente
#estacional aditivo 
holt_winters<-HoltWinters(serie_mensual)
plot(HoltWinters(serie_mensual), main="Holt-Winters filtering (HoltWinters function)")
legend("topleft", legend=c("Smoothed series",
                           "Monthly series"), 
       col=c("red", "black"), lty=1, cex=0.8)

library(forecast)
prediccion<-(forecast(holt_winters))
View(prediccion$upper)
plot(forecast(holt_winters), main="Forecast for monthly suicides
     (HoltWinters function)")


residuales_holt<-residuals(holt_winters)
plot.default(residuales_holt, type="l", col="blue")
shapiro.test(residuales_holt)
#Shapiro-Wilk normality test
#data:  residuales_holt
#W = 0.98599, p-value = 0.008555
#la nula es que es una dist normal 
#rechazo que mis residuales sean normales 

#se calcula el MSE 
mse_hotl_winters<-mean(residuales_holt ^2)




#se hace una segunda pruebla de Holt-Winters
#un alfa cercano a 1 le da m?s valor a observaciones recientes
#un valor cercano a 1 en beta, supone una tendencia estable 
#una gamma cercana a 1 indica que se le da mucho peso 
#al componente estacional 
holt_winter2<-HoltWinters(serie_mensual, seasonal = "multiplicative")
residuales_holt2<-residuals(holt_winter2)
shapiro.test(residuales_holt2)
plot.default(residuales_holt2, type="l", col="blue")
plot(forecast(holt_winter2))

forecasthw2<-predict(holt_winter2, n.ahead = 24)

#UTILIZANDO LA FUNCION HW EN VEZ DE LA HOLT-WINTERS
holtwinters<-hw(serie_mensual)
forecast<-forecast(holtwinters)
summary(holtwinters)
plot(forecast, main="Forecast for the hw funcion", 
     ylab="Number of suicides", xlab="Time")
plot(holtwinters$residuals)
plot(holtwinters$fitted)
plot(holtwinters$mean)
plot(holtwinters$upper)
plot(holtwinters$lower)
mse_hw<-mean(holtwinters$residuals^2)
sse_hw<-sum(holtwinters$residuals^2)

#ESTE MODELO DA PREDICCIONES NEGATIVAS Y NO ES MEJOR 
#QUE EL ANTERIOR, POR LO QUE SE DESCARTA
holtwinters2<-hw(serie_mensual, seasonal= "multiplicative")
forecast2<-forecast(holtwinters2)
summary(holtwinters)
plot(forecast2)
plot(holtwinters2$residuals)
plot(holtwinters2$fitted)
plot(holtwinters2$mean)
plot(holtwinters2$upper)
plot(holtwinters2$lower)

#ploteo de la serie original y la serie suavizada 
#con la funcion hw
plot(serie_mensual, type = "l", xlab = "Time", 
     ylab = "Observed/Smoothed", main="Holt-Winters filtering
     (hw function)")
lines(forecast$fitted, col = "blue")
legend("topleft", legend=c("Smoothed series",
                           "Monthly series"), 
       col=c("blue", "black"), lty=1, cex=0.8)




