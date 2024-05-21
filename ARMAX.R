# Cargar las librerías necesarias
library(astsa)
library(forecast)

# Cargar la serie temporal AirPassengers
data("AirPassengers")

# Simulamos una serie temporal para la variable exógena (por ejemplo, índice de producción industrial)
set.seed(123) # Para reproducibilidad
industrial_production <- ts(rnorm(length(AirPassengers), mean = 100, sd = 10), 
                            start = start(AirPassengers), 
                            frequency = frequency(AirPassengers))

# Extendemos la serie de producción industrial para el período de predicción
industrial_production_extended <- ts(rnorm(length(AirPassengers) + 12, mean = 100, sd = 10), 
                                     start = start(AirPassengers), 
                                     frequency = frequency(AirPassengers))

# Graficar las series temporales
#par(mfrow=c(2,1))
plot(AirPassengers, main="Número de pasajeros de aerolínea", ylab="Pasajeros", xlab="Tiempo")
plot(industrial_production, main="Índice de Producción Industrial (Simulado)", ylab="Producción Industrial", xlab="Tiempo")

# Ajustar un modelo ARMAX
# Utilizamos la función auto.arima del paquete forecast, que permite incluir regresores externos (xreg)
model_armax <- auto.arima(AirPassengers, xreg = industrial_production)

# Resumen del modelo
summary(model_armax)

# Predecir los próximos 12 meses
forecast_armax <- forecast(model_armax, xreg = industrial_production_extended[(length(AirPassengers) + 1):(length(AirPassengers) + 12)], h = 12)

# Graficar las predicciones
plot(forecast_armax, main="Pronóstico ARMAX para AirPassengers")

