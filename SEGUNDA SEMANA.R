AirPassengers
plot(AirPassengers)
class(AirPassengers)
descompuesta=plot(decompose(AirPassengers))
plot(descompuesta$seasonal)
plot(descompuesta$trend)
plot(descompuesta$random)
acf(AirPassengers)
pacf(AirPassengers)

library(DataSetsUni)
aire=(data_airpollution)
plot(aire, type="l")
print(aire)
class(aire)
#se crea el vector de fechas
fechas=as.Date("1973-05-01")+ 0:150
airets=ts(aire, start=c(1973,5,1), frequency=365)
airedesc=decompose(aire)
plot(airets)
acf(airets)
pacf(airets)


taxes=data_Taxes
plot(taxes, type="l")
class(taxes)
taxests=ts(taxes, start=c(2006,1), end=c(2010, 11),frequency=12)
plot(taxests, main="Impuestos pagados Egipto",
     xlab="mes", ylab="impuestos", col="blue")
class(taxests)
acf(taxests)
pacf(taxests)
