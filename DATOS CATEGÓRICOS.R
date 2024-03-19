library(readxl)
categoricos<-read_xlsx("E:/INVESTIGACION/SUICIDIO/BASEDEFINITIVA.xlsx")

municipio<-factor(categoricos$Municipality)
levels(municipio)
table(municipio)
class(municipio)


sexo<-factor(categoricos$SEX)
table(sexo)

estado_civil<-factor(categoricos$EDO_CIVIL)
table(estado_civil)

edad<-factor(categoricos$age)
table(edad)
edad_numerico<-as.numeric(as.character(edad))

class(edad_numerico)
grupos_edad<-cut(edad_numerico, breaks = c(4010, 4017, 4029, 4045,
                                           4059, 4097 ),
                 labels=c("Underage", "Young Adult", "Adult", 
                          "Mature", "Elderly"))
print(grupos_edad)
levels(grupos_edad)
tabla_frecuencia<-table(grupos_edad)

print(tabla_frecuencia)

mes<-factor(categoricos$MONTH)
table(mes)

dia<-factor(categoricos$DAY)
table(dia)

causa<-factor(categoricos$CAUSE)
table(causa)
levels(causa)
class(causa)


escolaridad<-factor(categoricos$ESCOLARIDA)
table(escolaridad)

lugar<-factor(categoricos$LUGAR_OCUR)
table(lugar)

#se descarta esta variable para el modelo machine learning 
#porque la mayor?a son lugares no especificado 
#solamente se puede decir que la mayor?a de casos 
#clasificados ocurrieron en una vivienda colectiva
#pero se cuenta para tener referencia 
trabajo<-factor(categoricos$OCURR_TRAB)
table(trabajo)


#quito ocupaci?n porque la base no est? homologada 
#cambi? con los a?os y no es posible conjuntarla 
#de forma r?pida
ocupacion<-factor(categoricos$OCUPACION)
table(ocupacion)
levels(ocupacion)


#GRAFICOS CATEGORICOS 
library(plotly)
class(categoricos)
municipios<-data.frame(Municipio=factor(c(municipio)))
ggplot(municipios, aes(x=municipio))+
  geom_bar(aes(y = ..count..), stat = "count")

municipios<-read_xlsx("E:/INVESTIGACION/SUICIDIO/MUNICIPIOS.xlsx")
barras<-barplot.default(municipios$`Numer of Suicides`, 
                main = "Suicides per Municipality", 
                ylab = "Suicides",
                names.arg = municipios$Municapality, las = 2,
                legend.text = TRUE, 
                ylim = c(0, 1200))
library(RColorBrewer)
help(RColorBrewer)
palette()
display.brewer.all() 
leyenda_municipios<-c(municipios$Municapality)
azul<-brewer.pal(11, "Spectral")
#gray<-gray(seq(0.2, 0.8, length=11))
par(pin=c(5,5.5))
dev.new()          
pie(municipios$Percentage, 
    main= "Percentage of suicides by municipality", 
    col = azul,
    #col = rainbow(length(municipios$Municapality)),
    labels=(municipios$Percentage), cex = 0.6)
legend("topright", legend=leyenda_municipios, cex=.7, fill=azul)
       #fill=rainbow(length(leyenda)))
par(pin=c(7,7))

causas<-read_xlsx("E:/INVESTIGACION/SUICIDIO/CAUSA.xlsx")
leyenda_causas<-c(causas$`Cause of death`)
orange<-brewer.pal(5, "Oranges")
#graycausas<-gray(seq(0.2, 0.8, length=5))
par(pin=c(5,5.5))
dev.new()
pie(causas$Percentage, 
    main="Percentage of suicides by cause", 
    col=orange, labels=(causas$`Percentage`), cex=0.8)
legend("topright", legend=leyenda_causas, cex=.7, fill= orange)
par(pin=c(7,7))

edades<-read_xlsx("E:/INVESTIGACION/SUICIDIO/EDADES.xlsx")
leyenda_edades<-c(edades$`Age group`)
purples<-brewer.pal(6, "Purples")
#grayedades<-gray(seq(0.1, 0.9, length=6))
par(pin=c(5,5.5))
dev.new()
#par(family="Times")
pie(edades$Percentage, main = "Percentage of suicide by age group",
    col=purples, labels=(edades$Percentage), cex=0.8,  )
legend("topright", legend=leyenda_edades, cex=0.7, fill=purples)
par(pin=c(7,7))

escolaridad<-read_xlsx("E:/INVESTIGACION/SUICIDIO/ESCOLARIDAD.xlsx")
leyenda_escolaridad<-c(escolaridad$Level)
set3multi<-brewer.pal(11, "Set3")
#grayescolaridad<-gray(seq(0, 1, length=11))
par(pin=c(5,5.5))
dev.new()
pie(escolaridad$Percentage, 
    main="Percentage of suicides by educational level",
    col=set3multi, labels=(escolaridad$Percentage), cex=0.8)
legend("topright", legend=leyenda_escolaridad, cex=0.7,
       fill=set3multi)
par(pin=c(7,7))

marital<-read_xlsx("E:/INVESTIGACION/SUICIDIO/MARITAL.xlsx")
leyenda_marital<-c(marital$`Marital status`)
green<-brewer.pal(8, "Greens")
#graymarital<-gray(seq(0.1, 0.9, length=8))
par(pin=c(5,5.5))
dev.new()
pie(marital$Percentage, main="Percentage of suicides by marital status", 
    col=green, labels=(marital$Percentage), cex=0.8,
    clockwise = TRUE)
legend("topright", legend=leyenda_marital, cex=0.6,
       fill=green)
par(pin=c(7,7))

place<-read_xlsx("E:/INVESTIGACION/SUICIDIO/PLACES.xlsx")
leyenda_place<-c(place$`Place of ocurrence`)
set3<-brewer.pal(10, "Set3")
#grayplace<-gray(seq(0.1, 0.9, length=10))
par(pin=c(5,5.5))
dev.new()
pie(place$Percentage, 
     main= "Percentage of suicides by place of ocurrence", 
     col=set3, labels=(place$Percentage), cex=0.7, fill=set3)
legend("topright", legend=leyenda_place, cex=0.6, 
       fill=set3)


