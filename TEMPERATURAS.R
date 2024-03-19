#ARCHIVOS PARA CORRELACIÓN ENTRE TEMPERATURA Y MUNICIPIO 
#SALTILLO 
library(readxl)
#DATOS MENSUALES
saltillo<-read_xlsx("H:/INVESTIGACION/SUICIDIO/TEMPSALTILLO.xlsx")
tempsaltillo<-data.frame(saltillo$Promedio, saltillo$Suicides)
corrsaltillo<-cor(tempsaltillo$saltillo.Promedio, 
    tempsaltillo$saltillo.Suicides)


torreon<-read_xlsx("H:/INVESTIGACION/SUICIDIO/TEMPTORREON.xlsx")
temptorreon<-data.frame(torreon$Promedio, torreon$Suicides)
corrtorreon<-cor(temptorreon)
#(temptorreon$torreon.Promedio, temptorreon$torreon.Suicides)


ramos<-read_xlsx("H:/INVESTIGACION/SUICIDIO/TEMPRAMOS.xlsx")
tempramos<-data.frame(ramos$Suicides, ramos$Promedio)
corrramos<-cor(tempramos$ramos.Suicides, tempramos$ramos.Promedio)


monclova<-read_xlsx("H:/INVESTIGACION/SUICIDIO/TEMPMONCLOVA.xlsx")
tempmonclova<-data.frame(monclova$Suicides, monclova$Promedio)
corrmonclova<-cor(tempmonclova)

piedras<-read_xlsx("H:/INVESTIGACION/SUICIDIO/TEMPPIEDRAS.xlsx")
temppiedras<-data.frame(piedras$Promedio, piedras$Suicides)
corrpiedras<-cor(temppiedras)

frontera<-read_xlsx("H:/INVESTIGACION/SUICIDIO/TEMPFRONTERA.xlsx")
tempfrontera<-data.frame(frontera$Promedio, frontera$Suicides)
corrfrontera<-cor(tempfrontera)

matamoros<-read_xlsx("H:/INVESTIGACION/SUICIDIO/TEMPMATAMOROS.xlsx")
tempmatamoros<-data.frame(matamoros$Promedio, matamoros$Suicides)
corrmatamoros<-cor(tempmatamoros)

muzquiz<-read_xlsx("H:/INVESTIGACION/SUICIDIO/TEMPMUZQUIZ.xlsx")
tempmuzquz<-data.frame(muzquiz$Promedio, muzquiz$Suicides)
corrmuzquiz<-cor(tempmuzquz)

acuña<-read_xlsx("H:/INVESTIGACION/SUICIDIO/TEMPACUÑA.xlsx")
tempacuña<-data.frame(acuña$Promedio,acuña$Suicides)
corracuña<-cor(tempacuña)

sanpedro<-read_xlsx("H:/INVESTIGACION/SUICIDIO/TEMPSANPEDRO.xlsx")
tempsanpedro<-data.frame(sanpedro$Promedio, sanpedro$Suicides)
cor(tempsanpedro)
corrsanpedro<-cor.test(tempsanpedro$sanpedro.Promedio, 
         tempsanpedro$sanpedro.Suicides)

coahuila<-read_xlsx("H:/INVESTIGACION/SUICIDIO/TEMPERATURAS.xlsx")
temperaturas<-data.frame(coahuila$Suicides, coahuila$Promedio)
corrgeneral<-cor(temperaturas)


library(lmtest)
#LA NULA ES QUE NO CAUSA
causalidadgeneral<-grangertest(coahuila$Suicides
                               ~ coahuila$Promedio, 
                               data = temperaturas, 
                               order = 2)

#NO CAUSA NI EXAGERANDO LOS REZAGOS 
causalidadsaltillo<-grangertest(tempsaltillo$saltillo.Suicides
                                ~tempsaltillo$saltillo.Promedio)

causalidadtorreon<-grangertest(temptorreon$torreon.Suicides
                               ~temptorreon$torreon.Promedio, 
                               order = 4)

causalidadmonclovaa<-grangertest(tempmonclova$monclova.Suicides
                                ~tempmonclova$monclova.Promedio, 
                                order = 4)

#CAUSA CON ORDEN 6 AL 10%
causalidadramos<-grangertest(tempramos$ramos.Suicides
                             ~tempramos$ramos.Promedio, 
                             order = 6)

#CAUSA AL 10 CON ORDEN 4 
causalidadpiedras<-grangertest(temppiedras$piedras.Suicides
                               ~temppiedras$piedras.Promedio, 
                               order = 4)

#ESTE CAUSA DESDE EL ORDEN 1
causalidadacuña<-grangertest(tempacuña$acuña.Suicides 
                             ~tempacuña$acuña.Promedio, 
                             order = 1)

#causa con orden 10 al 10
causalidadmuzquiz<-grangertest(tempmuzquz$muzquiz.Suicides
                               ~tempmuzquz$muzquiz.Promedio, 
                               order = 10)

#ESTE SIEMPRE CAUSA 
causalidadfrontera<-grangertest(tempfrontera$frontera.Suicides
                                ~tempfrontera$frontera.Promedio, 
                                order = 1)

#ESTE CAUSA EN EL ORDEN 5
causalidadmatamoros<-grangertest(tempmatamoros$matamoros.Suicides
                                 ~tempmatamoros$matamoros.Promedio, 
                                 order = 5)

#este causa desde el 1 
causalidadsanpedro<-grangertest(tempsanpedro$sanpedro.Suicides~
                                  tempsanpedro$sanpedro.Promedio, 
                                order = 1)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

library(corrplot)
cor(temperaturas)
corrplot(corrgeneral, method="shade",
         shade.col = NA, tl.col = "black", tl.srt = 45,
        addCoef.col = "black", 
         cl.pos = "n", order = "AOE")
matrizcorrelaciones<-matrix(c(corracuña, corrfrontera,
                              corrmatamoros, corrmonclova, 
                              corrmuzquiz, corrpiedras, 
                              corrsanpedro$estimate,
                              corrtorreon$estimate, corrramos, 
                              corrsaltillo))
nombres<-c("Acuña", "Frontera", "Matamoros", 
           "Monclova", "Muzquiz", "Piedras Negras", 
           "San Pedro", "Torreón", "Ramos Arizpe", 
           "Saltillo")
corrplot(matrizcorrelaciones, method="shade", 
         type = "lower", 
         tl.col = "black", tl.srt = 45, diag = FALSE, 
         cl.pos = "n", addCoef.col = "black", 
         addCoefasPercent = TRUE, p.mat = NULL,
         col = colorRampPalette(c("#FAD4D4", "#F7A3A3", "#FF0000"))(100),
         title = "Correlaciones", tl.cex = 0.8, tl.offset = 1,
         mar = c(1, 1, 2, 1))
