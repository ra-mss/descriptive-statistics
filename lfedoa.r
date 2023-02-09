library(tidyverse)
library(readxl)
library(fdth)
library(modeest)
library(xlsx)

base <- read_excel('../ra_mss/Desktop/R_Projects/LIMOSAFEDOA/BASE_LIMOSA.xlsx')
head(base)

#Medidas descriptivas de variables cuantitativas en L. fedoa
statistics = matrix(c(summarise_all(base, mean, na.rm=TRUE), 
                      summarise_all(base, median, na.rm=TRUE),
                      summarise_all(base, mfv1, na.rm=TRUE),
                      summarise_all(base, range, na.rm=TRUE),
                      summarise_all(base, var, na.rm=TRUE),
                      summarise_all(base, sd, na.rm=TRUE)),
                    ncol = 6)
CoefVarP <- round(100*sd(base$PESO,na.rm=TRUE)/mean(base$PESO,na.rm=TRUE), digits = 3)
CoefVarC <- round(100*sd(base$CULMEN,na.rm=TRUE)/mean(base$CULMEN,na.rm=TRUE), digits = 3)
CoefVarCa <- round(100*sd(base$`CAB/CUL`,na.rm=TRUE)/mean(base$`CAB/CUL`,na.rm=TRUE), digits = 3)
CoefVarT <- round(100*sd(base$TARSO,na.rm=TRUE)/mean(base$TARSO,na.rm=TRUE), digits = 3)
CoefVarCuA <- round(100*sd(base$CUERDA.ALAR,na.rm=TRUE)/mean(base$CUERDA.ALAR,na.rm=TRUE), digits = 3)

#Crear tabla (agrega la columna de CV)
STATS <- cbind(statistics, c(CoefVarP, CoefVarC, CoefVarCa, CoefVarT, CoefVarCuA))

#Cambiar nombres de columnas y filas
colnames(STATS) = c("Media",
                    "Mediana",
                    "Moda",
                    "Rango",
                    "Varianza",
                    "DesvStd",
                    "CV")

row.names(STATS) = c("Peso (g)",
                     "Culmen (mm)",
                     "Cabeza total (mm)",
                     "Tarso (mm)",
                     "Cuerda alar (cm)")

#Genera un archivo de excel  con la tabla de medidas estadisticas
Estadisticos <- t(STATS) %>% 
  write.xlsx2("../ra-mss/Desktop/R_Projects/LIMOSAFEDOA/Estadisticos.xlsx", sheetName = "L. fedoa", col.names = TRUE, row.names = TRUE, showNA = TRUE)

#Tablas de frecuencia (Sturges)
peso <- fdth::fdt(base$PESO, breaks=c("Sturges"), na.rm=TRUE)
culmen <- fdth::fdt(base$CULMEN, breaks=c("Sturges"), na.rm=TRUE)
cabeza <- fdth::fdt(base$`CAB/CUL`, breaks=c("Sturges"), na.rm=TRUE)
tarso <- fdth::fdt(base$TARSO, breaks=c("Sturges"), na.rm=TRUE)
cuerdaAlar <- fdth::fdt(base$CUERDA.ALAR, breaks=c("Sturges"), na.rm=TRUE)

#Tablas de frecuencia a .xlsx
write.xlsx(peso[["table"]], "C:/Users/rpmai/Desktop/TablasFrec.xlsx", sheetName = "Peso", col.names = TRUE, row.names = FALSE, showNA = TRUE)
write.xlsx(culmen[["table"]], "C:/Users/rpmai/Desktop/TablasFrec.xlsx", sheetName = "Culmen", col.names = TRUE, row.names = FALSE, showNA = TRUE, append = TRUE)
write.xlsx(cabeza[["table"]], "C:/Users/rpmai/Desktop/TablasFrec.xlsx", sheetName = "CabezaCulmen", col.names = TRUE, row.names = FALSE, showNA = TRUE, append = TRUE)
write.xlsx(tarso[["table"]], "C:/Users/rpmai/Desktop/TablasFrec.xlsx", sheetName = "Tarso", col.names = TRUE, row.names = FALSE, showNA = TRUE, append = TRUE)
write.xlsx(cuerdaAlar[["table"]], "C:/Users/rpmai/Desktop/TablasFrec.xlsx", sheetName = "Cuerda alar", col.names = TRUE, row.names = FALSE, showNA = TRUE, append = TRUE)

#Histogramas de frecuencia
hist(base$PESO, col = c("white", "black"), 
     border = "black", cex.lab=1, cex.main=1.5, 
     cex.axis=0.8,freq = FALSE, ann = FALSE)
title(main = "Peso", xlab = "Valores de peso (g)", ylab = "Frecuencia relativa")
lines(density(base$PESO, na.rm=TRUE),lwd=3,col="red")

hist(base$CULMEN, col = c("white", "black"), 
     border = "black", cex.lab=1, cex.main=1.5, 
     cex.axis=0.8,freq = FALSE, ann = FALSE)
title(main = "Culmen", xlab = "Culmen (mm)", ylab = "Frecuencia relativa")
lines(density(base$CULMEN, na.rm=TRUE),lwd=3,col="red")

hist(base$`CAB/CUL`, col = c("white", "black"), 
     border = "black", cex.lab=1, cex.main=1.5, 
     cex.axis=0.8,freq = FALSE, ann = FALSE)
title(main = "Cabeza/Culmen", xlab = "Cabeza/Culmen (mm)", ylab = "Frecuencia relativa")
lines(density(base$`CAB/CUL`, na.rm=TRUE),lwd=3,col="red")

hist(base$TARSO, col = c("white", "black"), 
     border = "black", cex.lab=1, cex.main=1.5, 
     cex.axis=0.8,freq = FALSE, ann = FALSE)
title(main = "Tarso", xlab = "Tarso (mm)", ylab = "Frecuencia relativa")
lines(density(base$TARSO, na.rm=TRUE),lwd=3,col="red")

hist(base$CUERDA.ALAR, col = c("white", "black"), 
     border = "black", cex.lab=1, cex.main=1.5, 
     cex.axis=0.8,freq = FALSE, ann = FALSE)
title(main = "Cuerda alar", xlab = "Cuerda alar (mm)", ylab = "Frecuencia relativa")
lines(density(base$CUERDA.ALAR, na.rm=TRUE),lwd=3,col="red")

