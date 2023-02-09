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

