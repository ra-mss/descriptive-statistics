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