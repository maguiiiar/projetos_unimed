require(dplyr)
require(psych)
require(readxl)
require(data.table)
require(tidyr)
require(openxlsx)
require(stringr)

setwd("C:/Users/mrrezende/Documents/ProjetosUnimed/Arquivos (.txt, .csv)/Planilhas Espaço Viver Bem")

benefppato <- read.xlsx("Beneficiarios por patologia.xlsx",sheet = 1, 
                           startRow = 1, colNames = TRUE,na.strings ="NA")
