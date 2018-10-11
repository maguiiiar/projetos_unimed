require(data.table)
require(dplyr)

dados.drg <- fread("DRG - 201701 a 201807 - COM UTI + CONDIÇÃO ADQ.txt", sep = ";", header = TRUE, dec = ",", na.strings = c("", NA))