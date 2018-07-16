require(dplyr)
require(psych)
require(stringr)
require(caret)
require(data.table)

setwd("C:/Users/mrrezende/Documents/despesas por benef/")

despesas_dyad <- fread("despesas_bind.txt", h=T, sep="\t",
                     encoding = "UTF-8", na.strings = c("-",NA)) 
#despesas do dyad

