require(data.table)
require(base)
i=1
dados1 <- fread("Axis2014_4.tab", sep = "\t", h = TRUE, na.strings = "")
assign(oname,  fread(sprintf("Axis%d_%d.tab", ano, i),  sep = "\t", h = TRUE, na.strings = ""))

ano = 2016
trimestres = 4

 i=1
 for(i in 1:trimestres){
 sprintf("dados.%d.%d", ano, i) = fread(sprintf("Axis%d_%d.tab", ano, i),  sep = "\t", h = TRUE, na.strings = "")
 a = if(i < 2) sprintf("dados.%d.%d", ano, i) else 
 }
 
 assign(sprintf("dados.%d", ano), sprintf("dados.%d", ano),  sep = "\t", h = TRUE, na.strings = ""))


dados.2015 = bind_rows(dados.2015.1, dados.2015.2, dados.2015.3)
 



dados2 <- fread("Layout da base de Contas M?dicas (HRP) Colab 201706.txt", sep = "|", h = TRUE, na.strings = "")
dados3 <- fread("Layout da base de Contas M?dicas (HRP) Colab 201707.txt", sep = "|", h = TRUE, na.strings = "")
dados4 <- fread("Layout da base de Contas M?dicas (HRP) Colab 201708.txt", sep = "|", h = TRUE, na.strings = "")
dados5 <- fread("Layout da base de Contas M?dicas (HRP) Colab 201709.txt", sep = "|", h = TRUE, na.strings = "")
dados6 <- fread("Layout da base de Contas M?dicas (HRP) 201705.txt", sep = "|", h = TRUE, na.strings = "")
dados7 <- fread("Layout da base de Contas M?dicas (HRP) 201706.txt", sep = "|", h = TRUE, na.strings = "")
dados8 <- fread("Layout da base de Contas M?dicas (HRP) 201707.txt", sep = "|", h = TRUE, na.strings = "")
dados9 <- fread("Layout da base de Contas M?dicas (HRP) 201708.txt", sep = "|", h = TRUE, na.strings = "")
dados10 <- fread("Layout da base de Contas M?dicas (HRP) 201709.txt", sep = "|", h = TRUE, na.strings = "")

dados <- rbind(dados1, dados2)
dados <- rbind(dados, dados3)

colnames(dados1)
colnames(dados2)
colnames(dados3)
colnames(dados4)
colnames(dados5)
