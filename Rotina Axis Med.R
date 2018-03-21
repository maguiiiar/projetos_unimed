require(data.table)

dados[i] <- 
dados1 <- fread("Layout da base de Contas M?dicas (HRP) Colab 201705.txt", sep = "|", h = TRUE, na.strings = "")
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
