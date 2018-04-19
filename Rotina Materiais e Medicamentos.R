require(data.table)
require(dplyr)

base.mat.cias = fread("Tabela Unimed Uberlândia de Materiais Cias - Versão 1.14 - Vigência 12.04.2018.csv", h = T, sep=";")
base.mat.clh = fread("Tabela Unimed Uberlândia de Materiais CLH-Versão 2.24 - Vigência - 12.04.2018.csv", h = T, sep=";")
base.mat.hosp = fread("Tabela Unimed Uberlândia de Materiais Hospitalares - Versão 5.7 - Vigência 13.04.2018.csv", h=T, sep=";")
base.mat.onco = fread("Tabela Unimed Uberlândia de Materiais Oncológicos Versão-2.12 -  Vigência - 12.04.2018.csv", h=T, sep=";")

base.med.cias = fread("Tabela Unimed Uberlândia de Medicamentos Cias - Versão 1.9.5 - Vigência 01.04.2018.csv", h=T, sep = ";")
base.med.clh = fread("Tabela Unimed Uberlândia de Medicamentos CLH - Versão 7.23 - Vigência 01-04-2018.csv", h=T, sep=";")
base.med.hosp = fread("Tabela Unimed Uberlândia de Medicamentos Hospitalar - Versão 10.7 - Vigência. 01.04.2018.csv", h=T, sep=";")
base.med.onco = fread("Tabela Unimed Uberlandia de Medicamentos Oncológicos- Versão 4.13 - Vigência 01.04.2018.csv", h=T, sep=";")

base.mat.cias = base.mat.cias %>% select(`Código - versão TISS 3.03.03`, `Nome Comercial`, `Descrição do Produto`, `Especialidade do Produto`, `Valor Pagamento`) %>%
                                  rename("código" = `Código - versão TISS 3.03.03`, nome = `Nome Comercial`, descrição = `Descrição do Produto`, especialidade  = `Especialidade do Produto`, valor = `Valor Pagamento`) %>%
                                  mutate(Versão = "CIAS", Tipo = "Material")     
base.mat.clh = base.mat.clh %>% select(`Código - versão TISS 3.03.03`, `Nome Comercial`, `Descrição do Produto`, `Especialidade do Produto`, `Valor Pagamento`) %>%
                                  rename("código" = `Código - versão TISS 3.03.03`, nome = `Nome Comercial`, descrição = `Descrição do Produto`, especialidade  = `Especialidade do Produto`, valor = `Valor Pagamento`) %>%
                                  mutate(Versão = "CLH", Tipo = "Material")     
base.mat.hosp = base.mat.hosp %>% select(`Código - versão TISS 3.03.03`, `Nome Comercial`, `Descrição do Produto`, `Especialidade do Produto`, `Valor Pagamento`) %>%
                                  rename("código" = `Código - versão TISS 3.03.03`, nome = `Nome Comercial`, descrição = `Descrição do Produto`, especialidade  = `Especialidade do Produto`, valor = `Valor Pagamento`) %>%
                                  mutate(Versão = "Hospitalar", Tipo = "Material")     
base.mat.onco = base.mat.onco %>% select(`Código - versão TISS 3.03.03`, `Nome Comercial`, `Descrição do Produto`, `Especialidade do Produto`, `Valor Pagamento`) %>%
                                  rename("código" = `Código - versão TISS 3.03.03`, nome = `Nome Comercial`, descrição = `Descrição do Produto`, especialidade  = `Especialidade do Produto`, valor = `Valor Pagamento`) %>%
                                  mutate(Versão = "Oncológico", Tipo = "Material")    

base.med.cias = base.med.cias %>% select(`Código - versão TISS 3.03.02`, `Nome e Apresentação Comercial`, `Grupo Farmacológico`, `Valor Pagamento`) %>%
                                  rename("código" = `Código - versão TISS 3.03.02`, nome = `Nome e Apresentação Comercial`, grupo = `Grupo Farmacológico`, valor = `Valor Pagamento`) %>%
                                  mutate(Versão = "CIAS", Tipo = "Medicamento") 
base.med.hosp = base.med.hosp %>% select(`Código - versão TISS 3.03.03`, `Nome e Apresentação Comercial`, `Grupo Farmacológico`, `Valor Pagamento`) %>%
                                  rename("código" = `Código - versão TISS 3.03.03`, nome = `Nome e Apresentação Comercial`, grupo = `Grupo Farmacológico`, valor = `Valor Pagamento`) %>%
                                  mutate(Versão = "Hospitalar", Tipo = "Medicamento") 
base.med.clh = base.med.clh   %>% select(`Código - versão TISS 3.03.03`, `Nome e Apresentação Comercial`, `Grupo Farmacológico`, Valor) %>%
                                  rename("código" = `Código - versão TISS 3.03.03`, nome = `Nome e Apresentação Comercial`, grupo = `Grupo Farmacológico`, valor = Valor) %>%
                                  mutate(Versão = "CLH", Tipo = "Medicamento") 
base.med.onco = base.med.onco %>% select(`Código - versão TISS 3.03.02`, `Nome e Apresentação Comercial`, `Grupo Farmacológico`, Valor) %>%
                                  rename("código" = `Código - versão TISS 3.03.02`, nome = `Nome e Apresentação Comercial`, grupo = `Grupo Farmacológico`, valor = Valor) %>%
                                  mutate(Versão = "Oncológico", Tipo = "Medicamento") 

base.mat = rbind(base.mat.cias, base.mat.clh, base.mat.hosp, base.mat.onco)
base.med = rbind(base.med.cias, base.med.clh, base.med.hosp, base.med.onco)
###

base.mat.cias = base.mat.cias %>% 