require(data.table)
require(dplyr)

# Materiais ativos
mat.cias.atv = fread("Tabela Unimed Uberlândia de Materiais Cias - Versão 1.14 - Vigência 12.04.2018 - Ativos.csv", h = T, sep=";")
mat.clh.atv = fread("Tabela Unimed Uberlândia de Materiais CLH-Versão 2.24 - Vigência - 12.04.2018 - Ativos.csv", h = T, sep=";")
mat.hosp.atv = fread("Tabela Unimed Uberlândia de Materiais Hospitalares - Versão 5.7 - Vigência 13.04.2018 - Ativos.csv", h=T, sep=";")
mat.onco.atv = fread("Tabela Unimed Uberlândia de Materiais Oncológicos Versão-2.12 -  Vigência - 12.04.2018 - Ativos.csv", h=T, sep=";")

# Medicamentos ativos
med.cias.atv = fread("Tabela Unimed Uberlândia de Medicamentos Cias - Versão 1.9.5 - Vigência 01.04.2018 - Ativos.csv", h=T, sep = ";")
med.clh.atv = fread("Tabela Unimed Uberlândia de Medicamentos CLH - Versão 7.23 - Vigência 01-04-2018 - Ativos.csv", h=T, sep=";")
med.hosp.atv = fread("Tabela Unimed Uberlândia de Medicamentos Hospitalar - Versão 10.7 - Vigência. 01.04.2018 - Ativos.csv", h=T, sep=";")
med.onco.atv = fread("Tabela Unimed Uberlandia de Medicamentos Oncológicos- Versão 4.13 - Vigência 01.04.2018 - Ativos.csv", h=T, sep=";")

# Materiais adicionados
mat.clh.adc = fread("Tabela Unimed Uberlândia de Materiais CLH-Versão 2.24 - Vigência - 12.04.2018 - Adicionados.csv", h = T, sep=";")
mat.cias.adc = fread("Tabela Unimed Uberlândia de Materiais Cias - Versão 1.14 - Vigência 12.04.2018 - Adicionados.csv", h = T, sep=";")
mat.hosp.adc = fread("Tabela Unimed Uberlândia de Materiais Hospitalares - Versão 5.7 - Vigência 13.04.2018 - Adicionados.csv", h=T, sep=";")
mat.onco.adc = fread("Tabela Unimed Uberlândia de Materiais Oncológicos Versão-2.12 -  Vigência - 12.04.2018 - Adicionados.csv", h=T, sep=";")

# Medicamentos adicionados
med.cias.adc = fread("Tabela Unimed Uberlândia de Medicamentos Cias - Versão 1.9.5 - Vigência 01.04.2018 - Adicionados.csv", h=T, sep = ";")
#med.clh.adc = fread("Tabela Unimed Uberlândia de Medicamentos CLH - Versão 7.23 - Vigência 01-04-2018 - Adicionados.csv", h=T, sep=";")
med.hosp.adc = fread("Tabela Unimed Uberlândia de Medicamentos Hospitalar - Versão 10.7 - Vigência. 01.04.2018 - Adicionados.csv", h=T, sep=";")
med.onco.adc = fread("Tabela Unimed Uberlandia de Medicamentos Oncológicos- Versão 4.13 - Vigência 01.04.2018 - Adicionados.csv", h=T, sep=";")

# Materiais inativos
mat.cias.inat = fread("Tabela Unimed Uberlândia de Materiais Cias - Versão 1.14 - Vigência 12.04.2018 - Inativos.csv", h = T, sep=";")
mat.clh.inat = fread("Tabela Unimed Uberlândia de Materiais CLH-Versão 2.24 - Vigência - 12.04.2018 - Inativos.csv", h = T, sep=";")
mat.hosp.inat = fread("Tabela Unimed Uberlândia de Materiais Hospitalares - Versão 5.7 - Vigência 13.04.2018 - Inativos.csv", h=T, sep=";")
mat.onco.inat = fread("Tabela Unimed Uberlândia de Materiais Oncológicos Versão-2.12 -  Vigência - 12.04.2018 - Inativos.csv", h=T, sep=";")

# Medicamentos inativos
med.cias.inat = fread("Tabela Unimed Uberlândia de Medicamentos Cias - Versão 1.9.5 - Vigência 01.04.2018 - Inativos.csv", h=T, sep = ";")
med.clh.inat = fread("Tabela Unimed Uberlândia de Medicamentos CLH - Versão 7.23 - Vigência 01-04-2018 - Inativos.csv", h=T, sep=";")
med.hosp.inat = fread("Tabela Unimed Uberlândia de Medicamentos Hospitalar - Versão 10.7 - Vigência. 01.04.2018 - Inativos.csv", h=T, sep=";")
med.onco.inat = fread("Tabela Unimed Uberlandia de Medicamentos Oncológicos- Versão 4.13 - Vigência 01.04.2018 - Inativos.csv", h=T, sep=";")

# Junção de materiais ativos
mat.cias.atv = mat.cias.atv %>% select(código, nome, descrição, especialidade, valor) %>%
                                mutate(versão = "CIAS", tipo = "Material", status = "Ativo") 
mat.clh.atv = mat.clh.atv %>% select(código, nome, descrição, especialidade, valor) %>%
                              mutate(versão = "CLH", tipo = "Material", status = "Ativo") 
mat.hosp.atv = mat.hosp.atv %>% select(código, nome, descrição, especialidade, valor) %>%
                                mutate(versão = "Hospitalar", tipo = "Material", status = "Ativo") 
mat.onco.atv = mat.onco.atv %>% select(código, nome, descrição, especialidade, valor) %>%
                                mutate(versão = "Oncológico", tipo = "Material", status = "Ativo")    

mat.atv = rbind(mat.cias.atv, mat.clh.atv, mat.hosp.atv, mat.onco.atv); rm(mat.cias.atv, mat.clh.atv, mat.hosp.atv, mat.onco.atv)

# Junção de medicamentos ativos
med.cias.atv = med.cias.atv %>% select(código, nome, princípio, grupo, classe, valor) %>%
                                mutate(versão = "CIAS", tipo = "Medicamento", status = "Ativo") 
med.hosp.atv = med.hosp.atv %>% select(código, nome, princípio, grupo, classe, valor) %>%
                                mutate(versão = "Hospitalar", tipo = "Medicamento", status = "Ativo") 
med.clh.atv = med.clh.atv   %>% select(código, nome, princípio, grupo, classe, valor) %>%
                                mutate(versão = "CLH", tipo = "Medicamento", status = "Ativo") 
med.onco.atv = med.onco.atv %>% select(código, nome, princípio, grupo, classe, valor) %>%
                                mutate(versão = "Oncológico", tipo = "Medicamento", status = "Ativo") 

med.atv = rbind(med.cias.atv, med.clh.atv, med.hosp.atv, med.onco.atv); rm(med.cias.atv, med.clh.atv, med.hosp.atv, med.onco.atv)

# Junção de materiais adicionados
mat.cias.adc = mat.cias.adc %>% select(código, nome, descrição, especialidade, vigência) %>%
                                mutate(versão = "CIAS", tipo = "Material", status = "Adicionado")     
mat.clh.adc = mat.clh.adc %>% select(código, nome, descrição, especialidade, vigência) %>%
                              mutate(versão = "CLH", tipo = "Material", status = "Adicionado") 
mat.hosp.adc = mat.hosp.adc %>% select(código, nome, descrição, especialidade, vigência) %>%
                                mutate(versão = "Hospitalar", tipo = "Material", status = "Adicionado")  
mat.onco.adc = mat.onco.adc %>% select(código, nome, descrição, especialidade, vigência) %>%
                                mutate(versão = "Oncológico", tipo = "Material", status = "Adicionado")  

mat.adc = rbind(mat.cias.adc, mat.clh.adc, mat.hosp.adc, mat.onco.adc); rm(mat.cias.adc, mat.clh.adc, mat.hosp.adc, mat.onco.adc)

# Junção de medicamentos adicionados
med.cias.adc = med.cias.adc %>% select(código, nome, princípio, grupo, classe, vigência) %>%
                                mutate(versão = "CIAS", tipo = "Material", status = "Adicionado")     
# med.clh.adc = med.clh.adc %>% select(código, nome, princípio, grupo, classe, vigência) %>%
#                               mutate(versão = "CLH", tipo = "Material", status = "Adicionado") 
med.hosp.adc = med.hosp.adc %>% select(código, nome, princípio, grupo, classe, vigência) %>%
                              mutate(versão = "Hospitalar", tipo = "Material", status = "Adicionado")  
med.onco.adc = med.onco.adc %>% select(código, nome, princípio, grupo, classe, vigência) %>%
                              mutate(versão = "Oncológico", tipo = "Material", status = "Adicionado")  

med.adc = rbind(med.cias.adc, med.hosp.adc, med.onco.adc); rm(med.cias.adc, med.hosp.adc, med.onco.adc)

# Junção de materiais inativos
mat.cias.inat = mat.cias.inat %>% select(código, nome, descrição, especialidade) %>%
  mutate(versão = "CIAS", tipo = "Material", status = "Inativo")     
mat.clh.inat = mat.clh.inat %>% select(código, nome, descrição, especialidade) %>%
  mutate(versão = "CLH", tipo = "Material", status = "Inativo") 
mat.hosp.inat = mat.hosp.inat %>% select(código, nome, descrição, especialidade) %>%
  mutate(versão = "Hospitalar", tipo = "Material", status = "Inativo")  
mat.onco.inat = mat.onco.inat %>% select(código, nome, descrição, especialidade) %>%
  mutate(versão = "Oncológico", tipo = "Material", status = "Inativo")  

mat.inat = rbind(mat.cias.inat, mat.clh.inat, mat.hosp.inat, mat.onco.inat); rm(mat.cias.inat, mat.clh.inat, mat.hosp.inat, mat.onco.inat)

# Junção de medicamentos inativos
med.cias.inat = med.cias.inat %>% select(código, nome, princípio, grupo, classe) %>%
  mutate(versão = "CIAS", tipo = "Medicamento", status = "Inativo")     
med.clh.inat = med.clh.inat %>% select(código, nome, princípio, grupo, classe) %>%
  mutate(versão = "CLH", tipo = "Medicamento", status = "Inativo") 
med.hosp.inat = med.hosp.inat %>% select(código, nome, princípio, grupo, classe) %>%
  mutate(versão = "Hospitalar", tipo = "Medicamento", status = "Inativo")  
med.onco.inat = med.onco.inat %>% select(código, nome, princípio, grupo, classe) %>%
  mutate(versão = "Oncológico", tipo = "Medicamento", status = "Inativo")  

med.inat = rbind(med.cias.inat, med.clh.inat, med.hosp.inat, med.onco.inat); rm(med.cias.inat, med.clh.inat, med.hosp.inat, med.onco.inat)

# Junção das bases de materiais (ativos e inativos)

mat.atv.inat = bind_rows(mat.atv, mat.inat)
med.atv.inat = bind_rows(med.atv, med.inat)

save(mat.atv.inat, file="base.mat.atv.inat.RData")
save(med.atv.inat, file="base.med.atv.inat.RData")

list.files(pattern = "*.csv")

#Reading all .csv files from the set directory and binding rows. All at once.
library(dplyr)
library(readr)

list_file <- list.files(pattern = "*.csv") %>% 
             lapply(fread, stringsAsFactors=F) %>% bind_rows


