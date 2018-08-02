require(data.table)
require(dplyr)

####################
#Junção DRG x Custo#
####################

dados.drg.2016 <- fread("DRG 2016.txt", sep = "\t", header = TRUE, dec = ",", na.strings = c("", NA))
dados.drg.2017 <- fread("DRG 2017.txt", sep = "\t", header = TRUE, dec = ",", na.strings = c("", NA))

custo.drg.2016 <- fread("Custo DRG 2016.txt", sep = "\t", header = TRUE, dec = ".", na.strings = c("-", NA))
custo.drg.2017 <- fread("Custo DRG 2017.txt", sep = "\t", header = TRUE, dec = ",", na.strings = c("-", NA))

custo.drg.2017$`Autorizacao` = as.character(custo.drg.2017$`Autorizacao`)

colnames(custo.drg.2016)[colnames(custo.drg.2016)=="Autorizacao"] <- "Número da Autorização"
colnames(custo.drg.2017)[colnames(custo.drg.2017)=="Autorizacao"] <- "Número da Autorização"

custo.drg.2016.soma <- custo.drg.2016 %>% group_by(`Número da Autorização`) %>% summarise(Custo = sum(`Custo Total (R$)`)); rm(custo.drg.2016)
custo.drg.2017.soma <- custo.drg.2017 %>% group_by(`Número da Autorização`) %>% summarise(Custo = sum(`Custo Total (R$)`)); rm(custo.drg.2017)

dados.drg.2016 <- left_join(dados.drg.2016, custo.drg.2016.soma, by = "Número da Autorização")
dados.drg.2017 <- left_join(dados.drg.2017, custo.drg.2017.soma, by = "Número da Autorização")

#save(dados.drg.2016, file = "DRG 2016 com custo.RData")
#save(dados.drg.2017, file = "DRG 2017 com custo.RData")

dados.drg.2016$`Código do Paciente` = as.character(dados.drg.2016$`Código do Paciente`)
dados.drg.2017$`Código do Paciente` = as.character(dados.drg.2017$`Código do Paciente`)

dados.drg.2016$`Data de Internação` = as.Date(dados.drg.2016$`Data de Internação`, format = "%d/%m/%Y")
dados.drg.2016$`Data da Alta` = as.Date(dados.drg.2016$`Data da Alta`, format = "%d/%m/%Y")
dados.drg.2017$`Data de Internação` = as.Date(dados.drg.2017$`Data de Internação`, format = "%d/%m/%Y")
dados.drg.2017$`Data da Alta` = as.Date(dados.drg.2017$`Data da Alta`, format = "%d/%m/%Y")

dados.drg.2016$`Mês Internação` = format(dados.drg.2016$`Data de Internação`, "%Y-%m")
dados.drg.2016$`Mês Alta` = format(dados.drg.2016$`Data da Alta`, "%Y-%m")
dados.drg.2017$`Mês Internação` = format(dados.drg.2017$`Data de Internação`, "%Y-%m")
dados.drg.2017$`Mês Alta` = format(dados.drg.2017$`Data da Alta`, "%Y-%m")

benef.UCE.2016 <- dados.drg.2016 %>% filter(`Código do Paciente` %in% c("141211108363002", "141155902179005", 
                                                                         "141111501061001", "142328002052506", 
                                                                         "141101012048005", "141113905008010", 
                                                                         "145010341206012", "140731743453006", 
                                                                         "145010313101007", "142783000004006", 
                                                                         "140698000240002", "140699410204004", 
                                                                         "141101006093002", "145481005198101", 
                                                                         "140000033201481", "148017305198106", 
                                                                         "140692000074510", "141158102335090", 
                                                                         "145010441134005", "142072379889005", 
                                                                         "145010694971096", "141102305223003", 
                                                                         "147443000055009", "142097110790001", 
                                                                         "145010004123014", "140751000024008", 
                                                                         "145999005996509", "140731613148009", 
                                                                         "141158204237004", "142226000003009", 
                                                                         "142303002918001", "141114003167015", 
                                                                         "142072501593004", "140000033162958", 
                                                                         "148003610561510", "145010341369515", 
                                                                         "141157206267000", "141158706035009", 
                                                                         "141111509188000", "145999003717505", 
                                                                         "140707718763002", "141157112039003", 
                                                                         "145010141316531", "140691014001008", 
                                                                         "141101212084005", "141157403135009", 
                                                                         "145010613545512", "141113203094005", 
                                                                         "145010350241003", "145010094342098", 
                                                                         "145010394342093", "145010013557501", 
                                                                         "145010341355514", "141157305208001", 
                                                                         "145010341356090", "145010310989007", 
                                                                         "141158208166004", "145010181292005", 
                                                                         "141158208187001", "141157207490004", 
                                                                         "141157309071000", "145010456063516", 
                                                                         "142072321941519", "141158201422099", 
                                                                         "142640125467516", "145010621188502", 
                                                                         "145010821188506", "141157502305000", 
                                                                         "145010312902103", "145010307479001", 
                                                                         "142201000020005", "141157102117010", 
                                                                         "141113002150005", "145010023283501", 
                                                                         "142308000349515", "148003800349512", 
                                                                         "140713376127013", "140706188731003", 
                                                                         "145010312366007", "142103980943004")) %>%
                                        group_by(`Tipo de DRG`, `Mês Internação`) %>%
                                        summarise(`Permanência Real` = sum(`Permanência Real`),
                                                  `Permanência Prevista na Alta` = sum(`Permanência Prevista na Alta`), 
                                                  `Permanência Prevista Internação` = sum(`Permanência Prevista Internação`),
                                                  `Custo` = sum(`Custo`),
                                                  `Pacientes Distintos` = n_distinct(`Identificador do Paciente`),
                                                  `Internações Distintas` = n_distinct(`Número da Autorização`))

benef.UCE.2017 <- dados.drg.2017 %>% filter(`Código do Paciente` %in% c("141211108363002", "141155902179005", 
                                                                       "141111501061001", "142328002052506", 
                                                                       "141101012048005", "141113905008010", 
                                                                       "145010341206012", "140731743453006", 
                                                                       "145010313101007", "142783000004006", 
                                                                       "140698000240002", "140699410204004", 
                                                                       "141101006093002", "145481005198101", 
                                                                       "140000033201481", "148017305198106", 
                                                                       "140692000074510", "141158102335090", 
                                                                       "145010441134005", "142072379889005", 
                                                                       "145010694971096", "141102305223003", 
                                                                       "147443000055009", "142097110790001", 
                                                                       "145010004123014", "140751000024008", 
                                                                       "145999005996509", "140731613148009", 
                                                                       "141158204237004", "142226000003009", 
                                                                       "142303002918001", "141114003167015", 
                                                                       "142072501593004", "140000033162958", 
                                                                       "148003610561510", "145010341369515", 
                                                                       "141157206267000", "141158706035009", 
                                                                       "141111509188000", "145999003717505", 
                                                                       "140707718763002", "141157112039003", 
                                                                       "145010141316531", "140691014001008", 
                                                                       "141101212084005", "141157403135009", 
                                                                       "145010613545512", "141113203094005", 
                                                                       "145010350241003", "145010094342098", 
                                                                       "145010394342093", "145010013557501", 
                                                                       "145010341355514", "141157305208001", 
                                                                       "145010341356090", "145010310989007", 
                                                                       "141158208166004", "145010181292005", 
                                                                       "141158208187001", "141157207490004", 
                                                                       "141157309071000", "145010456063516", 
                                                                       "142072321941519", "141158201422099", 
                                                                       "142640125467516", "145010621188502", 
                                                                       "145010821188506", "141157502305000", 
                                                                       "145010312902103", "145010307479001", 
                                                                       "142201000020005", "141157102117010", 
                                                                       "141113002150005", "145010023283501", 
                                                                       "142308000349515", "148003800349512", 
                                                                       "140713376127013", "140706188731003", 
                                                                       "145010312366007", "142103980943004")) %>%
                                                  group_by(`Tipo de DRG`, `Mês Internação`) %>%
                                                  summarise(`Permanência Real` = sum(`Permanência Real`),
                                                            `Permanência Prevista na Alta` = sum(`Permanência Prevista na Alta`), 
                                                            `Permanência Prevista Internação` = sum(`Permanência Prevista Internação`),
                                                            `Custo` = sum(`Custo`),
                                                            `Pacientes Distintos` = n_distinct(`Identificador do Paciente`),
                                                            `Internações Distintas` = n_distinct(`Número da Autorização`))

