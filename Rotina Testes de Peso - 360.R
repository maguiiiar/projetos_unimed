require(data.table)
require(dplyr)
require(broom)

base.pesos = fread("Testes Pesos.txt", h = T, sep="\t")
base.pesos = base.pesos %>% group_by(Especialidade, Grupo, ISE, `% Clientes Consultaram c/ médico da mesma Especialidade em até 45 dias`, `% Retorno serviços Urgência/ Emergência em até 3 dias`, `% Internações por Condições sensíveis à Atenção Primária`) %>% 
                            summarise(Economia = sum(`ISE ECONOMIA`, `% Clientes Consultaram c/ médico da mesma Especialidade em até 45 dias ECONOMIA`, `% Retorno serviços Urgência/ Emergência em até 3 dias ECONOMIA`, `% Internações por Condições sensíveis à Atenção Primária ECONOMIA`))

base.pesos$z_1 = scale(base.pesos$ISE)
base.pesos$z_2 = scale(base.pesos$`% Clientes Consultaram c/ médico da mesma Especialidade em até 45 dias`)
base.pesos$z_3 = scale(base.pesos$`% Retorno serviços Urgência/ Emergência em até 3 dias`)
base.pesos$z_4 = scale(base.pesos$`% Internações por Condições sensíveis à Atenção Primária`)
base.pesos$y = scale(base.pesos$Economia)

mod = lm(y ~ z_1 + z_2 + z_3 + z_4, data = base.pesos)
coef = data.frame(mod$coefficients)

mod2 = base.pesos %>% group_by(Grupo) %>% do(tidy(lm(y ~ z_1 + z_2 + z_3 + z_4 - 1, data = .)))

mod2$`estimate.mais.cem` = mod2$estimate+100

sum_by_group = mod2 %>% group_by(Grupo) %>% summarise(sum(estimate.mais.cem))
mod2 = left_join(mod2, sum_by_group, by = "Grupo")
mod2 = mod2 %>% group_by(Grupo) %>% mutate(importância = estimate.mais.cem/`sum(estimate.mais.cem)`)