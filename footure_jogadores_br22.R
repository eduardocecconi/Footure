library(readxl)
library(stats)
library(psych)
library(lavaan)
library(semPlot)
library(kableExtra)
library(tidyverse)
library(ggimage)
library(ggrepel) 
library(reshape2) 
library(ggpubr)

# carregamento do banco de dados extraído do Wyscout e pré-tratado no Excel
# São 674 jogadores com dados do Campeonato Brasileiro 2022, até a 24ª rodada
# São 97 variáveis

jogadores_brasileiro_2022 <- read_excel("wyscout_jogadores_br22.xlsx")

# função para extrair apenas a 1ª posição dos jogadores listados com mais de uma posição

posicoes <- word(jogadores_brasileiro_2022$Posição, 1)

# visualização do total de posições descritas
# há diversas duplicatas de conceitos e duplicatas de grafias seguidas com vírgulas

unique(posicoes)

# funções para reajuste da grafia e dos conceitos das posições

posicoes[posicoes %in% c('LCB', 'RCB,', 'RCB', 'LCB,', 'CB,')] <- 'CB' #zagueiro
posicoes[posicoes %in% c('RB,', 'RWB', 'RWB,')] <- 'RB' #lateral-direito
posicoes[posicoes %in% c('LB,', 'LWB', 'LWB,')] <- 'LB' #lateral-esquerdo
posicoes[posicoes %in% c('RDMF', 'RDMF,', 'DMF', 'LDMF,', 'LDMF', 'DMF,')] <- 'DM' #volante
posicoes[posicoes %in% c('RCMF,', 'LCMF,', 'RCMF', 'LCMF' )] <- 'CM' #meia-central
posicoes[posicoes %in% c('AMF,', 'RAMF', 'AMF', 'LAMF,', 'LAMF', 'RAMF,')] <- 'OM' #meia-ofensivo
posicoes[posicoes %in% c('LWF,', 'LW,', 'LW', 'LM')] <- 'LM' #extremo-esquerdo
posicoes[posicoes %in% c('RW,', 'RWF,', 'RW', 'RWF')] <- 'RM' #extremo-direito
posicoes[posicoes == 'CF,'] <- 'CF' #centroavante

# substituição da variável original pela variável ajustada

jogadores_brasileiro_2022$Posição <- posicoes

# Há ocorrência de apenas uma observação NA na posição
# O jogador é João Victor, do Botafogo. Em breve pesquisa encontra-se que ele é CF

jogadores_brasileiro_2022$Posição[is.na(jogadores_brasileiro_2022$Posição)] <- 'CF'

unique(jogadores_brasileiro_2022$Posição)

### DATA WRANGLING
    # Exclusão de variáveis que não interessam para a análise fatorial
    # Criação de métricas
    # Normalização das medidas
    # Redução de 97 para 23 variáveis (11 delas métricas agrupando variáveis relacionadas)
    # Filtro por jogos (>= 2 e minutos jogados (> 90) reduz de 674 para 579 observações

jogadores_brasileiro_2022_wrangling <- jogadores_brasileiro_2022 %>% 
                                        select(Jogador, Time, Posição, Pé, Altura,
                                               Peso, Idade, Jogos, Minutos, `Gols Total`,
                                               `xG Total`, `Assistências Total`, `xA Total`,
                                               `Duelos Defensivos`, `Duelos Ofensivos`,
                                               `Duelos Aéreos`, `(%) Duelos Defensivos Certos`,
                                               `(%) Duelos Ofensivos Certos`, `(%) Duelos Aéreos Certos`,
                                               Carrinhos, Bloqueios, Interceptações, Faltas, Amarelos,
                                               Vermelhos, Chutes, `(%) Chutes Certos`, Cruzamentos,
                                               `(%) Cruzamentos Certos`, Dribles, `(%) Dribles Certos`,
                                               `Toques na Área`, `Corridas Progressivas`, `(%) Passes Certos`,
                                               `Passes Recebidos`, `Faltas Sofridas`, Passes, `Segundos Passes`, 
                                               `Terceiros Passes`, `Passes-Chave`, `Passes 3/3`, 
                                               `Passes Área`, `Passes Profundos`, `Passes Recebidos`, 
                                               `(%) Passes 3/3 Certos`, `(%) Passes Área`,
                                               `(%) Passes Profundos Certos`, `(%) Passes Progressivos Certos`,
                                               `Passes Progressivos`, `Gols de Cabeça`) %>%
                                          mutate(Minutos_Media = Minutos / Jogos,
                                                 Conversao_Gol = `Gols Total` / `xG Total`,
                                                 Imposicao = (`Duelos Defensivos` * `(%) Duelos Defensivos Certos` / 100) + 
                                                   (`Duelos Ofensivos` * `(%) Duelos Ofensivos Certos` / 100) + 
                                                   (`Duelos Aéreos` * `(%) Duelos Aéreos Certos` / 100) + `Faltas Sofridas`,
                                                 Combatividade = (Carrinhos + Bloqueios + Interceptações) - 
                                                   (Faltas + Amarelos + Vermelhos),
                                                 Chutes_Certos = (Chutes * `(%) Chutes Certos`) / 100,
                                                 Cruzamentos_Certos = (Cruzamentos* `(%) Cruzamentos Certos`) / 100,
                                                 Individual = ((Dribles*`(%) Dribles Certos`) / 100) + 
                                                   `Corridas Progressivas`,
                                                 Criatividade = `Passes-Chave` + ((`Passes 3/3`*`(%) Passes 3/3 Certos`)/100) + 
                                                   ((`Passes Área`*`(%) Passes Área`)/100),
                                                 Profundidade = ((`Passes Profundos`*`(%) Passes Profundos Certos`)/100) +
                                                   ((`(%) Passes Progressivos Certos`*`Passes Progressivos`)/100) + 
                                                   `Toques na Área` + `Corridas Progressivas`,
                                                 Passe = ((Passes*`(%) Passes Certos`)/100) + `Passes Recebidos`,
                                                 Part_Jog_Gol = `Assistências Total` + `Segundos Passes` + 
                                                   `Terceiros Passes`) %>%
                                          rename(Posicao = Posição, 
                                                 Pe = Pé,
                                                 Gols = `Gols Total`,
                                                 xG = `xG Total`, 
                                                 Assistencias = `Assistências Total`, 
                                                 xA = `xA Total`) %>%
                                          filter(Jogos >= 2 & Minutos > 90) %>%
                                          select(Jogador, Time, Posicao, Pe, Altura, Peso, Idade, Jogos, 
                                                 Minutos_Media, Gols, xG, Assistencias, xA, Conversao_Gol,
                                                 Chutes_Certos, Cruzamentos_Certos,
                                                 Part_Jog_Gol, Individual, Criatividade, Profundidade,
                                                 Passe, Imposicao, Combatividade) %>%
                                          mutate_if(is.numeric, round, 2)


#NA's resultantes nas variáveis Conversão de Gol
                                               
jogadores_brasileiro_2022_wrangling$Conversao_Gol[is.na(jogadores_brasileiro_2022_wrangling$Conversao_Gol)] <- 0

# criação da matriz de correlações

correlacao_jogadores_brasileiro_2022_wrangling <- cor(jogadores_brasileiro_2022_wrangling[ , 5:23])

# Teste de Esfericidade de Bartlett
# Verifica se a correlação entre as variáveis permite uma análise fatorial
# resultado precisa ser p-valor < 5%
# com p-valor = 0, o procedimento se justifica 

cortest.bartlett(correlacao_jogadores_brasileiro_2022_wrangling, n = 579)

# Teste KMO (Kayser-Meyer-Olkin)
# Mede a adequação da amostra à análise fatorial
# Resultado precisa ser > 0.5
# Com resultado médio de 0.72, é possível dar continuidade à análise

KMO(jogadores_brasileiro_2022_wrangling[ , 5:23])

# Função para normalização das escalas numéricas

jogadores_brasileiro_2022_normalizado <- jogadores_brasileiro_2022_wrangling

jogadores_brasileiro_2022_normalizado [, 5:23] <- apply(jogadores_brasileiro_2022_normalizado [, 5:23],  2, scale)


########################################################################################

#ZAGUEIRO - 102 OBSERVAÇÕES
#SELEÇÃO DA POSIÇÃO
zagueiros_fatorial <- jogadores_brasileiro_2022_normalizado %>% 
  filter(Posicao == "CB") %>%
  select(everything())

#MATRIZ DE CORRELAÇÕES
zagueiros_correl <- cor(zagueiros_fatorial[ , 5:23])

#EXPLORAÇÃO DOS FATORES
fa.parallel(zagueiros_fatorial[ , 5:23],fm="pa", fa="pc", n.iter=500)

#DEFINIÇÃO DOS FATORES
fit_zagueiros <- fa(zagueiros_fatorial[ , 5:23], nfactors = 6, rotate = "Promax", scores = TRUE, fm = "pa")

print(fit_zagueiros, cut = .30, sort = TRUE, digits = 3)

#CRIAÇÃO DOS SCORES
scores_zagueiros <- factor.scores(zagueiros_fatorial[ , 5:23],fit_zagueiros, 
                        Phi = NULL, 
                        rho=NULL)

scores_zagueiros <- as.data.frame(scores_zagueiros$scores)

bind_zagueiros <- bind_cols(zagueiros_fatorial, scores_zagueiros)

#FORMATAÇÃO DOS SCORES
zagueiros_ranking <- bind_zagueiros %>% select(1:2, 24:29) %>%
  mutate(Total = PA1 + PA2 + PA3 + PA4 + PA5 + PA6) %>%
  rename(Zagueiro = Jogador) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(1:2, Total, PA1, PA2, PA3, PA4, PA5, PA6) %>%
  arrange(desc(Total))

zagueiros_ranking$Ranking <- c(1:102)

zagueiros_ranking <- zagueiros_ranking %>% relocate(Ranking, .before = Zagueiro)

#TABELAS RESUMIDAS - 10 MELHORES E TODOS OS JOGADORES DO CLUBE SELECIONADO
zagueiros_tabela <- zagueiros_ranking %>% select(1:3) %>%
  filter(Ranking <=10)

#VISUALIZAÇÃO DAS TABELAS

#10 MELHORES
kbl(zagueiros_tabela, booktabs = T) %>%
  kable_material_dark(full_width = F, "striped") %>%
  row_spec(row = 0, bold = T, color = "yellow", background = "#black")

########################################################################################

#LATERAIS-DIREITOS - 48 OBSERVAÇÕES
#SELEÇÃO DA POSIÇÃO
laterais_direitos_fatorial <- jogadores_brasileiro_2022_normalizado %>% 
  filter(Posicao == "RB") %>%
  select(everything())

#MATRIZ DE CORRELAÇÕES
laterais_direitos_correl <- cor(laterais_direitos_fatorial[ , 5:23])

#EXPLORAÇÃO DOS FATORES
fa.parallel(laterais_direitos_fatorial[ , 5:23],fm="pa", fa="pc", n.iter=500)

#DEFINIÇÃO DOS FATORES
fit_laterais_direitos <- fa(laterais_direitos_fatorial[ , 5:23], nfactors = 4, rotate = "Promax", scores = TRUE, fm = "pa")

print(fit_laterais_direitos, cut = .30, sort = TRUE, digits = 3)

#CRIAÇÃO DOS SCORES
scores_laterais_direitos <- factor.scores(laterais_direitos_fatorial[ , 5:23],fit_laterais_direitos, 
                                  Phi = NULL, 
                                  rho=NULL)

scores_laterais_direitos <- as.data.frame(scores_laterais_direitos$scores)

bind_laterais_direitos <- bind_cols(laterais_direitos_fatorial, scores_laterais_direitos)

#FORMATAÇÃO DOS SCORES
laterais_direitos_ranking <- bind_laterais_direitos %>% select(1:2, 24:27) %>%
  mutate(Total = PA1 + PA2 + PA3 + PA4) %>%
  rename("Lateral Direito" = Jogador) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(1:2, Total, PA1, PA2, PA3, PA4) %>%
  arrange(desc(Total))

laterais_direitos_ranking$Ranking <- c(1:48)

laterais_direitos_ranking <- laterais_direitos_ranking %>% relocate(Ranking, .before = "Lateral Direito")

#TABELAS RESUMIDAS - 10 MELHORES E TODOS OS JOGADORES DO CLUBE SELECIONADO
laterais_direitos_tabela <- laterais_direitos_ranking %>% select(1:3) %>%
  filter(Ranking <=10)

#VISUALIZAÇÃO DAS TABELAS

#10 MELHORES
kbl(laterais_direitos_tabela, booktabs = T) %>%
  kable_material_dark(full_width = F, "striped") %>%
  row_spec(row = 0, bold = T, color = "yellow", background = "#black")

########################################################################################

#LATERAIS-ESQUERDOS - 53 OBSERVAÇÕES
#SELEÇÃO DA POSIÇÃO
laterais_esquerdos_fatorial <- jogadores_brasileiro_2022_normalizado %>% 
  filter(Posicao == "LB") %>%
  select(everything())

#MATRIZ DE CORRELAÇÕES
laterais_esquerdos_correl <- cor(laterais_esquerdos_fatorial[ , 5:23])

#EXPLORAÇÃO DOS FATORES
fa.parallel(laterais_esquerdos_fatorial[ , 5:23],fm="pa", fa="pc", n.iter=500)

#DEFINIÇÃO DOS FATORES
fit_laterais_esquerdos <- fa(laterais_esquerdos_fatorial[ , 5:23], nfactors = 4, rotate = "Promax", scores = TRUE, fm = "pa")

print(fit_laterais_esquerdos, cut = .30, sort = TRUE, digits = 3)

#CRIAÇÃO DOS SCORES
scores_laterais_esquerdos <- factor.scores(laterais_esquerdos_fatorial[ , 5:23],fit_laterais_esquerdos, 
                                          Phi = NULL, 
                                          rho=NULL)

scores_laterais_esquerdos <- as.data.frame(scores_laterais_esquerdos$scores)

bind_laterais_esquerdos <- bind_cols(laterais_esquerdos_fatorial, scores_laterais_esquerdos)

#FORMATAÇÃO DOS SCORES
laterais_esquerdos_ranking <- bind_laterais_esquerdos %>% select(1:2, 24:27) %>%
  mutate(Total = PA1 + PA2 + PA3 + PA4) %>%
  rename("Lateral Esquerdo" = Jogador) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(1:2, Total, PA1, PA2, PA3, PA4) %>%
  arrange(desc(Total))

laterais_esquerdos_ranking$Ranking <- c(1:53)

laterais_esquerdos_ranking <- laterais_esquerdos_ranking %>% relocate(Ranking, .before = "Lateral Esquerdo")

#TABELAS RESUMIDAS - 10 MELHORES E TODOS OS JOGADORES DO CLUBE SELECIONADO
laterais_esquerdos_tabela <- laterais_esquerdos_ranking %>% select(1:3) %>%
  filter(Ranking <=10)

#VISUALIZAÇÃO DAS TABELAS

#10 MELHORES
kbl(laterais_esquerdos_tabela, booktabs = T) %>%
  kable_material_dark(full_width = F, "striped") %>%
  row_spec(row = 0, bold = T, color = "yellow", background = "#black")

########################################################################################

#VOLANTES - 49 OBSERVAÇÕES
#SELEÇÃO DA POSIÇÃO
volantes_fatorial <- jogadores_brasileiro_2022_normalizado %>% 
  filter(Posicao == "DM") %>%
  select(everything())

#MATRIZ DE CORRELAÇÕES
volantes_correl <- cor(volantes_fatorial[ , 5:23])

#EXPLORAÇÃO DOS FATORES
fa.parallel(volantes_fatorial[ , 5:23],fm="pa", fa="pc", n.iter=500)

#DEFINIÇÃO DOS FATORES - emitiu warning e precisou-se diminuir de 4 para 3
fit_volantes <- fa(volantes_fatorial[ , 5:23], nfactors = 3, rotate = "Promax", scores = TRUE, fm = "pa")

print(fit_volantes, cut = .30, sort = TRUE, digits = 3)

#CRIAÇÃO DOS SCORES
scores_volantes <- factor.scores(volantes_fatorial[ , 5:23],fit_volantes, 
                                           Phi = NULL, 
                                           rho=NULL)

scores_volantes <- as.data.frame(scores_volantes$scores)

bind_volantes <- bind_cols(volantes_fatorial, scores_volantes)

#FORMATAÇÃO DOS SCORES
volantes_ranking <- bind_volantes %>% select(1:2, 23:26) %>%
  mutate(Total = PA1 + PA2 + PA3) %>%
  rename("Volante" = Jogador) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(1:2, Total, PA1, PA2, PA3) %>%
  arrange(desc(Total))

volantes_ranking$Ranking <- c(1:49)

volantes_ranking <- volantes_ranking %>% relocate(Ranking, .before = "Volante")

#TABELAS RESUMIDAS - 10 MELHORES E TODOS OS JOGADORES DO CLUBE SELECIONADO
volantes_tabela <- volantes_ranking %>% select(1:3) %>%
  filter(Ranking <=10)

#VISUALIZAÇÃO DAS TABELAS

#10 MELHORES
kbl(volantes_tabela, booktabs = T) %>%
  kable_material_dark(full_width = F, "striped") %>%
  row_spec(row = 0, bold = T, color = "yellow", background = "#black")

########################################################################################

#MEIAS - 69 OBSERVAÇÕES
#SELEÇÃO DA POSIÇÃO
meias_fatorial <- jogadores_brasileiro_2022_normalizado %>% 
  filter(Posicao == "CM") %>%
  select(everything())

#MATRIZ DE CORRELAÇÕES
meias_correl <- cor(meias_fatorial[ , 5:23])

#EXPLORAÇÃO DOS FATORES 
fa.parallel(meias_fatorial[ , 5:23],fm="pa", fa="pc", n.iter=500)

#DEFINIÇÃO DOS FATORES
fit_meias <- fa(meias_fatorial[ , 5:23], nfactors = 4, rotate = "Promax", scores = TRUE, fm = "pa")

print(fit_meias, cut = .30, sort = TRUE, digits = 3)

#CRIAÇÃO DOS SCORES
scores_meias <- factor.scores(meias_fatorial[ , 5:23],fit_meias, 
                                 Phi = NULL, 
                                 rho=NULL)

scores_meias <- as.data.frame(scores_meias$scores)

bind_meias <- bind_cols(meias_fatorial, scores_meias)

#FORMATAÇÃO DOS SCORES
meias_ranking <- bind_meias %>% select(1:2, 24:27) %>%
  mutate(Total = PA1 + PA2 + PA3 + PA4) %>%
  rename("Meia" = Jogador) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(1:2, Total, PA1, PA2, PA3, PA4) %>%
  arrange(desc(Total))

meias_ranking$Ranking <- c(1:69)

meias_ranking <- meias_ranking %>% relocate(Ranking, .before = "Meia")

#TABELAS RESUMIDAS - 10 MELHORES E TODOS OS JOGADORES DO CLUBE SELECIONADO
meias_tabela <- meias_ranking %>% select(1:3) %>%
  filter(Ranking <=10)

#VISUALIZAÇÃO DAS TABELAS

#10 MELHORES
kbl(meias_tabela, booktabs = T) %>%
  kable_material_dark(full_width = F, "striped") %>%
  row_spec(row = 0, bold = T, color = "yellow", background = "#black")

########################################################################################

#MEIAS-OFENSIVOS - 72 OBSERVAÇÕES
#SELEÇÃO DA POSIÇÃO
meias_ofensivos_fatorial <- jogadores_brasileiro_2022_normalizado %>% 
  filter(Posicao == "OM") %>%
  select(everything())

#MATRIZ DE CORRELAÇÕES
meias_ofensivos_correl <- cor(meias_ofensivos_fatorial[ , 5:23])

#EXPLORAÇÃO DOS FATORES
fa.parallel(meias_ofensivos_fatorial[ , 5:23],fm="pa", fa="pc", n.iter=500)

#DEFINIÇÃO DOS FATORES - apesar do resultado, emitiu Warnings até reduzir de 5 para 3
fit_meias_ofensivos <- fa(meias_ofensivos_fatorial[ , 5:23], nfactors = 3, rotate = "Promax", scores = TRUE, fm = "pa")

print(fit_meias_ofensivos, cut = .30, sort = TRUE, digits = 3)

#CRIAÇÃO DOS SCORES
scores_meias_ofensivos <- factor.scores(meias_ofensivos_fatorial[ , 5:23],fit_meias_ofensivos, 
                              Phi = NULL, 
                              rho=NULL)

scores_meias_ofensivos <- as.data.frame(scores_meias_ofensivos$scores)

bind_meias <- bind_cols(meias_ofensivos_fatorial, scores_meias_ofensivos)

#FORMATAÇÃO DOS SCORES
meias_ofensivos_ranking <- bind_meias %>% select(1:2, 23:26) %>%
  mutate(Total = PA1 + PA2 + PA3) %>%
  rename("Meia Ofensivo" = Jogador) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(1:2, Total, PA1, PA2, PA3) %>%
  arrange(desc(Total))

meias_ofensivos_ranking$Ranking <- c(1:72)

meias_ofensivos_ranking <- meias_ofensivos_ranking %>% relocate(Ranking, .before = "Meia Ofensivo")

#TABELAS RESUMIDAS - 10 MELHORES E TODOS OS JOGADORES DO CLUBE SELECIONADO
meias_ofensivos_tabela <- meias_ofensivos_ranking %>% select(1:3) %>%
  filter(Ranking <=10)

#VISUALIZAÇÃO DAS TABELAS

#10 MELHORES
kbl(meias_ofensivos_tabela, booktabs = T) %>%
  kable_material_dark(full_width = F, "striped") %>%
  row_spec(row = 0, bold = T, color = "yellow", background = "#black")

########################################################################################

#EXTREMOS - 69 OBSERVAÇÕES
#SELEÇÃO DA POSIÇÃO
extremos_fatorial <- jogadores_brasileiro_2022_normalizado %>% 
  filter(Posicao %in% c("LM", "RM")) %>%
  select(everything())

#MATRIZ DE CORRELAÇÕES
extremos_correl <- cor(extremos_fatorial[ , 5:23])

#EXPLORAÇÃO DOS FATORES
fa.parallel(extremos_fatorial[ , 5:23],fm="pa", fa="pc", n.iter=500)

#DEFINIÇÃO DOS FATORES - apesar do resultado, emitiu Warnings até reduzir de 5 para 3
fit_extremos <- fa(extremos_fatorial[ , 5:23], nfactors = 3, rotate = "Promax", scores = TRUE, fm = "pa")

print(fit_extremos, cut = .30, sort = TRUE, digits = 3)

#CRIAÇÃO DOS SCORES
scores_extremos <- factor.scores(extremos_fatorial[ , 5:23],fit_extremos, 
                                        Phi = NULL, 
                                        rho=NULL)

scores_extremos <- as.data.frame(scores_extremos$scores)

bind_extremos <- bind_cols(extremos_fatorial, scores_extremos)

#FORMATAÇÃO DOS SCORES
extremos_ranking <- bind_extremos %>% select(1:2, 23:26) %>%
  mutate(Total = PA1 + PA2 + PA3) %>%
  rename("Extremo" = Jogador) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(1:2, Total, PA1, PA2, PA3) %>%
  arrange(desc(Total))

extremos_ranking$Ranking <- c(1:69)

extremos_ranking <- extremos_ranking %>% relocate(Ranking, .before = "Extremo")

#TABELAS RESUMIDAS - 10 MELHORES E TODOS OS JOGADORES DO CLUBE SELECIONADO
extremos_tabela <- extremos_ranking %>% select(1:3) %>%
  filter(Ranking <=10)

#VISUALIZAÇÃO DAS TABELAS

#10 MELHORES
kbl(extremos_tabela, booktabs = T) %>%
  kable_material_dark(full_width = F, "striped") %>%
  row_spec(row = 0, bold = T, color = "yellow", background = "#black")

########################################################################################

#CENTROAVANTES - 78 OBSERVAÇÕES
#SELEÇÃO DA POSIÇÃO
centroavantes_fatorial <- jogadores_brasileiro_2022_normalizado %>% 
  filter(Posicao == "CF") %>%
  select(everything())

#MATRIZ DE CORRELAÇÕES
centroavantes_correl <- cor(centroavantes_fatorial[ , 5:23])

#EXPLORAÇÃO DOS FATORES
fa.parallel(centroavantes_fatorial[ , 5:23],fm="pa", fa="pc", n.iter=500)

#DEFINIÇÃO DOS FATORES
fit_centroavantes <- fa(centroavantes_fatorial[ , 5:23], nfactors = 3, rotate = "Promax", scores = TRUE, fm = "pa")

print(fit_centroavantes, cut = .30, sort = TRUE, digits = 3)

#CRIAÇÃO DOS SCORES
scores_centroavantes <- factor.scores(centroavantes_fatorial[ , 5:23],fit_centroavantes, 
                                        Phi = NULL, 
                                        rho=NULL)

scores_centroavantes <- as.data.frame(scores_centroavantes$scores)

bind_centroavantes <- bind_cols(centroavantes_fatorial, scores_centroavantes)

#FORMATAÇÃO DOS SCORES
#Variável de posse teve valor dividido por 2 (PA2) porque estava provocando alto ruído
centroavantes_ranking <- bind_centroavantes %>% select(1:2, 23:26) %>%
  mutate(PA2 = PA2/2,
         Total = PA1 + PA2 + PA3) %>%
  rename("Centroavante" = Jogador) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(1:2, Total, PA1, PA2, PA3) %>%
  arrange(desc(Total))

centroavantes_ranking$Ranking <- c(1:78)

centroavantes_ranking <- centroavantes_ranking %>% relocate(Ranking, .before = "Centroavante")

#TABELAS RESUMIDAS - 10 MELHORES E TODOS OS JOGADORES DO CLUBE SELECIONADO
centroavantes_tabela <- centroavantes_ranking %>% select(1:3) %>%
  filter(Ranking <=10)

#VISUALIZAÇÃO DAS TABELAS

#10 MELHORES
kbl(centroavantes_tabela, booktabs = T) %>%
  kable_material_dark(full_width = F, "striped") %>%
  row_spec(row = 0, bold = T, color = "yellow", background = "#black")
