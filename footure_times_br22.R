library(readxl) # biblioteca para carregar o arquivo .xlsx
library(stats) # biblioteca com funções estatísticas e geração de números aleatórios
library(psych) # biblioteca para análise multivariada, principalmente fatorial
library(lavaan) # biblioteca para construção de SEM (Structural Equation Modeling)
library(semPlot) #biblioteca para plotar modelo
library(kableExtra) # biblioteca para plotagem de tabelas
library(tidyverse) # concentra 8 bibliotecas, entre elas dplyr e tidyr
                  # muito importantes para realizar o processo de Data Wrangling

# importação do arquivo com a base de dados de 24 rodadas do BR 2022
# dados coletados pelo Wyscout e extraídos por download no site
# são 97 variáveis e 480 observações (240 jogos, 2 times por jogo)
# neste caso, o arquivo já passou por um processo de limpeza prévio no Excel
# a limpeza no Excel serviu como uma espécie de "Staging Area" do E.T.L

brasileiro_2022 <- read_excel("wyscout_times_br22.xlsx")

# criação das variáveis RESULTADO e PONTOS, além de uma variável "GAMBIARRA"
# Gambiarra será descartada após ajudar na eventual criação de outras variáveis

brasileiro_2022 <- brasileiro_2022 %>% 
  mutate(Resultado = case_when(
    brasileiro_2022$Gols == brasileiro_2022$`Conceded goals` ~ "E",
    brasileiro_2022$Gols > brasileiro_2022$`Conceded goals` ~ "V",
    brasileiro_2022$Gols < brasileiro_2022$`Conceded goals` ~ "D"),
          Pontos = case_when(
    brasileiro_2022$Gols == brasileiro_2022$`Conceded goals` ~ 1,
    brasileiro_2022$Gols > brasileiro_2022$`Conceded goals` ~ 3,
    brasileiro_2022$Gols < brasileiro_2022$`Conceded goals` ~ 0),
        Gambiarra = rep(c("A", "B"), times = 240)) %>%
  select(1:4, Resultado, Pontos, everything())

# criação das variáveis XG CONCEDIDO, PASSES CERTOS PERMITIDOS, CHUTES CERTOS
# CONCEDIDOS, com ajuda da variável GAMBIARRA
brasileiro_2022 <- brasileiro_2022 %>% 
  mutate(xG_Concedido = case_when(
    brasileiro_2022$Gambiarra == "A" ~ lead(brasileiro_2022$xG),
    brasileiro_2022$Gambiarra == "B" ~ lag(brasileiro_2022$xG)),
        Chutes_Certos_Concedidos = case_when(
          brasileiro_2022$Gambiarra == "A" ~ lead(brasileiro_2022$`Chutes Certos`),
          brasileiro_2022$Gambiarra == "B" ~ lag(brasileiro_2022$`Chutes Certos`)),
        Passes_Certos_Permitidos = case_when(
          brasileiro_2022$Gambiarra == "A" ~ lead(brasileiro_2022$`Passes Certos`),
          brasileiro_2022$Gambiarra == "B" ~ lag(brasileiro_2022$`Passes Certos`))) %>%
  select(1:10, xG_Concedido, Chutes_Certos_Concedidos, Passes_Certos_Permitidos,
         everything())

# DATA WRANGLING:
# RENOMEAR variáveis (eliminar espaços, acentos, aspas, etc)
# ELIMINAR variáveis consideradas de menor relevância para a análise
# CRIAR MÉTRICAS agrupando variáveis
# ORDENAR variáveis por assunto (facilita a visualização)
# CRIAÇÃO DE UM NOVO OBJETO com as alterações, preservando o banco de dados
# REDUÇÃO DE 103 PARA 36 VARIÁVEIS

brasileiro_2022_wrangling <- brasileiro_2022 %>% 
  rename(Gols_Concedidos = `Conceded goals`,
         Chutes_Certos = `Chutes Certos`,
         Passes_Certos = `Passes Certos`,
         Posse = `Posse (%)`,
         Perdas = `Perdas Total`,
         Perdas_Defesa = `Perdas 1/3`,
         Posicionais_Finalizados = `Ataques Posicionais com Chute`,
         Contra_Ataques_Finalizados = `Contra-Ataques Posicionais com Chute`,
         Bolas_Paradas_Finalizadas = `Bolas Paradas com Chute`,
         Cruzamentos_Finalizados = `Cruzamentos com Chute`,
         Passes_Profundidade = `Passes em Profundidade`,
         Entradas_Area = `Entradas na Área`,
         Toques_Area = `Toques na Área`,
         Duelos_Ofensivos = `Duelos Ofensivos Ganhos`,
         Duelos_Defensivos = `Duelos Defensivos Ganhos`,
         Passes_Terco_Final = `Passes Terço Final Certos`,
         Passes_Progressivos = `Passes Progressivos Certos`,
         Passes_Minuto = `Passes por Minuto`,
         Chutes_Distancia = `Distância dos Chutes`,
         Passes_Distancia = `Distância dos Passes`,
         Tatica = Tática) %>%
  mutate(Conversao = Gols / xG,
         Conversao_Concedida = Gols_Concedidos / xG_Concedido,
         Combatividade = (`Recuperações Total` + `Duelos Ganhos` + 
                          Interceptações + `Duelos Aéreos Ganhos` + 
                          `Carrinho Certo` + Rebatidas) - Faltas2,
         Pressao = `Recuperações 3/3` * (1/PPDA)) %>%
  select(Data, Jogo, Time, Tatica, Local, Resultado, Pontos, Gols,
         Gols_Concedidos, xG, xG_Concedido, Conversao, Conversao_Concedida,
         Chutes_Certos, Posicionais_Finalizados, Contra_Ataques_Finalizados,
         Bolas_Paradas_Finalizadas, Entradas_Area, Toques_Area, Duelos_Ofensivos,
         Chutes_Distancia, Posse, Passes_Certos, Passes_Profundidade, 
         Passes_Progressivos, Passes_Terco_Final, Passes_Distancia, Passes_Minuto,
         Cruzamentos_Finalizados, Perdas, Perdas_Defesa, Chutes_Certos_Concedidos, 
         Passes_Certos_Permitidos, Combatividade, Pressao, Duelos_Defensivos) %>%
  mutate_if(is.numeric, round, 2)

# criação da matriz de correlações

correlacao_brasileiro_2022_wrangling <- cor(brasileiro_2022_wrangling[ , 7:36])

# Teste de Esfericidade de Bartlett
# Verifica se a correlação entre as variáveis permite uma análise fatorial
# resultado precisa ser p-valor < 5%
# com p-valor = 0, o procedimento se justifica 

cortest.bartlett(correlacao_brasileiro_2022_wrangling, n = 480)

# Teste KMO (Kayser-Meyer-Olkin)
# Mede a adequação da amostra à análise fatorial
# Resultado precisa ser > 0.5
# Com resultado médio de 0.78, é possível dar continuidade à análise

KMO(brasileiro_2022_wrangling[ , 7:36])

# Função para normalização das escalas numéricas

brasileiro_2022_normalizado <- brasileiro_2022_wrangling

brasileiro_2022_normalizado [, 7:36] <- apply(brasileiro_2022_normalizado [, 7:36],  2, scale)

# Função para determinar a quantidade de fatores
# Método utilizado: Principal Factor ("pa")
# 4 FATORES ficaram acima de 1 no eixo eigenvalue

fa.parallel(brasileiro_2022_normalizado[,7:36],fm="pa", fa="both", n.iter=500) 

#Função para criar os 4 fatores

fit <- fa(brasileiro_2022_normalizado[ , 7:36], nfactors = 4, rotate = "Promax", scores = TRUE, fm = "pa", SMC = FALSE)

# Visualização do Resultado

print(fit, cut = .30, sort = TRUE, digits = 3)

# Representação gráfica do modelo encontrado

fa.diagram(fit, digits = 2, main = "Factor Diagram", 
           cut = .30, 
           simple = T, 
           errors = T)

# CRIAÇÃO DOS SCORECARDS

  # scores de cada time, por jogo, em cada fator, com base no banco normalizado
Scores <- factor.scores(brasileiro_2022_normalizado[ , 7:36],fit, 
                        Phi = NULL, 
                        method = "tenBerge",
                        rho=NULL)

  # transformação dos scores em data.frame
Scores <- as.data.frame(Scores$scores)

  # reunião dos scores com o banco normalizado
Scores_Bind <- bind_cols(brasileiro_2022_normalizado, Scores)

  # criação da variável RANKING, com a soma dos 4 fatores
Scores_Bind <- Scores_Bind %>% select(3, 37:40) %>%
  mutate(Ranking = PA1 + PA4 + PA2 - PA3) %>%
  select(Time, Ranking, everything())

# CONSOLIDAÇÃO DOS SCORECARDS
Scores_Brasileiro22_Final <- Scores_Bind %>% group_by(Time) %>% 
  summarise(Ranking_Total=sum(Ranking),
            PA1_Total=sum(PA1),
            PA4_Total=sum(PA4),
            PA2_Total=sum(PA2),
            PA3_Total=sum(PA3)) %>%
  mutate(Ranking = Ranking_Total,
         Ataque = PA1_Total,
         Posse = PA4_Total,
         Eficiencia = PA2_Total,
         Defesa = PA3_Total) %>%
  select(Time, Ranking, Ataque, Posse, Defesa, Eficiencia) %>%
  mutate_if(is.numeric, round, 2) %>%
  arrange(desc(Ranking))

# IMPRESSÃO DE TABELAS PARA VISUALIZAR OS RESULTADOS - GERAL E POR FATOR

  # RANKING GERAL
ranking_geral <- Scores_Brasileiro22_Final %>%
  select(Time, Ranking) %>%
  arrange(desc(Ranking)) %>%
  mutate(Posicao = c(1:20)) %>%
 select(Posicao, Time, Ranking)

  # TABELA RANKING GERAL
kbl(ranking_geral, booktabs = T) %>%
  kable_classic(full_width = F) %>%
  kable_styling(stripe_color = "white") %>%
  row_spec(row = 0, bold = T, color = "black", background = "#f7f30a") %>%
  row_spec(row = 6, bold = T, color = "white", background = "#226500") %>%
  row_spec(row = 1:5, color = "white", background = "#0b009c") %>%
  row_spec(row = 7:20, color = "white", background = "#0b009c")


# RANKING ATAQUE
ranking_ataque <- Scores_Brasileiro22_Final %>%
  select(Time, Ataque) %>%
  arrange(desc(Ataque)) %>%
  mutate(Posicao = c(1:20)) %>%
  select(Posicao, Time, Ataque)

# TABELA RANKING ATAQUE
kbl(ranking_ataque, booktabs = T) %>%
  kable_classic(full_width = F) %>%
  kable_styling(stripe_color = "white") %>%
  row_spec(row = 0, bold = T, color = "black", background = "#f7f30a") %>%
  row_spec(row = 3, bold = T, color = "white", background = "#226500") %>%
  row_spec(row = 1:2, color = "white", background = "#0b009c") %>%
  row_spec(row = 4:20, color = "white", background = "#0b009c")

# RANKING POSSE
ranking_posse <- Scores_Brasileiro22_Final %>%
  select(Time, Posse) %>%
  arrange(desc(Posse)) %>%
  mutate(Posicao = c(1:20)) %>%
  select(Posicao, Time, Posse)

# TABELA RANKING POSSE
kbl(ranking_posse, booktabs = T) %>%
  kable_classic(full_width = F) %>%
  kable_styling(stripe_color = "white") %>%
  row_spec(row = 0, bold = T, color = "black", background = "#f7f30a") %>%
  row_spec(row = 7, bold = T, color = "white", background = "#226500") %>%
  row_spec(row = 1:6, color = "white", background = "#0b009c") %>%
  row_spec(row = 8:20, color = "white", background = "#0b009c")

# RANKING DEFESA
ranking_defesa <- Scores_Brasileiro22_Final %>%
  select(Time, Defesa) %>%
  arrange(desc(Defesa)) %>%
  mutate(Posicao = c(1:20)) %>%
  select(Posicao, Time, Defesa)

# TABELA RANKING DEFESA
kbl(ranking_defesa, booktabs = T) %>%
  kable_classic(full_width = F) %>%
  kable_styling(stripe_color = "white") %>%
  row_spec(row = 0, bold = T, color = "black", background = "#f7f30a") %>%
  row_spec(row = 2, bold = T, color = "white", background = "#226500") %>%
  row_spec(row = 1, color = "white", background = "#0b009c") %>%
  row_spec(row = 3:20, color = "white", background = "#0b009c")

# RANKING EFICIÊNCIA
ranking_eficiencia <- Scores_Brasileiro22_Final %>%
  select(Time, Eficiencia) %>%
  arrange(desc(Eficiencia)) %>%
  mutate(Posicao = c(1:20)) %>%
  select(Posicao, Time, Eficiencia)

# TABELA RANKING EFICIÊNCIA
kbl(ranking_eficiencia, booktabs = T) %>%
  kable_classic(full_width = F) %>%
  kable_styling(stripe_color = "white") %>%
  row_spec(row = 0, bold = T, color = "black", background = "#f7f30a") %>%
  row_spec(row = 3, bold = T, color = "white", background = "#226500") %>%
  row_spec(row = 1:2, color = "white", background = "#0b009c") %>%
  row_spec(row = 4:20, color = "white", background = "#0b009c")

################################################################################

### CRIAÇÃO DE MODELO PARA ANÁLISE FATORIAL CONFIRMATÓRIA
### Foram mantidos os 4 fatores da análise exploratória, mas seguindo 2 critérios:
  # PONTO DE CORTE: acima de 0.4 ou abaixo de -0.4 no carregamento dos fatores
  # (variáveis que não atenderem a este critério foram excluídas)
  # EXCLUSÃO DE REPETIÇÃO: variáveis não podem aparecer em 2 fatores
  # (em caso de repetição, serão direcionadas ao fator onde obtiveram carga mais alta)

# CONSTRUÇÃO DO MODELO SEGUINDO OS CRITÉRIOS ESTABELECIDOS

Modelo <- '
              Ataque =~ Entradas_Area + Toques_Area + Cruzamentos_Finalizados + 
                        Posicionais_Finalizados + xG + Passes_Profundidade + 
                        Passes_Progressivos + Pressao + Bolas_Paradas_Finalizadas + 
                        Passes_Certos_Permitidos
              Posse_Bola =~ Passes_Certos + Posse + Passes_Minuto + Passes_Terco_Final + 
                        Passes_Distancia
              Defesa =~ Gols_Concedidos + Combatividade + Chutes_Certos_Concedidos + 
                        Conversao_Concedida
              Eficiencia =~ Gols + Pontos + Chutes_Certos + Conversao
              Ranking =~ Posse + Ataque + Defesa + Eficiencia
              '                                              
# Função para ajuste do modelo à análise confirmatória
# Resultados não foram satisfatórios (modelo não converge)

Fit_Modelo <- cfa(Modelo, data= brasileiro_2022_normalizado, check.gradient = FALSE)

summary(Fit_Modelo, fit.measures = TRUE, standardized = TRUE)

# diagramas do modelo

semPaths(Fit_Modelo, "par", weighted = FALSE, nCharNodes = 7, shapeMan = "rectangle",
         sizeMan = 8, sizeMan2 = 5)

semPlot::semPaths(Fit_Modelo, "std")

# criação dos rankings considerando peso positivo ou negativo de cada variável escolhida

modelo_cfa_brasileiro22 <- brasileiro_2022_normalizado %>% 
    mutate(Ataque = (Entradas_Area + Toques_Area + Cruzamentos_Finalizados + 
             Posicionais_Finalizados + xG + Passes_Profundidade + 
             Passes_Progressivos + Pressao + Bolas_Paradas_Finalizadas) - 
             Passes_Certos_Permitidos,
           Posse = (Passes_Certos + Posse + Passes_Minuto + Passes_Terco_Final) - 
             Passes_Distancia,
           Defesa = Combatividade - (Gols_Concedidos + Chutes_Certos_Concedidos + 
                          Conversao_Concedida),
           Eficiencia = Gols + Chutes_Certos + Conversao,
           Ranking = Posse + Ataque + Defesa + Eficiencia) %>%
  select(Time, Jogo, Ranking, Posse, Ataque, Defesa, Eficiencia)

modelo_cfa_brasileiro22 <- modelo_cfa_brasileiro22 %>% group_by(Time) %>% 
  summarise(Posse=sum(Posse),
            Ataque=sum(Ataque),
            Defesa=sum(Defesa),
            Eficiencia=sum(Eficiencia),
            Ranking = sum(Ranking)) %>%
  select(Time, Ranking, Posse, Ataque, Defesa, Eficiencia) %>% 
  mutate_if(is.numeric, round, 2) %>%
  arrange(desc(Ranking))

# IMPRESSÃO DE TABELAS PARA VISUALIZAR OS RESULTADOS - GERAL E POR FATOR

# RANKING GERAL
ranking_geral_cfa <- modelo_cfa_brasileiro22 %>%
  select(Time, Ranking) %>%
  arrange(desc(Ranking)) %>%
  mutate(Posicao = c(1:20)) %>%
  select(Posicao, Time, Ranking)

# TABELA RANKING GERAL
kbl(ranking_geral_cfa, booktabs = T) %>%
  kable_classic(full_width = F) %>%
  kable_styling(stripe_color = "white") %>%
  row_spec(row = 0, bold = T, color = "white", background = "#000000") %>%
  row_spec(row = 4, bold = T, color = "white", background = "#226500") %>%
  row_spec(row = 1:3, color = "white", background = "#c40000") %>%
  row_spec(row = 5:20, color = "white", background = "#c40000")

# RANKING ATAQUE
ranking_ataque_cfa <- modelo_cfa_brasileiro22 %>%
  select(Time, Ataque) %>%
  arrange(desc(Ataque)) %>%
  mutate(Posicao = c(1:20)) %>%
  select(Posicao, Time, Ataque)

# TABELA RANKING ATAQUE
kbl(ranking_ataque_cfa, booktabs = T) %>%
  kable_classic(full_width = F) %>%
  kable_styling(stripe_color = "white") %>%
  row_spec(row = 0, bold = T, color = "white", background = "#000000") %>%
  row_spec(row = 3, bold = T, color = "white", background = "#226500") %>%
  row_spec(row = 1:2, color = "white", background = "#c40000") %>%
  row_spec(row = 4:20, color = "white", background = "#c40000")

# RANKING POSSE
ranking_posse_cfa <- modelo_cfa_brasileiro22 %>%
  select(Time, Posse) %>%
  arrange(desc(Posse)) %>%
  mutate(Posicao = c(1:20)) %>%
  select(Posicao, Time, Posse)

# TABELA RANKING POSSE
kbl(ranking_posse_cfa, booktabs = T) %>%
  kable_classic(full_width = F) %>%
  kable_styling(stripe_color = "white") %>%
  row_spec(row = 0, bold = T, color = "white", background = "#000000") %>%
  row_spec(row = 11, bold = T, color = "white", background = "#226500") %>%
  row_spec(row = 1:10, color = "white", background = "#c40000") %>%
  row_spec(row = 12:20, color = "white", background = "#c40000")

# RANKING DEFESA
ranking_defesa_cfa <- modelo_cfa_brasileiro22 %>%
  select(Time, Defesa) %>%
  arrange(desc(Defesa)) %>%
  mutate(Posicao = c(1:20)) %>%
  select(Posicao, Time, Defesa)

# TABELA RANKING DEFESA
kbl(ranking_defesa_cfa, booktabs = T) %>%
  kable_classic(full_width = F) %>%
  kable_styling(stripe_color = "white") %>%
  row_spec(row = 0, bold = T, color = "white", background = "#000000") %>%
  row_spec(row = 1, bold = T, color = "white", background = "#226500") %>%
  row_spec(row = 2:20, color = "white", background = "#c40000")

# RANKING EFICIÊNCIA
ranking_eficiencia_cfa <- modelo_cfa_brasileiro22 %>%
  select(Time, Eficiencia) %>%
  arrange(desc(Eficiencia)) %>%
  mutate(Posicao = c(1:20)) %>%
  select(Posicao, Time, Eficiencia)

# TABELA RANKING EFICIÊNCIA
kbl(ranking_eficiencia_cfa, booktabs = T) %>%
  kable_classic(full_width = F) %>%
  kable_styling(stripe_color = "white") %>%
  row_spec(row = 0, bold = T, color = "white", background = "#000000") %>%
  row_spec(row = 1, bold = T, color = "white", background = "#226500") %>%
  row_spec(row = 2:20, color = "white", background = "#c40000")

##############################################################################################

### REGRESSÃO LINEAR UTILIZANDO O MODELO ELABORADO

# Seleção da variável Pontos, sumarizada pela soma e agrupada por time

Pontos <- brasileiro_2022_wrangling %>% select(Time, Pontos) %>%
                                        group_by(Time) %>% 
                                        summarise(Pontos=sum(Pontos))

# Combinação da variável pontos com o ranking consolidado do modelo CFA

Regressao_Normalizado <- left_join(modelo_cfa_brasileiro22, Pontos, by = "Time")
                                                  
# Função para executar a Regressão Linear

Regressao <- lm(formula = Pontos ~ Ataque + Posse + Defesa + Eficiencia,
                         data = Regressao_Normalizado)

#Visualização da análise do algoritmo

summary(Regressao)

# Método Stepwise
# usado para selecionar quais variáveis mais influenciam o conjunto de saída,
# o que pode diminuir o número de variáveis que compõem a equação de regressão. 

step(Regressao)
