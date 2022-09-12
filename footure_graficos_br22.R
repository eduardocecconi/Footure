library(ggimage) #biblioteca com a função incluir os logos sobre os pontos
library(ggrepel) #biblioteca para reduzir sobreposição de pontos 
library(reshape2) #biblioteca para pivotar tabelas (função melted)
library(ggpubr) #adiciona mais possibilidades às customizações do ggplot2

# Primeiro, carregar o script "footure_times_br22.R" até a linha 92
# O ideal é construir um objeto específico para o gráfico, ao invés de se referir ao banco completo.

### SCATTERPLOT - COMPARATIVO PERIGO CRIADO X PERIGO CONVERTIDO

# Seleção das variáveis, sumarizadas pelas médias, em um novo objeto

xG_Conversao <- brasileiro_2022_wrangling %>% group_by(Time) %>% 
  summarise(xG=mean(xG),
            Conversao=mean(Conversao)) %>%
  select(Time, xG, Conversao) %>%
  mutate_if(is.numeric, round, 2)

# Criação de variável com os logos dos 20 clubes

xG_Conversao <- xG_Conversao %>% mutate(Logos = case_when(
  xG_Conversao$Time == "Atlético-MG" ~ "https://www.ogol.com.br/img/logos/equipas/2229_imgbank.png",
  xG_Conversao$Time == "Flamengo" ~ "https://www.ogol.com.br/img/logos/equipas/2240_imgbank.png",
  xG_Conversao$Time == "Palmeiras" ~ "https://www.ogol.com.br/img/logos/equipas/2248_imgbank.png",
  xG_Conversao$Time == "Fortaleza" ~ "https://www.ogol.com.br/img/logos/equipas/2239_imgbank.png",
  xG_Conversao$Time == "Corinthians" ~ "https://www.ogol.com.br/img/logos/equipas/2234_imgbank.png",
  xG_Conversao$Time == "RB Bragantino" ~ "https://www.ogol.com.br/img/logos/equipas/3156_imgbank.png",
  xG_Conversao$Time == "Fluminense" ~ "https://www.ogol.com.br/img/logos/equipas/2241_imgbank.png",
  xG_Conversao$Time == "América-MG" ~ "https://www.ogol.com.br/img/logos/equipas/2227_imgbank.png",
  xG_Conversao$Time == "Atlético-GO" ~ "https://www.ogol.com.br/img/logos/equipas/3129_imgbank.png",
  xG_Conversao$Time == "Santos" ~ "https://www.ogol.com.br/img/logos/equipas/2254_imgbank.png",
  xG_Conversao$Time == "Ceará" ~ "https://www.ogol.com.br/img/logos/equipas/3183_imgbank.png",
  xG_Conversao$Time == "Internacional" ~ "https://www.ogol.com.br/img/logos/equipas/2245_imgbank.png",
  xG_Conversao$Time == "São Paulo" ~ "https://www.ogol.com.br/img/logos/equipas/2256_imgbank.png",
  xG_Conversao$Time == "Athletico" ~ "https://www.ogol.com.br/img/logos/equipas/2230_imgbank.png",
  xG_Conversao$Time == "Cuiabá" ~ "https://www.ogol.com.br/img/logos/equipas/3220_imgbank.png",
  xG_Conversao$Time == "Juventude" ~ "https://www.ogol.com.br/img/logos/equipas/2246_imgbank.png",
  xG_Conversao$Time == "Avaí" ~ "https://www.ogol.com.br/img/logos/equipas/2615_imgbank.png",
  xG_Conversao$Time == "Coritiba" ~ "https://www.ogol.com.br/img/logos/equipas/2235_imgbank.png",
  xG_Conversao$Time == "Botafogo" ~ "https://www.ogol.com.br/img/logos/equipas/2233_imgbank.png",
  xG_Conversao$Time == "Goiás" ~ "https://www.ogol.com.br/img/logos/equipas/2244_imgbank.png"))

# Plotagem do gráfico de dispersão xG x Conversão
# Funções para visualizar os logos, dividir os quadrantes pelas médias e legendar os quadrantes

ggplot(data = xG_Conversao, mapping = aes(x = Conversao, y = xG)) +
  geom_point(alpha=3, shape=21, size = 2, show.legend = TRUE, position = "identity") +
  geom_hline(yintercept = mean(xG_Conversao$xG), colour = "green3", alpha = 0.3) + 
  geom_vline(xintercept = mean(xG_Conversao$Conversao), colour = "green3", alpha = 0.3) + 
  geom_image(aes(image=Logos), size = 0.04, by = "height", position = "identity") +  
  geom_text(aes(x = 0.65, y = 1.75, vjust = 0,  label = "Perigosos\nIneficientes"), size = 5, color = "black") +
  geom_text(aes(x = 1.05, y = 1.75, vjust = 0, label = "Perigosos\nEficientes"), size = 5, color = "black") +
  geom_text(aes(x = 0.65, y = 0.5, vjust = 0, label = "Pouco perigo\nIneficientes"), size = 5, color = "black") +
  geom_text(aes(x = 1.05, y = 0.5, vjust = 0, label = "Pouco perigo\nEficientes"), size = 5, color = "black") +
  theme_bw() +
  theme(legend.position="bottom",
        title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(title = "Relação entre Perigo Criado (xG) e Conversão",
       y = "xG (Expected Goals) - média por jogo",
       x = "Conversão de perigo em gol - média por jogo")

### SCATTERPLOT - PERIGO CONCEDIDO X CONVERSÃO CONCEDIDA

# Seleção das variáveis, sumarizadas pelas médias, em um novo objeto

xG_Conversao_Concedidos <- brasileiro_2022_wrangling %>% group_by(Time) %>% 
  summarise(xG_Concedido=mean(xG_Concedido),
            Conversao_Concedida=mean(Conversao_Concedida)) %>%
  select(Time, xG_Concedido, Conversao_Concedida) %>%
  mutate_if(is.numeric, round, 2)

# Criação de variável com os logos dos 20 clubes

xG_Conversao_Concedidos <- xG_Conversao_Concedidos %>% mutate(Logos = case_when(
  xG_Conversao_Concedidos$Time == "Atlético-MG" ~ "https://www.ogol.com.br/img/logos/equipas/2229_imgbank.png",
  xG_Conversao_Concedidos$Time == "Flamengo" ~ "https://www.ogol.com.br/img/logos/equipas/2240_imgbank.png",
  xG_Conversao_Concedidos$Time == "Palmeiras" ~ "https://www.ogol.com.br/img/logos/equipas/2248_imgbank.png",
  xG_Conversao_Concedidos$Time == "Fortaleza" ~ "https://www.ogol.com.br/img/logos/equipas/2239_imgbank.png",
  xG_Conversao_Concedidos$Time == "Corinthians" ~ "https://www.ogol.com.br/img/logos/equipas/2234_imgbank.png",
  xG_Conversao_Concedidos$Time == "RB Bragantino" ~ "https://www.ogol.com.br/img/logos/equipas/3156_imgbank.png",
  xG_Conversao_Concedidos$Time == "Fluminense" ~ "https://www.ogol.com.br/img/logos/equipas/2241_imgbank.png",
  xG_Conversao_Concedidos$Time == "América-MG" ~ "https://www.ogol.com.br/img/logos/equipas/2227_imgbank.png",
  xG_Conversao_Concedidos$Time == "Atlético-GO" ~ "https://www.ogol.com.br/img/logos/equipas/3129_imgbank.png",
  xG_Conversao_Concedidos$Time == "Santos" ~ "https://www.ogol.com.br/img/logos/equipas/2254_imgbank.png",
  xG_Conversao_Concedidos$Time == "Ceará" ~ "https://www.ogol.com.br/img/logos/equipas/3183_imgbank.png",
  xG_Conversao_Concedidos$Time == "Internacional" ~ "https://www.ogol.com.br/img/logos/equipas/2245_imgbank.png",
  xG_Conversao_Concedidos$Time == "São Paulo" ~ "https://www.ogol.com.br/img/logos/equipas/2256_imgbank.png",
  xG_Conversao_Concedidos$Time == "Athletico" ~ "https://www.ogol.com.br/img/logos/equipas/2230_imgbank.png",
  xG_Conversao_Concedidos$Time == "Cuiabá" ~ "https://www.ogol.com.br/img/logos/equipas/3220_imgbank.png",
  xG_Conversao_Concedidos$Time == "Juventude" ~ "https://www.ogol.com.br/img/logos/equipas/2246_imgbank.png",
  xG_Conversao_Concedidos$Time == "Avaí" ~ "https://www.ogol.com.br/img/logos/equipas/2615_imgbank.png",
  xG_Conversao_Concedidos$Time == "Coritiba" ~ "https://www.ogol.com.br/img/logos/equipas/2235_imgbank.png",
  xG_Conversao_Concedidos$Time == "Botafogo" ~ "https://www.ogol.com.br/img/logos/equipas/2233_imgbank.png",
  xG_Conversao_Concedidos$Time == "Goiás" ~ "https://www.ogol.com.br/img/logos/equipas/2244_imgbank.png"))

# Plotagem do gráfico de dispersão xG Concedido x Conversão Concedida
# Funções para visualizar os logos, dividir os quadrantes pelas médias e legendar os quadrantes

ggplot(data = xG_Conversao_Concedidos, mapping = aes(x = Conversao_Concedida, y = xG_Concedido)) +
  geom_point(alpha=3, shape=21, size = 2, show.legend = TRUE, position = "identity") +
  geom_hline(yintercept = mean(xG_Conversao_Concedidos$xG_Concedido), colour = "green3", alpha = 0.3) + 
  geom_vline(xintercept = mean(xG_Conversao_Concedidos$Conversao_Concedida), colour = "green3", alpha = 0.3) + 
  geom_image(aes(image=Logos), size = 0.04, by = "height", position = "identity") +
  geom_text(aes(x = 0.5, y = 1.7, vjust = 0,  label = "Inseguros\nEficientes"), size = 5, color = "black") +
  geom_text(aes(x = 1.3, y = 1.7, vjust = 0, label = "Inseguros\nIneficientes"), size = 5, color = "black") +
  geom_text(aes(x = 0.5, y = 0.5, vjust = 0, label = "Seguros\nEficientes"), size = 5, color = "black") +
  geom_text(aes(x = 1.3, y = 0.5, vjust = 0, label = "Seguros\nIneficientes"), size = 5, color = "black") +
  theme_bw() +
  theme(legend.position="bottom",
        title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(title = "Relação entre Perigo Concedido e Conversão Concedida",
       y = "Perigo concedido - média por jogo",
       x = "Conversão concedida - média por jogo")

######################################################################################################

# COMPARATIVO DE TRÊS VARIÁVEIS AO LONGO DO TEMPO - GRÁFICO DE BARRAS DUPLAS E LINHAS

Gambiarra <- brasileiro_2022 %>% select(Jogo, Gambiarra)

xG_xGA <- brasileiro_2022_wrangling %>% select(Time, Local, Resultado, Data,
                                     Pontos, Gols, Gols_Concedidos, xG,
                                     xG_Concedido, Conversao, Conversao_Concedida ) %>%
                                        mutate(Net_xG = xG - xG_Concedido) %>%
                                        select(Data, Time, Local, Resultado, 
                                               Pontos, Gols, Gols_Concedidos, 
                                               xG, xG_Concedido, Net_xG)

xG_xGA <- bind_cols(xG_xGA, Gambiarra)

xG_xGA <- xG_xGA %>% mutate(Oponente = case_when(
                                      xG_xGA$Gambiarra == "A" ~ lead(xG_xGA$Time),
                                      xG_xGA$Gambiarra == "B" ~ lag(xG_xGA$Time))) %>%
                            select(Data, Jogo, Time, Oponente, Local, Resultado, 
                                   Pontos, Gols, Gols_Concedidos, 
                                   xG, xG_Concedido, Net_xG)

Individual <- xG_xGA %>% filter(xG_xGA$Time == "Palmeiras") %>%
  mutate(Rodada = c(24:1)) %>%
  select(Rodada, Time, Oponente, Local, Resultado,
         xG, xG_Concedido, Net_xG)

xg_mean <- Individual %>%
  summarise(mean(xG)) 

xga_mean <- Individual %>%
  summarise(mean(xG_Concedido) * -1)

Individual_xG <- Individual %>% 
  mutate(xGxGA = xG,
         Logos = case_when(
           Individual$Oponente == "Atlético-MG" ~ "https://www.ogol.com.br/img/logos/equipas/2229_imgbank.png",
           Individual$Oponente == "Flamengo" ~ "https://www.ogol.com.br/img/logos/equipas/2240_imgbank.png",
           Individual$Oponente == "Palmeiras" ~ "https://www.ogol.com.br/img/logos/equipas/2248_imgbank.png",
           Individual$Oponente == "Fortaleza" ~ "https://www.ogol.com.br/img/logos/equipas/2239_imgbank.png",
           Individual$Oponente == "Corinthians" ~ "https://www.ogol.com.br/img/logos/equipas/2234_imgbank.png",
           Individual$Oponente == "RB Bragantino" ~ "https://www.ogol.com.br/img/logos/equipas/3156_imgbank.png",
           Individual$Oponente == "Fluminense" ~ "https://www.ogol.com.br/img/logos/equipas/2241_imgbank.png",
           Individual$Oponente == "América-MG" ~ "https://www.ogol.com.br/img/logos/equipas/2227_imgbank.png",
           Individual$Oponente == "Atlético-GO" ~ "https://www.ogol.com.br/img/logos/equipas/3129_imgbank.png",
           Individual$Oponente == "Santos" ~ "https://www.ogol.com.br/img/logos/equipas/2254_imgbank.png",
           Individual$Oponente == "Ceará" ~ "https://www.ogol.com.br/img/logos/equipas/3183_imgbank.png",
           Individual$Oponente == "Internacional" ~ "https://www.ogol.com.br/img/logos/equipas/2245_imgbank.png",
           Individual$Oponente == "São Paulo" ~ "https://www.ogol.com.br/img/logos/equipas/2256_imgbank.png",
           Individual$Oponente == "Athletico" ~ "https://www.ogol.com.br/img/logos/equipas/2230_imgbank.png",
           Individual$Oponente == "Cuiabá" ~ "https://www.ogol.com.br/img/logos/equipas/3220_imgbank.png",
           Individual$Oponente == "Juventude" ~ "https://www.ogol.com.br/img/logos/equipas/2246_imgbank.png",
           Individual$Oponente == "Avaí" ~ "https://www.ogol.com.br/img/logos/equipas/2615_imgbank.png",
           Individual$Oponente == "Coritiba" ~ "https://www.ogol.com.br/img/logos/equipas/2235_imgbank.png",
           Individual$Oponente == "Botafogo" ~ "https://www.ogol.com.br/img/logos/equipas/2233_imgbank.png",
           Individual$Oponente == "Goiás" ~ "https://www.ogol.com.br/img/logos/equipas/2244_imgbank.png")) %>%
  select(Rodada, Time, Oponente, Local, Resultado,
         xG, xG_Concedido, xGxGA, Net_xG, Logos)

Individual_xGA <- Individual %>% 
  mutate(xGxGA = -1 * xG_Concedido,
         Logos = case_when(
           Individual$Oponente == "Atlético-MG" ~ "https://www.ogol.com.br/img/logos/equipas/2229_imgbank.png",
           Individual$Oponente == "Flamengo" ~ "https://www.ogol.com.br/img/logos/equipas/2240_imgbank.png",
           Individual$Oponente == "Palmeiras" ~ "https://www.ogol.com.br/img/logos/equipas/2248_imgbank.png",
           Individual$Oponente == "Fortaleza" ~ "https://www.ogol.com.br/img/logos/equipas/2239_imgbank.png",
           Individual$Oponente == "Corinthians" ~ "https://www.ogol.com.br/img/logos/equipas/2234_imgbank.png",
           Individual$Oponente == "RB Bragantino" ~ "https://www.ogol.com.br/img/logos/equipas/3156_imgbank.png",
           Individual$Oponente == "Fluminense" ~ "https://www.ogol.com.br/img/logos/equipas/2241_imgbank.png",
           Individual$Oponente == "América-MG" ~ "https://www.ogol.com.br/img/logos/equipas/2227_imgbank.png",
           Individual$Oponente == "Atlético-GO" ~ "https://www.ogol.com.br/img/logos/equipas/3129_imgbank.png",
           Individual$Oponente == "Santos" ~ "https://www.ogol.com.br/img/logos/equipas/2254_imgbank.png",
           Individual$Oponente == "Ceará" ~ "https://www.ogol.com.br/img/logos/equipas/3183_imgbank.png",
           Individual$Oponente == "Internacional" ~ "https://www.ogol.com.br/img/logos/equipas/2245_imgbank.png",
           Individual$Oponente == "São Paulo" ~ "https://www.ogol.com.br/img/logos/equipas/2256_imgbank.png",
           Individual$Oponente == "Athletico" ~ "https://www.ogol.com.br/img/logos/equipas/2230_imgbank.png",
           Individual$Oponente == "Cuiabá" ~ "https://www.ogol.com.br/img/logos/equipas/3220_imgbank.png",
           Individual$Oponente == "Juventude" ~ "https://www.ogol.com.br/img/logos/equipas/2246_imgbank.png",
           Individual$Oponente == "Avaí" ~ "https://www.ogol.com.br/img/logos/equipas/2615_imgbank.png",
           Individual$Oponente == "Coritiba" ~ "https://www.ogol.com.br/img/logos/equipas/2235_imgbank.png",
           Individual$Oponente == "Botafogo" ~ "https://www.ogol.com.br/img/logos/equipas/2233_imgbank.png",
           Individual$Oponente == "Goiás" ~ "https://www.ogol.com.br/img/logos/equipas/2244_imgbank.png")) %>%
  select(Rodada, Time, Oponente, Local, Resultado,
         xG, xG_Concedido, xGxGA, Net_xG, Logos)

Team_xGxGA <- rbind(Individual_xG, Individual_xGA)

Team_xGxGA_color <- case_when(
  Team_xGxGA$xGxGA > 0 ~ "skyblue",
  Team_xGxGA$xGxGA < 0 ~ "firebrick1"
)

Result_color <- case_when(
  Team_xGxGA$Resultado == "V" ~ "forestgreen",
  Team_xGxGA$Resultado == "E" ~ "orange",
  Team_xGxGA$Resultado == "D" ~ "red2",
)

ggplot(data = Team_xGxGA, aes(x = Rodada, y = xGxGA)) +
  geom_bar(fill = Team_xGxGA_color, size = 0, stat = "identity", show.legend = FALSE) + 
  geom_image(aes(image=Logos, y = -4), size = 0.02) + 
  geom_point(aes(x = Rodada, y = 5),
             color = Result_color, show.legend = FALSE,
             shape = 21, stroke = 1.5, size = 8) +
  geom_text(aes(x = Rodada, y = 5, label = Resultado), size = 4, color = Result_color) +
  geom_point(aes(x = Rodada, y = Net_xG),
             show.legend = FALSE, shape = 21, stroke = 2, size = 3) +
  geom_line(aes(x = Rodada, y = Net_xG),
            size = 1.2) + 
  geom_label(data = Team_xGxGA %>% filter(xGxGA > 0), aes(x = Rodada, y = xGxGA, label = xGxGA), 
             nudge_y = 0.3, nudge_x = 0, size = 3, fill = "black", color = "white") +
  geom_label(data = Team_xGxGA %>% filter(xGxGA < 0), aes(x = Rodada, y = xGxGA, label = xGxGA), 
             nudge_y = -0.3, nudge_x = 0, size = 3, fill = "black", color = "white") + 
  geom_hline(data = xg_mean, aes(yintercept = as.numeric(round(xg_mean,2))), 
             color = "orange", linetype='dotted') +
  geom_hline(data = xga_mean, aes(yintercept = as.numeric(round(xga_mean,2))), 
             color = "orange", linetype='dotted') +
  scale_y_continuous(breaks = c(as.numeric(round(xga_mean,2)), as.numeric(round(xg_mean,2)))) +
  labs(title = paste0({Individual$Time[1]}," - Variação dos indicadores xG, xG Concedido e NETxG"),
       subtitle = "Brasileiro Série A 2022 - 24 rodadas",
       y = "xG / xGA",
       x = "Rodada") +
  theme_test()

######################################################################################################

# STACKED BARS - COMPARATIVO DE INDICADORES E TIMES

# Seleção da base de dados
stacked_dados <- modelo_cfa_brasileiro22

# Normalização das escalas numéricas
stacked_dados [, 3:6] <- apply(stacked_dados [, 3:6],  2, scale)

# Eliminação de observações negativas com exponencialização ao quadrado de todos os dados
# Filtragem pelos 5 clubes melhores colocados do Brasileiro 2022 na 24ª rodada

stacked_dados <- stacked_dados %>% select(Time, Posse, Ataque, Defesa, Eficiencia) %>%
  mutate(Posse = Posse^2,
         Ataque = Ataque^2,
         Defesa = Defesa^2,
         Eficiencia = Eficiencia^2) %>% 
  select(Time, Posse, Ataque, Defesa, Eficiencia) %>%
  filter(Time %in% c("Palmeiras", "Flamengo", "Fluminense",
                     "Corinthians", "Internacional")) %>%
  mutate_if(is.numeric, round, 2)

# Função para pivotar o data frame, necessário para atender aos atributos do gráfico de barras agrupadas
stacked_melted <- melt(stacked_dados)

# Plotagem do gráfico - COLUNAS AGRUPADAS

ggplot(stacked_melted, aes(fill=Time, y=value, x=reorder(variable, value), group = value)) + 
  geom_bar(position="dodge", stat="identity") +
  theme(plot.background = element_rect(color = "white", fill = "white"),
        panel.background = element_rect(color = "white", fill = "white"),
        panel.border = element_rect(color = "grey94", ggplot2::alpha(0.1)),
        panel.grid.major.x = element_line(color = "grey94", ggplot2::alpha(0.1)),
        panel.grid.major.y = element_line(color = "grey94", ggplot2::alpha(0.1)),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks=element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 15),
        legend.title = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 10),
        legend.direction = "vertical") +
  scale_fill_viridis_d(direction = 1, option = "C") +
  labs(title = "Comparativo de Indicadores\nG-5 do Brasileiro Série A 2022")

# Plotagem do gráfico - COLUNAS EMPILHADAS

ggplot(stacked_melted, aes(fill=Time, y=value, x=reorder(variable, value), group = value)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.background = element_rect(color = "white", fill = "white"),
        panel.background = element_rect(color = "white", fill = "white"),
        panel.border = element_rect(color = "grey94", ggplot2::alpha(0.1)),
        panel.grid.major.x = element_line(color = "grey94", ggplot2::alpha(0.1)),
        panel.grid.major.y = element_line(color = "grey94", ggplot2::alpha(0.1)),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks=element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 15),
        legend.title = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 10),
        legend.direction = "vertical") +
  scale_fill_viridis_d(direction = 1, option = "C") +
  labs(title = "Comparativo de Indicadores\nG-5 do Brasileiro Série A 2022")

# Plotagem do gráfico - COLUNAS AGRUPADAS COM COORD_POLAR

ggplot(stacked_melted, aes(fill=Time, y=value, x=reorder(variable, value), group = value)) + 
  geom_bar(position="dodge", stat="identity") +
  theme(plot.background = element_rect(color = "white", fill = "white"),
        panel.background = element_rect(color = "white", fill = "white"),
        panel.border = element_blank(),
        panel.grid.major.x = element_line(color = "grey94", ggplot2::alpha(0.1)),
        panel.grid.major.y = element_line(color = "grey94", ggplot2::alpha(0.1)),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks=element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 15),
        legend.title = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 10),
        legend.direction = "vertical") +
  scale_fill_viridis_d(direction = 1, option = "C") +
  labs(title = "Comparativo de Indicadores\nG-5 do Brasileiro Série A 2022") + 
  coord_polar()

# Plotagem do gráfico - COLUNAS EMPILHADAS COM COORD_POLAR

ggplot(stacked_melted, aes(fill=Time, y=value, x=reorder(variable, value), group = value)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.background = element_rect(color = "white", fill = "white"),
        panel.background = element_rect(color = "white", fill = "white"),
        panel.border = element_blank(),
        panel.grid.major.x = element_line(color = "grey94", ggplot2::alpha(0.1)),
        panel.grid.major.y = element_line(color = "grey94", ggplot2::alpha(0.1)),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks=element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 15),
        legend.title = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 10),
        legend.direction = "vertical") +
  scale_fill_viridis_d(direction = 1, option = "C") +
  labs(title = "Comparativo de Indicadores\nG-5 do Brasileiro Série A 2022") + 
  coord_polar()

# Plotagem do gráfico - COLUNAS EMPILHADAS COM COORD_POLAR E EIXOS INVERTIDOS

ggplot(stacked_melted, aes(fill=variable, y=value, x=reorder(Time, value), group = Time)) + 
  geom_bar(position="stack", stat="identity") +
  theme(plot.background = element_rect(color = "white", fill = "white"),
        panel.background = element_rect(color = "white", fill = "white"),
        panel.border = element_blank(),
        panel.grid.major.x = element_line(color = "grey94", ggplot2::alpha(0.1)),
        panel.grid.major.y = element_line(color = "grey94", ggplot2::alpha(0.1)),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks=element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 15),
        legend.title = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 10),
        legend.direction = "vertical") +
  scale_fill_viridis_d(direction = 1, option = "C") +
  labs(title = "Comparativo de Indicadores\nG-5 do Brasileiro Série A 2022") + 
  coord_polar()

