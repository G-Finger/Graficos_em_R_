# Estrutura do R 
# Exercícios - Lista 03 (Conhecendo o mercado aéreo brasileiro)
# Aluno: Gustavo Finger
# Professor Wagner Bonnat

# Chmando as bibliotecas(que devem estar devidamente instaladas no Rstudio). 
library(tidyverse)
library(readr)
library(ggplot2)


# importando os dados (salvei no meu PC). 
url<-"C:/Users/finge/OneDrive/2. Doutorado/1. Disciplina Métodos Computacionais Para estatística/Aula 2 - Módulos/Módulos/Modulo2/Dados_Estatisticos.csv"

#url <- "https://sistemas.anac.gov.br/dadosabertos/Voos%20e%20opera%C3%A7%C3%B5es%20a%C3%A9reas/Dados%20Estat%C3%ADsticos%20do%20Transporte%20A%C3%A9reo/Dados_Estatisticos.csv"


dados<-read_csv2(url, col_names=TRUE, skip=1) # o skip me ajuda a pular a primeira linha, pois este arquivo estava zuado

print(paste0("Número de colunas nos dados: ",ncol(dados)))
colnames(dados)

questao_1_lista3  <- dados %>%
                    filter(EMPRESA_NACIONALIDADE == "BRASILEIRA") %>%
                    count(EMPRESA_NOME, name = "Decolagens") %>%
                    arrange(desc(Decolagens))%>%
                    mutate(percentual = Decolagens / sum(Decolagens) * 100) %>%
                    slice_head(n=10) %>%
                    pull(EMPRESA_NOME) 
                    
# Guardando os dados no tempo
questao_1_lista3_b <- dados %>%
                      filter(EMPRESA_NACIONALIDADE == "BRASILEIRA", EMPRESA_NOME %in% questao_1_lista3) %>%
                      count(ANO, EMPRESA_NOME, name = "Decolagens")
                      
                      
ggplot(questao_1_lista3_b, aes(x = ANO, y = Decolagens, color = EMPRESA_NOME)) +
        geom_line(linewidth = 1.2) +
        labs(
          title = "Série de tempo de decolagens (Top 10 empresas do nosso Brasilzão)",
          x = "Ano",
          y = "Número de decolagens"
            ) +
        scale_x_continuous(breaks = seq(min(questao_1_lista3_b$ANO), max(questao_1_lista3_b$ANO), by = 2))

# vamos usar a Facet que o professor falou para analisarmos.   

ggplot(questao_1_lista3_b, aes(x = ANO, y = Decolagens, color = EMPRESA_NOME)) +
        geom_line(linewidth = 1.2) +
        facet_wrap(~EMPRESA_NOME,ncol = 2, labeller = label_wrap_gen(width = 45))+
        labs(
          title = "Série de tempo de decolagens por empresa",
          x = "Ano",
          y = "Número de decolagens"
            ) +
        scale_x_continuous(breaks = seq(min(questao_1_lista3_b$ANO), max(questao_1_lista3_b$ANO), by = 4))+
        guides(color = guide_legend(ncol = 1))+
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1),
          strip.text = element_text(size = 10)
             )

##########################################        
questao_2_lista3 <- dados %>%
                    group_by(ANO, EMPRESA_NACIONALIDADE)  %>%
                    summarise(passageiro_pagantes = sum(PASSAGEIROS_PAGOS, na.rm=TRUE))



ggplot(questao_2_lista3, 
       mapping = aes(x = ANO, y = passageiro_pagantes, fill = EMPRESA_NACIONALIDADE)) +
        geom_col (position = "dodge")+
        labs(
          title = "Comparação de Pagantes entre empresas nacionais e estrangeiras (Em milhões)",
          x = "Ano",
          y = "Número de Passageiros que Pagam"
            ) +
        scale_x_continuous(breaks = seq(min(questao_2_lista3$ANO), max(questao_2_lista3$ANO), by = 1))+
        scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = " MM")) +
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1),
          strip.text = element_text(size = 10)
              )

##########################################  

questao_3_lista3 <- dados %>%
                    filter(!is.na(GRUPO_DE_VOO), GRUPO_DE_VOO!="NÃO IDENTIFICADO") %>%
                    group_by(GRUPO_DE_VOO, EMPRESA_NACIONALIDADE) %>%
                    summarise(passageiro_pagantes = sum(PASSAGEIROS_PAGOS, na.rm=TRUE))%>%
                    group_by(EMPRESA_NACIONALIDADE) %>%
                    mutate(percentual = passageiro_pagantes / sum(passageiro_pagantes))
                    
questao_3_lista3                            
                    
ggplot(questao_3_lista3,
       aes(x = "", y = percentual, fill = GRUPO_DE_VOO)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      facet_wrap(~EMPRESA_NACIONALIDADE) +
      
      labs(
        title = "Distribuição de passageiros por grupo de voo",
        fill = "Grupo de voo"
      ) +
      
      theme_void() +  # remove tudo
      
      theme(
        plot.title = element_text(hjust = 0.5)
      )

questao_4_lista3 <- dados %>%
                    filter(!is.na(AEROPORTO_DE_ORIGEM_REGIAO)) %>%
                    group_by(AEROPORTO_DE_ORIGEM_REGIAO,NATUREZA) %>%
                    summarise(passageiro_pagantes = sum(PASSAGEIROS_PAGOS, na.rm=TRUE))
                    
questao_4_lista3

ggplot(questao_4_lista3, aes(x = NATUREZA, y = passageiro_pagantes, fill = NATUREZA)) +
        geom_col (position = "dodge") +
        scale_y_log10(questao_4_lista3 <- dados %>%
                    filter(!is.na(AEROPORTO_DE_ORIGEM_REGIAO)) %>%
                    group_by(AEROPORTO_DE_ORIGEM_REGIAO,NATUREZA) %>%
                    summarise(passageiro_pagantes = sum(PASSAGEIROS_PAGOS, na.rm=TRUE)))
                    
questao_4_lista3

ggplot(questao_4_lista3, aes(x = NATUREZA, y = passageiro_pagantes, fill = NATUREZA)) +
        geom_col (position = "dodge") +
        scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale()))+
        facet_wrap(~AEROPORTO_DE_ORIGEM_REGIAO,ncol = 1, labeller = label_wrap_gen(width = 45))+
        labs(
          title = "Passageiros por região de Origem",
          x = "Natureza de Origem",
          y = "Passageiros Pagantes(log10)"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          strip.text = element_text(size = 10),
          legend.position = "none" 
             )+
        facet_wrap(~AEROPORTO_DE_ORIGEM_REGIAO,ncol = 1, labeller = label_wrap_gen(width = 45))+
        labs(
          title = "Passageiros por região de Origem",
          x = "Natureza de Origem",
          y = "Passageiros Pagantes(log10)"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          strip.text = element_text(size = 10),
          axis.title.y = element_text(margin = margin(r = 10)),
          legend.position = "none" 
             )+
        geom_text(
          aes(label = scales::label_number(scale_cut = scales::cut_short_scale())(passageiro_pagantes)),
          position = position_stack(vjust = 0.5),
          color = "white",
          fontface = "bold",
          size = 4
        )

questao_5_lista3 <- dados %>%
                    filter(!is.na(AEROPORTO_DE_DESTINO_REGIAO),!is.na(AEROPORTO_DE_ORIGEM_REGIAO),!is.na(DISTANCIA_VOADA_KM), !is.na(COMBUSTIVEL_LITROS), COMBUSTIVEL_LITROS>0, DISTANCIA_VOADA_KM>0) %>%
                    group_by(AEROPORTO_DE_ORIGEM_REGIAO,AEROPORTO_DE_DESTINO_REGIAO ) %>%
                    summarise(consumo_KM_L = sum(DISTANCIA_VOADA_KM) / sum(COMBUSTIVEL_LITROS), .groups = "drop") %>%
                    arrange(desc(consumo_KM_L)) #%>%
                    #slice_head(n = 50)# somentes as cinco principais empresas 

questao_5_lista3

ggplot(questao_5_lista3,
       aes(x = AEROPORTO_DE_DESTINO_REGIAO,
           y = AEROPORTO_DE_ORIGEM_REGIAO,
           fill = consumo_KM_L)) +
      geom_tile()+
      scale_fill_viridis_c(trans = "log10") +
      labs(
        title = "Eficiência de combustível por rota (origem × destino)",
        x = "Região de destino",
        y = "Região de origem",
        fill = "Eficiência (km/l)"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(r = 15)),
        strip.text = element_text(size = 10),
        axis.title.y = element_text(margin = margin(r = 15)),
        legend.position = "right"
        
           )

questao_6_lista3 <- dados %>%     
                    filter(!is.na(DISTANCIA_VOADA_KM),!is.na(COMBUSTIVEL_LITROS), COMBUSTIVEL_LITROS>50, DISTANCIA_VOADA_KM>100,DISTANCIA_VOADA_KM<20000, ANO>2020) %>%
                    group_by(EMPRESA_NACIONALIDADE,ANO, DISTANCIA_VOADA_KM) %>%
                    mutate(consumo_KM_L = DISTANCIA_VOADA_KM / COMBUSTIVEL_LITROS) 
                    
  

questao_6_lista3

ggplot(questao_6_lista3,
       aes(x = DISTANCIA_VOADA_KM,
           y = consumo_KM_L,
           color = EMPRESA_NACIONALIDADE)) +
           geom_point(alpha = 0.5) +
                     facet_wrap(~ANO) +
                      labs(
              title = "Relação entre distância voada e eficiência de combustível",
              x = "Distância voada (km)",
              y = "Eficiência (km/l)",
              color = "Nacionalidade"
            ) +
            theme_minimal()


questao_8_lista3 <- dados %>%     
                    filter(EMPRESA_NACIONALIDADE == "BRASILEIRA", !is.na(AEROPORTO_DE_ORIGEM_REGIAO), COMBUSTIVEL_LITROS>0, DISTANCIA_VOADA_KM>0,DISTANCIA_VOADA_KM<20000 ) %>%
                    group_by(AEROPORTO_DE_ORIGEM_REGIAO,  DISTANCIA_VOADA_KM) %>% 
                    mutate(consumo_KM_L = DISTANCIA_VOADA_KM / COMBUSTIVEL_LITROS) 
  
                    
ggplot(questao_8_lista3,
       aes(x = DISTANCIA_VOADA_KM,
           y = COMBUSTIVEL_LITROS,
            color = AEROPORTO_DE_ORIGEM_REGIAO)) +
            scale_x_log10() +
            scale_y_log10() + 
            geom_point(alpha = 0.4) +
            geom_smooth(method = "lm", se = FALSE) +
            labs(
              title = "Relação entre combustível e distância por região de origem",
              x = "Distância voada (km)",
              y = "Combustível (litros)",
              color = "Região"
            ) +
            theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#rtk - revenue tonne kilometer
questao_9_lista3 <- dados %>%     
                    filter(!is.na(RTK), COMBUSTIVEL_LITROS>0, RPK>0, DISTANCIA_VOADA_KM>0, ANO>2020) 
                   
questao_9_lista3


ggplot(questao_9_lista3,
       aes(x = RTK,
           y = COMBUSTIVEL_LITROS),
           color = ANO
           ) +
          geom_point(alpha = 0.4) +
          geom_smooth(method = "lm", se = FALSE) +
          
          facet_wrap(~ANO,ncol = 2) +
          
          scale_x_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
          scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
          
          labs(
            title = "Relação entre RTK e consumo de combustível ao longo dos último 6 anos",
            x = "RTK (log)",
            y = "Combustível (litros, log)",
            subtitle = "RTK: revenue tonne kilometer | Combustível: litros consumidos"
          ) +
          
          theme_minimal()+
          theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(r = 15)),
                axis.title.y = element_text(margin = margin(r = 25)),
                legend.position = "right"
               )

#ATK - Available tonne kilometer

questao_10_lista3 <- dados %>%     
                     filter(!is.na(ATK), !is.na(GRUPO_DE_VOO) ) %>%
                     group_by(GRUPO_DE_VOO,  NATUREZA)
        

questao_10_lista3

ggplot(questao_10_lista3,
       aes(x = NATUREZA,
           y = ATK,
           fill = GRUPO_DE_VOO)) +
  
          geom_boxplot() +
          
          facet_wrap(~ANO, ncol= 4) +
          
          scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
          
          theme_minimal()+
         
          labs(
            title = "Relação entre ATK, Grupo de VOO e natureza",
            x = "Natureza",
            y = "Available tonne kilometer",
            subtitle = "Anos 2000 até 2026"
              ) +
          
  
          theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(angle = 90, hjust = 0.25, margin = margin(r = 15)),
          axis.title.y = element_text(margin = margin(r = 25)),
          legend.position = "right"
                )
