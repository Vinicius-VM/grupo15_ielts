#construção dos graficos usados no trabalho
# Fica mais fácil separando do arquivo de analise, pois esse serve para oferecer diretamente as visualizações



# GRÁFICO 1 - NACIONALIDADE

grafico_1 <- function(tipo, nacionalidade, ano) {
  
  dado <- performance_by_nationality %>%
    filter(
      type == tipo,
      nationality == nacionalidade,
      year == ano
    )
  
  grafico <- dado %>%
    ggplot(aes(x = part, y = score)) +
    geom_col(fill = "#287C83", width = 0.7) +
    labs(
      x = "Componente do IELTS",
      y = "Nota",
      title = paste("Desempenho por nacionalidade:", nacionalidade)
    ) +
    theme_minimal()
  
  return(grafico)
}


# GRÁFICO 2 - LINGUAGEM

grafico_2 <- function(tipo, linguagem, ano) {
  
  dado <- performance_by_first_language %>%
    filter(
      type == tipo,
      language == linguagem,
      year == ano
    )
  
  grafico <- dado %>%
    ggplot(aes(x = part, y = score)) +
    geom_col(fill = "#287C83", width = 0.7) +
    labs(
      x = "Componente do IELTS",
      y = "Nota",
      title = paste("Desempenho por primeira língua:", linguagem)
    ) +
    theme_minimal()
  
  return(grafico)
}


# GRÁFICO 3 - MOTIVAÇÕES

grafico_3 <- function(tipo, motivo, ano) {
  
  dado <- demo_by_reasons %>%
    filter(
      type == tipo,
      reason == motivo,
      year == ano
    )
  
  grafico <- dado %>%
    ggplot(aes(x = band, y = percent)) +
    geom_col(fill = "#287C83") +
    labs(
      x = "Nota (Band)",
      y = "Porcentagem",
      title = "Distribuição das notas por motivo"
    ) +
    scale_y_continuous(
      labels = scales::label_percent()
    )
  
  return(grafico)
}
  

#GRÁFICO 4
  
  grafico_4 <- function(){
    return(grafico)
  }

#Grafico 5 - distribuição media acumulada
  grafico_5 <- function(ano, tabela) {
    
    dado <- switch(
      tabela,
      "nationality" = demo_by_nationality,
      "language" = demo_by_first_language,
      "reasons" = demo_by_reasons
    )
    
    dado <- dado %>%
      filter(year == ano) %>%
      mutate(
        band_ord = ifelse(
          as.character(band) == "<4",
          3.5,
          as.numeric(as.character(band))
        )
      ) %>%
      group_by(type, band_ord) %>%
      summarise(
        percent = mean(percent, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(type, band_ord) %>%
      group_by(type) %>%
      mutate(
        percent_acumulado = cumsum(percent)
      ) %>%
      ungroup()
    
    grafico <- dado %>%
      ggplot(
        aes(
          x = band_ord,
          y = percent_acumulado,
          color = type,
          group = type
        )
      ) +
      geom_step(linewidth = 1.2) +
      labs(
        x = "Faixa de pontuação",
        y = "Percentual acumulado",
        color = "Tipo de teste",
        title = "Distribuição acumulada das pontuações"
      ) +
      scale_x_continuous(
        breaks = c(3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9),
        labels = c("<4", "4", "4.5", "5", "5.5", "6",
                   "6.5", "7", "7.5", "8", "8.5", "9")
      ) +
      scale_y_continuous(
        labels = scales::label_percent(),
        limits = c(0, 1)
      ) +
      theme_minimal()
    
    return(grafico)
  }
  
  
  
# GRÁFICO 6 - COMPARAÇÃO ENTRE TIPOS DE TESTE
  
  grafico_6 <- function(ano, tabela) {
    
    # Seleciona a tabela
    dado <- switch(
      tabela,
      "Nacionalidade" = performance_by_nationality,
      "Primeira língua" = performance_by_first_language
    )
    
    # Filtra o ano e calcula as médias
    dado <- dado %>%
      filter(year == ano) %>%
      group_by(type, part) %>%
      summarise(
        score = mean(score, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Cria o gráfico
    grafico <- dado %>%
      ggplot(aes(
        x = score,
        y = part,
        fill = type
      )) +
      geom_col(
        position = "dodge",
        width = 0.7
      ) +
      labs(
        x = "Nota média",
        y = "Componente do IELTS",
        fill = "Tipo de teste",
        title = "Comparação de desempenho entre tipos de teste"
      ) +
      scale_x_continuous(
        limits = c(0, 9),
        breaks = 0:9
      ) +
      theme_minimal()
    
    return(grafico)
  }
  
# GRÁFICO 7 - DESEMPENHO POR CONTINENTE
  
  grafico_7 <- function(parte, ano, tipo) {
    
    dado <- performance_by_nationality %>%
      filter(
        part == parte,
        year == ano,
        type == tipo,
        !is.na(continent),
        !is.na(score)
      )
    
    grafico <- dado %>%
      ggplot(aes(
        x = continent,
        y = score,
        fill = continent
      )) +
      geom_boxplot() +
      labs(
        x = "Continente",
        y = "Nota",
        title = "Distribuição das notas por continente"
      ) +
      scale_y_continuous(breaks = 0:9) +
      coord_cartesian(ylim = c(0, 9)) +
      theme_minimal() +
      theme(
        legend.position = "none"
      )
    
    return(grafico)
  }
  #GRÁFICO 8
  
  grafico_8 <-  function(){
    return(grafico)
  }
  
#GRÁFICO 9
  
  grafico_9 <- function(){
    return(grafico)
  }
  
#GRÁFICO 10
  
  grafico_10 <-  function(){
    return(grafico)
  }
