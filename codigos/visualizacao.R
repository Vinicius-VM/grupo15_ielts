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
    ggplot() +
    aes(x = part, y = score) +
    geom_col()
  
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
    ggplot() +
    aes(x = part, y = score) +
    geom_col()
  
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
    ggplot() +
    aes(x = reason) +
    geom_bar()
  
  return(grafico)
}
    
#GRÁFICO 4
  
  grafico_4 <- function(){
    return(grafico)
  }
  
#GRÁFICO 5
  
  grafico_5 <-  function(){
    return(grafico)
  }
  
#GRÁFICO 6
  
  grafico_6 <-  function(){
    return(grafico)
  }
  
#GRÁFICO 7
  
  grafico_7 <-  function(){
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
