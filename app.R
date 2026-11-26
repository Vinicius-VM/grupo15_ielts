## seçao do App

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)


##############

# Seção de carregar os códigos e os dados, usando a função carregar_dados() desenvolvida na analise.R

##############

scripts <- c(
  "codigos/analise.R",
  "codigos/visualizacao.R"
)

# Verificar existência dos arquivos
for (script in scripts) {
  
  if (!file.exists(script)) {
    stop(paste("Arquivo não encontrado:", script))
  }
  
  source(script)
}

# Carregar dados do IELTS
dados <- carregar_dados_ielts()

# Parte de adicionar cada uma
demo_by_first_language <- dados$demo_by_first_language
demo_by_nationality <- dados$demo_by_nationality
demo_by_reasons <- dados$demo_by_reasons
performance_by_first_language <- dados$performance_by_first_language
performance_by_nationality <- dados$performance_by_nationality


##############

# breve introdução ao tema

##############

#feita essa variável para consultar na hora de editar o tema (serve junto com o Css)

tema <- bs_theme(
  version = 5,
  bootswatch = "slate",               #Aqui, basta consultar o site do bootswatch e pegar o que preferir
  primary = "#315B78",
  secondary = "#6C8495",
  base_font = font_google("Source Sans 3")
)


##############

# Organização das páginas

##############

paginas <- c(
  "pagina_1"  = "01 · Nacionalidade",
  "pagina_2"  = "02 · Linguagem",
  "pagina_3"  = "03 · Motivações",
  "pagina_4"  = "04 · Distribuições",
  "pagina_5"  = "05 · Comparações",
  "pagina_6"  = "06 · Desempenho",
  "pagina_7"  = "07 · Grupos",
  "pagina_8"  = "08 · Relações",
  "pagina_9"  = "09 · Tendências",
  "pagina_10" = "10 · Exploração"
)


##############

# Interface do app

##############

ui <- page_sidebar(
  
  theme = tema,
  
  # Título superior
  title = div(
    tags$strong("IELTS Statistics Observatory"),
    
    tags$small(
      "STATISTICAL REPORT",
      style = "
        display: block;
        font-size: 0.65rem;
        letter-spacing: 2px;
        opacity: 0.7;
      "
    )
  ),
  
  ##############
  
  # barra lateral (de consulta e filtros)
  
  ##############
  
  sidebar = sidebar(
    
    width = "270px",
    open = "desktop",
    
    # PÁGINAS
    h5("PÁGINAS"),
    
    selectInput(
      inputId = "pagina_atual",
      label = NULL,
      
      choices = setNames(
        names(paginas),
        unname(paginas)
      ),
      
      selected = "pagina_1",
      width = "100%"
    ),
    
    hr(),
    
    # FILTROS
    h5("FILTROS"),
    
    uiOutput("filtros_pagina"),
    hr(),
    
    # Botão de limpeza
    actionButton(
      "limpar_filtros",
      "Limpar filtros",
      icon = icon("rotate-left"),
      class = "btn-outline-secondary btn-sm w-100"
    )
    
  ),
  
  
  ##############
  
  # Area principal
  
  ##############
  
  uiOutput("conteudo_pagina")
  
)


##############

# servidor

##############

server <- function(input, output, session) {
  
  
  # Conteúdo das paginas
  output$filtros_pagina <- renderUI({
    
    req(input$pagina_atual)
    
    switch(
      input$pagina_atual,
      
      # PÁGINA 1 - NACIONALIDADE
      "pagina_1" = tagList(
        
        selectInput(
          "nationality",
          "Nacionalidade",
          choices = vetor_nationality(performance_by_nationality)
        ),
        
        selectInput(
          "year",
          "Ano de realização",
          choices = vetor_year(performance_by_nationality)
        ),
      
      selectInput(
        "type",
        "Tipo de candidato",
        choices = vetor_type(performance_by_nationality)
        )
    ),
      
      # PÁGINA 2 - LINGUAGEM
      "pagina_2" = tagList(
        
        selectInput(
          "language",
          "Primeira língua",
          choices = vetor_language(performance_by_first_language)
        ),
        
        selectInput(
          "year",
          "Ano de realização",
          choices = vetor_year(performance_by_first_language)
        ),
        
        selectInput(
          "type",
          "Tipo de candidato",
          choices = vetor_type(performance_by_first_language)
        )
        
      ),
      
      # PÁGINA 3 - MOTIVAÇÕES
      "pagina_3" = tagList(
        
        selectInput(
          "reason",
          "Motivação para realizar o teste",
          choices = vetor_reason(demo_by_reasons)
        ),
        
        selectInput(
          "year",
          "Ano de realização",
          choices = vetor_year(demo_by_reasons)
        ),
        
        selectInput(
          "type",
          "Tipo de candidato",
          choices = vetor_type(demo_by_reasons)
        )
      ),
      
      # DEMAIS PÁGINAS
      "pagina_4" = NULL,
      "pagina_5" = NULL,
      "pagina_6" = NULL,
      "pagina_7" = NULL,
      "pagina_8" = NULL,
      "pagina_9" = NULL,
      "pagina_10" = NULL
      
    )
    
  })
  ##############
  
  # conteúdo das páginas
  
  ##############
  
  output$conteudo_pagina <- renderUI({
    
    req(input$pagina_atual)
    
    numero <- as.integer(
      sub("pagina_", "", input$pagina_atual)
    )
    
    titulo <- paginas[[input$pagina_atual]]
    
    tagList(
      
      h3(titulo),
      
      p(
        "IELTS Test Statistics",
        style = "color: #82909B; font-size: 0.85rem;"
      ),
      
      br(),
      
      card(
        card_header("Visualização"),
        
        plotOutput(
          paste0("grafico_", numero),
          height = "550px"
        )
      )
      
    )
    
  })
  
  
  ##############
  
  # limpar alteraçoes nos filtros
  
  ##############
  
  observeEvent(input$limpar_filtros, {
    
    updateSelectInput(
      session, "pais",
      selected = ""
    )
    
    updateSelectInput(
      session, "motivacao",
      selected = ""
    )
    
    updateSelectInput(
      session, "componente",
      selected = ""
    )
    
    updateSelectInput(
      session, "tipo",
      selected = ""
    )
    
    updateSliderInput(
      session, "pontuacao",
      value = c(0, 9)
    )
    
    updateSelectInput(
      session, "ano",
      selected = ""
    )
    
  })
  
  
  ##############
  
  # criaçao dos espaços de gráficos (feita de maneira que não ocupa muitas linhas)
  
  ##############
  
  for (i in 1:10) {
    
    local({
      
      pagina <- i
      
      id <- paste0("grafico_", pagina)
      
      output[[id]] <- renderPlot({
        
        if (pagina == 1) {
          
          req(input$nationality, input$type, input$year)
          
          grafico_1(
            tipo = input$type,
            nacionalidade = input$nationality,
            ano = input$year
          )
          
        } else if (pagina == 2) {
          
          req(input$language, input$type, input$year)
          
          grafico_2(
            tipo = input$type,
            linguagem = input$language,
            ano = input$year
          )
          
        } else if (pagina == 3) {
          
          req(input$reason, input$type, input$year)
          
          grafico_3(
            tipo = input$type,
            motivo = input$reason,
            ano = input$year
          )
          
        } else {
          
          plot.new()
          
          title(main = paste("Página", pagina))
          
          text(
            0.5, 0.5,
            "Espaço reservado para visualização",
            col = "gray50",
            cex = 1.1
          )
          
        }
        
      })
      
    })
    
  }
  
}


##############

# execução

##############

shinyApp(
  ui = ui,
  server = server
)


