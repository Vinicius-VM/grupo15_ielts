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
  "pagina_1"  = "01 · Desempenho",
  "pagina_2"  = "02 · Nacionalidade",
  "pagina_3"  = "03 · Motivações",
  "pagina_4"  = "04 · Comparações",
  "pagina_5"  = "05 · Distribuições",
  "pagina_6"  = "06 · Componentes",
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
    
    # Filtro 1
    selectInput(
      "pais",
      "Nacionalidade",
      choices = c(
        "Selecionar..." = "",
        "Todos os países" = "Todos"
      ),
      selected = ""
    ),
    
    # Filtro 2
    selectInput(
      "motivacao",
      "Motivação para realizar o teste",
      choices = c(
        "Selecionar..." = "",
        "Todas as motivações" = "Todas"
      ),
      selected = ""
    ),
    
    # Filtro 3
    selectInput(
      "componente",
      "Componente do teste",
      choices = c(
        "Selecionar..." = "",
        "Todos os componentes" = "Todos"
      ),
      selected = ""
    ),
    
    # Filtro 4
    selectInput(
      "tipo_candidato",
      "Tipo de candidato",
      choices = c(
        "Selecionar..." = "",
        "Todos os candidatos" = "Todos"
      ),
      selected = ""
    ),
    
    # Filtro 5
    sliderInput(
      "pontuacao",
      "Faixa de pontuação",
      min = 0,
      max = 9,
      value = c(0, 9),
      step = 0.5
    ),
    
    # Filtro 6
    selectInput(
      "ano",
      "Ano de realização",
      choices = c(
        "Todos os anos" = "",
        "Selecionar ano..." = "Selecionar"
      ),
      selected = ""
    ),
    
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
      
      # Título da página
      h3(titulo),
      
      p(
        "IELTS Test Statistics",
        style = "color: #82909B; font-size: 0.85rem;"
      ),
      
      br(),
      
      # Dois espaços de gráficos
      layout_columns(
        
        card(
          card_header("Visualização 1"),
          
          plotOutput(
            paste0("grafico_", numero, "_1"),
            height = "380px"
          )
        ),
        
        card(
          card_header("Visualização 2"),
          
          plotOutput(
            paste0("grafico_", numero, "_2"),
            height = "380px"
          )
        ),
        
        col_widths = c(6, 6),
        gap = "1rem"
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
      session, "tipo_candidato",
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
    
    for (j in 1:2) {
      
      local({
        
        pagina <- i
        posicao <- j
        
        id <- paste0(
          "grafico_",
          pagina,
          "_",
          posicao
        )
        
        output[[id]] <- renderPlot({
          
          plot.new()
          
          title(
            main = paste(
              "Página",
              pagina,
              "- Gráfico",
              posicao
            )
          )
          
          text(
            0.5,
            0.5,
            "Espaço reservado para visualização",
            col = "gray50",
            cex = 1.1
          )
          
        })
        
      })
      
    }
    
  }
  
}


##############

# execução

##############

shinyApp(
  ui = ui,
  server = server
)


