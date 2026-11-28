## seçao do App

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(DT)

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

tema <- bs_theme(       #Aqui, basta consultar o site do bootswatch e pegar o que preferir
  version = 5,
  bootswatch = "darkly"
  
)

##############

# Organização das páginas

##############

paginas <- c(
  "pagina_1"  = "Nacionalidade",
  "pagina_2"  = "Linguagem",
  "pagina_3"  = "Motivações",
  "pagina_4"  = "Tabelas",
  "pagina_5"  = "Distribuições",
  "pagina_6"  = "Comparações",
  "pagina_7"  = "Continente",
  "pagina_8"  = "Relações",
  "pagina_9"  = "Tendências",
  "pagina_10" = "Exploração"
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
  
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "style.css"
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
    #PÁGINA 4 - TABELAS
            "pagina_4" = tagList(
        selectInput(
          "tabela_selecionada",
          "Escolha a tabela:",
          choices = c(
            "Demografia por primeira língua" = "demo_by_first_language",
            "Demografia por nacionalidade" = "demo_by_nationality",
            "Demografia por razões" = "demo_by_reasons",
            "Desempenho por primeira língua" = "performance_by_first_language",
            "Desempenho por nacionalidade" = "performance_by_nationality"
          )
        ),
        uiOutput("filtros_tabela")
      ),
    
    #PAGINA 5 - Distribuições medias acumuladas
    
    "pagina_5" = tagList(
      selectInput(
        "tabela_distribuicao",
        "Tabela de dados",
        choices = c(
          "Nacionalidade" = "nationality",
          "Primeira língua" = "language",
          "Motivações" = "reasons"
        )
      ),
      selectInput(
        "ano_distribuicao",
        "Ano de realização",
        choices = sort(unique(demo_by_nationality$year))
      )
    ),
    
    # PÁGINA 6 - COMPARAÇÕES
    "pagina_6" = tagList(
      
      selectInput(
        "ano_comparacao",
        "Ano de realização",
        choices = sort(unique(performance_by_nationality$year))
      ),
      
      selectInput(
        "tabela_comparacao",
        "Tabela de comparação",
        choices = c(
          "Nacionalidade" = "Nacionalidade",
          "Primeira língua" = "Primeira língua"
        )
      )
      
    ),
    
    #box plot continente
    "pagina_7" = tagList(
      
      selectInput(
        "part_continente",
        "Componente do IELTS",
        choices = sort(unique(performance_by_nationality$part))
      ),
      
      selectInput(
        "year_continente",
        "Ano de realização",
        choices = sort(unique(performance_by_nationality$year))
      ),
      
      selectInput(
        "type_continente",
        "Tipo de candidato",
        choices = sort(unique(performance_by_nationality$type))
      )
      
    ),
    
      "pagina_8" = NULL,
      "pagina_9" = NULL,
      "pagina_10" = NULL
      
    )
    
  })
  
  ##############
  # ATUALIZAÇÃO DOS FILTROS DE DESEMPENHO
  ##############
  
  observeEvent(
    list(input$pagina_atual, input$nationality, input$language),
    {
      
      # GRÁFICO 1 - NACIONALIDADE
      if (identical(input$pagina_atual, "pagina_1") &&
          !is.null(input$nationality)) {
        
        dado <- performance_by_nationality %>%
          filter(nationality == input$nationality)
        
        # GRÁFICO 2 - PRIMEIRA LÍNGUA
      } else if (identical(input$pagina_atual, "pagina_2") &&
                 !is.null(input$language)) {
        
        dado <- performance_by_first_language %>%
          filter(language == input$language)
        
      } else {
        return()
      }
      
      # Evita atualizar os filtros se não houver dados
      if (nrow(dado) == 0) return()
      
      # Opções disponíveis para o país ou idioma selecionado
      anos <- sort(unique(as.character(dado$year)))
      tipos <- sort(unique(as.character(dado$type)))
      
      # Preserva a seleção atual, se ainda for válida
      ano_atual <- isolate(input$year)
      tipo_atual <- isolate(input$type)
      
      if (is.null(ano_atual) || !(ano_atual %in% anos)) {
        ano_atual <- anos[1]
      }
      
      if (is.null(tipo_atual) || !(tipo_atual %in% tipos)) {
        tipo_atual <- tipos[1]
      }
      
      # Atualiza o filtro de ano
      updateSelectInput(
        session,
        "year",
        choices = anos,
        selected = ano_atual
      )
      
      # Atualiza o filtro de tipo
      updateSelectInput(
        session,
        "type",
        choices = tipos,
        selected = tipo_atual
      )
      
    },
    ignoreInit = FALSE
  )
  
  
  output$filtros_tabela <- renderUI({
    
    req(input$tabela_selecionada)
    
    dado <- switch(
      input$tabela_selecionada,
      "demo_by_first_language" = demo_by_first_language,
      "demo_by_nationality" = demo_by_nationality,
      "demo_by_reasons" = demo_by_reasons,
      "performance_by_first_language" = performance_by_first_language,
      "performance_by_nationality" = performance_by_nationality
    )
    
    filtros <- list()
    
    if ("type" %in% names(dado)) {
      filtros <- append(filtros, list(
        selectInput("filtro_type", "Tipo:",
                    choices = c("Todos" = "", sort(unique(dado$type))))
      ))
    }
    
    if ("language" %in% names(dado)) {
      filtros <- append(filtros, list(
        selectInput("filtro_language", "Idioma:",
                    choices = c("Todos" = "", sort(unique(dado$language))))
      ))
    }
    
    if ("nationality" %in% names(dado)) {
      filtros <- append(filtros, list(
        selectInput("filtro_nationality", "Nacionalidade:",
                    choices = c("Todos" = "", sort(unique(dado$nationality))))
      ))
    }
    
    if ("reason" %in% names(dado)) {
      filtros <- append(filtros, list(
        selectInput("filtro_reason", "Razão:",
                    choices = c("Todos" = "", sort(unique(dado$reason))))
      ))
    }
    
    if ("continent" %in% names(dado)) {
      filtros <- append(filtros, list(
        selectInput("filtro_continent", "Continente:",
                    choices = c("Todos" = "", sort(unique(na.omit(dado$continent))))
        )
      ))
    }
    
    if ("year" %in% names(dado)) {
      filtros <- append(filtros, list(
        selectInput("filtro_year", "Ano:",
                    choices = c("Todos" = "", sort(unique(dado$year))))
      ))
    }
    
    do.call(tagList, filtros)
  })
  
  #funçãp para filtrar dados da tabela
    
    dados_tabela_filtrados <- reactive({
      
      req(input$tabela_selecionada)
      
      dado <- switch(
        input$tabela_selecionada,
        "demo_by_first_language" = demo_by_first_language,
        "demo_by_nationality" = demo_by_nationality,
        "demo_by_reasons" = demo_by_reasons,
        "performance_by_first_language" = performance_by_first_language,
        "performance_by_nationality" = performance_by_nationality
      )
      
      # Aplica somente filtros cujas colunas existem na tabela
      filtros <- list(
        type = input$filtro_type,
        language = input$filtro_language,
        nationality = input$filtro_nationality,
        reason = input$filtro_reason,
        continent = input$filtro_continent,
        year = input$filtro_year
      )
      
      for (coluna in names(filtros)) {
        
        valor <- filtros[[coluna]]
        
        if (
          coluna %in% names(dado) &&
          !is.null(valor) &&
          valor != ""
        ) {
          
          dado <- dado %>%
            filter(.data[[coluna]] == valor)
          
        }
      }
      
      dado
    })
    
  
  #
  output$tabela_dados <- DT::renderDT({
    
    DT::datatable(
      dados_tabela_filtrados(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(
          search = "Pesquisar:",
          lengthMenu = "Mostrar _MENU_ registros",
          info = "Mostrando _START_ até _END_ de _TOTAL_ registros",
          paginate = list(
            previous = "Anterior",
            `next` = "Próximo"
          )
        )
      ),
      rownames = FALSE
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
      
      if (input$pagina_atual == "pagina_4") {
        
        tagList(
          h3(titulo),
          
          p(
            "IELTS Test Statistics",
            style = "color: #82909B; font-size: 0.85rem;"
          ),
          
          br(),
          
          card(
            card_header("Visualização da tabela"),
            DT::DTOutput("tabela_dados")
          )
        )
        
      } else {
        
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
        
      }
      
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
          
          } else if (pagina == 5) {
           req(input$tabela_distribuicao, input$ano_distribuicao)
           
           grafico_5(
             ano = input$ano_distribuicao,
             tabela = input$tabela_distribuicao
           )
           
           } else if (pagina == 6) {
           
           req(
             input$ano_comparacao,
             input$tabela_comparacao
           )
           
           grafico_6(
             ano = input$ano_comparacao,
             tabela = input$tabela_comparacao
           ) 
           
        } else if (pagina == 7) {
          
          req(
            input$part_continente,
            input$year_continente,
            input$type_continente
          )
          
          grafico_7(
            parte = input$part_continente,
            ano = input$year_continente,
            tipo = input$type_continente
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


