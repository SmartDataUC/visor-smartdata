library(shiny)
library(bslib)
library(shinyWidgets)
library(shinyjs)
library(tidyverse)
library(highcharter)

# Define UI for application that draws a histogram
ui <- page_navbar(
  lang = "es",
  inverse = FALSE,
  sidebar = NULL,
  # inicio ------------------------------------------------------------------
  nav_panel(
    shinyjs::useShinyjs(),  # Set up shinyjs
    title = tags$span("Tendencias", class = "me-3"),
    icon  = icon("arrow-trend-up"),
    
    shinyWidgets::searchInput(
      inputId = "tags",
      # label = "Click search icon to update or hit 'Enter'",
      label = NULL,
      placeholder = "Ingresa un término para analizar",
      btnSearch = icon("add"), 
      btnReset = icon("remove"),
      # btnReset = NULL, 
      width = "100%"
    ),
    
    uiOutput("value"),
    
    shinyjs::disabled(shiny::actionButton("term_go", "Analizar")),
    
    highchartOutput("trend_hc1")
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  trend_terms <- reactiveVal(c())
  
  # observador, que agrega términos a la comparacion cuando se hace enter o click
  # en el searchInput
  observe({
    # si es un valor distinto de nulo, agregamos a la lista y borramos contenido
    if(nchar(input$tags) > 0 & input$tags != ""){
      trend_terms(unique(c(trend_terms(), input$tags)))
      
      updateSearchInput(session, "tags", value = "")
      
    }
  }) |> 
    bindEvent(input$tags)
  
  # observador que elimina items cuando se hace click en el closebutton
  observe({
    print(input$tag_remove)
    trend_terms(trend_terms()[-input$tag_remove$id])
  }) |> 
    bindEvent(input$tag_remove)
  
  # Observa cantidad de elementos para 
  # - modificar placeholder
  # - disable o enable el input
  observe({
    nt <- length(trend_terms())
    
    plc_hld <- case_when(
      nt <= 0 ~ "Ingresa un término para analizar",
      nt <= 4 ~ "Agrega otro ítem para comparar",
      nt >=  5 ~ "No se pueden comparar más de 5 términos"
    ) 
    updateSearchInput(session, "tags", placeholder = plc_hld)
    
    if(nt >= 5){
      shinyjs::disable(selector = "#tags_text")
    } else {
      shinyjs::enable(selector = "#tags_text")
    }
    
    if(nt == 0){
      shinyjs::disable(id = "term_go")
    } else {
      shinyjs::enable(id = "term_go")
    }
    
  }) |> 
    bindEvent(trend_terms())
  
  output$value <- renderUI({
    trend_terms()         
    t <- trend_terms()
    c <- viridis::viridis(length(t))
    
    # <h2>
    #   <span class="badge" style="background-color:darkred;color:white;cursor:default">
    #     Escándalo presicendial&nbsp;<i class="fas fa-times-circle" onClick="Shiny.onInputChange('tag_remove', 2)"  style="cursor: pointer;"></i>&nbsp;
    #   </span>
    # </h2>
    
    tibble(term  = t, color = c) |>
      mutate(id = row_number()) |> 
      pmap(function(term = "asd", color = "red", id = 2){
        tags$span(
          class = "badge",
          style = str_glue("background-color:{color};color:white;cursor:default"),
          term,
          tags$i(
            class = "fas fa-times-circle",
            style = "cursor: pointer;",
            onClick = str_glue("Shiny.onInputChange('tag_remove', {{ 'id': {id}, '.nonce': Math.random()}})") 
          )
        ) 
        
      }) |> 
      tags$h2()
    
  })
  
  output$trend_hc1 <- renderHighchart({
    
    input$term_go
    terms <- isolate(trend_terms())
    if(length(terms) == 0) return(highchart())
    
    tibble(term = terms, color =  viridis::viridis(length(terms))) |> 
      mutate(value = nchar(term)) |> 
      hchart("column", hcaes(x = term, y = value, color = color))
    
  })
  
}

shinyApp(ui = ui, server = server)