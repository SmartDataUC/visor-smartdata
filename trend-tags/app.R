library(shiny)
library(bslib)
library(shinyWidgets)
# Define UI for application that draws a histogram
ui <- page_navbar(
  lang = "es",
  inverse = FALSE,
  sidebar = NULL,
  # inicio ------------------------------------------------------------------
  nav_panel(
    title = tags$span("Tendencias", class = "me-3"),
    icon  = icon("arrow-trend-up"),
    
    shinyWidgets::searchInput(
      inputId = "tags",
      # label = "Click search icon to update or hit 'Enter'",
      label = NULL,
      placeholder = "Agrega un término para analizar",
      btnSearch = icon("add"), 
      btnReset = icon("remove"),
      # btnReset = NULL, 
      width = "100%"
    ),
    
    textOutput("value")
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  trend_terms <- reactiveVal(c())
  
  # agrega terminos a la comparacion cuando se hace enter o click
  observe({
    
    print(trend_terms())
    
    # si es un valor distinto de nulo, agregamos a la lista y borramos contenido
    if(nchar(input$tags) > 0 & input$tags != ""){
      trend_terms(unique(c(trend_terms(), input$tags)))
      updateSearchInput(session, "tags", value = "", placeholder = "Agregar otros ítem para comparar")
    }
    
    print(trend_terms())
    
  }) |> 
    bindEvent(input$tags)
  
  output$value <- renderText({
    trend_terms()           
    # <h2>
    #   <span class="badge" style="background-color:darkred;color:white;cursor:default">
    #     Escándalo presicendial&nbsp;<i class="fas fa-times-circle" onClick="Shiny.onInputChange('tag_remove', 2)"  style="cursor: pointer;"></i>&nbsp;
    #     </span>
    #       </h2>
  }) 
  
  
}

shinyApp(ui = ui, server = server)
