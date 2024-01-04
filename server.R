# input <- list(
#   categorias = c("economia", "salud"),
#   tiempo     = 24 * 1,
#   fecha      = c(Sys.Date() - days(6), Sys.Date()),
#   ngram      = 2,
#   term       = "años"
# )

server <- function(input, output, session) {
  
  # actualizar dateRange para la última semana al partir sesión
  updateDateRangeInput(
    session = getDefaultReactiveDomain(),
    "fecha",
    end   = Sys.Date(),
    start = Sys.Date() - days(6),
    max = Sys.Date()
    )
  
  output$fecha_info <- renderUI(diffdate2(input$fecha[1], input$fecha[2]))
  
  # inicialización
  data_noticias <- reactiveVal(get_noticias_date_range(Sys.Date() - days(6), Sys.Date()))
  # data_noticias <- get_noticias_date_range(Sys.Date() - days(6), Sys.Date())
  
  output$hc_conceptos <- renderHighchart({
    data_noticias <- isolate(data_noticias())
    data_noticias_ngram <- head(get_noticias_ngram(data_noticias, 1), 10)
    highchart() |>
      # hc_subtitle(text = "Top 10 Trending Palabras") |>
      hc_add_series(
        data = data_noticias_ngram$n,
        id = "data",
        type = "bar",
        color = PARS$color_chart,
        showInLegend = FALSE,
        name = "Conceptos más frecuentes"
      ) |>
      hc_xAxis(categories = data_noticias_ngram$ngram) |>
      hc_plotOptions(
        series = list(
          cursor = "pointer",
          point = list(events = list(click = JS("function(){ Shiny.onInputChange('term', this.category) }")))
        )
      )
  })
  
  output$hc_noticiasc <- renderHighchart({
    data_noticias <- isolate(data_noticias())
    data_noticias_categorias <- get_noticias_categorias(data_noticias)
    highchart() |>
      # hc_subtitle(text = "Treding Temas") |> 
      hc_add_series(
        data = data_noticias_categorias$n,
        id = "data",
        type = "bar",
        color = PARS$color_chart,
        showInLegend = FALSE,
        name = "Noticias por categoría"
      ) |> 
      hc_xAxis(categories = data_noticias_categorias$categoria)
  })
  
  output$hc_hechosnot <- renderHighchart({
    
  })
  
  # data_noticias <- reactive({
  #   data_noticias <- get_noticias_date_range(input$fecha[1], input$fecha[2], input$categorias)
  #   }) |>
  #   debounce(2000)

  observe({
    
    cli::cli_alert_info("observe actualización graficos")
    
    highchartProxy("hc_conceptos") |> hcpxy_loading(action = "show")
    highchartProxy("hc_noticiasc") |> hcpxy_loading(action = "show")
    
    cli::cli_alert_info("observe actualización graficos: data")
    # data_noticias            <- get_noticias_date_range(input$fecha[1], input$fecha[2], input$categorias)
    data_noticias(get_noticias_date_range(input$fecha[1], input$fecha[2], input$categorias))
    
    data_noticias            <- data_noticias()
    data_noticias_ngram      <- get_noticias_ngram(data_noticias, as.numeric(input$ng))
    data_noticias_categorias <- get_noticias_categorias(data_noticias)
    # data_dimension           <- get_data_dimension()
    
    cli::cli_alert_info("observe actualización graficos: graficos")
    
    highchartProxy("hc_conceptos") |> 
      hcpxy_update(xAxis = list(categories = head(data_noticias_ngram$ngram, 10))) |> 
      hcpxy_update_series(id = "data", data =  head(data_noticias_ngram$n, 10))
    
    highchartProxy("hc_noticiasc") |> 
      hcpxy_update(xAxis = list(categories = data_noticias_categorias$categoria)) |> 
      hcpxy_update_series(id = "data", data =  data_noticias_categorias$n)
    
    highchartProxy("hc_conceptos") |> hcpxy_loading(action = "hide")
    highchartProxy("hc_noticiasc") |> hcpxy_loading(action = "hide")
    
    cli::cli_alert_info("observe actualización graficos: fin")
    
  }) |> 
    bindEvent(input$fecha, input$categorias) |> 
    debounce(2000) 
  
  observe({
    
    highchartProxy("hc_conceptos") |> hcpxy_loading(action = "show")
    
    # data_noticias            <- get_noticias_date_range(input$fecha[1], input$fecha[2], input$categorias)
    data_noticias            <- data_noticias()
    data_noticias_ngram      <- get_noticias_ngram(data_noticias, as.numeric(input$ng))
    
    highchartProxy("hc_conceptos") |> 
      hcpxy_update(xAxis = list(categories = head(data_noticias_ngram$ngram, 10))) |> 
      hcpxy_update_series(id = "data", data =  head(data_noticias_ngram$n, 10))
    
    highchartProxy("hc_conceptos") |> hcpxy_loading(action = "hide")
    
  }) |> 
    bindEvent(input$ng)
  
  observe({
    cli::cli_inform("observe input$term: {input$term}")
    
    term <- input$term
    data_noticias <- data_noticias()
    
    dout <- data_noticias |> 
      filter(str_detect(body, regex(term, ignore_case = TRUE))) |> 
      mutate(
        title = str_glue("<a href=\"{url}\" target=\"_blank\">{str_trunc(title, 40)}</a>")
      ) |> 
      select(-body)
    
    hc1 <- dout |> 
      count(date) |> 
      hchart("area", hcaes(date, n), fillOpacity = 0.1,  color = PARS$color_chart, name = "Noticias") |> 
      hc_plotOptions(series = list(marker = list(enabled = FALSE))) |> 
      hc_xAxis(title = "") |> hc_yAxis(title = "")
    
    hc2 <- dout |> 
      count(categoria, sort = TRUE) |> 
      mutate(categoria = fct_inorder(categoria)) |> 
      hchart("pie", hcaes(name = categoria, y = n), name = "Noticias") |> 
      hc_plotOptions(pie = list(dataLabels = list(enabled = TRUE))) |> 
      hc_colors(sort(PARS$palette))
  
    doutdt <- dout |> 
      select(Título = title, Fecha = date, Medio = media, Categoría = categoria) |> 
      datatable(
        escape = FALSE,
        rownames = FALSE,
        options = list(
          bPaginate = FALSE,
          searching = FALSE,
          info = FALSE,
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
          )
        )
    
    showModal(
      modalDialog(
        title = NULL,
        size = "xl",
        easyClose = TRUE,
        fade = TRUE,
        footer = NULL,
        # footer = modalButton("Cerrar"),
        tags$h4(str_glue("Análisis concepto: {str_to_title(term)}")),
        # tags$hr(),
        layout_column_wrap(
          width = NULL, height = 250, fill = FALSE,
          style = htmltools::css(grid_template_columns = "6fr 6fr"),
          card(card_header("Tendencia histórica"), hc1), 
          card(card_header("Distribución categorías"), hc2)
        ),
        card(card_header("Noticias donde se encuentra presente el concepto"), doutdt, height = "350px")
        )
      )
    }) |> 
    bindEvent(input$term)
    
  
}
