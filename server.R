# input <- list(
#   categorias = NULL,
#   tiempo     = 24 * 1,
#   ngram      = 2,
#   term       = "años"
# )

server <- function(input, output, session) {
  
  # inicialización
  data_noticias <- reactiveVal(get_noticias_ultimas_horas(24))
  # data_noticias <- get_noticias_ultimas_horas(24 * 7)
  
  output$hc0 <- renderHighchart({
    data_dimension <- get_data_dimension()
    highchart() |>
      hc_subtitle(text = "Dimensiones") |> 
      hc_chart(polar = TRUE) |> 
      hc_add_series(
        data = data_dimension$score,
        id = "data",
        type = "area",
        fillOpacity = 0.25,
        color = PARS$color_chart,
        showInLegend = FALSE,
        name = "Dimensiones"
      ) |> 
      hc_yAxis(min = 0, max = 100) |> 
      hc_xAxis(categories = data_dimension$dimension)
  })
  
  output$hc1 <- renderHighchart({
    data_noticias <- isolate(data_noticias())
    data_noticias_ngram <- head(get_noticias_ngram(data_noticias, 1), 10)
    highchart() |>
      hc_subtitle(text = "Top 10 Trending Palabras") |> 
      hc_add_series(
        data = data_noticias_ngram$n,
        id = "data",
        type = "bar",
        color = PARS$color_chart,
        showInLegend = FALSE,
        name = "Trending Palabras"
      ) |> 
      hc_xAxis(categories = data_noticias_ngram$ngram) |> 
      hc_plotOptions(
        series = list(
          cursor = "pointer",
          point = list(events = list(click = JS("function(){ Shiny.onInputChange('term', this.category) }")))
        )
      )
  })
  
  output$hc2 <- renderHighchart({
    data_noticias <- isolate(data_noticias())
    data_noticias_categorias <- get_noticias_categorias(data_noticias)
    highchart() |>
      hc_subtitle(text = "Treding Temas") |> 
      hc_add_series(
        data = data_noticias_categorias$n,
        id = "data",
        type = "bar",
        color = PARS$color_chart,
        showInLegend = FALSE,
        name = "Trending Temas"
      ) |> 
      hc_xAxis(categories = data_noticias_categorias$categoria)
  })
  
  output$hc3 <- renderHighchart({
    data_noticias <- isolate(data_noticias())
    data_noticias_ngram <- head(get_noticias_ngram(data_noticias, 1), 100)
    highchart() |>
      hc_colors(PARS$palette) |> 
      hc_add_series(
        data = list_parse2(data_noticias_ngram),
        id = "data",
        type = "wordcloud",
        # color = PARS$color_chart,
        showInLegend = FALSE,
        name = "Ocurrencias",
        style = list(fontFamily = "Montserrat")
        ) |> 
      hc_plotOptions(
        series = list(
          cursor = "pointer",
          point = list(events = list(click = JS("function(){ Shiny.onInputChange('term', this.name) }")))
        )
      )
  })
  
  # data_noticias <- reactive({
  #   data_noticias <- get_noticias_ultimas_horas(input$tiempo, input$categorias)
  #   }) |>
  #   debounce(2000)

  observe({
    
    highchartProxy("hc0") |> hcpxy_loading(action = "show")
    highchartProxy("hc1") |> hcpxy_loading(action = "show")
    highchartProxy("hc2") |> hcpxy_loading(action = "show")
    highchartProxy("hc3") |> hcpxy_loading(action = "show")

    data_noticias(get_noticias_ultimas_horas(input$tiempo, input$categorias))
    data_noticias            <- get_noticias_ultimas_horas(input$tiempo, input$categorias)
    data_noticias_ngram      <- get_noticias_ngram(data_noticias, as.numeric(input$ng))
    data_noticias_categorias <- get_noticias_categorias(data_noticias)
    data_dimension           <- get_data_dimension()
    
    highchartProxy("hc0") |> 
      hcpxy_update_series(id = "data", data =  data_dimension$score)
    
    highchartProxy("hc1") |> 
      hcpxy_update(xAxis = list(categories = head(data_noticias_ngram$ngram, 10))) |> 
      hcpxy_update_series(id = "data", data =  head(data_noticias_ngram$n, 10))
    
    highchartProxy("hc2") |> 
      hcpxy_update(xAxis = list(categories = data_noticias_categorias$categoria)) |> 
      hcpxy_update_series(id = "data", data =  data_noticias_categorias$n)
    
    highchartProxy("hc3") |> 
      hcpxy_update_series(id = "data", data =  list_parse2(head(data_noticias_ngram, 100)))
    
    highchartProxy("hc0") |> hcpxy_loading(action = "hide")
    highchartProxy("hc1") |> hcpxy_loading(action = "hide")
    highchartProxy("hc2") |> hcpxy_loading(action = "hide")
    highchartProxy("hc3") |> hcpxy_loading(action = "hide")
    
  }) |> 
    bindEvent(input$tiempo, input$categorias) |> 
    debounce(2000) 
  
  observe({
    
    highchartProxy("hc1") |> hcpxy_loading(action = "show")
    
    # data_noticias            <- get_noticias_ultimas_horas(input$tiempo, input$categorias)
    data_noticias            <- data_noticias()
    data_noticias_ngram      <- get_noticias_ngram(data_noticias, as.numeric(input$ng))
    
    highchartProxy("hc1") |> 
      hcpxy_update(xAxis = list(categories = head(data_noticias_ngram$ngram, 10))) |> 
      hcpxy_update_series(id = "data", data =  head(data_noticias_ngram$n, 10))
    
    highchartProxy("hc1") |> hcpxy_loading(action = "hide")
    
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
        title = str_glue("Análisis término: {str_to_title(term)}"),
        size = "xl",
        easyClose = TRUE,
        fade = TRUE,
         # footer = modalButton("Cerrar"),
        
        layout_column_wrap(
          width = NULL, height = 300, fill = FALSE,
          style = htmltools::css(grid_template_columns = "6fr 6fr"),
          card(hc1), 
          card(hc2)
        ),
        
        card(doutdt, height = "300px")
        )
      )
    }) |> 
    bindEvent(input$term)
    
  
}
