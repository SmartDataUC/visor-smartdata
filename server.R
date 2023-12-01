# input <- list(
#   categorias = NULL,
#   tiempo     = 24 * 1,
#   ngram      = 2
# )

server <- function(input, output, session) {
  
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
    data_noticias_ngram <- head(get_noticias_ngram(data_noticias_once, 1), 10)
    highchart() |>
      hc_subtitle(text = "Top 10 Trending Palabras") |> 
      hc_add_series(
        data = data_noticias_ngram$n,
        id = "data",
        type = "bar",
        showInLegend = FALSE,
        name = "Trending Palabras"
      ) |> 
      hc_xAxis(categories = data_noticias_ngram$ngram)
  })
  
  output$hc2 <- renderHighchart({
    data_noticias_categorias <- get_noticias_categorias(data_noticias_once)
    highchart() |>
      hc_subtitle(text = "Treding Temas") |> 
      hc_add_series(
        data = data_noticias_categorias$n,
        id = "data",
        type = "bar",
        showInLegend = FALSE,
        name = "Trending Temas"
      ) |> 
      hc_xAxis(categories = data_noticias_categorias$categoria)
  })
  
  output$hc3 <- renderHighchart({
    data_noticias_ngram <- head(get_noticias_ngram(data_noticias_once, 1), 100)
    highchart() |>
      hc_colors(c("#a5c0c8", "#77959e", "#496973", "#2f4951")) |> 
      hc_add_series(
        data = list_parse2(data_noticias_ngram),
        id = "data",
        type = "wordcloud",
        # color = PARS$color_chart,
        showInLegend = FALSE,
        name = "Ocurrencias",
        style = list(fontFamily = "Montserrat")
        )
  })
  
  # data_noticias <- reactive({
  #  
  # }) |> 
  #   debounce(2000)
  
  observe({
    
    highchartProxy("hc0") |> hcpxy_loading(action = "show")
    highchartProxy("hc1") |> hcpxy_loading(action = "show")
    highchartProxy("hc2") |> hcpxy_loading(action = "show")
    highchartProxy("hc3") |> hcpxy_loading(action = "show")

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
    
    data_noticias            <- get_noticias_ultimas_horas(input$tiempo, input$categorias)
    data_noticias_ngram      <- get_noticias_ngram(data_noticias, as.numeric(input$ng))
    
    highchartProxy("hc1") |> 
      hcpxy_update(xAxis = list(categories = head(data_noticias_ngram$ngram, 10))) |> 
      hcpxy_update_series(id = "data", data =  head(data_noticias_ngram$n, 10))
    
    highchartProxy("hc1") |> hcpxy_loading(action = "hide")
    
  }) |> 
    bindEvent(input$ng)
    
  
}
