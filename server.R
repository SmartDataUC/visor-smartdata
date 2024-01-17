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
    start = Sys.Date() - months(1) + days(1),
    end   = Sys.Date(),
    max   = Sys.Date()
    )
  
  output$fecha_info <- renderUI({
    data_noticias <- data_noticias()
    str_glue(
      "{nnews} noticias seleccionadas en {time}",
      nnews = comma(nrow(data_noticias)),
      time = diffdate2(input$fecha[1], input$fecha[2])
    )
  })
  
  # inicialización
  data_noticias <- reactiveVal(get_noticias_date_range(Sys.Date() - months(1) + days(1), Sys.Date()))
  # data_noticias <- get_noticias_date_range(Sys.Date() - months(1) + days(1), Sys.Date())
  
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
      hc_xAxis(categories = data_noticias_categorias$categoria) |> 
      hc_plotOptions(
        series = list(
          cursor = "pointer",
          point = list(events = list(click = JS("function(){ Shiny.onInputChange('modal_noticias_cat', this.category) }")))
        )
      )
    
  })
  
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
          point = list(events = list(click = JS("function(){ Shiny.onInputChange('modal_conceptos_term', this.category) }")))
        )
      )
  })
  
  output$hc_gorepresc <- renderHighchart({
    data_noticias <- isolate(data_noticias())
    
    data_noticias_prescgore <- data_noticias |> 
      group_by(name = categoria) |> 
      summarise(value = 100 * mean(as.double(gore))) |> 
      arrange(desc(value)) |> 
      highcharter::list_parse()
    
    highchart() |>
      hc_colors(PARS$palette) |>
      hc_add_series(
        data = data_noticias_prescgore,
        id = "data",
        type = "packedbubble",
        # type = "pie",
        # color = PARS$color_chart,
        # color = "red",
        showInLegend = FALSE,
        name = "Presencia GORE en noticias",
        colorByPoint = TRUE,
        dataLabels = list(
          enabled = TRUE,
          format = "{point.name}"
        ),
        marker = list(enabled = TRUE),
        maxSize = "100%",
        minSize = "5%",
        states = list(hover = list(enabled = FALSE))
      ) |> 
      hc_tooltip(
        valueDecimals = 2,
        pointFormat = "{point.name}: {point.value}%"
        )
    
  })
  
  output$hc_prcepcion <- renderHighchart({
    data_noticias <- isolate(data_noticias())

    data_noticias_gorepresc <- data_noticias |>
      mutate(sentiment = coalesce(sentiment, "NEU")) |>
      count(date, sentiment) |>
      mutate(
        sentiment = case_when(
          sentiment == "NEU" ~ "Neutral",
          sentiment == "NEG" ~ "Negativo",
          TRUE               ~ "Positivo"
        ),
        sentiment = factor(sentiment, levels = c("Negativo", "Neutral", "Positivo"))
      ) |>
      complete(date, sentiment, fill = list(n = 0))

    data_noticias_gorepresc

    highchart() |>
      hc_colors(PARS$palette) |>
      hc_xAxis(type = "datetime") |>
      hc_add_series(
        data = data_noticias_gorepresc,
        hcaes(date, n, group = sentiment),
        type = "area",
        stacking = "normal"
      ) |>
      hc_tooltip(table = TRUE, sort = TRUE)

  })
  
  # mapa principal
  output$map <- renderLeaflet({
    
    leaflet(
      options = leafletOptions(
        attributionControl = FALSE,
        zoomControl = TRUE
      )
    ) |>
      setView(lng =  -70.64827, lat = -33.45694, zoom = 9) |>
      addProviderTiles(providers$CartoDB.Positron,  group = "Administrativo") 
    
  })
  
  # data_noticias <- reactive({
  #   data_noticias <- get_noticias_date_range(input$fecha[1], input$fecha[2], input$categorias)
  #   }) |>
  #   debounce(2000)

  # observeEvent para actualizar los 3 primero graficos
  observe({
    
    cli::cli_alert_info("observe actualización graficos")
    
    highchartProxy("hc_conceptos") |> hcpxy_loading(action = "show")
    highchartProxy("hc_noticiasc") |> hcpxy_loading(action = "show")
    highchartProxy("hc_gorepresc") |> hcpxy_loading(action = "show")
    highchartProxy("hc_prcepcion") |> hcpxy_loading(action = "show")
    
    cli::cli_alert_info("observe actualización graficos: data")
    # data_noticias            <- get_noticias_date_range(input$fecha[1], input$fecha[2], input$categorias, input$comunas)
    data_noticias(get_noticias_date_range(input$fecha[1], input$fecha[2], input$categorias, input$comunas))
    
    data_noticias            <- data_noticias()
    data_noticias_ngram      <- get_noticias_ngram(data_noticias, as.numeric(input$ng))
    data_noticias_categorias <- get_noticias_categorias(data_noticias)
    
    data_noticias_prescgore  <- data_noticias |> 
      group_by(name = categoria) |> 
      summarise(value = 100 * mean(as.double(gore))) |> 
      arrange(desc(value)) |> 
      highcharter::list_parse()
    
    data_noticias_gorepresc <- data_noticias |>
      mutate(sentiment = coalesce(sentiment, "NEU")) |>
      count(date, sentiment) |>
      mutate(
        sentiment = case_when(
          sentiment == "NEU" ~ "Neutral",
          sentiment == "NEG" ~ "Negativo",
          TRUE               ~ "Positivo"
        ),
        sentiment = factor(sentiment, levels = c("Negativo", "Neutral", "Positivo"))
      ) |>
      complete(date, sentiment, fill = list(n = 0))

    # glimpse(data_noticias_prescgore)
    # glimpse(data_noticias_gorepresc)
  
    cli::cli_alert_info("observe actualización graficos: graficos")
    
    highchartProxy("hc_conceptos") |> 
      hcpxy_update(xAxis = list(categories = head(data_noticias_ngram$ngram, 10))) |> 
      hcpxy_update_series(id = "data", data =  head(data_noticias_ngram$n, 10))

    highchartProxy("hc_noticiasc") |> 
      hcpxy_update(xAxis = list(categories = data_noticias_categorias$categoria)) |> 
      hcpxy_update_series(id = "data", data =  data_noticias_categorias$n)
    
    highchartProxy("hc_gorepresc") |>
      hcpxy_update_series(id = "data", data = data_noticias_prescgore)
    
    highchartProxy("hc_prcepcion") |>
      hcpxy_set_data(
        type = "area",
        data = data_noticias_gorepresc,
        hcaes(date, n, group = sentiment),
        redraw = TRUE
      )
    
    highchartProxy("hc_conceptos") |> hcpxy_loading(action = "hide")
    highchartProxy("hc_noticiasc") |> hcpxy_loading(action = "hide")
    highchartProxy("hc_gorepresc") |> hcpxy_loading(action = "hide")
    highchartProxy("hc_prcepcion") |> hcpxy_loading(action = "hide")
    
    cli::cli_alert_info("observe actualización graficos: fin")
    
    
    
  # })
  }) |>
    bindEvent(input$fecha, input$categorias, input$comunas)
  
  # mismo event que el de los graficos, pero para separar las logicas
  observe({
    
    data_noticias            <- data_noticias()
    
    dgeo <- data_noticias |>
      count(categoria, comunas) |> 
      mutate(comunas = map(comunas, ~ str_squish(unlist(str_split(.x, "\\,"))))) |> 
      unnest(comunas) |>
      mutate(comunas = str_remove_all(comunas, "\"|\\'|\\[|\\]|\\{|\\}")) |> 
      filter(comunas != "") |> 
      count(comuna = comunas, wt = n, sort = TRUE)
    
    dgeo <- left_join(dcomunas, dgeo, by = join_by(comuna)) |> 
      mutate(
        # n = coalesce(n, 0),
        id = snakecase::to_snake_case(stringi::stri_trans_general(comuna, id = "Latin-ASCII")),
        nf = coalesce(comma(n), "sin")
      )
    
    # viridisLite::plasma(15, end = .85) |> scales::show_col()
    cols <-  viridisLite::plasma(15, end = .85, direction = -1)
    # cols <- sort(PARS$palette)
    colorData <- dgeo[["n"]]
    pal  <- colorBin(cols, colorData, 7, pretty = TRUE, na.color = "#C0C0C0")
    
    # m <- leaflet() |> addTiles()
    m <- leafletProxy("map")
    
    m |> 
      clearShapes() |>
      clearTopoJSON() |>
      leaflet::addPolygons(
        data             = dgeo,
        fillColor        = ~ pal(colorData),
        weight           = .5,
        dashArray        = "3",
        stroke           = NULL,
        fillOpacity      = 0.7,
        layerId          = ~ id,
        # popup            = popp,
        label            =  ~ str_glue("{comuna}: {nf} noticias."),
        highlightOptions = highlightOptions(
          color        = "white",
          weight       = 2,
          fillColor    = PARS$color_chart,
          bringToFront = TRUE
        ),
        labelOptions = labelOptions(
          style = list(
            "font-family"  = PARS$base_font,
            "box-shadow"   = "2px 2px rgba(0,0,0,0.15)",
            "font-size"    = "15px",
            "padding"      = "15px",
            "border-color" = "rgba(0,0,0,0.15)"
          )
        )
      ) |> 
      addLegend(
        position  = "topright",
        na.label  = "Sin noticias",
        pal       = pal,
        values    = colorData,
        # labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE)),
        layerId   = "colorLegend",
        title = "Nº noticias"
      )

  }) |>
    bindEvent(input$fecha, input$categorias, input$comunas, input$mainnav) 

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

  # modal analisis noticias por categoria
  observe({
    cli::cli_inform("observe input$modal_noticias_cat: {input$modal_noticias_cat}")
    
    categ <- input$modal_noticias_cat
    data_noticias <- data_noticias()
 
    showModal(reporte_noticias_categoria(data_noticias, categ))
    
  }) |>
    bindEvent(input$modal_noticias_cat)
  
  # modal analisis termino
  observe({
    cli::cli_inform("observe input$modal_conceptos_term: {input$modal_conceptos_term}")

    term <- input$modal_conceptos_term
    data_noticias <- data_noticias()
    
    showModal(reporte_concepto_termino(data_noticias, term))

  }) |>
    bindEvent(input$modal_conceptos_term)

  # modal analisis comuna
  observe({
    cli::cli_inform("observe input$map_shape_click: {input$map_shape_click}")
    
    comunaid <- input$map_shape_click$id
    data_noticias <- data_noticias()
    
    showModal(reporte_comuna(data_noticias, comunaid))
    
  }) |>
    bindEvent(input$map_shape_click)
  
  
    
}
