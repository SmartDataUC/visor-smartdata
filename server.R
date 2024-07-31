# input <- list(
#   categorias = c("Economía", "Salud"),
#   tiempo     = 24 * 1,
#   fecha      = c(Sys.Date() - days(6), Sys.Date()),
#   ngram      = 2,
#   term       = "años"
# )

server <- function(input, output, session) {
  
  # auth --------------------------------------------------------------------
  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(
      db = PARS$slqlite_path,
      passphrase = NULL
    )
  )

  # Inicio ------------------------------------------------------------------
  # actualizar dateRange para la última semana al partir sesión
  updateDateRangeInput(
    session = getDefaultReactiveDomain(),
    "fecha",
    # start = Sys.Date() - months(1) + days(1),
    start = Sys.Date() - days(6),
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
  
  # Noticias ----------------------------------------------------------------
  # inicialización
  # data_noticias <- reactiveVal(get_noticias_date_range(Sys.Date() - months(1) + days(1), Sys.Date()))
  data_noticias <- reactiveVal(get_noticias_date_range(Sys.Date() - days(6), Sys.Date()))
  # data_noticias <- get_noticias_date_range(Sys.Date() - months(1) + days(1), Sys.Date())
  
  output$resumen <- renderUI(get_resumen())
  
  output$hc_noticiasc <- renderHighchart({
    
    data_noticias <- isolate(data_noticias())
    
    data_noticias_categorias <- get_noticias_categorias(data_noticias)
    
    data_noticias_categorias <- data_noticias_categorias |> 
      mutate(color = vector_a_colores(n, color_min = PARS$color_gray, color_max = PARS$color_chart))
    
    dlist <- data_noticias_categorias |> 
      select(name = categoria, y = n, color) |> 
      list_parse() 
    
    highchart() |>
      hc_add_series(
        data = dlist,
        id = "data",
        type = "bar",
        # color = PARS$color_chart,
        showInLegend = FALSE,
        colorByPoint = TRUE,
        name = "Noticias por categoría"
      ) |> 
      hc_xAxis(
        categories = data_noticias_categorias$categoria,
        labels = list(
          align = 'right',
          useHTML = FALSE
          )
        ) |> 
      hc_plotOptions(
        series = list(
          cursor = "pointer",
          point = list(events = list(click = JS("function(){ Shiny.onInputChange('modal_noticias_cat', {'cat': this.category, '.nonce': Math.random()}) }")))
        )
      )
    
  })
  
  output$hc_conceptos <- renderHighchart({
    data_noticias <- isolate(data_noticias())
    
    data_noticias_ngram <- head(get_noticias_ngram(data_noticias, 1), 10)
    
    data_noticias_ngram <- data_noticias_ngram |> 
      mutate(color = vector_a_colores(n, color_min = PARS$color_gray, color_max = PARS$palette[1]))
    
    dlist <- data_noticias_ngram |> 
      select(name = ngram, y = n, color) |> 
      list_parse() 
    
    highchart() |>
      hc_add_series(
        data = dlist,
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
          point = list(events = list(click = JS("function(){ Shiny.onInputChange('modal_conceptos_term', {'term': this.category, '.nonce': Math.random()}) }")))
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
        ) |> 
      hc_plotOptions(
        series = list(
          cursor = "pointer",
          point = list(events = list(click = JS("function(){ Shiny.onInputChange('modal_presencia_cat', {'cat': this.name, '.nonce': Math.random()}) }")))
        )
      )
    
  })
  
  output$hc_prcepcion <- renderHighchart({
    data_noticias <- isolate(data_noticias())

    data_noticias_prcp <- data_noticias |>
      mutate(date = as_date(date)) |> 
      count(date, semaforo) |> 
      complete(date, semaforo, fill = list(n = 0)) |> 
      mutate(
        semaforo = if_else(semaforo == 1, "Negativo", "Positivo/Neutro"),
        semaforo = factor(semaforo, levels = c("Negativo", "Positivo/Neutro"))
        )
    
    data_noticias_prcp
    
    hchart(
      data_noticias_prcp,
      "area", 
      hcaes(date, n, group = semaforo),
      color = c(PARS$palette[1], PARS$palette[5]),
      id = c("neg", "posneu"),
      stacking = "normal"
    ) |> 
      hc_tooltip(table = TRUE) |> 
      hc_xAxis(title = list(text = "Fecha")) |> 
      hc_yAxis(title = list(text = "Cantidad"))

      # hc_plotOptions(
      #   series = list(
      #     cursor = "pointer",
      #     events = list(click = JS("function(event){ Shiny.onInputChange('modal_percepcion', {'percp': this.name, '.nonce': Math.random()}) }"))
      #     )
      #   )

  })
  
  # mapa principal
  output$map <- renderLeaflet({
    
    leaflet(
      options = leafletOptions(
        attributionControl = FALSE,
        zoomControl = TRUE
      )
    ) |>
      setView(lng =  -70.64827, lat = -33.45494, zoom = 9) |>
      addProviderTiles(providers$CartoDB.Positron,  group = "Administrativo") 
    
  })
  
  # data_noticias <- reactive({
  #   data_noticias <- get_noticias_date_range(input$fecha[1], input$fecha[2], input$categorias)
  #   }) |>
  #   debounce(2000)

  # observeEvent para actualizar los 4 primeros graficos
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
    
    if(nrow(data_noticias) == 0) return(TRUE)
    
    data_noticias_categorias <- get_noticias_categorias(data_noticias)
    data_noticias_categorias <- data_noticias_categorias |> 
      mutate(color = vector_a_colores(n, color_min = PARS$color_gray, color_max = PARS$color_chart))
    dlist_categorias <- data_noticias_categorias |> 
      select(name = categoria, y = n, color) |> 
      list_parse() 
    
    data_noticias_ngram <- get_noticias_ngram(data_noticias, as.numeric(input$ng))
    data_noticias_ngram <- head(data_noticias_ngram, 10)
    data_noticias_ngram <- data_noticias_ngram |> 
      mutate(color = vector_a_colores(n, color_min = PARS$color_gray, color_max = PARS$palette[1]))
    
    dlist_ngram <- data_noticias_ngram |> 
      select(name = ngram, y = n, color) |> 
      list_parse() 
    
    data_noticias_prescgore  <- data_noticias |>
      group_by(name = categoria) |>
      summarise(value = 100 * mean(as.double(gore))) |>
      arrange(desc(value)) |>
      highcharter::list_parse()

    data_noticias_prcp <- data_noticias |>
      mutate(date = as_date(date)) |> 
      count(date, semaforo) |> 
      complete(date, semaforo, fill = list(n = 0)) |> 
      mutate(
        semaforo = if_else(semaforo == 1, "Negativo", "Positivo/Neutro"),
        semaforo = factor(semaforo, levels = c("Negativo", "Positivo/Neutro"))
      )

    # glimpse(data_noticias_prescgore)
    # glimpse(data_noticias_gorepresc)

    cli::cli_alert_info("observe actualización graficos: graficos")

    highchartProxy("hc_noticiasc") |>
      hcpxy_update(xAxis = list(categories = highcharter:::fix_1_length_data(data_noticias_categorias$categoria))) |>
      hcpxy_update_series(id = "data", data =  dlist_categorias)
    
    highchartProxy("hc_conceptos") |>
      hcpxy_update(xAxis = list(categories = data_noticias_ngram$ngram)) |>
      hcpxy_update_series(id = "data", data = dlist_ngram)

    highchartProxy("hc_gorepresc") |>
      hcpxy_update_series(id = "data", data = data_noticias_prescgore)

    highchartProxy("hc_prcepcion") |>
      hcpxy_set_data(
        type = "area",
        data = data_noticias_prcp,
        hcaes(date, n, group = semaforo),
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
  
  # observe para ngrama conceptos
  observe({
    
    highchartProxy("hc_conceptos") |> hcpxy_loading(action = "show")
    
    # data_noticias     <- get_noticias_date_range(input$fecha[1], input$fecha[2], input$categorias)
    data_noticias       <- data_noticias()
    data_noticias_ngram <- get_noticias_ngram(data_noticias, as.numeric(input$ng))
    data_noticias_ngram <- head(data_noticias_ngram, 10)
    data_noticias_ngram <- data_noticias_ngram |> 
      mutate(color = vector_a_colores(n, color_min = PARS$color_gray, color_max = PARS$palette[1]))
    
    dlist <- data_noticias_ngram |> 
      select(name = ngram, y = n, color) |> 
      list_parse() 
    
    highchartProxy("hc_conceptos") |>
      hcpxy_update(xAxis = list(categories = data_noticias_ngram$ngram)) |>
      hcpxy_update_series(id = "data", data = dlist)
    
    highchartProxy("hc_conceptos") |> hcpxy_loading(action = "hide")
    
  }) |>
    bindEvent(input$ng)  
  
  # r <- reactive({
  #   invalidateLater(1000)
  #   runif(1)
  #   })
  
  # comunas -----------------------------------------------------------------
  # mismo event que el de los graficos, pero para separar las logicas
  observe({
    cli::cli_alert_info("observe del mapa: inicio")
    
    data_noticias <- data_noticias()
    
    # noticias con solamente comunas
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
    # cols <-  viridisLite::plasma(15, end = .85, direction = -1)
    # cols <- sort(PARS$palette)
    cols <- c(PARS$color_gray, PARS$color_chart)
    colorData <- dgeo[["n"]]
    pal  <- colorBin(cols, 
                     colorData, 7,
                     pretty = TRUE,
                     # na.color = PARS$color_gray,
                     na.color = "transparent"
                     )
    
    # m <- leaflet() |> addTiles()
    m <- leafletProxy("map")
    
    m |> 
      clearShapes() |>
      clearTopoJSON() |>
      leaflet::addPolygons(
        data             = dgeo,
        fillColor        = ~ pal(colorData),
        weight           = .75,
        dashArray        = "3",
        stroke           = NULL,
        fillOpacity      = 0.5,
        layerId          = ~ id,
        # popup            = popp,
        label            =  ~ str_glue("{comuna}: {nf} noticias."),
        highlightOptions = highlightOptions(
          color        = "white",
          weight       = 2,
          fillColor    = PARS$palette[1],
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
    
    cli::cli_alert_info("observe del mapa: fin")

  }) |>
    bindEvent(input$fecha, input$categorias, input$comunas, input$medionav) 
  
  # solamente noticias con comunas 
  data_noticias_comuna <- reactive({
    data_noticias <- data_noticias()
    data_noticias_comuna <- data_noticias |> 
      filter(comunas != "")
    data_noticias_comuna
  })
  
  output$comunas_tend <- renderHighchart({
    data_noticias_comuna <- data_noticias_comuna()
    
    data_noticias_comuna |> 
      count(Fecha = date, name = "Cantidad") |> 
      hchart("line", hcaes(Fecha, Cantidad), color = PARS$color_chart, name = "Noticias con comunas descritas")
    
  })
  
  output$comunas_conc <- renderHighchart({
    data_noticias_comuna <- data_noticias_comuna()
    data_noticias_ngram <- head(get_noticias_ngram(data_noticias_comuna, 1), 10)
    
    data_noticias_ngram <- data_noticias_ngram |> 
      mutate(color = vector_a_colores(n, color_min = PARS$color_gray, color_max = PARS$palette[1]))
    
    dlist <- data_noticias_ngram |> 
      select(name = ngram, y = n, color) |> 
      list_parse() 
    
    highchart() |>
      hc_add_series(
        data = dlist,
        id = "data",
        type = "bar",
        color = PARS$color_chart,
        showInLegend = FALSE,
        name = "Conceptos más frecuentes en noticias con comunas"
      ) |>
      hc_xAxis(categories = data_noticias_ngram$ngram) |>
      hc_plotOptions(
        series = list(
          cursor = "pointer",
          point = list(events = list(click = JS("function(){ Shiny.onInputChange('modal_conceptos_term', {'term': this.category, '.nonce': Math.random()}) }")))
        )
      )
    
  })
  
  output$comunas_tbl <- renderDataTable({
    data_noticias_comuna <- data_noticias_comuna()
    
    data_noticias_comuna |>
      # filter(!is.na(title)) |>
      # arrange(desc(80*comments + 20*likes)) |> 
      # head(100) |>
      mutate(
        title = str_squish(title),
        title = str_glue("<a href=\"{url}\" target=\"_blank\">{str_trunc(title, 40)}</a>"),
        # date = as.Date(date)
        hace_dias = map_chr(date, diffdate2, d2 = Sys.Date()),
        fecha = str_glue("{date} (hace {hace_dias})"),
        fecha = str_remove(fecha, "\\.")
      ) |> 
      select(Titular = title, Comunas = comunas, `Categoría` = categoria, Fecha = fecha) |>
      datatable()
    
  })

  # Análisis Modales --------------------------------------------------------
  # modal analisis noticias por categoria
  observe({
    cli::cli_inform("observe input$modal_noticias_cat: {input$modal_noticias_cat}")
    
    categ <- input$modal_noticias_cat$cat
    data_noticias <- data_noticias()
 
    showModal(reporte_noticias_categoria(data_noticias, categ))
    
  }) |>
    bindEvent(input$modal_noticias_cat)
  
  # modal analisis termino
  observe({
    cli::cli_inform("observe input$modal_conceptos_term: {input$modal_conceptos_term}")

    term <- input$modal_conceptos_term$term
    data_noticias <- data_noticias()
    
    showModal(reporte_concepto_termino(data_noticias, term))

  }) |>
    bindEvent(input$modal_conceptos_term)
  
  observe({
    cli::cli_inform("observe input$modal_presencia_cat: {input$modal_presencia_cat}")
    print(input$modal_presencia_cat$cat)
    categ <- input$modal_presencia_cat$cat
    data_noticias <- data_noticias()
    data_noticias <- data_noticias |> filter(gore == 1)
    
    showModal(reporte_noticias_categoria(data_noticias, categ))
    
  }) |>
    bindEvent(input$modal_presencia_cat)
  
  # modal analisis percepcion
  observe({
    cli::cli_inform("observe input$modal_percepcion: {input$modal_percepcion}")
    
    percp         <- input$modal_percepcion$percp
    data_noticias <- data_noticias()

    showModal(reporte_percepcion(data_noticias, percp))
    
  }) |>
    bindEvent(input$modal_percepcion)
  
  # modal analisis comuna
  observe({
    cli::cli_inform("observe input$map_shape_click: {input$map_shape_click}")

    print(input$map_shape_click)

    comunaid <- input$map_shape_click$id
    data_noticias <- data_noticias()

    showModal(reporte_comuna(data_noticias, comunaid))

  }) |>
    bindEvent(input$map_shape_click)
  
    
  # RRSS --------------------------------------------------------------------
  # si se hace click en seccion se cierra sidebar
  # observe({
  #   x <- str_to_lower(input$mainnav)
  #   if(str_detect(x, "redes sociales")){
  #     sidebar_toggle(id = "sidebar", open = FALSE)
  #   } else {
  #     sidebar_toggle(id = "sidebar", open = TRUE)
  #   }
  # }) |
  #   bindEvent(input$mainnav)
  
  
  dinst <- reactive({
    
    cli::cli_inform("reactive `dinst`")
    
    dinst1 <- get_tabla_instagram(input$fecha[1], input$fecha[2], comuna = input$comunas)
    
    if(is.null(input$categorias)) {
      dinst <- dinst1
      return(dinst)
    }
    
    dinst <- map_df(input$categorias, function(cat = "Saluds"){
      if(is.null(lista_palabras_clave[[cat]])) return(tibble())
      search_keywords(dinst1, cat) |> mutate(categoria = cat)
    })
  
    dinst <- dinst |> 
      # por si una publicacion esta en 2 o mas categorias
      distinct(caption, .keep_all = TRUE)
      
    dinst
    
  })
  
  # dinstcommnets <- reactive({
  #   get_tabla_insta_gore(input$fecha[1], input$fecha[2] - days(500)) |> 
  #     filter(date == max(date))
  # 
  #   
  #   dinstcommnets <- get_comments_instagram(input$fecha[1], input$fecha[2], comuna = input$comunas)
  #   dinstcommnets
  # })
  # 
  output$rrss_insta_post_fecha <- renderHighchart({

    dinst <- dinst()
    
    if(nrow(dinst) == 0) return(highchart() |> hc_subtitle(text = "No existen post con dichos parámetros"))
    
    dinst |> 
      count(fecha = date, name = "cantidad") |> 
      hchart("line", hcaes(fecha, cantidad), color = PARS$color_chart, name = "Posts por fecha")
  })
  
  output$rrss_insta_hashtags <- renderPlot({
    dinst <- dinst()
    
    if(nrow(dinst) == 0) return(plot.new())
    
    create_dfm(dinst) |>
      get_top_hashtags() |>
      # as.igraph() |> igraph::plot.igraph()
      quanteda.textplots::textplot_network(vertex_color = PARS$palette[1], edge_color = PARS$palette[2])
  })
  
  output$rrss_insta_post_activos <- DT::renderDataTable({
    dinst <- dinst()
    
    dinst |>
      filter(!is.na(caption)) |>
      arrange(desc(80*comments + 20*likes)) |> 
      head(100) |>
      mutate(
        caption = str_squish(caption),
        caption = str_glue("<a href=\"{url}\" target=\"_blank\">{str_trunc(caption, 40)}</a>"),
        # date = as.Date(date)
        hace_dias = map_chr(date, diffdate2, d2 = Sys.Date()),
        fecha = str_glue("{date} (hace {hace_dias})"),
        fecha = str_remove(fecha, "\\.")
      ) |> 
      select(Usuario = user, Comuna = comuna, Contenido = caption, Fecha = fecha, "\u2764" = likes, "&#128488;" = comments) |>
      datatable() |> 
      formatCurrency(c(5, 6),currency = "", interval = 3, mark = ".", digits  = 0)

  })
  
  # Tendencias --------------------------------------------------------------
  # para tendecias colocaremos de ejemplo las dos conceptos mas frecuentes
  trend_terms <- reactiveVal(
    get_noticias_date_range(Sys.Date(), Sys.Date() - 7) |> 
      get_noticias_ngram(2) |> 
      pull(1) |> 
      head(3)
  )
  
  # observador, que agrega términos a la comparacion cuando se hace enter o click
  # en el searchInput
  observe({
    # si es un valor distinto de nulo, agregamos a la lista y borramos contenido
    if(nchar(input$tags) > 0 & input$tags != ""){
      
      t <- trend_terms()
      tnew <- c(t, input$tags)
      trend_terms(str_to_title(tnew[!duplicated(str_clean(tnew))]))
      updateSearchInput(session, "tags", value = "")
      
    }
  }) |> 
    bindEvent(input$tags)
  
  # observador que elimina items cuando se hace click en el closebutton
  observe({
    # print(input$tag_remove)
    trend_terms(trend_terms()[-input$tag_remove$id])
  }) |> 
    bindEvent(input$tag_remove)
  
  # Observa vector de elementos de términos para:
  # - modificar placeholder
  # - disable o enable el input de busqueda
  observe({
    nt <- length(trend_terms())
    
    plc_hld <- case_when(
      nt <= 0 ~ "Ingresa un término para analizar",
      nt <= 4 ~ "Agrega otro término para comparar",
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
  
  output$tags_ui <- renderUI({
    trend_terms()         
    t <- trend_terms()
    # c <- viridis::viridis(length(t))
    c <- pallete_tendencias(length(t))
    
    # <span class="badge" style="background-color:darkred;color:white;cursor:default">
    #   Escándalo presicendial&nbsp;<i class="fas fa-times-circle" onClick="Shiny.onInputChange('tag_remove', 2)"  style="cursor: pointer;"></i>&nbsp;
    # </span>
    
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
  
  data_trend <- reactive({
    
    data_noticias <- data_noticias()
    terms         <- trend_terms()
    
    # terms <- c("cathy barriga", "arresto domiciliario", "estafa", "asdxx")
    
    withProgress(
      message = "Procesando datos",
      value = 0,
      {
        data_trend <- map_df(1:length(terms), function(i = 2){
          
          t <- terms[i]
          
          if(!interactive()) incProgress(1/length(terms), message = str_glue("Procesando término '{t}'") )
          
          dt <- data_noticias |> 
            filter(str_detect(body, str_clean(t))) |> 
            mutate(term = t, .before = 1)
          
          # esto es para generar datos para todas las categorias
          if(nrow(dt) == 0) return(tibble(term = t))
        
          dt
          
        })
      })
    
    data_trend <- data_trend |> 
      mutate(term = fct_inorder(term))
  
    data_trend
      
  }) # |> bindEvent(input$term_go)
  
  output$trend_hc1 <- renderHighchart({
    
    # shinyjs::disable(id = "term_go")
    terms      <- trend_terms()
    if(length(terms) == 0) return(highchart() |> hc_subtitle(text = "Agregue términos de búsqueda"))
     
    data_trend <- data_trend()
   
    
    # c <- viridis::viridis(length(unique(data_trend$term)))
    c <- pallete_tendencias(length(unique(data_trend$term)))
    
    data_trend |> 
      count(term, date) |> 
      complete(term, date, fill = list(n = 0)) |> 
      hchart(hcaes(date, n, group = term), type = "line") |> 
      hc_tooltip(table = TRUE, sort = TRUE) |> 
      hc_colors(c) |> 
      hc_xAxis(title = list(text = "Fecha")) |> 
      hc_yAxis(title = list(text = "Cantidad"))
    
  })
  
  output$trend_hc2 <- renderHighchart({
    
    terms      <- trend_terms()
    if(length(terms) == 0) return(highchart() |> hc_subtitle(text = "Agregue términos de búsqueda"))
    
    data_trend <- data_trend()
    
    # c <- viridis::viridis(length(unique(data_trend$term)))
    c <- pallete_tendencias(length(unique(data_trend$term)))
    
    data_trend |> 
      # eliminamos registros ficticioes
      filter(!is.na(date)) |> 
      group_by(term, .drop = FALSE) |> 
      count() |>  
      ungroup() |> 
      hchart(hcaes(term, n), type = "column", colorByPoint = TRUE, name = "Cantidad de Menciones del Término") |> 
      hc_colors(c) |> 
      hc_xAxis(title = list(text = "Término")) |> 
      hc_yAxis(title = list(text = "Cantidad"))
    
  })
  
  
}
