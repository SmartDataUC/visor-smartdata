page_navbar(
  id = "mainnav",
  title = tagList(
    tags$img(src = "logo.png", width = "100px", height = "auto", class = "me-3"),
    tags$b("SmartDataCiudadano", class = "h6", style = "font-weight:800")
  ),
  bg = PARS$bg,
  lang = "es",
  theme = smart_theme,
  inverse = FALSE,
  sidebar = smart_sidebar,
  # inicio ------------------------------------------------------------------
  nav_panel(
    shinyjs::useShinyjs(),  # Set up shinyjs
    autoWaiter(color = alpha(PARS$color_chart, alpha = 0.2), fadeout = TRUE),
    tags$head(tags$link(href = "favicon-32x32.png", rel = "icon"),),
    tags$style(".modal-dialog { top: -90px !important;}"),
    title = tags$span("Inicio", class = "me-3"),
    icon  = icon("dashboard"),
    layout_column_wrap(
      width = 1/2,
      card(
        card_header(
          "Noticias por categoría",
          popover(
            bs_icon("info-circle"),
            title = "Noticias por categoría",
            "Este gráfico de barras representa la distribución de las noticias publicadas en los medios de acuerdo con su categoría, ordenadas de mayor a menor. Si posiciona el cursor por encima de las barras del gráfico, podrá visualizar el número de noticias publicadas en cada categoría."
            ),
          ),
        highchartOutput("hc_noticiasc")
        ),
      card(
        card_header(
          tags$span(
            "Conceptos más frecuentes",
            popover(
              bs_icon("info-circle"),
              title = "Conceptos más frecuentes",
              "Este gráfico de barras representa los 10 conceptos más frecuentes en las noticias analizadas, ordenadas de mayor a menor. En la parte superior izquierda del gráfico, puede ajustar la cantidad de palabras que conforman los conceptos, pudiendo elegir conceptos simples de una palabra o conceptos compuestos de dos o más palabras. Si posiciona el cursor por encima de las barras del gráfico, podrá visualizar el número de conceptos presentes en las noticias analizadas. Si pulsa la barra del gráfico, una nueva ventana le presentará un análisis más detallado del concepto en las noticias analizadas incluyendo: (1) la tendencia histórica del concepto; (2) su distribución en las noticias analizadas de acuerdo con sus categorías; y (3) una tabla con las noticias donde se encuentra presente dicho concepto."
            )
          ),
          # tags$span(),
          tags$small(radioButtons("ng", NULL, c("Una palabra" = 1, "Dos palabras" =  2, "Tres palabras" = 3), inline = TRUE)),
          class = "d-flex justify-content-between align-items-center"
        ),
        highchartOutput("hc_conceptos")
        ),
      card(
        card_header(
          tags$span(
            "Presencia GORE en noticias",
            popover(
              bs_icon("info-circle"),
              title = "Hechos noticiosos",
              "Texto sobre Hechos noticiosos"
              )
            )
          ),
        highchartOutput("hc_gorepresc")
        ),
      card(
        card_header(
          tags$span(
            "Percepción de noticias",
            popover(
              bs_icon("info-circle"),
              title = "Hechos noticiosos",
              "Texto sobre Hechos noticiosos"
            )
          )
        ),
        highchartOutput("hc_prcepcion")
        ),
      )
    ),
  # comunas -----------------------------------------------------------------
  nav_panel(
    title = tags$span("Comunas", class = "me-3"),
    icon  = icon("map-location-dot"),
    leafletOutput("map", width="100%", height="100%")
  ),

  # rrss --------------------------------------------------------------------
  nav_panel(
    title = tags$span("Redes Sociales", class = "me-3"),
    icon  = icon("temperature-half"),
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Posts por fecha"),
        highchartOutput("hc_rrss_fecha")
      ),
      card(
        card_header("Chart 2"),
        highchartOutput("hc_rrss_2")
      ),
      card(
        card_header("Chart 3"),
        highchartOutput("hc_rrss_3")
      ),
      card(
        card_header("Top 100 Contenido con mayor actividad"),
        DT::dataTableOutput("dt_rrss_mas_activos")
      ),
    )
  ),
  # tendencias --------------------------------------------------------------
  nav_panel(
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
    uiOutput("tags_ui"),
    shinyjs::disabled(shiny::actionButton("term_go", "Analizar", icon = icon("search"))),
    
    layout_column_wrap(
      fillable = TRUE,
      width = 1/2,
      
      card(
        card_header("Tendencia temporal"),
           # min_heigth = 300,
           # withSpinner(
           highchartOutput("trend_hc1")
           # )
        ),
      card(
        card_header("Tendencia temporal"),
        highchartOutput("trend_hc2")
          )
      )
    ),
  # acerca de ---------------------------------------------------------------
  nav_panel(
    title = tags$span("Acerca de", class = "me-3"),
    icon  = icon("info-circle")
    )
  # fin ---------------------------------------------------------------------
)