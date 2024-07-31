page_navbar(
  id = "mainnav",
  window_title = "Visor Smart-data",
  title = tagList(
    tags$img(src = "logo-gorecegir.png", width = "400px", height = "auto", class = "me-3")
  ),
  # bg = PARS$bg,
  lang = "es",
  theme = smart_theme,
  inverse = TRUE,
  sidebar = smart_sidebar,

  # resumen - home ----------------------------------------------------------
  nav_panel(
    title = tags$span("Resumen", class = "me-3"),
    icon = icon("home"),
    shinyjs::useShinyjs(),  # Set up shinyjs
    tags$head(
      tags$link(href = "favicon-32x32.png", rel = "icon"),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    tags$style(".modal-dialog { top: -90px !important;}"),
    uiOutput("resumen")
  ),
  # medios de comunicación --------------------------------------------------
  nav_panel(
    title = tags$span("Medios", class = "me-3"),
    icon  = icon("newspaper"),
    value = "medios",
    tabsetPanel(
      type = "pills",
      id = "medionav",
      tabPanel(
        title = tags$span(icon("newspaper"), "Categoría"),
        layout_column_wrap(
          width = 1/2,
          card(
            card_header(
              tags$span("Noticias por categoría", class = "tt"),
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
                tags$span("Conceptos más frecuentes", class = "tt"),
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
                tags$span("Presencia GORE en noticias", class = "tt"),
                popover(
                  bs_icon("info-circle"),
                  title = "Presencia GORE en noticias",
                  "Este gráfico de burbujas agrupadas representa la distribución de noticias publicadas en los medios que hagan referencia al Gobierno Regional de Santiago o al Gobernador Regional. Las noticias son presentadas por categoría de noticia. Cada burbuja representa una categoría, siendo su tamaño proporcional respecto a su presencia en el grupo de noticias, siendo la burbuja más grande el tema en donde más se hace referencia al Gobierno Regional de Santiago o al Gobernado Regional para el periodo analizado."
                  )
                )
              ),
            highchartOutput("hc_gorepresc")
            ),
          card(
            card_header(
              tags$span(
                tags$span("Percepción de noticias", class = "tt"),
                popover(
                  bs_icon("info-circle"),
                  title = "Hechos noticiosos",
                  "Texto sobre Hechos noticiosos"
                  )
                )
              ),
            highchartOutput("hc_prcepcion")
            )
          )
        ),
      tabPanel(
        title = tags$span(icon("map-location-dot"), "Comunas"),
        layout_column_wrap(
          width = 1,
          card(leafletOutput("map") |> withSpinner()),
          ),
        layout_column_wrap(
          width = 1/2,
          card(
            card_header(tags$span(tags$span("Tendencia Histórica", class = "tt"))),
            highchartOutput("comunas_tend") |> withSpinner()
            ),
          card(
            card_header(tags$span(tags$span("Conceptos más frecuentes", class = "tt"))),
            highchartOutput("comunas_conc") |> withSpinner()
            ),
          ),
        layout_column_wrap(
          width = 1,
          card(
            card_header(tags$span(tags$span("Noticias en donde se identifica la presencia de la comuna", class = "tt"))),
            DT::dataTableOutput("comunas_tbl") |> withSpinner()
            )
          )
        ),
      tabPanel(
        title = tags$span(icon("arrow-trend-up"), "Tendencias"),
        popover(
          tags$span(tags$span("Instrucciones"), bs_icon("info-circle")),
          title = "Funcionalidad",
          "La herramienta de tendencias permite buscar y explorar la frecuencia de palabras específicas en artículos noticiosos publicados en las fechas seleccionadas por el usuario.
      Para utilizar la herramienta, primero ajuste en el menú izquierdo el rango de fechas en que desea realizar la búsqueda.
      Luego, ingrese una palabra en la herramienta y pulse el botón con el ícono '+' para agregarla.
      Puede agregar más de una palabra si desea comparar la frecuencia de varios términos. Cuando finalice de agregar palabras, haga click en el botón 'Analizar'.
      Los resultados se desplegarán en dos visualizaciones: una corresponde a una serie de tiempo para el periodo analizado, y la segunda es un gráfico de barras que representa la cantidad de veces que cada término aparece en todos los artículos analizados."
        ),
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
        # shinyjs::disabled(shiny::actionButton("term_go", "Analizar", icon = icon("search"))),
        layout_column_wrap(
          fillable = TRUE,
          width = 1/2,
          card(
            card_header(tags$span("Tendencia temporal", class = "tt")),
            highchartOutput("trend_hc1") |> withSpinner()
          ),
          card(
            card_header(tags$span("Cantidad de Menciones del Término en Artículos Noticiosos", class = "tt")),
            highchartOutput("trend_hc2") |> withSpinner()
            )
          )
        )
      )
    ),
  # rrss --------------------------------------------------------------------
  nav_panel(
    title = tags$span("Redes Sociales", class = "me-3"),
    icon  = icon("temperature-half"),
    value = "rrss",
    tabsetPanel(
      type = "pills",
      id = "rrssnav",
      tabPanel(
        title = tags$span(icon("instagram"), "Instragram"),
        layout_column_wrap(
          width = 1,
          layout_column_wrap(
            width = 1/2,
            card(
              card_header(
                tags$span("Posts por fecha", class = "tt"),
              ),
              highchartOutput("rrss_insta_post_fecha") |> withSpinner()
            ),
            card(
              card_header(
                tags$span("Relaciones por Coocurrencia de Hashtags", class =  "tt")
              ),
              plotOutput("rrss_insta_hashtags") |> withSpinner()
            )
          )
        ),
        layout_column_wrap(
          width = 1,
          card(
            card_header(
              tags$span("Top 100 Posts Más Activos", class = "tt"),
            ),
            DT::dataTableOutput("rrss_insta_post_activos") |> withSpinner()
          )
        )
      ),
      tabPanel(
        title = tags$span(icon("instagram"), "Instragram GORE"),
        layout_column_wrap(
          width = 1,
          layout_column_wrap(
            width = 1/2,
            card(
              card_header(
                tags$span("Posts por fecha", class = "tt"),
              ),
              highchartOutput("rrss_insta_gore_post_fecha") |> withSpinner()
            ),
            card(
              card_header(
                tags$span("Relaciones por Coocurrencia de Hashtags", class =  "tt")
              ),
              plotOutput("rrss_insta_gore_hashtags") |> withSpinner()
            )
          )
        ),
        layout_column_wrap(
          width = 1,
          card(
            card_header(
              tags$span("Top 100 Posts Más Activos", class = "tt"),
            ),
            DT::dataTableOutput("rrss_insta_gore_post_activos") |> withSpinner()
          )
        )
      ),
      tabPanel(
        title = tags$span(icon("facebook"), "Facebook"),
        layout_column_wrap(
          width = 1,
        )
      ),
      tabPanel(
        title = tags$span(icon("search"), "Búsqueda en RRSS"),
        layout_column_wrap(
          width = 1, textInput("rrss_search", "termino a buscar")
        )
      )
    )
  ),
  # acerca de ---------------------------------------------------------------
  nav_panel(
    title = tags$span("Acerca de", class = "me-3"),
    icon  = icon("info-circle"),
   #style = "display: none;",
    value = "acercade",
    layout_column_wrap(
      fillable = TRUE,
      width = 1/1,
      column(
        width = 6,
        offset = 3,
        includeMarkdown("Rmd/acercade.Rmd")
        )
      )
    ),
  nav_spacer(),
  nav_item(
    tags$img(src = "logo-core.svg", width = "100px", height = "auto", class = "me-3")
  ),
  nav_item(
    shinyauthr::logoutUI(
      id = "logout", 
      label = "Cerrar Sesión",
      class = "btn-primary",
      # style = "color: #5b365c;",
      icon = icon("rectangle-xmark")
    )
  )
  # fin ---------------------------------------------------------------------
) |> 
  # shinymanager::secure_app(language = "es", theme = smart_theme, enable_admin = TRUE, id = "loginid") |>
  identity() 
  