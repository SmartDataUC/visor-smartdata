page_navbar(
  title = tagList(
    tags$img(src = "logo.png", width = "100px", height = "auto", class = "me-3"),
    tags$b("SmartDataCiudadano", class = "h6", style = "font-weight:800")
  ),
  bg = PARS$bg,
  lang = "es",
  theme = smart_theme,
  inverse = FALSE,
  sidebar = smart_sidebar,
  nav_panel(
    tags$head(tags$link(href = "favicon-32x32.png", rel = "icon"),),
    title = tags$span("Inicio", class = "me-3"),
    icon  = icon("dashboard"),
    layout_column_wrap(
      width = 1/2,
      card(highchartOutput("hc0")),
      card(highchartOutput("hc2")),
      card(
        card_header(
          tags$span(),
          radioButtons("ng", NULL, c(1, 2, 3), inline = TRUE),
          class = "d-flex justify-content-between align-items-center"
        ),
        highchartOutput("hc1")
        ),
      card(highchartOutput("hc3"))
      )
    ),
  nav_panel(
    title = tags$span("Medios", class = "me-3"),
    icon  = icon("newspaper")
    ),
  nav_panel(
    title = tags$span("Municipios", class = "me-3"),
    icon  = icon("city")
  ),
  nav_panel(
    title = tags$span("Redes", class = "me-3"),
    icon  = icon("circle-nodes")
    )
  )