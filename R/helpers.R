# data functions ----------------------------------------------------------
get_noticias_date_range <- function(d1, d2, categorias = NULL, comunas = NULL){

  # d1 <- Sys.Date() - days(1)
  # d2 <- Sys.Date() - days(1)
  # categorias <- NULL
  # comunas <- NULL
  
  # if(is.null(d1)) return(tibble())
  
  ds <- c(d1, d2)
  d1 <- min(ds)
  d2 <- max(ds)

  cli::cli_inform("running get_noticias_date_range: {d1} a {d2}. {diffdate2(d1, d2)}")
  
  data_noticias <- tbl(pool, "news") |> 
    # fix #9
    filter(d1 <= as_date(date), as_date(date) <= d2)
  
  if(!is.null(categorias)){
    data_noticias <- data_noticias |> 
      filter(category_1 %in% categorias)
  }
  
  if(!is.null(comunas)){
    comunas_regex <- str_c(comunas, collapse = "|")
    data_noticias <- data_noticias |> 
      filter(str_detect(comunas, comunas_regex))
  }
  
  data_noticias <- data_noticias |> 
    select(title = title, body = clean_body, categoria = category_1, url, 
           media, date, comunas, gore, sentiment) |> 
    collect() |> 
    mutate(date_time = date, date = as_date(date))
  
  cli::cli_inform("running get_noticias_date_range: removing duplicated rows.")
  
  data_noticias <- data_noticias |>
    arrange(date_time) |> 
    group_by(title, media) |> 
    # filter(date_time == max(date_time)) |>
    filter(row_number() == n()) |>
    ungroup() # |>  select(-date_time)
  
  cli::cli_inform("running get_noticias_date_range: str_clean a body.")
  
  data_noticias <- data_noticias |> 
    mutate(body = str_clean(body)) 
  
  cli::cli_inform("running get_noticias_date_range: {scales::comma(nrow(data_noticias))} filas.")
  
  # glimpse(data_noticias)
  
  data_noticias
  
}

get_noticias_ngram <- function(data_noticias, ng = 3){
  # ng <- 2
  
  cli::cli_inform("running get_noticias_ngram: {ng}")
  
  if(nrow(data_noticias) == 0) return(tibble(ngram = "", n = 0))
    
  data_noticias_ngram <- data_noticias |> 
    unnest_tokens(ngram, body, token = "ngrams", n = ng) |> 
    count(ngram, sort = TRUE)
  
  # data_noticias |> 
  #   mutate(body2 = str_remove_all(body, stopwords_es_2))
  
  # for(i in 1:ng){
  #   cli::cli_inform("running get_noticias_ngram: removing stopwods in {i} space, nrow {nrow(data_noticias_ngram)}")
  #   # data_noticias_ngram |>
  #   #   mutate(w = word(ngram, start = 1L))
  #   # data_noticias_ngram <- ""
  # 
  #   data_noticias_ngram <- data_noticias_ngram |>
  #     filter(!word(ngram, start = i) %in% stopwords_es)
  # 
  # }
  # 
  data_noticias_ngram
  
}

get_noticias_categorias <- function(data_noticias){

  cli::cli_inform("running get_noticias_categorias")

  data_noticias_categorias <- data_noticias |>
    count(categoria) |>
    collect() |>
    mutate(categoria = str_to_title(categoria)) |>
    arrange(desc(n)) |> 
    filter(!is.na(categoria))

  data_noticias_categorias

}

# diffdate2(Sys.Date() - days(0), Sys.Date())
# diffdate2(Sys.Date() - days(8), Sys.Date())
# diffdate2(Sys.Date() - months(8), Sys.Date())
# diffdate2(Sys.Date() - years(2), Sys.Date())
# diffdate2(Sys.Date() - years(2) - months(10) - days(10), Sys.Date())
diffdate2 <- function(d1, d2) {
  # d1 <- Sys.Date() - years(2) - months(5) - days(2)
  # d2 <- Sys.Date()
  
  # d1 <- Sys.Date() - days(0)
  # d2 <- Sys.Date()
  
  daux <- d1
  d    <- interval(d1, d2)
  d
  
  anios <- d %/% years(1)
  daux  <- daux + years(anios)
  d     <- interval(daux, d2)
  d
  
  meses <- d %/% months(1)
  daux  <- daux + months(meses)
  d     <- interval(daux, d2)
  d
  
  # se suma uno para contar el día no la diferencia (caso from = to)
  dias  <- d %/% days(1) + 1
  
  c(anios, meses, dias)
  
  a <- if_else(anios > 0, str_glue("{anios} año{ifelse(anios>1,'s','')}"), NA)
  m <- if_else(meses > 0, str_glue("{meses} mes{ifelse(meses>1,'es','')}"), NA)
  d <- if_else(dias  > 0, str_glue("{dias} día{ifelse(dias>1,'s','')}"), NA)
  
  t <- str_flatten(na.omit(c(a, m, d)), collapse = ", ", last = " y ")
  t <- str_glue("{t}.")
  t
}

str_clean <- function(x = c("Cath{í   B.arriga ", "otroa cosa")){
  
  x |> 
    str_to_lower() |> 
    stringi::stri_trans_general(id = "Latin-ASCII") |> 
    str_extract_all("[a-z]| ") |> 
    map_chr(str_c, collapse = "") |> 
    str_squish()
    
}

# get_data_dimension <- function(){
#   data_dimension <- tibble(
#     dimension = c("Medio Ambiente", "Seguridad", "Movilidad", "Emergencias", "Resilencia"),
#     score     = round(100 * rbeta(length(dimension), 10, 4), 0)
#   )
#   data_dimension
# }
# 
# get_noticias_ultimas_horas <- function(horas = 24, categorias = NULL){
#   # horas = 24 * 300
#   # categorias = NULL
#   cli::cli_inform("running get_noticias_ultimas_horas: dias {as.numeric(horas)/24}, categorias: {str_c(categorias, sep = ', ')}")
#   
#   since <- Sys.Date() - hours(horas)
#   
#   data_noticias <- tbl(pool, "news") |> 
#     filter(date >= since)
#   
#   if(!is.null(categorias)){
#     data_noticias <- data_noticias |> 
#       filter(category_1 %in% categorias)
#   }
#   
#   data_noticias <- data_noticias |> 
#     select(title = clean_title, body = clean_body, categoria = category_1, url, media, date) |> 
#     collect() |> 
#     mutate(date = as_date(date))
#   
#   cli::cli_inform("running get_noticias_ultimas_horas: {scales::comma(nrow(data_noticias))} filas.")
#   
#   data_noticias
#   
# }


# tabla to html text ------------------------------------------------------
library(htmltools)

tabla_a_html <- function(tabla, color_cat) {
  filas_html <- list()
  
  for (i in 1:nrow(tabla)) {
    primera_columna <- tags$b(style = str_glue("font-size: large;color:{color_cat};"), str_to_title(tabla[i, 1]))
    segunda_columna <- tags$p(
      tags$b(style = str_glue("font-size: large;color:{PARS$fg};"), tabla[i, 2]), 
      tags$span(style = "font-size: x-small;", " Noticias")
    )
    fila_html <- tagList(primera_columna, segunda_columna)
    filas_html <- c(filas_html, fila_html)
  }
  
  contenido_html <- tags$div(style = "line-height:1.4;",
    do.call(tagList, filas_html)
  )
  
  return(contenido_html)
}

tabla_a_html_link_web <- function(tabla) {
    filas_html <- list()
    
    for (i in 1:nrow(tabla)) {
      primera_columna <- tags$b(str_to_title(tabla[i, 1]))
      segunda_columna <-  segunda_columna <- tags$a(href = tabla[i, 2],  primera_columna)
      fila_html <- tagList(segunda_columna)
      filas_html <- c(filas_html, fila_html)
    }
    
    contenido_html <- do.call(tagList, filas_html)
    
    return(contenido_html)
  }

# highcharts functions ----------------------------------------------------
hc_area_date <- function(d, ...){
  d |>
    count(date) |>
    hchart("area", hcaes(date, n), fillOpacity = 0.1,  color = PARS$color_chart, ...) |>
    hc_plotOptions(series = list(marker = list(enabled = FALSE))) |>
    hc_xAxis(title = "") |>
    hc_yAxis(title = "")
}

hc_pie <- function(d, ...){
  d |> 
    set_names("variable") |> 
    count(variable, sort = TRUE) |>
    mutate(variable = fct_inorder(variable)) |>
    hchart("pie", hcaes(name = variable, y = n), ...) |>
    hc_plotOptions(pie = list(dataLabels = list(enabled = TRUE))) |>
    hc_colors(sort(PARS$palette))

}

hc_bar_terms <- function(data_noticias2, ng = 1, ...){
  
  get_noticias_ngram(data_noticias2, ng = 1) |> 
    head(10) |> 
    hchart(
      hcaes(ngram, n),
      type = "bar",
      color = PARS$color_chart,
      showInLegend = FALSE,
      name = "Conceptos más frecuentes",
      ...
    ) |> 
    hc_xAxis(title = "") |>
    hc_yAxis(title = "")
  
} 

# report functions --------------------------------------------------------
reporte_noticias_categoria <- function(data_noticias, categ){
  
  dout <- data_noticias  |> 
    filter(categoria == categ) |> 
    mutate(title = str_glue("<a href=\"{url}\" target=\"_blank\">{str_trunc(title, 40)}</a>")) |>
    select(-body)
  
  hc1 <- hc_area_date(dout, name = "Noticias")
  
  hc2 <- dout |>
    select(media) |> 
    hc_pie(name = "Noticias")
  
  doutdt <- dout |>
    select(Título = title, Fecha = date, Medio = media, Categoría = categoria) |>
    datatable()
  
  modalDialog(
    tags$h4(str_glue("Análisis categoría: {str_to_title(categ)}")),
    layout_column_wrap(
      width = NULL, height = 250, fill = FALSE,
      style = htmltools::css(grid_template_columns = "6fr 6fr"),
      card(card_header(tags$span("Tendencia histórica", class = "tt"))    , hc1),
      card(card_header(tags$span("Distribución de medios", class = "tt")), hc2)
    ),
    card(card_header(tags$span("Noticias de la categoría", class = "tt")), doutdt, height = "350px")
  )
  
}

reporte_concepto_termino <- function(data_noticias, term){
  
  dout <- data_noticias |>
    filter(str_detect(body, regex(term, ignore_case = TRUE))) |>
    mutate(title = str_glue("<a href=\"{url}\" target=\"_blank\">{str_trunc(title, 40)}</a>")) |>
    select(-body)
  
  hc1 <- hc_area_date(dout, name = "Noticias")
  
  hc2 <- dout |>
    select(categoria) |> 
    hc_pie(name = "Noticias")
    
  doutdt <- dout |>
    select(Título = title, Fecha = date, Medio = media, Categoría = categoria) |>
    datatable()
    
  modalDialog(
    tags$h4(str_glue("Análisis concepto: {str_to_title(term)}")),
    layout_column_wrap(
      width = NULL, height = 250, fill = FALSE,
      style = htmltools::css(grid_template_columns = "6fr 6fr"),
      card(card_header(tags$span("Tendencia histórica", class = "tt")), hc1),
      card(card_header(tags$span("Distribución de medios", class = "tt")), hc2)
    ),
    card(card_header(tags$span("Noticias donde se encuentra presente el concepto", class = "tt")), doutdt, height = "350px")
  )
  
}

reporte_percepcion <- function(data_noticias, percp = "Negativo"){
  
  dout <- data_noticias |>
    filter(sentiment == stringr::str_to_upper(stringr::str_sub(percp, 0, 3))) |>
    mutate(title = str_glue("<a href=\"{url}\" target=\"_blank\">{str_trunc(title, 40)}</a>")) |>
    select(-body)
  
  hc1 <- hc_area_date(dout, name = "Noticias")
  hc2 <- hc_bar_terms(data_noticias, ng = 1)
  
  doutdt <- dout |>
    select(Título = title, Fecha = date, Medio = media, Categoría = categoria) |>
    datatable()
  
  modalDialog(
    tags$h4(str_glue("Análisis de noticias {str_to_title(percp)}")),
    layout_column_wrap(
      width = NULL, height = 250, fill = FALSE,
      style = htmltools::css(grid_template_columns = "6fr 6fr"),
      card(card_header(tags$span("Tendencia histórica", class = "tt"))    , hc1),
      card(card_header(tags$span("Conceptos más frecuentes", class = "tt")), hc2)
    ),
    card(card_header(tags$span("Noticias asociadas al sentimiento", class = "tt")), doutdt, height = "350px")
  )
  
}

reporte_comuna <- function(data_noticias, comunaid = "pudahuel", ng = 1){
  
  data_noticias2 <- data_noticias |> 
    mutate(comunas = map(comunas, ~ str_squish(unlist(str_split(.x, "\\,")))))  |> 
    unnest(comunas) |> 
    mutate(comunas = str_remove_all(comunas, "\"|\\'|\\[|\\]|\\{|\\}")) |> 
    filter(comunas != "") |> 
    filter(comunaid == snakecase::to_snake_case(stringi::stri_trans_general(comunas, id = "Latin-ASCII")))
  
  if(nrow(data_noticias2) == 0){
    return(modalDialog(tags$h4("Comunas sin noticias")))
  }
  
  comuna <- data_noticias2 |> 
    pull(comunas) |> 
    unique()
  
  dout <- data_noticias2 |> 
    mutate(title = str_glue("<a href=\"{url}\" target=\"_blank\">{str_trunc(title, 40)}</a>")) |>
    select(-body)
  
  hc1 <- hc_area_date(dout, name = "Noticias")
  hc2 <- hc_bar_terms(data_noticias2, ng = 1)

  doutdt <- dout |>
    select(Título = title, Fecha = date, Medio = media, Categoría = categoria) |>
    datatable()
  
  modalDialog(
    tags$h4(str_glue("Análisis de noticias comunales: {str_to_title(comuna)}")),
    layout_column_wrap(
      width = NULL, height = 250, fill = FALSE,
      style = htmltools::css(grid_template_columns = "6fr 6fr"),
      card(card_header(tags$span("Tendencia histórica", class = "tt"))    , hc1),
      card(card_header(tags$span("Conceptos más frecuentes", class = "tt")), hc2)
    ),
    card(card_header(tags$span("Noticias en donde se identifica la presencia de la comuna", class = "tt")), doutdt, height = "350px")
  )
  
}


# home / resumen ----------------------------------------------------------
get_resumen <- function(){
  
  hoy <- Sys.Date()
  # hacer una logica que si son entre las 1 y 4 AM mirar el dia anterior
  
  data_noticias <- get_noticias_date_range(hoy - 1, hoy)
  
  dcomentario <- drrss |> 
    count(caption, sort = TRUE) |> 
    head(1) |> 
    pull(caption)
  
  
  dcomunas <- data_noticias |> 
    mutate(comunas = map(comunas, ~ str_squish(unlist(str_split(.x, "\\,")))))  |> 
    unnest(comunas) |> 
    mutate(comunas = str_remove_all(comunas, "\"|\\'|\\[|\\]|\\{|\\}")) |> 
    filter(comunas != "") |> 
    count(comunas, sort = TRUE) |> 
    head(1)
  
  dcomunas_low <- data_noticias |> 
    mutate(comunas = map(comunas, ~ str_squish(unlist(str_split(.x, "\\,")))))  |> 
    unnest(comunas) |> 
    mutate(comunas = str_remove_all(comunas, "\"|\\'|\\[|\\]|\\{|\\}")) |> 
    filter(comunas != "") |> 
    count(comunas, sort = TRUE) |> 
    tail(1)
  
  data_noticias_comuna <- data_noticias |> 
    filter(str_detect(comunas, dcomunas$comunas)) |> 
    head(3)

  
  stringr::str_trunc("asda sdas dasd asdasd asd asdasd asd a asdasda dasd asdasd asd asd", 20)
  
  
  
  layout_column_wrap(
    width = 1,
    fill = TRUE,
    fillable = TRUE,
    layout_column_wrap(
      width = 1/3,
      fill = TRUE,
      fillable = TRUE,
      card(card_header(class = "primary", style = str_glue("background-color:{PARS$color_chart};color:white;"),
                       tags$p("Las", tags$b("noticias más relevantes hoy"), "son de")),
           tabla_a_html(get_noticias_categorias(data_noticias) %>% 
                          head(3), 
                        PARS$color_chart)
      ),
      card(card_header(class = "secondary", style = str_glue("background-color:{PARS$palette[1]};color:white;"), 
                       tags$p("Los", tags$b("conceptos más frecuentes hoy"), "son")),
           tabla_a_html(get_noticias_ngram(data_noticias, 3) %>% 
                          head(3), PARS$palette[1])
      ),
      card(card_header(class = "primary", style = str_glue("background-color:{PARS$palette[4]};color:white;"),
                       tags$p("El", tags$b("GS fue mencionado hoy"), "en")),
           data_noticias %>% 
             filter(gore == 1) %>% 
             select(media, url) %>% 
             head(5) %>% 
             tabla_a_html_link_web()
      )
    ),
    layout_column_wrap(
      style = css(grid_template_columns = "2fr 1fr 1fr"),
      fill = TRUE,
      fillable = TRUE,
      card(card_header(class = "primary", tags$p("La", tags$b("noticia más comentada"), "es")),
          tags$p(
            tags$b(style = str_glue("font-size: large;color:{PARS$palette[4]};"), stringr::str_trunc(dcomentario, 50)),
            tags$br(),
            tags$span(style = "font-size: small;", "Medio: Emol"),
            tags$span(style = "font-size: small;", "Categoría: Política")
          )
      ),
      card(card_header(class = "secondary", tags$p("La", tags$b("presencia del GS en noticias"))),
           tags$p(
             tags$b(style = str_glue("font-size: large;color:{PARS$palette[1]};"), 
                    "La última semana ha aumentado en un 21%")
           )
      ),
      card(card_header(class = "secondary", tags$p("La", tags$b("comuna con menos noticias"))),
           tags$p(
             tags$b(style = str_glue("font-size: x-large;color:{PARS$palette[1]};"), 
                    dcomunas_low$comunas),
             tags$br(),
             tags$span(style = "font-size: small;", "Sin noticias esta semana")
           )
      )
    ),
    layout_column_wrap(
      width = 1,
      card(card_header(class = "primary",  style = str_glue("background-color:{PARS$palette[5]};color:white;"), 
                       tags$p("La", tags$b("comuna con más noticias hoy"), "es", tags$b(dcomunas$comunas), "con", tags$b(paste0(dcomunas$n, " noticias")))),
           layout_column_wrap(
             width = 1/3,
             card(
               tags$p(tags$a(href = data_noticias_comuna[1,]$url, data_noticias_comuna[1,]$title),
                      tags$br(),
                      tags$span(style = "font-size: small;", paste0("Medio: ", str_to_title(data_noticias_comuna[1,]$media))),
                      tags$span(style = "font-size: small;", paste0("Categoria: ", str_to_title(data_noticias_comuna[1,]$categoria)))
                      )
             ),
             card(
               tags$p(tags$a(href = data_noticias_comuna[2,]$url, data_noticias_comuna[2,]$title),
                      tags$br(),
                      tags$span(style = "font-size: small;", paste0("Medio: ", str_to_title(data_noticias_comuna[2,]$media))),
                      tags$span(style = "font-size: small;", paste0("Categoria: ", str_to_title(data_noticias_comuna[2,]$categoria)))
               )
             ),
             card(
               tags$p(tags$a(href = data_noticias_comuna[3,]$url, data_noticias_comuna[3,]$title),
                      tags$br(),
                      tags$span(style = "font-size: small;", paste0("Medio: ", str_to_title(data_noticias_comuna[3,]$media))),
                      tags$span(style = "font-size: small;", paste0("Categoria: ", str_to_title(data_noticias_comuna[3,]$categoria)))
               )
             )
           )
           
           )
    )
  )
  
  
  
}


# colors ------------------------------------------------------------------
get_ncolors <- function(colors, n){
  cols <- colorRampPalette(colors= colors)(n)
  cols
}

vector_a_colores <- function(valores, color_min = "gray", color_max = "darkred") {
  
  if(length(valores) == 1) return(color_max)
  
  # Normalizar los valores entre 0 y 1
  valores_norm <- (valores - min(valores)) / (max(valores) - min(valores))
  
  # Crear una paleta de colores
  paleta_colores <- colorRampPalette(c(color_min, color_max))(length(valores))
  
  # Asignar colores basados en los valores normalizados
  colores <- paleta_colores[cut(valores_norm, breaks = length(valores), labels = FALSE)]
  
  cut(valores_norm, breaks = length(valores))
  
  return(colores)
}

pallete_tendencias <- function(n = 3){
  
  df <- tibble(
    nc = c(1, 2, 3, 4, 5),
    ids = list(
      c(1),
      c(1, 5),
      c(1, 3, 5),
      c(1, 3, 4, 5),
      c(1, 2, 3, 4, 5)
      )
  )
  
  ids <- df |> 
    filter(nc == n) |> 
    pull(ids) |> 
    unlist()
  
  PARS$palette[ids]
  
}


