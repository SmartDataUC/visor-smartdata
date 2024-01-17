# data functions ----------------------------------------------------------
get_noticias_date_range <- function(d1, d2, categorias = NULL, comunas = NULL){

  # d1 <- Sys.Date() - days(30)
  # d2 <- Sys.Date()
  # categorias <- NULL
  # comunas <- NULL
  
  ds <- c(d1, d2)
  d1 <- min(ds)
  d2 <- max(ds)

  cli::cli_inform("running get_noticias_date_range: {d1} a {d2}. {diffdate2(d1, d2)}")
  
  data_noticias <- tbl(pool, "news") |> 
    filter(d1 <= date, date <= d2)
  
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
    select(title = clean_title, body = clean_body, categoria = category_1, url, 
           media, date, comunas, gore, sentiment) |> 
    collect() |> 
    mutate(date = as_date(date))
  
  cli::cli_inform("running get_noticias_ultimas_horas: {scales::comma(nrow(data_noticias))} filas.")
  
  # glimpse(data_noticias)
  
  data_noticias
  
}

get_noticias_ngram <- function(data_noticias, ng = 3){
  # ng <- 2
  
  cli::cli_inform("running get_noticias_ngram: {ng}")
  
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
    )  |> 
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
      card(card_header("Tendencia histórica")    , hc1),
      card(card_header("Distribución medios"), hc2)
    ),
    card(card_header("Noticias de la categoría"), doutdt, height = "350px")
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
      card(card_header("Tendencia histórica")    , hc1),
      card(card_header("Distribución categorías"), hc2)
    ),
    card(card_header("Noticias donde se encuentra presente el concepto"), doutdt, height = "350px")
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
      card(card_header("Tendencia histórica")    , hc1),
      card(card_header("Conceptos más frecuentes"), hc2)
    ),
    card(card_header("Noticias asociadas al sentimiento"), doutdt, height = "350px")
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
      card(card_header("Tendencia histórica")    , hc1),
      card(card_header("Conceptos más frecuentes"), hc2)
    ),
    card(card_header("Noticias en donde se identifica la presencia de la comuna"), doutdt, height = "350px")
  )
  
}