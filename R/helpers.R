# pool <- pool::dbPool(
#   drv = RPostgres::Postgres(),
#   # dbname = "smartdata",
#   user = "postgres",
#   password = Sys.getenv("PASS"),
#   host = Sys.getenv("HOST"),
#   port = 5432
# )
# DBI::dbListTables(pool)

get_noticias_ultimas_horas <- function(horas = 24, categorias = NULL){
  # horas = 24 * 300
  # categorias = NULL
  cli::cli_inform("running get_noticias_ultimas_horas: dias {as.numeric(horas)/24}, categorias: {str_c(categorias, sep = ', ')}")
  
  since <- Sys.Date() - hours(horas)
  
  data_noticias <- tbl(pool, "news") |> 
    filter(date >= since)
  
  if(!is.null(categorias)){
    data_noticias <- data_noticias |> 
      filter(category_1 %in% categorias)
  }
  
  data_noticias <- data_noticias |> 
    select(title = clean_title, body = clean_body, categoria = category_1, url, media, date) |> 
    collect() |> 
    mutate(date = as_date(date))
  
  cli::cli_inform("running get_noticias_ultimas_horas: {scales::comma(nrow(data_noticias))} filas.")
  
  data_noticias
  
}

get_noticias_date_range <- function(d1, d2, categorias = NULL){
 
  # d1 <- Sys.Date() - days(6)
  # d2 <- Sys.Date()
  
  cli::cli_inform("running get_noticias_date_range: {d1} a {d2}. {diffdate2(d1, d2)}")
  
  data_noticias <- tbl(pool, "news") |> 
    filter(d1 <= date, date <= d2)
  
  if(!is.null(categorias)){
    data_noticias <- data_noticias |> 
      filter(category_1 %in% categorias)
  }
  
  data_noticias <- data_noticias |> 
    select(title = clean_title, body = clean_body, categoria = category_1, url, media, date) |> 
    collect() |> 
    mutate(date = as_date(date))
  
  cli::cli_inform("running get_noticias_ultimas_horas: {scales::comma(nrow(data_noticias))} filas.")
  
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
