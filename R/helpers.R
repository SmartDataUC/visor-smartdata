get_noticias_ultimas_horas <- function(horas = 24, categorias = NULL){
  # horas = 24 * 30
  cli::cli_inform("running get_noticias_ultimas_horas: dias {as.numeric(horas)/24}, categorias: {str_c(categorias, sep = ', ')}")
  
  since <- Sys.Date() - hours(horas)
  
  data_noticias <- tbl(pool, "noticias") |> 
    filter(date >= since)
  
  if(!is.null(categorias)){
    data_noticias <- data_noticias |> 
      filter(category_1 %in% categorias)
  }
  
  data_noticias <- data_noticias |> 
    select(body, categoria = category_1) |> 
    collect()
  
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
  
  for(i in 1:ng){
    cli::cli_inform("running get_noticias_ngram: removing stopwods in {i} space, nrow {nrow(data_noticias_ngram)}")
    # data_noticias_ngram |>
    #   mutate(w = word(ngram, start = 1L))
    # data_noticias_ngram <- ""

    data_noticias_ngram <- data_noticias_ngram |>
      filter(!word(ngram, start = i) %in% stopwords_es)

  }
  # 
  data_noticias_ngram
  
}

get_noticias_categorias <- function(data_noticias){

  cli::cli_inform("running get_noticias_categorias")

  data_noticias_categorias <- data_noticias |>
    count(categoria) |>
    collect() |>
    mutate(categoria = str_to_title(categoria)) |>
    arrange(desc(n))

  data_noticias_categorias

}

get_data_dimension <- function(){
  data_dimension <- tibble(
    dimension = c("Medio Ambiente", "Seguridad", "Movilidad", "Emergencias", "Resilencia"),
    score     = round(100 * rbeta(length(dimension), 10, 4), 0)
  )
  data_dimension
}
