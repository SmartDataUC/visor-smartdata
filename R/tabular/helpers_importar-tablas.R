# importar tablas principales ----------------------
get_tabla_instagram <- function(d1, d2, comuna = NULL){
  
  #d1 <- Sys.Date() - days(1)
  #d2 <- Sys.Date() - days(45)
  #comunas <- "NULL"
  
  # if(is.null(d1)) return(tibble())
  
  ds <- c(d1, d2)
  d1 <- min(ds)
  d2 <- max(ds)
  
  cli::cli_inform("get_tabla_instagram: {d1} a {d2} =  {diffdate2(d1, d2)}")
  
  data_instagram <- tbl(pool, "instagram_municipalidades_copy") |> 
    filter(d1 <= as_date(timestamp), as_date(timestamp) <= d2)
  
  if(!is.null(comuna)){
    comunas_regex <- str_c(comuna, collapse = "|")
    data_instagram <- data_instagram |> 
      filter(str_detect(comuna, comunas_regex))
  }
  
  data_instagram <- data_instagram |> 
    select(id, comuna, user = ownerUsername, caption, url, comments = commentsCount, likes = likesCount, date_time = timestamp) |> 
    collect() |> 
    mutate(id = as.character(id), 
           date = as_date(date_time), 
           comuna = stringr::str_to_title(comuna),
           comuna = stringr::str_replace(comuna, "De", "de"), 
           .after = id) # dejamos siempre una versión dttm por si acaso se necesita en algún momento la hora
  
  data_instagram <- data_instagram |> 
    distinct(id, .keep_all = TRUE)
  
  cli::cli_inform("running get_tabla_instagram: removing duplicated rows.")
  
  
  insta_munis_sentimiento <- tbl(pool, "instagram_municipalidades_publicaciones__copy") |> # este nombre va a cambiar
    filter(d1 <= as_date(timestamp), as_date(timestamp) <= d2) |> 
    select(id, sentimiento, negativo = NEG, neutro = NEU, positivo = POS) |> 
    distinct(id, .keep_all = TRUE) |> 
    collect() |> 
    mutate(id = as.character(suppressWarnings(as.numeric(id))))
  
  
  data_instagram <- suppressMessages(left_join(data_instagram, insta_munis_sentimiento))
  
  
  insta_munis_comentarios_sentimientos <- tbl(pool, "instagram_municipalidades_comentarios__copy") |> 
    filter(d1 <= as_date(date), as_date(date) <= d2) |> 
    select(id, comment_text, sentimiento, negativo = NEG, neutro = NEU, positivo = POS) |> 
    distinct(comment_text, .keep_all = TRUE) |> 
    collect() |> 
    mutate(id = as.character(suppressWarnings(as.numeric(id)))) |> 
    summarise(comentarios_negativos = mean(negativo, na.rm = TRUE),
              comentarios_neutros = mean(neutro, na.rm = TRUE),
              comentarios_positivos = mean(positivo, na.rm = TRUE),
              .by = id)
  
  data_instagram <- suppressMessages(left_join(data_instagram, insta_munis_comentarios_sentimientos))
  
  
  cli::cli_inform("running get_tabla_instagram: getting sentiment data.")
  
  

  
  # glimpse(data_instagram)
  
  return(data_instagram)
  
}

get_tabla_facebook <- function(d1, d2, comuna = NULL){
  
  #d1 <- Sys.Date() - days(1)
  #d2 <- Sys.Date() - days(45)
  #comunas <- "NULL"
  
  # if(is.null(d1)) return(tibble())
  
  ds <- c(d1, d2)
  d1 <- min(ds)
  d2 <- max(ds)
  
  cli::cli_inform("get_tabla_facebook: {d1} a {d2} =  {diffdate2(d1, d2)}")
  
  data_facebook <- tbl(pool, "facebook_municipalidades_copy") |> 
    filter(d1 <= as_date(time), as_date(time) <= d2, sharedPost == "{}") # solo publicaciones propias
  
  if(!is.null(comuna)){
    comunas_regex <- str_c(comuna, collapse = "|")
    data_facebook <- data_facebook |> 
      filter(str_detect(comuna, comunas_regex))
  }
  
  data_facebook <- data_facebook |> 
    select(id, post_id = postId, caption = text, user, url, likes, comments, shares, comuna, date_time = time) |> 
    filter(caption != "") |> 
    collect() |> 
    mutate(id = as.character(id),
           date = as_date(date_time), 
           comuna = stringr::str_to_title(comuna),
           comuna = stringr::str_replace(comuna, "De", "de"),
           user = map_chr(map(user, fromJSON), ~ pluck(.x, "name", .default = NA_character_)), # no deberían haber casos sin name, pero por si acaso
           .after = id)
  
  data_facebook <- data_facebook |> 
    distinct(post_id, .keep_all = TRUE)
  
  cli::cli_inform("running get_tabla_facebook: removing duplicated rows.")
  
  fb_munis_sentimiento <- tbl(pool, "facebook_municipalidades_publicaciones__copy") |>
    filter(d1 <= as_date(time), as_date(time) <= d2) |> 
    select(id, sentimiento, negativo = NEG, neutro = NEU, positivo = POS) |> 
    distinct(id, .keep_all = TRUE) |>
    collect() |>
    mutate(id = as.character(suppressWarnings(as.numeric(id))))
  
  data_facebook <- suppressMessages(left_join(data_facebook, fb_munis_sentimiento))
  
  fb_munis_comentarios_sentimientos <- tbl(pool, "facebook_municipalidades_comentarios__copy") |> 
    filter(d1 <= as_date(time), as_date(time) <= d2) |> 
    select(id, comment_text, sentimiento, negativo = NEG, neutro = NEU, positivo = POS) |> 
    distinct(comment_text, .keep_all = TRUE) |> 
    collect() |> 
    mutate(id = as.character(suppressWarnings(as.numeric(id)))) |> 
    summarise(comentarios_negativos = mean(negativo, na.rm = TRUE),
              comentarios_neutros = mean(neutro, na.rm = TRUE),
              comentarios_positivos = mean(positivo, na.rm = TRUE),
              .by = id)
  
  data_facebook <- suppressMessages(left_join(data_facebook, fb_munis_comentarios_sentimientos))
  
  cli::cli_inform("running get_tabla_facebook: getting sentiment data.")
  
  # glimpse(data_facebook)
  
  
  return(data_facebook)
  
}

get_tabla_insta_gore <- function(d1, d2){
  
  #d1 <- Sys.Date() - days(1)
  #d2 <- Sys.Date() - days(45)
  
  # if(is.null(d1)) return(tibble())
  
  ds <- c(d1, d2)
  d1 <- min(ds)
  d2 <- max(ds)
  
  cli::cli_inform("get_tabla_insta_gore: {d1} a {d2} =  {diffdate2(d1, d2)}")
  
  data_instagram_gore <- tbl(pool, "instagram_gore_copy") |> 
    filter(d1 <= as_date(timestamp), as_date(timestamp) <= d2)
  
  data_instagram_gore <- data_instagram_gore |> 
    select(id, caption, user = ownerUsername, url, comments = commentsCount, likes = likesCount, date_time = timestamp
    ) |> 
    collect() |> 
    mutate(id = as.character(id), date = as_date(date_time),  .after = id)
  
  data_instagram_gore <- data_instagram_gore |>
    distinct(id, .keep_all = TRUE) 
  
  cli::cli_inform("running get_tabla_insta_gore: removing duplicated rows.")
  
  insta_gore_sentimiento <- tbl(pool, "instagram_gore_publicaciones__copy") |> # este nombre va a cambiar
    filter(d1 <= as_date(timestamp), as_date(timestamp) <= d2) |> 
    select(id, sentimiento, negativo = NEG, neutro = NEU, positivo = POS) |> 
    distinct(id, .keep_all = TRUE) |> 
    collect() |> 
    mutate(id = as.character(suppressWarnings(as.numeric(id))))
  
  data_instagram_gore <- suppressMessages(left_join(data_instagram_gore, insta_gore_sentimiento))
  
  
  insta_gore_comentarios_sentimientos <- tbl(pool, "instagram_gore_comentarios__copy") |> 
    filter(d1 <= as_date(date), as_date(date) <= d2) |> 
    select(id, comment_text, sentimiento, negativo = NEG, neutro = NEU, positivo = POS) |> 
    distinct(comment_text, .keep_all = TRUE) |> 
    collect() |> 
    mutate(id = as.character(suppressWarnings(as.numeric(id)))) |> 
    summarise(comentarios_negativos = mean(negativo, na.rm = TRUE),
              comentarios_neutros = mean(neutro, na.rm = TRUE),
              comentarios_positivos = mean(positivo, na.rm = TRUE),
              .by = id)
  
  data_instagram_gore <- suppressMessages(left_join(data_instagram_gore, insta_gore_comentarios_sentimientos))
  
  cli::cli_inform("running get_tabla_insta_gore: getting sentiment data.")
  

  # glimpse(data_instagram_gore)
  
  return(data_instagram_gore)
  
}


# Importar comentarios -------------------
get_comments_instagram <- function(d1, d2, comuna = NULL){
  
  #d1 <- Sys.Date() - days(1)
  #d2 <- Sys.Date() - days(45)
  #comunas <- "NULL"
  
  # if(is.null(d1)) return(tibble())
  
  ds <- c(d1, d2)
  d1 <- min(ds)
  d2 <- max(ds)
  
  cli::cli_inform("get_comments_instagram: {d1} a {d2} =  {diffdate2(d1, d2)}")
  
  data_instagram <- tbl(pool, "instagram_municipalidades_copy") |> 
    filter(d1 <= as_date(timestamp), as_date(timestamp) <= d2)
  
  if(!is.null(comuna)){
    comunas_regex <- str_c(comuna, collapse = "|")
    data_instagram <- data_instagram |> 
      filter(str_detect(comuna, comunas_regex))
  }
  
  data_comments_instagram <- data_instagram |> 
    select(id, comuna, user = ownerUsername, caption, url, comment = commentsAll, date_time = timestamp) |> 
    collect() 
  
  cli::cli_inform("get_comments_instagram: extracting comments")  
  
  data_comments_instagram <-  data_comments_instagram |> 
    mutate(id = as.character(id), 
           date = as_date(date_time), 
           comuna = stringr::str_to_title(comuna),
           comuna = stringr::str_replace(comuna, "De", "de"), 
           .after = id) |> 
    mutate(comment = map(comment, fromJSON),
           comment = map(comment, as_tibble)) |> 
    unnest(comment, names_sep = "_") |> 
    mutate(comment_id = as.character(comment_id)) |> 
    select(id, date, comuna, comment_id, comment_text, caption, user, url)
  
  
  # glimpse(data_comments_instagram)
  
  return(data_comments_instagram)
  
}

get_comments_facebook <- function(d1, d2, comuna = NULL){
  
  #d1 <- Sys.Date() - days(1)
  #d2 <- Sys.Date() - days(45)
  #comunas <- "NULL"
  
  # if(is.null(d1)) return(tibble())
  
  ds <- c(d1, d2)
  d1 <- min(ds)
  d2 <- max(ds)
  
  cli::cli_inform("get_comments_facebook: {d1} a {d2} =  {diffdate2(d1, d2)}")
  
  data_facebook <- tbl(pool, "facebook_municipalidades_copy") |> 
    filter(d1 <= as_date(time), as_date(time) <= d2, sharedPost == "{}") # solo publicaciones propias
  
  if(!is.null(comuna)){
    comunas_regex <- str_c(comuna, collapse = "|")
    data_facebook <- data_facebook |>
      filter(str_detect(comuna, comunas_regex))
  }
  
  data_facebook <- data_facebook |> 
    select(id, post_id = postId, caption = text, user, url, comment = commentsAll, comuna, date_time = time) |> 
    collect() 
  
  cli::cli_inform("get_comments_facebook: extracting comments")  
  
  data_comments_facebook <- data_facebook |>
    mutate(id = as.character(id),
           date = as_date(date_time), 
           comuna = stringr::str_to_title(comuna),
           comuna = stringr::str_replace(comuna, "De", "de"),
           user = map_chr(map(user, fromJSON), ~ pluck(.x, "name", .default = NA_character_)), 
           .after = id)  
  
  cli::cli_inform("get_comments_facebook: formating data")  
  
  
  data_comments_facebook <-  data_comments_facebook |> 
    mutate(comment = str_replace_all(comment, "NaN", "null"),
           comment = map(comment, ~fromJSON(.x, simplifyDataFrame = TRUE)),
           comment = map(comment, ~as_tibble(.x))) |> 
    unnest(comment, names_sep = "_") |> 
    select(id, date, comuna, comment_id, comment_text, caption, user, url) |> 
    filter(!is.na(comment_text))
  
  
  
  # glimpse(data_comments_facebook)
  
  
  return(data_comments_facebook)
  
}

get_comments_instagram_gore <- function(d1, d2){
  
  #d1 <- Sys.Date() - days(1)
  #d2 <- Sys.Date() - days(45)
  #comunas <- "NULL"
  
  # if(is.null(d1)) return(tibble())
  
  ds <- c(d1, d2)
  d1 <- min(ds)
  d2 <- max(ds)
  
  cli::cli_inform("get_comments_instagram_gore: {d1} a {d2} =  {diffdate2(d1, d2)}")
  
  data_instagram_gore <- tbl(pool, "instagram_gore_copy") |> 
    filter(d1 <= as_date(timestamp), as_date(timestamp) <= d2)
  
  
  data_comments_instagram_gore <- data_instagram_gore |> 
    select(id, user = ownerUsername, caption, url, comment = commentsAll, date_time = timestamp) |> 
    collect() 
  
  cli::cli_inform("get_comments_instagram_gore: extracting comments")  
  
  data_comments_instagram_gore <-  data_comments_instagram_gore |> 
    mutate(id = as.character(id), 
           date = as_date(date_time), 
           .after = id) |> 
    mutate(comment = map(comment, fromJSON),
           comment = map(comment, as_tibble)) |> 
    unnest(comment, names_sep = "_") |> 
    mutate(comment_id = as.character(comment_id)) |> 
    select(id, date, comment_id, comment_text, caption, user, url)
  
  
  # glimpse(data_comments_instagram_gore)
  
  return(data_comments_instagram_gore)
  
}