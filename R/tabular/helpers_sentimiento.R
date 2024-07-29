# identificar publicaciones más positivas o negativas  ----------


get_publicaciones_positivas <- function(data) {
  data |> 
    filter(!is.na(sentimiento)) |> 
    arrange(desc(positivo)) |> 
    select(any_of(c("date", "comuna", "user", "caption", "url", "comments", "likes", "shares"))) |> 
    mutate(caption = str_squish(caption)) 
}

# get_publicaciones_positivas(data_facebook)
# get_publicaciones_positivas(data_instagram)


get_publicaciones_negativas <- function(data) {
  data |> 
    filter(!is.na(sentimiento)) |> 
    arrange(desc(negativo)) |> 
    select(any_of(c("date", "comuna", "user", "caption", "url", "comments", "likes", "shares"))) |> 
    mutate(caption = str_squish(caption)) 
}

# get_publicaciones_negativas(data_facebook)
# get_publicaciones_negativas(data_instagram)
# get_publicaciones_negativas(data_instagram_gore)

# identificar publicaciones con comentarios más positivos o negativos ------


get_comentarios_positivos <- function(data) {
  data |> 
    filter(!is.na(comentarios_positivos), comments >= 2) |> 
    arrange(desc(comentarios_positivos)) |> 
    select(any_of(c("date", "comuna", "user", "caption", "url", "comments", "likes", "shares"))) |> 
    mutate(caption = str_squish(caption)) 
}

# get_comentarios_positivos(data_instagram) 
# get_comentarios_positivos(data_instagram_gore)
# get_comentarios_positivos(data_facebook)

get_comentarios_negativos <- function(data) {
  data |> 
    filter(!is.na(comentarios_negativos), comments >= 2) |> 
    arrange(desc(comentarios_negativos)) |> 
    select(any_of(c("date", "comuna", "user", "caption", "url", "comments", "likes", "shares"))) |> 
    mutate(caption = str_squish(caption)) 
}

# get_comentarios_negativos(data_instagram) 
# get_comentarios_negativos(data_instagram_gore) 
# get_comentarios_negativos(data_facebook) 
