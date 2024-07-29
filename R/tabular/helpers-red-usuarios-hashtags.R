# crear matrices de co-ocurrencia de usuarios y hashtags en Instagram para crear red
create_dfm <- function(data){
  dfm_posts <- data |> 
    select(id, caption) |> 
    corpus(text_field = "caption", docid_field = "id") |> 
    tokens(remove_punct = TRUE) |> 
    dfm()
  return(dfm_posts)
}


get_top_users <- function(df_matrix, top = 50) {  # hay que evaluar si este es el mejor valor por default
  dfm_users <- dfm_select(df_matrix, pattern = "@*")
  top_users <- names(topfeatures(dfm_users, n = top))
  fcm_users <- fcm_select(fcm(dfm_users), pattern = top_users)
  return(fcm_users)
}
 
get_top_hashtags <- function(df_matrix, top = 50) { # lo mismo en este caso
  dfm_hashtags <- dfm_select(df_matrix, pattern = "#*")
  top_hashtags <- names(topfeatures(dfm_hashtags, n = top))
  fcm_hashtags <- fcm_select(fcm(dfm_hashtags), pattern = top_hashtags)
  return(fcm_hashtags)
}

# data_instagram <- get_tabla_instagram(d1, d2)
# create_dfm(data_instagram) |>
#   get_top_users() |>
#   quanteda.textplots::textplot_network()
# 
# create_dfm(data_instagram) |>
#   get_top_hashtags() |>
#   quanteda.textplots::textplot_network()
# 
# create_dfm(data_facebook) |>
#   get_top_users() |>
#   quanteda.textplots::textplot_network()
# 
# create_dfm(data_facebook) |>
#   get_top_hashtags() |>
#   quanteda.textplots::textplot_network()

