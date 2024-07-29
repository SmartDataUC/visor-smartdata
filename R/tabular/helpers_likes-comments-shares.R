# identificar publicaciones con más likes, comments y shares ----------


# más likes ---------------------------------
get_max_likes <- function(data) {
  data |> 
    filter(likes >= 1) |> 
    arrange(desc(likes)) |> 
    select(any_of(c("date", "comuna", "user", "caption", "url", "comments", "likes", "shares")))  |> 
    mutate(caption = str_squish(caption)) 
}

# get_max_likes(data_facebook)
# get_max_likes(data_instagram)
# get_max_likes(data_instagram_gore)

# más comentarios ---------------------------------

get_max_comments <- function(data) {
  
  data |> 
    filter(comments >= 1) |>  
    arrange(desc(comments)) |> 
    select(any_of(c("date", "comuna", "user", "caption", "url", "comments", "likes", "shares"))) |>  
    mutate(caption = str_squish(caption)) 
}


# get_max_comments(data_facebook)
# get_max_comments(data_instagram)
# get_max_comments(data_instagram_gore)

# Más veces compartido (solo para FB) -----------------

get_max_shares <- function(data) {
  data |> 
    filter(shares >= 1) |> 
    arrange(desc(shares)) |> 
    select(any_of(c("date", "comuna", "user", "caption", "url", "comments", "likes", "shares"))) |>  
    mutate(caption = str_squish(caption)) 
}

# get_max_shares(data_facebook)
