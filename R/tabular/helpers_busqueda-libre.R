# Búsqueda libre en las publicaciones de Instagram o Facebook ----------


search_caption <- function(data, pattern, ignore_case = TRUE) {
  search_result <- data |> 
    filter(str_detect(caption, regex(pattern, ignore_case = ignore_case))) |> 
    select(any_of(c("date", "comuna", "user", "caption", "url", "comments", "likes", "shares"))) |> 
    mutate(caption = str_squish(caption))
  
  return(search_result)
}


# search_caption(data_instagram, "gato") 
# search_caption(data_instagram_gore, "lluvia") 
# search_caption(data_facebook, "cine") 

search_comment <- function(data, pattern, ignore_case = TRUE) {
  search_result <- data |> 
    filter(str_detect(comment_text, regex(pattern, ignore_case = ignore_case))) |> 
    select(any_of(c("date", "comuna", "comment_text", "caption", "url", "user"))) |> 
    mutate(comment_text = str_squish(comment_text), caption = str_squish(caption))
  
  return(search_result)
}


# search_comment(data_comments_instagram, "gato") 
# search_comment(data_comments_instagram_gore, "nunca") 
# search_comment(data_comments_facebook, "perro") 



