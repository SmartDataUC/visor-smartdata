# buscar por categorías predefinidas ------------------------

ruta_archivo <- "data/palabras-clave.xlsx"

# lista con listas de palabras 
lista_palabras_clave <- excel_sheets(ruta_archivo) |> 
  set_names() |> 
  map(read_excel, path = ruta_archivo)

# cargar categorías disponibles 
# en la documentación vamos a explicar cómo agregar más
categorias <- excel_sheets(ruta_archivo)

search_keywords <- function(data, category, ignore_case = TRUE) {
  
  pattern <- lista_palabras_clave[[category]] |> 
    pull() |> 
    paste0("\\b", . = _, "\\b", collapse = "|")
  
  search_result <- data |> 
    filter(str_detect(caption, regex(pattern, ignore_case = ignore_case))) |> 
    select(any_of(c("date", "comuna", "user", "caption", "url", "comments", "likes", "shares"))) |> 
    mutate(caption = str_squish(caption))
  
  return(search_result)
}

# search_keywords(data_facebook, "Educación") 
# search_keywords(data_instagram, "Seguridad") 
# search_keywords(data_instagram_gore, "Medioambiente") 

