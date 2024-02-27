# packages ----------------------------------------------------------------
cli::cli_h1("packages")
suppressWarnings(library(tidyverse))
library(shiny)
library(bslib)
library(tidytext)
library(DBI)
library(highcharter)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(DT)
library(bsicons)
library(scales)
library(leaflet)
library(sf)
library(snakecase)
library(dbplyr)
library(pool)
library(RPostgres)
loadNamespace("dbplyr")

source("R/helpers.R")

# parameters --------------------------------------------------------------
cli::cli_h1("parameters")
PARS <- list(
  bg = "#fff",
  fg = "#454546",
  color_chart = "#6E438B",
  base_font = "Open Sans",
  heading_font = "Montserrat",
  palette = c("#C4308F", "#B23FA5", "#6E438B", "#485497", "#53BAAE")
)

# data --------------------------------------------------------------------
cli::cli_h1("data")
stopwords_es   <- readLines("https://raw.githubusercontent.com/Alir3z4/stop-words/master/spanish.txt")
# stopwords_es_2 <- paste(paste0("\\b", stopwords_es, "\\b"), collapse = "|")

dcomunas <- sf::read_sf("data/comunas_0001.gpkg")

pool <- pool::dbPool(
  drv = RPostgres::Postgres(),
  # dbname = "smartdata",
  user = "postgres",
  password = Sys.getenv("PASS"),
  host = Sys.getenv("HOST"),
  port = 5432
  )

if(!interactive()) {
  onStop(function() { pool::poolClose(pool)})
}


# data rrss ---------------------------------------------------------------
# read_csv("data/gore_insta_posts.csv") |> glimpse()
drrhh <- bind_rows(
  read_csv("data/gore_insta_posts.csv") |> mutate(categoria = "GORE"),
  read_csv("data/orrego_insta_posts.csv") |> mutate(categoria = "Gobernador")
) |> 
  select(id, categoria, caption, inputUrl, timestamp, likesCount, commentsCount, url, inputUrl,
         commentsAll) |> 
  distinct(id, .keep_all = TRUE) 

# theme -------------------------------------------------------------------
cli::cli_h1("theme")
smart_theme <- bs_theme(
  bg = PARS$bg,
  fg = PARS$fg,
  # primary = "#000",
  primary = PARS$color_chart,
  base_font = font_google(PARS$base_font),
  heading_font = font_google(PARS$heading_font),
  "popover-max-width" = "400px"
)

# bslib::bs_theme_preview(smart_theme)


# highcharter -------------------------------------------------------------
cli::cli_h1("highcharter")

hcopts <- getOption("highcharter.chart")
hcopts$exporting <- list(
  enabled = TRUE,
  buttons = list(
    contextButton = list(
      menuItems = list(
        "printChart",
        # "separator",
        # "downloadPNG",
        "downloadJPEG",
        "downloadPDF",
        # "downloadSVG",
        # "separator",
        # "downloadCSV",
        "downloadXLS"
      ),
      symbolStrokeWidth = 1,
      symbolFill =  '#C0C0C0',
      symbolStroke = '#C0C0C0'
    )
  )
)

newlang_opts <- getOption("highcharter.lang")
newlang_opts$weekdays     <- c("domingo", "lunes", "martes", "miércoles", "jueves", "viernes", "sábado")
newlang_opts$months       <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio",
                               "agosto", "septiembre", "octubre", "noviembre", "diciembre")
newlang_opts$shortMonths  <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep",
                               "oct", "nov", "dic")
newlang_opts$drillUpText  <- "◁ Volver a {series.name}"
newlang_opts$loading      <- "<i class='fas fa-circle-notch fa-spin fa-4x'></i>"
# newlang_opts$loading      <- "Cargando información"

newlang_opts$downloadCSV  <- "Descargar CSV"
newlang_opts$downloadJPEG <- "Descargar JPEG"
newlang_opts$downloadPDF  <- "Descargar PDF"
newlang_opts$downloadPNG  <- "Descargar PNG"
newlang_opts$downloadSVG  <- "Descargar SVG"
newlang_opts$downloadXLS  <- "Descargar Excel"
newlang_opts$printChart   <- "Imprimir gráfico"
newlang_opts$viewFullscreen <- "Ver pantalla completa"
newlang_opts$resetZoom    <- "Resetear zoom"
newlang_opts$thousandsSep <- "."
newlang_opts$decimalPoint <- ","

newlang_opts$contextButtonTitle <- "Menú contextual del gráfico"
newlang_opts$numericSymbols <- JS("null")

options(
  highcharter.lang = newlang_opts,
  highcharter.chart = hcopts,
  highcharter.theme = hc_theme_smpl(
    colors = PARS$color_chart,
    chart = list(
      style = list(
        fontFamily = PARS$base_font,
        color = PARS$fg
      )
    ),
    title = list(
      align = "left",
      style = list(
        fontFamily = PARS$heading_font,
        fontWeight = "bold",
        color = PARS$fg
      )
    ),
    subtitle = list(
      align = "left",
      style = list(
        fontFamily = PARS$heading_font,
        fontWeight = "bold",
        color = PARS$fg
      )
    ),
    plotOptions = list(
      series = list(marker = list(enabled = FALSE))
      )
    )
  )

# sidebar and options -----------------------------------------------------
cli::cli_h1("sidebar and options")

# valor para dateRangeInput
mindate <- tbl(pool, "news") |> 
  summarise(min(date, na.rm = TRUE)) |> 
  collect() |> 
  pull() |> 
  as.Date()

opts_categorias <- tbl(pool, "news") |> 
  filter(gore == 1) |> 
  count(categoria = category_1, sort = TRUE) |> 
  collect() |> 
  pull(categoria)

opts_categorias <- set_names(opts_categorias, str_to_title(opts_categorias))

opts_comunas <- tbl(pool, "news") |>  
  count(comunas, sort = TRUE) |>
  collect() |> 
  pull(comunas) |> 
  str_remove_all("\'|\"|\\[|\\]|\\{|\\}") |> 
  str_split(",") |> 
  unlist() |> 
  unique() |> 
  str_squish() |> 
  setdiff("")
  
# opts_tiempo <-  c(
#   # "Ultimas 24 horas" = 24,
#   "Última semana"    = 24 * 7,
#   "Último mes"       = 24 * 30,
#   "Último 3 meses"  = 24 * 30 * 3,
#   "Último 6 meses"  = 24 * 30 * 6
#   # "Último año"       = 24 * 365
#   )


# opts_fechas <- tbl(pool, "noticias") |> 
#   summarise(as_date(min(date)), as_date(max(date))) |> 
#   collect() |> 
#   pivot_longer(cols = everything()) |> 
#   pull(value)
# opts_fechas <- seq.Date(opts_fechas[1], opts_fechas[2], by = "day")
# opts_fechas_sel <- as.character(c(opts_fechas[2] - days(6), opts_fechas[2]))

smart_sidebar <- sidebar(
  id = "sidebar",
  open = "open",
  bg = PARS$bg,
  width = 300,
  dateRangeInput(
    "fecha",
    "Fechas", 
    min   = mindate,
    start = Sys.Date() - months(1) + days(1),
    end   = Sys.Date(),
    max   = Sys.Date(),
    separator = " a ", 
    language = "es"),
  selectizeInput(
    "categorias",
    "Categorías",
    choices = opts_categorias,
    multiple  = TRUE,
    options = list(placeholder = "Todas las categorías", maxItems = 99999)
  ),
  selectizeInput(
    "comunas",
    "Comunas",
    choices = opts_comunas,
    multiple  = TRUE,
    options = list(placeholder = "Todas las comunas")
  ),
  tags$small(bs_icon("info-circle"), uiOutput("fecha_info", inline = TRUE), class = "text-muted float-right")
  # selectizeInput("tiempo", "Periodo", choices = opts_tiempo),
  # sliderTextInput("fecha", "Fecha", choices = opts_fechas, selected = )
)


# partials ----------------------------------------------------------------
card        <- purrr::partial(bslib::card, full_screen = TRUE)
comma       <- purrr::partial(scales::comma, big.mark = ".", decimal.mark = ",")
modalDialog <- purrr::partial(
  shiny::modalDialog,
  title = NULL,
  size = "xl",
  easyClose = TRUE,
  fade = TRUE,
  footer = NULL
  )

datatable <- purrr::partial(
  DT::datatable,
  escape = FALSE,
  rownames = FALSE,
  options = list(
    bPaginate = FALSE,
    searching = FALSE,
    info = FALSE,
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
    )
  )

# test --------------------------------------------------------------------
DBI::dbListTables(pool)

# tbl(pool, "noticias")
# 
# tbl(pool, "noticias") |>
#   summarise(min(date), max(date))


