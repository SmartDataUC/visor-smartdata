# packages ----------------------------------------------------------------
tictoc::tic("packages")
library(shiny)
library(bslib)
library(tidyverse)
library(tidytext)
library(DBI)
library(highcharter)
library(shinyWidgets)
library(tictoc)

source("R/helpers.R")

tictoc::toc()
# parameters --------------------------------------------------------------
PARS <- list(
  bg = "#fff",
  fg = "#454546",
  color_chart = "#496973",
  base_font = "Open Sans",
  heading_font = "Montserrat"
)

# data --------------------------------------------------------------------
tictoc::tic("data")
stopwords_es   <- readLines("https://raw.githubusercontent.com/Alir3z4/stop-words/master/spanish.txt")
stopwords_es_2 <- paste(paste0("\\b", stopwords_es, "\\b"), collapse = "|")

pool <- pool::dbPool(
  drv = RPostgres::Postgres(),
  # dbname = "smartdata",
  user = "postgres",
  password = Sys.getenv("PASS"),
  host = Sys.getenv("HOST"),
  port = 5432
  )

# data para los graficos de inicio
data_noticias_once <- get_noticias_ultimas_horas(24)
# data_noticias <- data_noticias_once

onStop(function() { pool::poolClose(pool)})

tictoc::toc()
# theme -------------------------------------------------------------------
smart_theme <- bs_theme(
  bg = PARS$bg,
  fg = PARS$fg,
  primary = "#000",
  "navbar-bg" = PARS$bg,
  base_font = font_google(PARS$base_font),
  heading_font = font_google(PARS$heading_font)
)

# bslib::bs_theme_preview(smart_theme)


# highcharter -------------------------------------------------------------
langs <- getOption("highcharter.lang")
langs$loading <- "<i class='fas fa-circle-notch fa-spin fa-4x'></i>"

options(
  highcharter.lang = langs,
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
      series = list(marker = list(enabled = TRUE))
      )
    )
  )

# sidebar and options -----------------------------------------------------
tictoc::tic("sidebar and options")
opts_categorias <- tbl(pool, "noticias") |> 
  count(categoria = category_1, sort = TRUE) |> 
  collect() |> 
  pull(categoria)

opts_categorias <- set_names(opts_categorias, str_to_title(opts_categorias))

opts_tiempo <-  c("Ultimas 24 horas" = 24,
                  "Última semana"    = 24 * 7,
                  "Últimos 30 días"  = 24 * 30,
                  "Último año"       = 24 * 365)


# opts_fechas <- tbl(pool, "noticias") |> 
#   summarise(as_date(min(date)), as_date(max(date))) |> 
#   collect() |> 
#   pivot_longer(cols = everything()) |> 
#   pull(value)
# opts_fechas <- seq.Date(opts_fechas[1], opts_fechas[2], by = "day")
# opts_fechas_sel <- as.character(c(opts_fechas[2] - days(6), opts_fechas[2]))

smart_sidebar <- sidebar(
  bg = PARS$bg,
  width = 300,
  selectizeInput(
    "categorias",
    "Categorías",
    choices = opts_categorias,
    multiple  = TRUE,
    options = list(placeholder = "Todas las categorías")
  ),
  selectizeInput("tiempo", "Periodo", choices = opts_tiempo)
  # sliderTextInput("fecha", "Fecha", choices = opts_fechas, selected = )
)



tictoc::toc()
# partials ----------------------------------------------------------------
card <- purrr::partial(bslib::card, full_screen = TRUE)

# test --------------------------------------------------------------------
DBI::dbListTables(pool)

# tbl(pool, "noticias")
# 
# tbl(pool, "noticias") |>
#   summarise(min(date), max(date))


