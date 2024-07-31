# Documentación de la Aplicación **Visor-Smartdata**


- [Introducción](#introducción)
- [Objetivo de la Aplicación](#objetivo-de-la-aplicación)
- [Características Principales](#características-principales)
  - [Funcionalidades](#funcionalidades)
  - [Tipos de Datos](#tipos-de-datos)
  - [Visualizaciones](#visualizaciones)
- [Detalles Técnicos](#detalles-técnicos)
  - [Paquetes de R Utilizados](#paquetes-de-r-utilizados)
  - [Requerimientos del Servidor](#requerimientos-del-servidor)
  - [Estructura de Archivos](#estructura-de-archivos)
- [Forma de Uso](#forma-de-uso)
  - [Inicio de la Aplicación](#inicio-de-la-aplicación)
  - [Roles de Usuario](#roles-de-usuario)
  - [Navegación y Secciones](#navegación-y-secciones)
- [Seguridad](#seguridad)
- [Despliegue](#despliegue)
  - [Alojamiento](#alojamiento)
  - [Pasos para el Despliegue](#pasos-para-el-despliegue)
- [Mantenimiento y Actualizaciones](#mantenimiento-y-actualizaciones)
  - [Gestión de Actualizaciones](#gestión-de-actualizaciones)
  - [Contacto para Mantenimiento](#contacto-para-mantenimiento)

## Introducción

**Visor-Smartdata** es una aplicación desarrollada en Shiny, diseñada
para ofrecer una visualización interactiva y detallada de la información
extraída de medios de comunicación escritos en internet y redes
sociales. Su principal objetivo es proporcionar un panorama noticioso
actualizado de la realidad de Chile y el Gobierno de Santiago (GS).

## Objetivo de la Aplicación

El propósito principal de **Visor-Smartdata** es permitir a los usuarios
visualizar y analizar datos provenientes de distintos medios de
comunicación y redes sociales, brindando un entendimiento profundo tanto
del comportamiento general como específico de la información. Esta
herramienta está destinada a facilitar el seguimiento de la actualidad
noticiosa y las tendencias informativas que afectan al país y a su
gobierno.

## Características Principales

### Funcionalidades

La aplicación cuenta con varias funcionalidades diseñadas para ofrecer
una experiencia de usuario completa:

- **Visualización de Contenido**: Permite ver el contenido extraído y
  analizado de varios medios de comunicación y redes sociales.
- **Interactividad**: Los usuarios pueden interactuar con gráficos,
  tablas y mapas para explorar los datos de manera dinámica y
  personalizada.
- **Análisis Específico y General**: Los datos pueden ser visualizados
  tanto de manera global como segmentada por categorías, comunas y
  tendencias.

### Tipos de Datos

Los datos manejados por la aplicación incluyen:

- Información extraída de medios de comunicación escritos: [La
  Tercera](https://www.latercera.com), [El
  Dínamo](https://www.eldinamo.cl), [El
  Mostrador](https://www.elmostrador.cl),
  [BioBioChile](https://www.biobiochile.cl), [CNN
  Chile](https://www.cnnchile.com), [The
  Clinic](https://www.theclinic.cl), [ADN
  Radio](https://www.adnradio.cl),
  [Publimetro](https://www.publimetro.cl).

- Datos de redes sociales, específicamente de Instagram y Facebook, que
  han sido previamente descargados y analizados.

### Visualizaciones

Las visualizaciones proporcionadas por la aplicación son:

- **Gráficos de Barra**: Para representar distribuciones y comparaciones
  de datos.
- **Mapas**: Para visualizar información geoespacial.
- **Tablas**: Para mostrar datos detallados de manera estructurada.

## Detalles Técnicos

### Paquetes de R Utilizados

La aplicación utiliza un conjunto de paquetes de R, entre las cuales se
incluyen:

- [`shiny`](https://cran.r-project.org/web/packages/shiny/index.html):
  Para la creación de aplicaciones web interactivas.
- [`bslib`](https://cran.r-project.org/web/packages/bslib/index.html):
  Para temas y estilos.
- [`tidytext`](https://cran.r-project.org/web/packages/tidytext/index.html):
  Para el procesamiento de texto.
- [`DBI`](https://cran.r-project.org/web/packages/DBI/index.html): Para
  la interacción con bases de datos.
- [`highcharter`](https://cran.r-project.org/web/packages/highcharter/index.html):
  Para gráficos interactivos.
- [`shinyWidgets`](https://cran.r-project.org/web/packages/shinyWidgets/index.html):
  Para componentes interactivos adicionales.
- [`DT`](https://cran.r-project.org/web/packages/DT/index.html): Para
  tablas de datos interactivas.
- [`dbplyr`](https://cran.r-project.org/web/packages/dbplyr/index.html):
  Para la manipulación de bases de datos con dplyr.

La lista completa está descrita en la siguiente tabla:

| Paquete            | Versión | Fecha (UTC) | Documentación Oficial                                                 |
|--------------------|---------|-------------|-----------------------------------------------------------------------|
| bsicons            | 0.1.2   | 2023-11-04  | [bsicons](https://cran.r-project.org/package=bsicons)                 |
| bslib              | 0.6.1   | 2023-11-28  | [bslib](https://rstudio.github.io/bslib/)                             |
| DBI                | 1.2.3   | 2024-06-02  | [DBI](https://cran.r-project.org/package=DBI)                         |
| dbplyr             | 2.4.0   | 2023-10-26  | [dbplyr](https://dbplyr.tidyverse.org/)                               |
| dplyr              | 1.1.4   | 2023-11-17  | [dplyr](https://dplyr.tidyverse.org/)                                 |
| DT                 | 0.33    | 2024-04-04  | [DT](https://rstudio.github.io/DT/)                                   |
| forcats            | 1.0.0   | 2023-01-29  | [forcats](https://forcats.tidyverse.org/)                             |
| ggplot2            | 3.5.1   | 2024-04-23  | [ggplot2](https://ggplot2.tidyverse.org/)                             |
| highcharter        | 0.9.4   | 2022-01-03  | [highcharter](http://jkunst.com/highcharter/)                         |
| jsonlite           | 1.8.8   | 2023-12-04  | [jsonlite](https://cran.r-project.org/package=jsonlite)               |
| leaflet            | 2.2.2   | 2024-03-26  | [leaflet](https://rstudio.github.io/leaflet/)                         |
| lubridate          | 1.9.3   | 2023-09-27  | [lubridate](https://lubridate.tidyverse.org/)                         |
| markdown           | 1.13    | 2024-06-04  | [markdown](https://cran.r-project.org/package=markdown)               |
| ngram              | 3.2.3   | 2023-12-10  | [ngram](https://cran.r-project.org/package=ngram)                     |
| pool               | 1.0.3   | 2024-02-14  | [pool](https://cran.r-project.org/package=pool)                       |
| purrr              | 1.0.2   | 2023-08-10  | [purrr](https://purrr.tidyverse.org/)                                 |
| quanteda           | 4.0.2   | 2024-04-24  | [quanteda](https://quanteda.io/)                                      |
| quanteda.textplots | 0.94.4  | 2024-01-25  | [quanteda.textplots](https://quanteda.io/reference/textplots.html)    |
| quanteda.textstats | 0.97    | 2024-04-08  | [quanteda.textstats](https://quanteda.io/reference/textstats.html)    |
| readr              | 2.1.5   | 2024-01-10  | [readr](https://readr.tidyverse.org/)                                 |
| readxl             | 1.4.3   | 2023-07-06  | [readxl](https://readxl.tidyverse.org/)                               |
| RPostgres          | 1.4.7   | 2024-05-27  | [RPostgres](https://cran.r-project.org/package=RPostgres)             |
| scales             | 1.3.0   | 2023-11-28  | [scales](https://scales.r-lib.org/)                                   |
| sf                 | 1.0-16  | 2024-03-24  | [sf](https://r-spatial.github.io/sf/)                                 |
| shiny              | 1.8.0   | 2023-11-17  | [shiny](https://shiny.rstudio.com/)                                   |
| shinycssloaders    | 1.0.0   | 2020-07-28  | [shinycssloaders](https://cran.r-project.org/package=shinycssloaders) |
| shinyjs            | 2.1.0   | 2021-12-23  | [shinyjs](https://deanattali.com/shinyjs/)                            |
| shinyWidgets       | 0.8.2   | 2024-03-01  | [shinyWidgets](https://dreamrs.github.io/shinyWidgets/)               |
| snakecase          | 0.11.1  | 2023-08-27  | [snakecase](https://cran.r-project.org/package=snakecase)             |
| stringr            | 1.5.1   | 2023-11-14  | [stringr](https://stringr.tidyverse.org/)                             |
| tibble             | 3.2.1   | 2023-03-20  | [tibble](https://tibble.tidyverse.org/)                               |
| tidyr              | 1.3.1   | 2024-01-24  | [tidyr](https://tidyr.tidyverse.org/)                                 |
| tidytext           | 0.4.2   | 2024-04-10  | [tidytext](https://www.tidytextmining.com/)                           |
| tidyverse          | 2.0.0   | 2023-02-22  | [tidyverse](https://www.tidyverse.org/)                               |

### Requerimientos del Servidor

La aplicación necesita un servidor capaz de instalar
[`shiny-server`](https://posit.co/products/open-source/shinyserver/) y
que tenga acceso a internet para conectarse a la base de datos
PostgreSQL alojada en
`smartdata.cwt3zjjzj7as.sa-east-1.rds.amazonaws.com`.

### Estructura de Archivos

La estructura de archivos de la aplicación sigue un formato estándar:

- **Raíz del Proyecto**: Contiene los archivos `global.R`, `ui.R` y
  `server.R`.
- **Carpeta `www/`**: Almacena archivos CSS y JavaScript.
- **Carpeta `R/`**: Almacena funciones helper y otros scripts
  desarrollados por el equipo. Estas funciones descargan datos,
  construyen $n$-gramas y generan reportes.
- **Carpeta `R/tabular/`**: Contiene scripts para la descarga y análisis
  de información de redes sociales. [Repositorio del equipo
  Tabular](https://github.com/tabular-lab).
- **Carpeta `data/`**: Almacena distintos archivos requeridos para el
  funcionamiento de la aplicación: `palabras-clave.xlsx`,
  `comunas_0001.gpkg`, `users.xlsx`.

## Forma de Uso

### Inicio de la Aplicación

Para iniciar la aplicación, los usuarios deben acceder a la URL
específica del servidor, por ejemplo,
`http://18.230.138.21:3838/visor-smartdata/`. Una vez ingresado, se
presentará un formulario de inicio de sesión donde se deben ingresar el
usuario y la contraseña.

### Roles de Usuario

La aplicación distingue entre dos roles de usuario:

- **Administrador**: Tiene la capacidad de crear nuevos usuarios y
  gestionar los existentes.
- **Usuario Normal**: Puede acceder y utilizar las funcionalidades de la
  aplicación sin permisos administrativos.

### Navegación y Secciones

Después de iniciar sesión, la aplicación presenta un menú principal en
el lado izquierdo con opciones para seleccionar fecha, categoría y
comunas. La aplicación se divide en tres secciones principales:

1.  **Resumen**: Proporciona información del día actual.
2.  **Medios**: Incluye análisis de información de medios de
    comunicación, con subsecciones para categorías, comunas y
    tendencias.
3.  **Redes Sociales**: Muestra datos y análisis específicos de redes
    sociales.

## Seguridad

La seguridad en **Visor-Smartdata** está garantizada mediante el uso de
[`shinymanager`](https://cran.r-project.org/web/packages/shinymanager/index.html),
que proporciona autenticación y administración de usuarios para proteger
el acceso a la aplicación.

## Despliegue

### Alojamiento

Actualmente, la aplicación está alojada en un servidor AWS, pero puede
desplegarse en cualquier servidor que cumpla con los requisitos
necesarios.

### Pasos para el Despliegue

1.  Instalar
    [`shiny-server`](https://posit.co/products/open-source/shinyserver/)
    en el servidor.
2.  Instalar *R* (versión \>= 4.1) desde
    [CRAN](https://cran.r-project.org/bin/windows/base/).
3.  Instalar las librerías requeridas para la aplicación.
4.  Asegurar acceso a internet para la conexión con la base de datos.
5.  Clonar el repositorio que contiene el código de la aplicación.
6.  Generar en la raíz del proyecto un archivo `.Renviron` que contenga
    tanto el `HOST` como `PASS` de acceso a la base de datos como se
    describe a continuación:

<!-- -->

    HOST="ip.del.host"
    PASS="clavedebbdd"

## Mantenimiento y Actualizaciones

### Gestión de Actualizaciones

Actualmente, no hay un plan definido para las actualizaciones de la
aplicación. Cualquier cambio o mejora se gestiona según sea necesario.

### Contacto para Mantenimiento

El equipo de mantenimiento puede ser contactado a través de Iván
Olivares, DATAUC.
