
# Documentación de la Aplicación **Visor-Smartdata**

## Introducción
**Visor-Smartdata** es una aplicación desarrollada en Shiny, diseñada para ofrecer una visualización interactiva y detallada de la información extraída de medios de comunicación escritos en internet y redes sociales. Su principal objetivo es proporcionar un panorama noticioso actualizado de la realidad de Chile y el Gobierno de Santiago (GS).

## Objetivo de la Aplicación
El propósito principal de **Visor-Smartdata** es permitir a los usuarios visualizar y analizar datos provenientes de distintos medios de comunicación y redes sociales, brindando un entendimiento profundo tanto del comportamiento general como específico de la información. Esta herramienta está destinada a facilitar el seguimiento de la actualidad noticiosa y las tendencias informativas que afectan al país y a su gobierno.

## Características Principales

### Funcionalidades
La aplicación cuenta con varias funcionalidades diseñadas para ofrecer una experiencia de usuario completa:
- **Visualización de Contenido**: Permite ver el contenido extraído y analizado de varios medios de comunicación y redes sociales.
- **Interactividad**: Los usuarios pueden interactuar con gráficos, tablas y mapas para explorar los datos de manera dinámica y personalizada.
- **Análisis Específico y General**: Los datos pueden ser visualizados tanto de manera global como segmentada por categorías, comunas y tendencias.

### Tipos de Datos
Los datos manejados por la aplicación incluyen:
- Información extraída de medios de comunicación escritos como emol.cl y elmostrador.cl.
- Datos de redes sociales, específicamente de Instagram y Facebook, que han sido previamente descargados y analizados.

### Visualizaciones
Las visualizaciones proporcionadas por la aplicación son:
- **Gráficos de Barra**: Para representar distribuciones y comparaciones de datos.
- **Mapas**: Para visualizar información geoespacial.
- **Tablas**: Para mostrar datos detallados de manera estructurada.

## Detalles Técnicos

### Principales Librerías de R Utilizadas
La aplicación utiliza un conjunto robusto de librerías de R, entre las cuales se incluyen:
- [`shiny`](https://cran.r-project.org/web/packages/shiny/index.html): Para la creación de aplicaciones web interactivas.
- [`bslib`](https://cran.r-project.org/web/packages/bslib/index.html): Para temas y estilos.
- [`tidytext`](https://cran.r-project.org/web/packages/tidytext/index.html): Para el procesamiento de texto.
- [`DBI`](https://cran.r-project.org/web/packages/DBI/index.html): Para la interacción con bases de datos.
- [`highcharter`](https://cran.r-project.org/web/packages/highcharter/index.html): Para gráficos interactivos.
- [`shinyWidgets`](https://cran.r-project.org/web/packages/shinyWidgets/index.html): Para componentes interactivos adicionales.
- [`DT`](https://cran.r-project.org/web/packages/DT/index.html): Para tablas de datos interactivas.
- [`dbplyr`](https://cran.r-project.org/web/packages/dbplyr/index.html): Para la manipulación de bases de datos con dplyr.

### Requerimientos del Servidor
La aplicación necesita un servidor capaz de instalar [`shiny-server`](https://posit.co/products/open-source/shinyserver/) y que tenga acceso a internet para conectarse a la base de datos PostgreSQL alojada en `smartdata.cwt3zjjzj7as.sa-east-1.rds.amazonaws.com`.

### Estructura de Archivos
La estructura de archivos de la aplicación sigue un formato estándar:
- **Raíz del Proyecto**: Contiene los archivos `global.R`, `ui.R` y `server.R`.
- **Carpeta `www`**: Almacena archivos CSS y JavaScript.
- **Carpeta `R`**: Almacena funciones helper y otros scripts desarrollados por el equipo.
- **Carpeta `tabluar`**: Contiene scripts para la descarga y análisis de información de redes sociales. [Repositorio del equipo Tabular](https://github.com/tabular-lab).

## Forma de Uso

### Inicio de la Aplicación
Para iniciar la aplicación, los usuarios deben acceder a la URL específica del servidor, por ejemplo, `http://18.230.138.21:3838/visor-smartdata/`. Una vez ingresado, se presentará un formulario de inicio de sesión donde se deben ingresar el usuario y la contraseña.

### Roles de Usuario
La aplicación distingue entre dos roles de usuario:
- **Administrador**: Tiene la capacidad de crear nuevos usuarios y gestionar los existentes.
- **Usuario Normal**: Puede acceder y utilizar las funcionalidades de la aplicación sin permisos administrativos.

### Navegación y Secciones
Después de iniciar sesión, la aplicación presenta un menú principal en el lado izquierdo con opciones para seleccionar fecha, categoría y comunas. La aplicación se divide en tres secciones principales:
1. **Resumen**: Proporciona información del día actual.
2. **Medios**: Incluye análisis de información de medios de comunicación, con subsecciones para categorías, comunas y tendencias.
3. **Redes Sociales**: Muestra datos y análisis específicos de redes sociales.

## Seguridad
La seguridad en **Visor-Smartdata** está garantizada mediante el uso de [`shinymanager`](https://cran.r-project.org/web/packages/shinymanager/index.html), que proporciona autenticación y administración de usuarios para proteger el acceso a la aplicación.

## Despliegue

### Alojamiento
Actualmente, la aplicación está alojada en un servidor AWS, pero puede desplegarse en cualquier servidor que cumpla con los requisitos necesarios.

### Pasos para el Despliegue
1. **Instalar [`shiny-server`](https://posit.co/products/open-source/shinyserver/)** en el servidor.
2. **Instalar R** (versión >= 4.1) desde [CRAN](https://cran.r-project.org/bin/windows/base/).
3. **Instalar las librerías requeridas** para la aplicación.
4. **Asegurar acceso a internet** para la conexión con la base de datos.
5. **Clonar el repositorio** que contiene el código de la aplicación.

## Mantenimiento y Actualizaciones

### Gestión de Actualizaciones
Actualmente, no hay un plan definido para las actualizaciones de la aplicación. Cualquier cambio o mejora se gestiona según sea necesario.

### Contacto para Mantenimiento
El equipo de mantenimiento puede ser contactado a través de Iván Olivares.
