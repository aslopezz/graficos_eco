library(shiny)
library(shinydashboard)
library(bslib)
library(leaflet)
library(vegan)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(ggdendro)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(DT)
library(RColorBrewer)
library(zip)
# ggreppel es para mejorar el pie hcart
# library(ggrepel)

graf_a <- "#209b87"
graf_b <- "#5ecfa6"
graf_c <- "#4db686"
color <- "#80cf7f"

# https://www.datanovia.com/blog/r-color-palettes
pal_set3 <- brewer.pal(12, "Set3")
pal_set2 <- brewer.pal(8, "Set2")
pal_pastel1 <- brewer.pal(9, "Pastel1")
pal_pastel2 <- brewer.pal(8, "Pastel2")

# como no dependen de `input`, conviene cargarlas una sola vez en
# global.R en vez de en cada sesión de server();
comunas_chile <- readRDS("./datos/comunas.rds")

regiones_chile <- readRDS("./datos/regiones.rds") %>%
  dplyr::filter(
    trimws(Region) != "Zona sin demarcar"
  )

source("R/01_cargar_datos.R")
source("R/02_utils.R")
source("R/03_indices_diversidad.R")
source("R/04_graficos.R")
source("R/05_curvas_acumulacion.R")
source("R/06_analisis_espacial.R") 
source("R/07_dendogramas.R") 
