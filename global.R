library(shiny)
library(shinydashboard)
library(bslib)
library(leaflet)
library(vegan)
library(dplyr)
library(ggplot2)
library(DT)
library(RColorBrewer)

graf_a <- "#209b87"
graf_b <- "#5ecfa6"
graf_c <- "#4db686"
color <- "#80cf7f"

# https://www.datanovia.com/blog/r-color-palettes
pal_set3 <- brewer.pal(12, "Set3")
pal_set2 <- brewer.pal(8, "Set2")
pal_pastel1 <- brewer.pal(9, "Pastel1")
pal_pastel2 <- brewer.pal(8, "Pastel2")

source("R/curvas.R") # funcion curca de acumulación
source("R/analysis.R") 
source("R/plots.R")
source("R/helpers.R")
source("R/colors.R")
