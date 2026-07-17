setwd("C:/Users/cvara/Documents/py/registros_app/")

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
# graf_dos <- c("#008080","#b9e576")
# paleta <- c( "#008080", "#80cf7f", "#99a4da", "#ff8749", "#89eae9", "#eee8a9", "#95f7b1", "#a65696", "#6470a3", "#00c9cd", "#95b1b0")
# paleta <- c(
#   "#8dd3c7", "#ffffb3","#bebada","#fb8072", "#80b1d3",
#   "#fdb462", "#b3de69", "#fccde5","#d9d9d9"
# )
paleta <- brewer.pal(12, "Set3")

map_colors <- function(categories, pal) {
  cats <- unique(categories)
  setNames(rep_len(pal, length(cats)), cats)
}

# Datos
# Mapas
# Gráficos taxonómicos
# Estructura comunitaria
# Curvas de acumulación de especies
# Analisis de similitud
# Descargar Darwin Core

ui <- navbarPage(
  title = "Visor de Registros",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  tabPanel("Datos",
           sidebarLayout(
             sidebarPanel(
               fileInput("archivo_csv", "Sube tu archivo CSV (registros)",
                         accept = c("text/csv",".csv"), buttonLabel = "Examinar…",
                         placeholder = "Ningún archivo seleccionado"),
               hr(),
               h4("Parámetros Analíticos"),
               numericInput("n_perm", "Permutaciones (acumulación", value=500, min=100, max=1000, step=1),
               numericInput("k_grupos", "Grupos (k) para dendogramas y SIMPER", value=5, min=2, max=10, step=1),
               # input$n_perm
               # input$k_grupos
               hr(),
               actionButton("run_analisis", "Ejecutar análisis", icon = icon("play"), class = "btn-primary w-100")
             ),
             mainPanel(
               uiOutput("resumen_carga"),
               br(),
               DTOutput("tabla_resultados")
             )
           )
  ),
  tabPanel("Gráficos taxonómicos",
           sidebarLayout(
             sidebarPanel(
               h5("Configuración"),
               radioButtons("tipo_grafico","Tipo de gráfico",
                            choices = c("Barras" = "barras","Torta" = "torta")),
               radioButtons("metricas","Métricas (para Estaciones y Metodologías)",
                            choices = c("Abundancia" = "abundancia",
                                        "Riqueza" = "riqueza",
                                        "Abundancia y riqueza"= "ambas"),
                            selected = "abundancia"),
               hr(),
               h5("Filtros opcionales"),
               uiOutput("filtro_terreno_ui"),
               uiOutput("filtro_clase_ui"),
               hr(),
               actionButton("run_graficos","Generar gráficos",
                            icon = icon("chart-bar"), class = "btn-success w-100")
             ),
             mainPanel(
               # 1: por estación y por metodología
               fluidRow(
                 column(6, h5("Por estación"), plotOutput("graf_estacion",    height = "320px")),
                 column(6, h5("Por metodología"), plotOutput("graf_metodologia", height = "320px"))
               ),
               br(),
               # 2: por orden y por clase
               fluidRow(
                 column(6, h5("Composición por orden"), plotOutput("graf_orden",  height = "320px")),
                 column(6, h5("Composición por clase"), plotOutput("graf_clase",  height = "320px"))
               ),
             )
           )
  ),
  tabPanel("Mapas",
           sidebarLayout(
             sidebarPanel(
               h5("Opciones del mapa")
             ),
             mainPanel(
               leaflet::leafletOutput("mapa", height = "700px")
             )
           )
  ),
  tabPanel("Estructura comunitaria", h3("Sección en construcción")),
  tabPanel("Curvas de acumulación de especies", h3("Sección en construcción")),
  tabPanel("Análisis de similitud", h3("Sección en construcción")),
  tabPanel("Descargar Darwin Core", h3("Sección en construcción"))
)


server <- function(input, output, session) {
  
  # carga de datos 
  datos_reactivos <- reactive({
    req(input$archivo_csv)
    df <- read.csv(input$archivo_csv$datapath,
                   sep = ";", stringsAsFactors = FALSE,
                   encoding = "UTF-8", check.names = FALSE)
    # limpiar espacios en nombres de columna
    names(df) <- trimws(names(df))
    # nombres vacíos
    names(df)[names(df) == ""] <- paste0(
      "COL_",
      which(names(df) == "")
    )
    # forzar numérico en CANTIDAD
    df$CANTIDAD <- suppressWarnings(as.numeric(df$CANTIDAD))
    df$CANTIDAD[is.na(df$CANTIDAD)] <- 0
    # construir nombre científico
    df$ESPECIE <- paste(trimws(df$GENERO), trimws(df[["EPITETO ESPECIFICO"]]))
    # print(names(df))
    df
  })
  
  # resumen de carga 
  output$resumen_carga <- renderUI({
    req(datos_reactivos())
    df <- datos_reactivos()
    tags$div(class = "alert alert-info",
             tags$b("Archivo cargado:"),
             sprintf(" %d registros · %d columnas · %d especies únicas · abundancia total: %d",
                     nrow(df),
                     ncol(df),
                     length(unique(df$ESPECIE)),
                     sum(df$CANTIDAD, na.rm = TRUE))
    )
  })
  
  # tabla de datos 
  output$tabla_resultados <- renderDT({
    datos_reactivos()
  }, options = list(scrollX = TRUE, scrollY = "400px", pageLength = 15),
  rownames = FALSE)
  
  # filtros dinámicos
  output$filtro_terreno_ui <- renderUI({
    req(datos_reactivos())
    terrenos <- sort(unique(datos_reactivos()$TERRENO))
    selectInput("filtro_terreno","Terreno (todos)", choices = c("Todos", terrenos), selected = "Todos")
  })
  
  output$filtro_clase_ui <- renderUI({
    req(datos_reactivos())
    clases <- sort(unique(datos_reactivos()$CLASE))
    selectInput("filtro_clase","Clase taxonómica (todas)", choices = c("Todas", clases), selected = "Todas")
  })
  
  # datos filtrados para gráficos 
  datos_graficos <- eventReactive(input$run_graficos, {
    df <- datos_reactivos()
    if (!is.null(input$filtro_terreno) && input$filtro_terreno != "Todos")
      df <- df[df$TERRENO == input$filtro_terreno, ]
    if (!is.null(input$filtro_clase) && input$filtro_clase != "Todas")
      df <- df[df$CLASE == input$filtro_clase, ]
    df
  })
  
  # helper para construir resumen por variable 
  resumen_por <- function(df, var, metrica) {
    var_sym <- as.name(var)
    df <- df[!is.na(df[[var]]) & df[[var]] != "", ]
    
    if (metrica == "abundancia") {
      res <- df |> group_by(across(all_of(var))) |>
        summarise(valor = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") |>
        rename(categoria = 1)
    } else if (metrica == "riqueza") {
      res <- df |> group_by(across(all_of(var))) |>
        summarise(valor = n_distinct(ESPECIE), .groups = "drop") |>
        rename(categoria = 1)
    } else { # ambas → abundancia (eje primario para ggplot simple)
      res <- df |> group_by(across(all_of(var))) |>
        summarise(abundancia = sum(CANTIDAD, na.rm = TRUE),
                  riqueza    = n_distinct(ESPECIE), .groups = "drop") |>
        rename(categoria = 1)
    }
    res
  }
  
  # para graficar
  hacer_grafico <- function(df, var, titulo, tipo, metrica, paleta) {
    res <- resumen_por(df, var, metrica)
    
    if (metrica == "ambas") {
      res_long <- tidyr::pivot_longer(
        res,
        cols = c(abundancia, riqueza),
        names_to = "metrica",
        values_to = "valor"
      )
      p <- ggplot(
        res_long,
        aes(x = categoria, y = valor, fill = metrica)
      ) +
        geom_col(
          position = position_dodge(width = 0.8),
          width = 0.7
        ) +
        scale_fill_manual(
          values = c(abundancia = "#008080", riqueza = "#b9e576"),
          labels = c(abundancia = "Abundancia", riqueza = "Riqueza")
        ) +
        labs(
          title = titulo,
          x = NULL,
          y = "Valor",
          fill = NULL
        ) +
        theme_minimal(base_size = 11) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          plot.title = element_text( face = "bold", size = 11)
        )
      return(p)
    }
    
    # etiqueta del eje y
    y_lab <- if (metrica == "abundancia") "Nº individuos" else "Nº especies"
    
    if (tipo == "barras") {
      res <- res |> arrange(desc(valor))
      
      p <- ggplot(res, aes(x = categoria, y = valor)) +
        geom_col(fill = "#80cf7f", show.legend = FALSE, width = 0.7, alpha = 0.9) +
        geom_text(aes(label = valor), vjust = -0.5, size = 3.5) +
        labs(title = titulo, x = NULL, y = y_lab) +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(face = "bold", size = 11))
    } else { # torta
      res$pct   <- round(res$valor / sum(res$valor) * 100, 1)
      res$label <- paste0(res$categoria, "\n", res$pct, "%")
      p <- ggplot(res, aes(x = "", y = valor, fill = categoria)) +
        geom_col(width = 1, color = "white", linewidth = 0.4) +
        coord_polar("y") +
        scale_fill_manual(values = map_colors(res$categoria, paleta)) +
        labs(title = titulo, fill = NULL) +
        theme_void(base_size = 11) +
        theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
              legend.position = "right")
    }
    p
  }

  output$graf_estacion <- renderPlot({
    req(datos_graficos())
    hacer_grafico(datos_graficos(), "ESTACION",
                  "",
                  input$tipo_grafico, input$metricas, paleta)
  })
  
  output$graf_metodologia <- renderPlot({
    req(datos_graficos())
    hacer_grafico(datos_graficos(), "NOMBRE METODOLOGIA",
                  "",
                  input$tipo_grafico, input$metricas, paleta)
  })
  
  output$graf_orden <- renderPlot({
    req(datos_graficos())
    hacer_grafico(datos_graficos(), "ORDEN",
                  "",
                  input$tipo_grafico, input$metricas, paleta)
  })
  
  output$graf_clase <- renderPlot({
    req(datos_graficos())
    hacer_grafico(datos_graficos(), "CLASE",
                  "",
                  input$tipo_grafico, input$metricas, paleta)
  })
}

shinyApp(ui, server)
# options(shiny.autoreload = TRUE)