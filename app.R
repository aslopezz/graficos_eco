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
               # numericInput("n_perm", "Permutaciones (acumulación", value=500, min=100, max=1000, step=1),
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
               uiOutput("filtro_clase_ui"),
               uiOutput("filtro_protocolo_ui"),
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
             ),
           )
    ),
    tabPanel(
    "Diversidad y abundancia",
    sidebarLayout(
      sidebarPanel(
        h5("Configuración"),
        selectInput(
          "nivel_diversidad",
          "Calcular diversidad por:",
          choices = c(
            "Todas las muestras" = "total",
            "Estación" = "ESTACION",
            "Metodología" = "NOMBRE METODOLOGIA"
          )
        ),
        actionButton(
          "run_diversidad",
          "Calcular índices",
          icon = icon("play"),
          class = "btn-primary w-100"
        )
      ),
      
      mainPanel(
        h4("Índices de diversidad"),
        DTOutput("tabla_indices"),
        
        br(),
        
        h4("Abundancia por especie"),
        DTOutput("tabla_abundancia")
      )
    )
  ),
  # tabPanel("Mapas",
  #          sidebarLayout(
  #            sidebarPanel(
  #              h5("Opciones del mapa")
  #            ),
  #            mainPanel(
  #              leaflet::leafletOutput("mapa", height = "700px")
  #            )
  #          )
  # ),
  tabPanel("Tránsito Aéreo",
           sidebarLayout(
             sidebarPanel(
               h5("Análisis de Vuelo"),
               hr(),
               uiOutput("estacion_ui"),
               actionButton(
                 "run_radar",
                 "Genera gráficos de radar",
                 icon = icon("play"),
                 class = "btn-success w-100"
               ),
               downloadButton("download_all_radars", "Descargar Gráficos")
             ),
             mainPanel(
               plotOutput("graf_radar", height = "500px", width = "500px"),
               br(),
             )
           )),
  tabPanel("Curva de acumulación de especies",
    sidebarLayout(
      sidebarPanel(
        h5("Configuración"),
        numericInput(
          "n_perm_acum",
          "Número de permutaciones", value = 500, min = 100, max = 5000, step = 100
        ),
        uiOutput("estacion_acum_ui"),
        actionButton(
          "run_acumulacion",
          "Generar curva",
          icon = icon("play"),
          class = "btn-primary w-100"
        )
      ),
      mainPanel(
        plotOutput("curva_acumulacion", height = "500px")
      )
    )
  )
  # tabPanel("Estructura comunitaria", h3("Sección en construcción")),
  # tabPanel("Análisis de similitud", h3("Sección en construcción")),
  # tabPanel("Descargar Darwin Core", h3("Sección en construcción"))
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
    df$FECHA <- as.Date(df$FECHA, format = "%d-%m-%Y")
    df$MUESTRA <- paste(
      df$FECHA,
      df$ESTACION,
      df$`NOMBRE METODOLOGIA`,
      sep = "_"
    )
    df
  })

  # indices
  calcular_indices <- function(df){
    abundancias <- df %>%
      group_by(ESPECIE) %>%
      summarise(
        abundancia = sum(CANTIDAD, na.rm = TRUE),
        .groups = "drop"
      )
    abundancias <- abundancias %>%
      filter(abundancia > 0)
    S <- nrow(abundancias)
    total <- sum(abundancias$abundancia)
    abundancias <- abundancias %>%
      mutate(
        abundancia_relativa = abundancia / total * 100
      )
    pi <- abundancias$abundancia_relativa / 100
    # Shannon
    H <- -sum(pi * log(pi))
    Simpson <- 1 - sum(pi^2)
    Pielou <- H / log(S)
    indices <- data.frame(
      Riqueza = S,
      Abundancia_total = total,
      Shannon = round(H,4),
      Simpson = round(Simpson,4),
      Pielou = round(Pielou,4)
    )
    list(
      indices = indices,
      abundancia = abundancias %>%
        arrange(desc(abundancia))
    )
  }

  resultado_diversidad <- eventReactive(
    input$run_diversidad,
    {
      df <- datos_reactivos()
      req(df)
      if(input$nivel_diversidad != "total"){
        df <- df %>%
          group_by(
            across(all_of(input$nivel_diversidad))
          ) %>%
          group_split()
      } else {
        df <- list(df)
      }
      resultado <- lapply(
        df,
        calcular_indices
      )
      resultado
    }
  )
  
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
  
  output$filtro_protocolo_ui <- renderUI({
    req(datos_reactivos())
    protocolos <- sort(unique(datos_reactivos()[["PROTOCOLO MUESTREO"]]))
    selectInput("filtro_protocolo", "Protocolo de Muestreo", choices = c("Todos", protocolos), selected = "Todos")
  })

  output$filtro_clase_ui <- renderUI({
    req(datos_reactivos())
    clases <- sort(unique(datos_reactivos()$CLASE))
    selectInput("filtro_clase","Clase taxonómica (todas)", choices = c("Todas", clases), selected = "Todas")
  })
  
  # datos filtrados para gráficos 
  datos_graficos <- eventReactive(input$run_graficos, {
    df <- datos_reactivos()
    if (!is.null(input$filtro_clase) && input$filtro_clase != "Todas")
      df <- df[df$CLASE == input$filtro_clase, ]
    if (!is.null(input$filtro_protocolo) && input$filtro_protocolo != "Todos")
      df <- df[df[["PROTOCOLO MUESTREO"]] == input$filtro_protocolo, ]
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
  
  output$tabla_indices <- renderDT({
    req(resultado_diversidad())
    datos <- resultado_diversidad()
    tabla <- bind_rows(
      lapply(datos, function(x){
        x$indices
      })
    )
    datatable(
      tabla,
      options=list(
        pageLength=10,
        scrollX=TRUE
      )
    )
  })

  output$tabla_abundancia <- renderDT({
    req(resultado_diversidad())
    datos <- resultado_diversidad()
    tabla <- bind_rows(
      lapply(datos, function(x){
        x$abundancia
      })
    )
    datatable(
      tabla,
      options=list(
        pageLength=20,
        scrollX=TRUE
      ),
      rownames=FALSE
    )
  })
  
  # para curva de acumulación de especies
  datos_acumulacion <- eventReactive(input$run_acumulacion, {
    df <- datos_reactivos()  
    df <- df %>%
      filter(
        !is.na(FECHA),
        ESPECIE != ""
      ) %>%
      arrange(FECHA)    
    dias <- sort(unique(df$FECHA))    
    especies <- character()
    riqueza <- numeric(length(dias))    
    for(i in seq_along(dias)){      
      especies <- union(
        especies,
        df$ESPECIE[df$FECHA == dias[i]]
      )      
      riqueza[i] <- length(especies)      
    }    
    data.frame(
      FECHA = dias,
      RIQUEZA = riqueza
    )    
  })
  
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

  # output curva de acumulación de especies
  output$curva_acumulacion <- renderPlot({
    curva <- datos_acumulacion()
    plot(
      curva$FECHA,
      curva$RIQUEZA,
      type = "b",
      pch = 16,
      lwd = 3,
      col = "#008080",
      xlab = "Fecha",
      ylab = "Especies acumuladas",
      main = "Curva de acumulación de especies"
    )
  })

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
  
  datos_aereo <- reactive({
    req(datos_reactivos())
    df <- datos_reactivos()
    df <- df %>%
      filter(
        toupper(trimws(CLASE)) == "AVES",
        !is.na(DIRECCIÓN),
        trimws(DIRECCIÓN) != "",
        toupper(trimws(DIRECCIÓN)) != "NINGUNO"
      )
    
    df$dir_destino <- toupper(trimws(df$DIRECCIÓN))
    
    df
  })
  
  output$estacion_ui <- renderUI({
    req(datos_aereo())
    estaciones <- sort(unique(datos_aereo()$ESTACION))
    selectInput(
      "estacion",
      "Estación",
      estaciones
    )
  })
  
  radar_data <- eventReactive(
    input$run_radar, {
      
      datos_aereo() %>%
        group_by(ESTACION, dir_destino) %>%
        summarise(
          abundancia = sum(CANTIDAD, na.rm = TRUE),
          .groups = "drop"
        )
    }
  )
  
  radar_maximo <- reactive({
    req(radar_data())
    
    max(radar_data()$abundancia, na.rm = TRUE)
  })
  
  # observeEvent(input$run_radar, {
  #   print(radar_data())
  # })
  #***********************
  # REVISAR 
  #***********************
  crear_radar <- reactive({
    
    req(input$estacion)
    
    df <- radar_data()
    
    # máximo global de todas las estaciones
    max_global <- max(df$abundancia, na.rm = TRUE)
    
    if(max_global == 0) max_global <- 1
    
    df <- df %>%
      filter(ESTACION == input$estacion)
    
    direcciones <- c(
      "N","NO","O","SO", "S", "SE","E","NE"
    )
    
    radar <- tibble(
      dir_destino = direcciones
    ) %>%
      left_join(df, by="dir_destino")
    
    radar$abundancia[is.na(radar$abundancia)] <- 0
    
    valores <- radar$abundancia
    
    radar_final <- rbind(
      rep(max_global,8),
      rep(0,8),
      valores
    )
    
    radar_final <- as.data.frame(radar_final)
    
    colnames(radar_final) <- direcciones
    
    radar_final
  })
  
  output$graf_radar <- renderPlot({
    req(input$run_radar)
    radar <- crear_radar()
    fmsb::radarchart(
      radar,
      axistype=1,
      pcol="#008080",
      pfcol=scales::alpha("#80cf7f",0.4),
      plwd=3,
      cglcol="grey70",
      axislabcol = "black",
      caxislabels=seq(
        0,
        max(radar[1,]),
        length.out=5
      )
    )
    title(input$estacion)
    
  })
  
  output$download_all_radars <- downloadHandler(
    filename=function(){
      paste0(
        input$estacion,
        "_radar.png"
      )
      
    },
    
    content=function(file){
      png(file,
          width=2200,
          height=2200,
          res=300)
      
      radar <- crear_radar()
      fmsb::radarchart(
        radar,
        axistype=1,
        pcol="#008080",
        axislabcol = "black",
        pfcol=scales::alpha("#80cf7f",0.4),
        plwd=3
      )
      title(input$estacion)
      dev.off()
    }
    
  )
}

shinyApp(ui, server)
# options(shiny.autoreload = TRUE)