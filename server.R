server <- function(input, output, session) {
  # carga de datos 
  datos_reactivos <- reactive({
    req(input$archivo_csv)
    df <- read.csv(input$archivo_csv$datapath,
                   sep = ";", stringsAsFactors = FALSE,
                   encoding = "UTF-8", check.names = FALSE)
    # datos.R
    procesar_datos_entrada(df)
  })

  # calcula los índices para todo el conjunto de datos
  resultado_indices <- eventReactive(
    input$run_diversidad,
    {
      df <- datos_reactivos()
      calcular_indices(df) # en diversidad.R
    }
  )

  resultado_tabla <- eventReactive(
    input$run_diversidad,
    {
      df <- datos_reactivos()
      # en diversidad.R
      tabla_riqueza_abundancia(
        df,
        input$nivel_riqueza
      )
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
  
  output$tabla_indices <- renderDT({
    req(resultado_indices())
    datatable(
      resultado_indices()$indices,
      options=list(
        pageLength=10,
        scrollX=TRUE
      )
    )
  })

  output$tabla_abundancia <- renderDT({
    req(resultado_tabla())
    datatable(
      resultado_tabla(),
      options=list(
        pageLength=20,
        scrollX=TRUE
      ),
      rownames=FALSE
    )
  })
  
  # para curva de acumulación de especies
  datos_acumulacion <- eventReactive(input$run_acumulacion, {
    req(datos_reactivos())
    
    withProgress(
      message = "Calculando curva...",
      value = 2,
      {
        df <- datos_reactivos()
        df$MUESTRA <- paste(
          df$FECHA,
          df$ESTACION,
          df$`NOMBRE METODOLOGIA`,
          sep = "_"
        )
        
        matriz <- xtabs(
          CANTIDAD ~ MUESTRA + ESPECIE,
          data = df
        )
        
        matriz <- as.matrix(matriz)
        
        curva <- calcular_curva_acumulacion(
          matriz = matriz,
          estimador = input$estimador,
          nperm = input$n_perm_acum
        )
        
        incProgress(1, detail="Finalizado")
        curva
      }
    )
  })

  # output curva de acumulación de especies
  output$curva_acumulacion <- renderPlot({
    req(datos_acumulacion())
    plot_curva_acumulacion(datos_acumulacion())
  })

  grafico_estacion <- reactive({
    req(datos_graficos())
    # plots.R
    hacer_grafico(datos_graficos(), "ESTACION",
                  "",
                  input$tipo_grafico, input$metricas, pal_set2)
  })

  output$graf_estacion <- output$graf_estacion <- renderPlot({
    grafico_estacion()
  })
  
  output$graf_metodologia <- renderPlot({
    req(datos_graficos())
    hacer_grafico(datos_graficos(), "NOMBRE METODOLOGIA",
                  "",
                  input$tipo_grafico, input$metricas, pal_pastel2)
  })
  
  output$graf_orden <- renderPlot({
    req(datos_graficos())
    hacer_grafico(datos_graficos(), "ORDEN",
                  "",
                  input$tipo_grafico, input$metricas, pal_pastel1)
  })
  
  output$graf_clase <- renderPlot({
    req(datos_graficos())
    hacer_grafico(datos_graficos(), "CLASE",
                  "",
                  input$tipo_grafico, input$metricas, pal_pastel1)
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
      cglcol="grey80",
      axislabcol = "black",
      caxislabels=seq(
        0,
        max(radar[1,]),
        length.out=5
      )
    )
    title(input$estacion)
    
  })
  
 output$download_estacion <- downloadHandler(
    filename = function(){
      "Grafico_Estacion.png"
    },
    content = function(file){
      ggsave(
        filename = file,
        plot = grafico_estacion(),
        device = "png",
        width = 8,
        height = 6,
        units = "in",
        dpi = 300,
        bg = "white"
      )
    }
  )
 
 output$download_curva <- downloadHandler(
   filename = function(){
     "curva_acumulacion.png"
   },
   
   content = function(file){
     
     ggsave(
       filename = file,
       plot = plot_curva_acumulacion(datos_acumulacion()),
       width = 8,
       height = 5,
       dpi = 300,
       bg = "white"
     )
   }
 )

comunas_chile <- sf::st_read(
  "./datos/comunas.gpkg",
  quiet = TRUE
)

regiones_chile <- rnaturalearth::ne_states(
  country = "Chile",
  returnclass = "sf"
)

observe({
  regiones <- comunas_chile %>%
    sf::st_drop_geometry() %>%
    dplyr::pull(Region) %>%
    as.character() %>%
    trimws() %>%
    unique() %>%
    sort()


  updateSelectInput(
    session,
    "region_mapa",
    choices = regiones,
    selected = regiones[1]
  )
})

observeEvent(input$region_mapa,{
    req(input$region_mapa)

    comunas <- comunas_chile %>%
      dplyr::filter(
        trimws(Region) ==
          trimws(input$region_mapa)
      ) %>%
      sf::st_drop_geometry() %>%
      dplyr::pull(Comuna) %>%
      as.character() %>%
      trimws() %>%
      unique() %>%
      sort()

    updateSelectInput(
      session,
      "comuna_mapa",

      choices = comunas,

      selected = if (
        length(comunas) > 0
      ) {
        comunas[1]
      } else {
        NULL
      }
    )
  }
)


datos_espaciales <- eventReactive(input$run_espacial,{
    req(datos_reactivos())
    req(input$huso_utm)
    
    withProgress(
      message = "Generando capa espacial...",
      value = 0,
      {
        incProgress(
          0.3,
          detail = "Procesando coordenadas..."
        )

        capa <- preparar_datos_espaciales(
          datos_reactivos(),
          huso = input$huso_utm
        )

        incProgress(
          1,
          detail = "Finalizado"
        )

        capa
      }
    )
  }
)


output$mapa_espacial <- renderPlot({
    req(datos_espaciales())
    req(input$region_mapa)
    req(input$comuna_mapa)

    capa <- datos_espaciales()

    region <- regiones_chile %>%
      dplyr::filter(
        name == input$region_mapa
      )

    region <- sf::st_transform(
      region,
      4326
    )

    comuna <- comunas_chile %>%
      dplyr::filter(
        trimws(Region) ==
          trimws(input$region_mapa),

        trimws(Comuna) ==
          trimws(input$comuna_mapa)
      )

    comuna <- sf::st_transform(
      comuna,
      4326
    )

    tiene_geometria <- !sf::st_is_empty(
      sf::st_geometry(capa)
    )

    puntos <- capa[
      tiene_geometria, ,drop = FALSE]

    if (nrow(puntos) > 0) {
      puntos <- sf::st_transform(
        puntos,
        4326
      )
    }

    p <- ggplot() +
      geom_sf(
        data = region,
        fill = "#F5F5F5",
        color = "#D62728",
        linewidth = 0.8
      ) +
      geom_sf(
        data = comuna,
        fill = "#FFF2CC",
        color = "#1565C0",
        linewidth = 1.2,
        alpha = 0.7
      )

    if (nrow(puntos) > 0) {
      p <- p +
        geom_sf(
          data = puntos,
          aes(
            color = ORIGEN_COORDENADA
          ),
          size = 3,
          alpha = 0.85
        )
    }


    if (nrow(comuna) > 0) {
      bbox <- sf::st_bbox(
        comuna
      )

      p <- p +
        coord_sf(
          xlim = c(
            bbox["xmin"],
            bbox["xmax"]
          ),
          ylim = c(
            bbox["ymin"],
            bbox["ymax"]
          ),
          expand = TRUE
        )

    } else {
      # Si por alguna razón no encuentra la comuna,
      # mostrar la región completa.
      p <- p +
        coord_sf(
          datum = sf::st_crs(4326),
          expand = TRUE
        )
    }

    p +
      ggspatial::annotation_scale(
        location = "bl",
        width_hint = 0.25
      ) +
      ggspatial::annotation_north_arrow(
        location = "tl",
        style =
          ggspatial::north_arrow_fancy_orienteering
      ) +
      labs(
        title = paste(
          input$comuna_mapa,
          "-",
          input$region_mapa
        ),
        subtitle = paste(
          "Puntos con coordenadas:",
          nrow(puntos)
        ),
        x = "Longitud",
        y = "Latitud",
        color = "Origen coordenada"
      ) +

      theme_classic() +

      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold",
          size = 16
        ),
        plot.subtitle = element_text(
          hjust = 0.5,
          size = 11
        ),
        legend.position = "bottom"
      )

  })

}