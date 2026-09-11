ui <- navbarPage(
  title = "Visor de Registros",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  tabPanel("Datos", sidebarLayout(
    sidebarPanel(
      fileInput(
        "archivo_csv",
        "Sube tu archivo CSV (registros)",
        accept = c("text/csv", ".csv"),
        buttonLabel = "Examinar…",
        placeholder = "Ningún archivo seleccionado"
      ),
    ),
    mainPanel(
      uiOutput("resumen_carga"),
      br(),
      DTOutput("tabla_resultados")
    )
  )),
  tabPanel( "Gráficos taxonómicos",
    sidebarLayout(
      sidebarPanel(
        h5("Configuración"),
        radioButtons(
          "tipo_grafico",
          "Tipo de gráfico",
          choices = c("Barras" = "barras", "Torta" = "torta")
        ),
        radioButtons(
          "metricas",
          "Métricas (para Estaciones y Metodologías)",
          choices = c(
            "Abundancia" = "abundancia",
            "Riqueza" = "riqueza",
            "Abundancia y riqueza" = "ambas"
          ),
          selected = "abundancia"
        ),
        hr(),
        h5("Filtros opcionales"),
        uiOutput("filtro_clase_ui"),
        uiOutput("filtro_protocolo_ui"),
        hr(),
        actionButton(
          "run_graficos",
          "Generar gráficos",
          icon = icon("chart-bar"),
          class = "btn-primary w-100"
        ),
        # downloadButton("download_taxonomicos", "Descargar gráficos", class = "btn-primary w-100")
        downloadButton("download_estacion", "Descargar Estación", class = "btn-success w-100 mt-3")
      ),
      mainPanel(
        # por estación y por metodología
        fluidRow(column(
          6,
          h5("Por estación"),
          plotOutput("graf_estacion", height = "320px")
        ), column(
          6,
          h5("Por metodología"),
          plotOutput("graf_metodologia", height = "320px")
        )),
        br(),
        # por orden y por clase
        fluidRow(column(
          6,
          h5("Composición por orden"),
          plotOutput("graf_orden", height = "320px")
        ), column(
          6,
          h5("Composición por clase"),
          plotOutput("graf_clase", height = "320px")
        )),
      ),
    )
  ),
  tabPanel("Diversidad y abundancia",
    sidebarLayout(
      sidebarPanel(
        h5("Configuración"),
        selectInput(
          "nivel_riqueza",
          "Agrupar por:",
          choices = c(
            "Clase" = "CLASE",
            "Orden" = "ORDEN",
            "Familia" = "FAMILIA",
            "Especie" = "ESPECIE"
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
  tabPanel("Tránsito Aéreo", sidebarLayout(
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
      #downloadButton("download_all_radars", "Descargar Gráficos")
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
          "Número de permutaciones",
          value = 500,
          min = 100,
          max = 1000,
          step = 100
        ),
        uiOutput("estacion_acum_ui"),
        selectInput(
          "estimador",
          "Estimador",
          choices = c("Chao 1", "Chao 2", "Jackknife 1", "Bootstrap", "ACE"),
          selected = "Chao 1"
        ),
        actionButton(
          "run_acumulacion",
          "Generar curva",
          icon = icon("play"),
          class = "btn-primary w-100"
        ),
        downloadButton("download_curva", "Descargar curva", class = "btn-success w-100 mt-3")
      ),
      mainPanel(plotOutput("curva_acumulacion", height = "500px"))
    )
  ),
  tabPanel("Datos espaciales", sidebarLayout(
    sidebarPanel(
      h4("Configuración espacial"),
      
      radioButtons(
        "huso_utm",
        "Huso UTM",
        choices = c(
          "18S — WGS84 / UTM 18S (EPSG:32718)" = "18S",
          "19S — WGS84 / UTM 19S (EPSG:32719)" = "19S"
        ),
        selected = "19S"
      ),
      
      selectInput(
        inputId = "region_mapa",
        label = "Región:",
        choices = NULL
      ),
      
      selectInput(
        inputId = "comuna_mapa",
        label = "Comuna:",
        choices = NULL
      ),
      
      hr(),
      
      actionButton(
        "run_espacial",
        "Visualizar puntos en mapa",
        icon = icon("map"),
        class = "btn-primary w-100 mb-3"
      ),

      h5("Exportar imagenes de área"),
      downloadButton("download_mapa_chile", "Chile", class = "btn-success w-100 mb-2"),
      downloadButton("download_mapa_region", "Región", class = "btn-success w-100 mb-2"),

      h5("Datos para el shp"),
      dateInput(
        inputId = "fecha_ini_campana",
        label = "Fecha inicio de campaña:",
        value = Sys.Date(),
        format = "dd-mm-yyyy",
        language = "es"
      ),

      dateInput(
        inputId = "fecha_ter_campana",
        label = "Fecha término de campaña:",
        value = Sys.Date(),
        format = "dd-mm-yyyy",
        language = "es"
      ),
      
      downloadButton("download_shp", "Descargar SHP", class = "btn-success w-100")
    ),
    
    mainPanel(
      h4("Vista previa espacial"),
      plotOutput("mapa_espacial", height = "600px")
    )
  )),
  tabPanel("Similitud entre especies",
    sidebarLayout(
      sidebarPanel(
        h5("Configuración del dendograma"),
        uiOutput("filtro_clase_especies_ui"),
        radioButtons(
          "metodo_dist_especies",
          "Método de distancia:",
          choices = c("Bray-Curtis (abundancia)" = "bray", "Jaccard (presencia/ausencia)" = "jaccard"),
          selected = "bray"
        ),
        selectInput(
          "metodo_clust_especies",
          "Método de aglomeración:",
          choices = c("Average" = "average", "Ward" = "ward.D2",
                      "Complete" = "complete", "Single" = "single")
        ),
        hr(),
        h4("Parámetros Analíticos"),
        # numericInput("n_perm", "Permutaciones (acumulación", value=500, min=100, max=1000, step=1),
        numericInput(
          "k_grupos",
          "Grupos (k) para dendogramas", # y SIMPER ???
          value = 5,
          min = 2,
          max = 10,
          step = 1
        ),
        # input$n_perm
        # input$k_grupos
        hr(),
        actionButton(
          "run_dendo_especies",
          "Generar dendograma",
          icon = icon("play"),
          class = "btn-primary w-100"
        ),
        downloadButton("download_dendo_especies", "Descargar dendograma", class = "btn-success w-100 mt-3")
      ),
      mainPanel(
        plotOutput("graf_dendo_especies", height = "600px")
      )
    )
  )
  # tabPanel("Análisis de similitud", h3("Sección en construcción")),
  # tabPanel("Descargar Darwin Core", h3("Sección en construcción"))
)