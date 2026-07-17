install.packages(c(
  "shiny",
  "leaflet",
  "dplyr",
  "tidyr"
))

library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)

ui <- fluidPage(
  
  titlePanel("Biodiversity Maps"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput(
        "csv_file",
        "Upload CSV",
        accept = ".csv"
      )
      
    ),
    
    mainPanel(
      
      leafletOutput(
        "map",
        height = 700
      )
      
    )
    
  )
  
)

server <- function(input, output, session){
  
  records <- reactive({
    
    req(input$csv_file)
    
    read.csv(
      input$csv_file$datapath,
      stringsAsFactors = FALSE
    )
    
  })
  
  output$map <- renderLeaflet({
    
    req(records())
    
    df <- records()
    
    leaflet(df) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addCircleMarkers(
        lng = ~decimalLongitude,
        lat = ~decimalLatitude,
        radius = 5,
        stroke = FALSE,
        fillOpacity = 0.8
      )
    
  })
  
}

records <- reactive({
  
  req(input$csv_file)
  
  df <- read.csv(
    input$csv_file$datapath,
    stringsAsFactors = FALSE
  )
  
  df <- df %>%
    tidyr::separate(
      `COORDENADAS METODOLOGIA`,
      into = c("decimalLatitude", "decimalLongitude"),
      sep = ","
    ) %>%
    mutate(
      decimalLatitude = as.numeric(decimalLatitude),
      decimalLongitude = as.numeric(decimalLongitude)
    )
  
  df
  
})

addMarkers(
  lng = ~decimalLongitude,
  lat = ~decimalLatitude,
  popup = ~scientificName,
  clusterOptions = markerClusterOptions()
)

shinyApp(ui, server)