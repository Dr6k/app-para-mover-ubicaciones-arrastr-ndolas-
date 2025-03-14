library(shiny)
library(leaflet)
library(leaflet.extras)  
library(DBI)
library(pool)
library(RPostgres)
library(dplyr)

get_ubicaciones <- function() {
  dbGetQuery(pool, "SELECT id, nombre, longitud, latitud FROM ubicaciones")
}

# UI
ui <- fluidPage(
  titlePanel("Mover Boyas - Arrastrar y Guardar"),
  
  leafletOutput("mapa_boyas", height = "600px"),
  verbatimTextOutput("coordenadas")
)

# Server
server <- function(input, output, session) {
  
  boyas <- reactiveVal(get_ubicaciones())
  
  output$mapa_boyas <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addAwesomeMarkers(
        data = boyas(),
        lng = ~longitud, lat = ~latitud,
        layerId = ~id,
        label = ~nombre,
        icon = makeAwesomeIcon(icon = "map-marker", markerColor = "blue"),
        options = markerOptions(draggable = TRUE) 
      )
  })
  
  observeEvent(input$mapa_boyas_marker_dragend, {
    info <- input$mapa_boyas_marker_dragend
    
    if (!is.null(info)) {
      id_boya <- as.integer(info$id)
      nueva_lat <- info$lat
      nueva_lng <- info$lng
      
      output$coordenadas <- renderText({
        paste("Boya", id_boya, "movida a:", nueva_lat, nueva_lng)
      })
      
      query <- sprintf(
        "UPDATE ubicaciones SET latitud = %f, longitud = %f WHERE id = %d",
        nueva_lat, nueva_lng, id_boya
      )
      dbExecute(pool, query)
      
      boyas(get_ubicaciones())
      
      leafletProxy("mapa_boyas") %>%
        clearMarkers() %>%
        addAwesomeMarkers(
          data = boyas(),
          lng = ~longitud, lat = ~latitud,
          layerId = ~id,
          label = ~nombre,
          icon = makeAwesomeIcon(icon = "map-marker", markerColor = "blue"),
          options = markerOptions(draggable = TRUE)
        )
    }
  })
}

shinyApp(ui, server)
