library(shiny)
library(leaflet)
library(leaflet.extras)  # Librería necesaria para hacer marcadores arrastrables
library(DBI)
library(pool)
library(RPostgres)
library(dplyr)

# Función para obtener ubicaciones desde la BD
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
  
  # Cargar ubicaciones de la BD
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
        options = markerOptions(draggable = TRUE)  # ¡Ahora sí funciona!
      )
  })
  
  # Detectar cuando se mueve un marcador
  observeEvent(input$mapa_boyas_marker_dragend, {
    info <- input$mapa_boyas_marker_dragend
    
    if (!is.null(info)) {
      id_boya <- as.integer(info$id)
      nueva_lat <- info$lat
      nueva_lng <- info$lng
      
      # Mostrar coordenadas en la consola
      output$coordenadas <- renderText({
        paste("Boya", id_boya, "movida a:", nueva_lat, nueva_lng)
      })
      
      # Actualizar en la base de datos
      query <- sprintf(
        "UPDATE ubicaciones SET latitud = %f, longitud = %f WHERE id = %d",
        nueva_lat, nueva_lng, id_boya
      )
      dbExecute(pool, query)
      
      # Actualizar datos reactivos
      boyas(get_ubicaciones())
      
      # Refrescar el mapa
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
