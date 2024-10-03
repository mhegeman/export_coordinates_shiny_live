library(shiny)
library(leaflet)
library(leaflet.esri)
library(bslib)

ui <- page_sidebar(
  title = "Interactive Map Coordinate Finder",
  sidebar = sidebar(
    width = 600,
    h4("Map Controls"),
    checkboxInput("toggle_shellfish", "Show Shellfish Layer", value = FALSE),
    checkboxInput("toggle_pin_dropping", "Enable Pin Dropping", value = FALSE),
    hr(),
    h4("Pin Information"),
    tableOutput("click_info"),
    textInput("point_description", "Enter description for the last point:"),
    actionButton("add_description", "Add Description", class = "btn-primary mt-2"),
    actionButton("clear_points", "Clear All Points", class = "btn-danger mt-3"),
    downloadButton("download_data", "Download Data as CSV", class = "btn-success mt-3")
  ),
  leafletOutput("map")
)

server <- function(input, output, session) {
  points <- reactiveVal(data.frame(Number = integer(), Latitude = numeric(), Longitude = numeric(), Description = character()))

  # Create a reactive value to store the map state
  map_data <- reactiveVal(list(
    center = c(lat = 40.7128, lng = -74.0060),
    zoom = 12
  ))

  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = map_data()$center["lng"], lat = map_data()$center["lat"], zoom = map_data()$zoom)
  })

  # Observer to add or remove the shellfish layer
  observe({
    leafletProxy("map") %>%
      clearGroup("shellfish") # Clear existing layer

    if (input$toggle_shellfish) {
      leafletProxy("map") %>%
        addEsriFeatureLayer(
          url = "https://services6.arcgis.com/DZHaqZm9cxOD4CWM/arcgis/rest/services/Shellfish/FeatureServer/3",
          layerId = "shellfish",
          group = "shellfish"
        )
    }
  })

  # Update map_data when the view changes
  observeEvent(input$map_zoom, {
    current <- map_data()
    current$zoom <- input$map_zoom
    map_data(current)
  })

  observeEvent(input$map_center, {
    current <- map_data()
    current$center <- input$map_center
    map_data(current)
  })

  observeEvent(input$map_click, {
    if (input$toggle_pin_dropping) {
      click <- input$map_click
      current_points <- points()
      new_number <- nrow(current_points) + 1
      new_point <- data.frame(
        Number = new_number,
        Latitude = round(click$lat, 6),
        Longitude = round(click$lng, 6),
        Description = ""
      )
      points(rbind(current_points, new_point))

      leafletProxy("map") %>%
        addMarkers(lng = click$lng, lat = click$lat,
                   label = as.character(new_number),
                   labelOptions = labelOptions(noHide = TRUE, direction = "center",
                                               textOnly = TRUE, textsize = "14px",
                                               style = list("font-weight" = "bold",
                                                            color = "white",
                                                            "background-color" = "black",
                                                            "border-color" = "black",
                                                            "border-radius" = "10px",
                                                            padding = "5px 8px")))
    }
  })

  observeEvent(input$add_description, {
    current_points <- points()
    if (nrow(current_points) > 0) {
      current_points[nrow(current_points), "Description"] <- input$point_description
      points(current_points)
      updateTextInput(session, "point_description", value = "")
    }
  })

  output$click_info <- renderTable({
    req(nrow(points()) > 0)
    points()
  }, align = 'cccc', width = "100%", bordered = TRUE, digits = 6)

  observeEvent(input$clear_points, {
    points(data.frame(Number = integer(), Latitude = numeric(), Longitude = numeric(), Description = character()))
    leafletProxy("map") %>% clearMarkers()
  })

  output$download_data <- downloadHandler(
    filename = function() {
      paste("map_points_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(points(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
