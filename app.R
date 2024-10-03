library(shiny)
library(leaflet)
library(leaflet.esri)
library(bslib)
library(tidyverse)
library(sf)

ui <- page_sidebar(
  title = "Interactive Map Coordinate Finder",
  sidebar = sidebar(
    width = 600,
    h4("Map Controls"),
    checkboxInput("toggle_shellfish", "Show Shellfish Layer", value = FALSE),
    textInput("custom_feature_url", "Enter Custom Feature Service URL:"),
    actionButton("add_custom_feature", "Add Custom Feature", class = "btn-primary mt-2"),
    checkboxInput("toggle_custom_feature", "Show Custom Feature Layer", value = FALSE),
    checkboxInput("toggle_pin_dropping", "Enable Pin Dropping", value = FALSE),
    hr(),
    h4("Pin Information"),
    tableOutput("click_info"),
    textInput("point_description", "Enter description for the last point:"),
    actionButton("add_description", "Add Description", class = "btn-primary mt-2"),
    actionButton("clear_points", "Clear All Points", class = "btn-danger mt-3"),
    downloadButton("download_data", "Download Data as CSV", class = "btn-success mt-3"),
    hr(),
    h4("Shapefile Creation"),
    textInput("shapefile_name", "Enter shapefile name:", value = "my_shapefile"),
    selectInput("shapefile_type", "Select shapefile type:",
                choices = c("Point" = "point", "Line" = "line", "Polygon" = "polygon"),
                selected = "point"),
    downloadButton("download_shapefile", "Download Shapefile", class = "btn-info mt-3")
  ),
  leafletOutput("map")
)

server <- function(input, output, session) {
  points <- reactiveVal(tibble(Number = integer(), Latitude = numeric(), Longitude = numeric(), Description = character()))
  custom_feature_url <- reactiveVal(NULL)

  map_data <- reactiveVal(list(
    center = c(lat = 40.7128, lng = -74.0060),
    zoom = 12
  ))

  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = map_data()$center["lng"], lat = map_data()$center["lat"], zoom = map_data()$zoom)
  })

  observe({
    leafletProxy("map") %>%
      clearGroup("shellfish")

    if (input$toggle_shellfish) {
      leafletProxy("map") %>%
        addEsriFeatureLayer(
          url = "https://services6.arcgis.com/DZHaqZm9cxOD4CWM/arcgis/rest/services/Shellfish/FeatureServer/3",
          layerId = "shellfish",
          group = "shellfish"
        )
    }
  })

  observeEvent(input$add_custom_feature, {
    custom_feature_url(input$custom_feature_url)
    updateCheckboxInput(session, "toggle_custom_feature", value = TRUE)
  })

  observe({
    leafletProxy("map") %>%
      clearGroup("custom_feature")

    if (input$toggle_custom_feature && !is.null(custom_feature_url())) {
      leafletProxy("map") %>%
        addEsriFeatureLayer(
          url = custom_feature_url(),
          layerId = "custom_feature",
          group = "custom_feature"
        )
    }
  })

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
      new_point <- tibble(
        Number = new_number,
        Latitude = round(click$lat, 6),
        Longitude = round(click$lng, 6),
        Description = ""
      )
      points(bind_rows(current_points, new_point))

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
    points(tibble(Number = integer(), Latitude = numeric(), Longitude = numeric(), Description = character()))
    leafletProxy("map") %>% clearMarkers()
  })

  output$download_data <- downloadHandler(
    filename = function() {
      paste("map_points_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(points(), file)
    }
  )

  output$download_shapefile <- downloadHandler(
    filename = function() {
      paste0(input$shapefile_name, ".zip")
    },
    content = function(file) {
      req(nrow(points()) > 0)

      tmp_dir <- tempdir()

      # Convert points to sf object
      sf_points <- st_as_sf(points(), coords = c("Longitude", "Latitude"), crs = 4326)

      # Create geometry based on selected type
      sf_geometry <- switch(input$shapefile_type,
                            "point" = sf_points,
                            "line" = st_cast(st_combine(sf_points), "LINESTRING"),
                            "polygon" = st_cast(st_combine(sf_points), "POLYGON")
      )

      # If line or polygon, we need to put it into a data frame
      if (input$shapefile_type != "point") {
        sf_geometry <- st_sf(geometry = sf_geometry)
      }

      # Save as shapefile
      shapefile_name <- input$shapefile_name
      st_write(sf_geometry, dsn = tmp_dir, layer = shapefile_name, driver = "ESRI Shapefile", quiet = TRUE)

      # List all files related to this shapefile
      shapefile_files <- list.files(tmp_dir, pattern = paste0("^", shapefile_name, "\\.(shp|shx|dbf|prj)$"), full.names = TRUE)

      # Create the zip file, changing the working directory temporarily
      current_wd <- getwd()
      setwd(tmp_dir)
      zip(file, files = basename(shapefile_files))
      setwd(current_wd)

      # Clean up: remove the shapefile components from the temp directory
      file.remove(shapefile_files)
    },
    contentType = "application/zip"
  )

  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)
