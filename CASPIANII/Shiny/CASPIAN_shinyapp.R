rm(list=ls())

# Install required packages if not already installed
# install.packages(c("shiny", "leaflet", "sf", "DT"))

# Load required libraries
library(shiny)
library(leaflet)
library(sf)
library(DT)

# Load Germany municipal boundaries data (GADM level 3)
# Replace 'path_to_municipal_shapefile' with the actual path to your shapefile
germany_municipalities <- st_read(dsn=file.path("C:","Hanno","Bioinvasion","CASPIANII","CASPIANII","SDM","Data","Input","Shapefiles"),layer="gadm41_DEU_3")

# Assuming you have a dataframe named 'species_occurrences' with occurrence data
# This should include columns like 'Municipality', 'Species', 'Latitude', 'Longitude'

# Define UI
ui <- fluidPage(
  titlePanel("Species Occurrences in German Municipalities"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("municipality_select", label = "Select Municipality", choices = NULL, multiple = TRUE),
      downloadButton("download_data", "Download Selected Data")
    ),
    mainPanel(
      leafletOutput("map"),
      DTOutput("table")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Update municipality choices based on loaded data
  observe({
    choices <- unique(germany_municipalities$NAME_3)
    # updateSelectizeInput(session, "municipality_select", choices = choices)
  })
  
  # Create the leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = germany_municipalities, 
                  fillOpacity = 0.5,
                  color = "black",
                  weight = 1,
                  group = "Municipalities",
                  label = ~paste(NAME_3),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "red",
                    bringToFront = TRUE
                  )
      )
  })
  
  # Create the datatable
  output$table <- renderDT({
    selected_municipalities <- input$municipality_select
    if (!is.null(selected_municipalities)) {
      filtered_data <- subset(species_occurrences, Municipality %in% selected_municipalities)
      datatable(filtered_data)
    }
  })
  
  # Download selected data as CSV
  output$download_data <- downloadHandler(
    filename = function() {
      paste("selected_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      selected_municipalities <- input$municipality_select
      if (!is.null(selected_municipalities)) {
        filtered_data <- subset(species_occurrences, Municipality %in% selected_municipalities)
        write.csv(filtered_data, file)
      }
    }
  )
}

# Run the application
shinyApp(ui, server)
