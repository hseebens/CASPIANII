rm(list=ls())

library(shiny)
library(sf)
library(leaflet)
library(ggplot2)

germany_municipalities <- st_read(dsn=file.path("C:","Hanno","Bioinvasion","CASPIANII","CASPIANII","SDM","Data","Input","Shapefiles"),layer="gadm41_DEU_3")


ui <- fluidPage(
  titlePanel("Gemeinden"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Gemeinde", "Gemeinde", choices=NULL,multiple = TRUE  ),
      downloadButton("download_data", "Download Liste Neobiota")
    ),
    mainPanel(
      # plotOutput("hist")
      leafletOutput("map"),
      # DTOutput("table")
    )
  ),
  # textInput("text")
  
)


server <- function(input, output, session) {

  # Update municipality choices based on loaded data
  observe({
    choices <- unique(germany_municipalities$NAME_3)
    updateSelectizeInput(session, "Gemeinde", choices = choices)
  })
  # municipatilities <- unique(germany_municipalities$NAME_3)
  # updateSelectizeInput(session, 'Gemeinde', choices = municipatilities, server = TRUE)

  # output$hist <- renderPlot({
  #   means <- replicate(1e4, mean(runif(input$m)))
  #   hist(means, breaks = 20)
  # }, res = 96)
  
  # Create the leaflet map
  output$map <- renderLeaflet({
    selected_municipality <- input$Gemeinde
    filtered_data <- filter(germany_municipalities, NAME_3 == selected_municipality)
    
    leaflet() %>%
      setView(lng=10,lat=51.2, zoom = 4) %>% 
      addTiles() %>%
      addPolygons(data = germany_municipalities,
                  fillOpacity = 0.2,
                  color = "black",
                  weight = 1,
                  group = "Gemeinden",
                  label = ~paste(NAME_3),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "red",
                    bringToFront = TRUE
                  )
      ) %>%
      addPolygons(data = filtered_data,
                  fillOpacity = 0.5,
                  color = "blue",
                  weight = 3,
                  group = "SelectedMunicipality"
      )
  })
    
  
  
}


shinyApp(ui, server)
