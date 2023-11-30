rm(list=ls())


library(shiny)
#library(shinycssloaders)
library(shinybusy)

library(sf)
library(DT)

library(leaflet)
# library(rgdal)
# library(rmapshaper)
# library(sf)

library(dplyr)
library(tidyr)
library(data.table)


# 
# # Prep data----
# 
# species lists
# nspez <- fread(file.path("Shiny","Daten","Neobiota_ist_nSpez_Gemeinden.gz"),colClasses = c("character","integer"))
# liste <- fread(file.path("Shiny","Daten","Neobiota_IstPot_Liste_Gemeinden.gz"), colClasses=c("character","character","character","character","character","numeric"))
# 
# # regions
# regions <- st_read(dsn=file.path("C:","Hanno","Bioinvasion","CASPIANII","CASPIANII","SDM","Data","Input","Shapefiles"),layer="gadm41_DEU_3")
# regions$RegionName <- paste0(regions$NAME_3," (",regions$NAME_2,")")
# 
# # Build joins
# regions_nspez = regions %>%
#   inner_join(nspez, by = 'CC_3') %>%
#   dplyr::select(RegionName, CC_3, nSpec_obs, geometry) %>%
#   # cleaning step that should be removed
#   distinct(CC_3, .keep_all = TRUE)
# 
# st_write(regions_nspez, file.path("Shiny","Daten","regions_nspez.shp"), delete_layer=T)
# 
# # simplify geometry
# regions_nspez_simp <- ms_simplify(regions_nspez)
# st_write(regions_nspez_simp, file.path("Shiny","Daten","regions_nspez_simp.shp"), delete_layer=T)
# 
# 
# # species occurrences
# point_data <- fread(paste0(data_dir,"/species/Neobiota_AllOccurrences.gz")) 
# 
# ab_sp = filter(point_data %>%  group_by(Taxon) %>% count(), n >  100718)
# 
# point_data2 = point_data %>% 
#   distinct()
# 
# point_data3 = point_data %>% 
#   filter(!Taxon %in% ab_sp$Taxon) %>% 
#   rbind(point_data2)


# Shiny app----

## load data----

# polygon data sets
map_fine <- st_read(dsn=file.path("..","Daten"), layer= "regions_nspez")
uni_regs <- sort(unique(map_fine$RegionName))
all_regs <- unique(cbind.data.frame(map_fine$RegionName,map_fine$CC_3))
colnames(all_regs) <- c("RegionName","CC_3")

map_simp <- st_read(dsn=file.path("..","Daten"),layer= "regions_nspez_simp")


# species list (by region)
data <- fread(file.path("..","Daten","Neobiota_IstPot_Liste_Gemeinden.gz"), colClasses=c("character","character","character","character","character","numeric"))
setorderv(data,cols=c("NAME_3","HabitatEignung"),order=-1)
colnames(data) <- c("NAME_3","CC_3","Art","Ist","Pot","Habitateignung (0-1)")
data <- merge(data,all_regs, by="CC_3", all=T)



# species occurrences
point_data <- fread(file.path("..","Daten","Neobiota_AllOccurrences.gz"))
uni_spec <- unique(point_data$Taxon)

## potential new arrivals
all_pot_spec <- readRDS(file.path("..","Daten","List_all_pot_spec.rds"))
# cbind(all_pot_spec[["Hausen"]],1)

## common names
common_names <- fread(file.path("..","Daten","Tabelle_DeutscheName.csv"))
colnames(common_names) <- c("Deutscher Artname", "Taxon_wissensch")



## build app----
# ui object (ui <- fluidPage( ))
ui <- fluidPage(
  
  ### General stuff
  add_busy_bar(centered=TRUE, color = "#FF0000"),
  tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
  titlePanel(div(h1("Verbreitung von Neobiota in Deutschland"),align="left",style="color:darkgreen")),
  
  ### Layout #########################
  sidebarLayout(
    
    ## Sidebar panel 
    sidebarPanel(
      
      # h3("Auswahl einer Gemeinde"),
      h4("Neobiota für eine ausgewählte Gemeinde", align="center"),
      
      # select municipalities #######################
      selectizeInput(
        inputId = "Gemeinde_Daten",
        label = "Gemeinde",
        choices = c("Keine",uni_regs)
      ),
      
      # download button
      h5("Download Liste Neobiota für Gemeinde"),
      downloadButton("download_ist", "Download .csv"),
      
      # download button
      h5("Download Liste potenzieller Neobiota für Gemeinde"),
      downloadButton("download_pot", "Download .csv"),
      hr(),
      
      # select species data ########################
      h4("Verbreitung einer Art", align="center"),
      selectizeInput(
        inputId = "Art_Daten",
        label = "Wissenschaftlicher Name",
        choices = c("Keine",uni_spec)
      ),
      
      # download button
      h5("Download der Vorkommen dieser Art in Deutschland"),
      downloadButton("download_spec", "Download .csv"),

      # adds text
      br(),
      br(),
      p("Made with", a("Shiny", href = "http://shiny.rstudio.com")),
      
    ),
    
    ## Main panel ####################################
    
    mainPanel(
      
      # interactive map
      leafletOutput(outputId = "map"),
      
      # output table 1
      hr(),
      h3("Alle Neobiota in der Gemeinde"),
      DTOutput(outputId = "ListeNeobiota"),
      
      # output table 2
      br(),
      hr(),
      h3("Neobiota mit hohem Potenzial zur Etablierung in der Gemeinde"),
      DTOutput(outputId = "table")
    )
  )
)


# server function (server <- function(input, output){})
server <- function(input, output){
  
  test_data <- merge(data,common_names,by.x="Art", by.y="Taxon_wissensch")

  # render the output table
  output$table <- renderDT(

    test_data[RegionName%in%input$Gemeinde_Daten &  Art%in%all_pot_spec[[input$Gemeinde_Daten]],c("Art","Deutscher Artname","Habitateignung (0-1)")]

  )
  output$ListeNeobiota <- renderDT(
    # show only species in the selected municipality
    
    test_data[RegionName%in%input$Gemeinde_Daten & Ist=="x", c("Art","Deutscher Artname")]

  )
  
  ## region download #######################
  data_regs <- reactive({
    test_data[RegionName%in%input$Gemeinde_Daten & Ist=="x", c("RegionName","Art","Deutscher Artname")]
  })
  
  output$download_ist <- downloadHandler(
    filename = function() {
      paste0("ListeNeobiota_", input$Gemeinde_Daten, ".csv")
    },
    content = function(file) {
      write.table(data_regs(), file)
    }
  )
  
  ## pot species download ##################
  data_potspec <- reactive({
    test_data[RegionName%in%input$Gemeinde_Daten &  Art%in%all_pot_spec[[input$Gemeinde_Daten]],c("RegionName","Art","Deutscher Artname","Habitateignung (0-1)")]
  })

  output$download_pot <- downloadHandler(
    filename = function() {
      paste0("ListePotenzielleNeobiota_",input$Gemeinde_Daten, ".csv")
    },
    content = function(file) {
      write.table(data_potspec(), file)
    }
  )
  
  
  ## species download ######################
  data_spec <- reactive({
    subset(point_data, Taxon==input$Art_Daten)
  })
  
  # output$preview <- renderTable({
    # head(data_spec())
  # })
  
  output$download_spec <- downloadHandler(
    filename = function() {
      paste0(input$Art_Daten, ".csv")
    },
    content = function(file) {
      write.table(data_spec(), file)
    }
  )
  
  
  # render interactive map ##################
  output$map <- renderLeaflet({
    
    # add data to map
    if(input$Gemeinde_Daten == "Keine"){
      mapfiltered <- map_simp
    } else {
      mapfiltered <- map_fine[which(map_fine$RegionName == input$Gemeinde_Daten), ]
    }
    
    # create leaflet
    pal <- colorBin("YlOrRd", domain = mapfiltered$nSpec_obs, bins = 7)
    
    labels <- sprintf("%s: %g", mapfiltered$RegionName, mapfiltered$nSpec_obs) %>%
      lapply(htmltools::HTML)
    
    #plot the base map
    if(input$Gemeinde_Daten == "Keine"){
      leaflet(mapfiltered) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~ pal(nSpec_obs),
          color = NA,
          dashArray = "3",
          fillOpacity = 0.7,
          label = labels) %>%
        leaflet::addLegend(
          pal = pal, values = ~nSpec_obs,
          opacity = 0.7, title = "Anzahl Neobiota")
    } else {
      leaflet(mapfiltered) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~ pal(nSpec_obs),
          color = NA,
          dashArray = "3",
          fillOpacity = 0.7,
          label = labels)
      # leaflet::addLegend(
      #   pal = pal, values = ~nSpec_obs,
      #   opacity = 0.7, title = "Anzahl Neobiota")
    }
  })
  
  
  # add species occurrences #################
  observeEvent(input$Art_Daten, {
    
    spfiltered <- point_data[which(point_data$Taxon == input$Art_Daten), ]
    
    if (nrow(spfiltered)>10000){
      
      # output$text1 <- renderText({paste("You have selected a large data set")})
      
      # ext <- st_bbox(st_buffer(mapfiltered,5))
      # spfiltered <- subset(spfiltered, Laengengrad<ext$xmax & Laengengrad>ext$xmin & Breitengrad<ext$ymax & Breitengrad>ext$ymin)
      spfiltered <- spfiltered[sample(1:nrow(spfiltered),10000)]
    }
    
    leafletProxy("map", data = spfiltered) %>%
      clearMarkers() %>% 
      addMarkers(
        lat = spfiltered$Breitengrad,
        lng = spfiltered$Laengengrad
      )
    # addCircles(
    #   lat = spfiltered$Breitengrad,
    #   lng = spfiltered$Laengengrad,
    #   color = "#000000",
    #   fillColor = "#000000",
    #   weight = 5
    # )
    
  })
  
}

# Run the app-----
shinyApp(ui = ui, server = server)


# shiny::runGitHub(repo="CASPIANII", subdir="CASPIANII/Shiny/R",username= "hseebens")
