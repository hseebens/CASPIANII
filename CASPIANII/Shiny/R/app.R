rm(list=ls())


library(shiny)
#library(shinycssloaders)
library(shinybusy)

library(sf)
library(DT)

library(leaflet)
# library(rgdal)
# library(sf)

library(dplyr)
library(tidyr)
library(data.table)
library(units)

# setwd(file.path("Shiny","R"))


# Shiny app----

## load data----

dist_buff <- 100
units(dist_buff) <- as_units("km")

# polygon data sets
map_fine <- st_read(dsn=file.path("..","Daten"), layer= "AnzahlNeobiota_ist_Kreise")
uni_regs <- sort(unique(map_fine$NAME_2))
all_regs <- unique(cbind.data.frame(map_fine$NAME_2,map_fine$CC_2))
colnames(all_regs) <- c("RegionName","CC_2")

map_simp <- st_read(dsn=file.path("..","Daten"),layer= "AnzahlNeobiota_ist_Kreise_simpl")
map_simp_eu <- st_read(dsn=file.path("..","Daten"),layer= "AnzahlNeobiota_ist_Kreise_simpl_EUIAS")


# species list (by region)
spec_istpot <- fread(file.path("..","Daten","Neobiota_IstPot_Liste_Kreise.gz"), colClasses=c("character","character","character","character","character","numeric"))
setorderv(spec_istpot,cols=c("NAME_2","HabitatEignung"),order=-1)
colnames(spec_istpot) <- c("NAME_2","CC_2","Art","Ist","Pot","Habitateignung (0-1)")
spec_istpot_regs <- merge(spec_istpot,all_regs, by="CC_2", all=T)


# EU IAS list (by region)
spec_istpot_eu <- fread(file.path("..","Daten","Neobiota_IstPot_Liste_Kreise_EUIAS.gz"), colClasses=c("character","character","character","character","character","numeric"))
setorderv(spec_istpot_eu,cols=c("NAME_2","HabitatEignung"),order=-1)
colnames(spec_istpot_eu) <- c("NAME_2","CC_2","Art","Ist","Pot","Habitateignung (0-1)")
spec_istpot_regs_eu <- merge(spec_istpot_eu,all_regs, by="CC_2", all=T)



# species occurrences
point_data <- fread(file.path("..","Daten","Vorkommen_alleArten.gz"))
uni_spec <- unique(point_data$Taxon)

## potential new arrivals
all_pot_spec <- readRDS(file.path("..","Daten","Liste_PotSpez_Kreise.rds"))
# cbind(all_pot_spec[["Hausen"]],1)

## common names
common_names <- fread(file.path("..","Daten","Tabelle_DeutscheName.csv"))
colnames(common_names) <- c("Deutscher Artname", "Taxon_wissensch")

region_lists <- merge(spec_istpot_regs,common_names,by.x="Art", by.y="Taxon_wissensch")
region_lists_eu <- merge(spec_istpot_regs_eu,common_names,by.x="Art", by.y="Taxon_wissensch")


## build app----
# ui object (ui <- fluidPage( ))
ui <- fluidPage(
  
  ### General stuff
  add_busy_bar(centered=TRUE, color = "#FF0000"),
  tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
  titlePanel(div(h1("NaVI - Neobiota an Verkehrswegen Informationssystem"),align="left",style="color:darkgreen")),
  
  ### Layout #########################
  sidebarLayout(
    
    ## Sidebar panel 
    sidebarPanel(
      
      h4("Welche Artengruppe?"),
      actionButton("dataNeobiota","Alle Neobiota"),
      actionButton("dataEUIAS","Arten der Unionsliste"),
      hr(),
      
      # h3("Auswahl eines Kreises"),
      h4("Welcher Landkreis?"), #, align="center"
      
      # select Kreise #######################
      selectizeInput(
        inputId = "Kreise_Daten",
        label = "Landkreis",
        choices = c("Keine",uni_regs)
      ),
      
      # download button
      h5("Download Liste Neobiota für Kreise"),
      downloadButton("download_ist", "Download .csv"),
      
      # download button
      h5("Download Liste potenzieller Neobiota für Kreise"),
      downloadButton("download_pot", "Download .csv"),
      hr(),
      
      # select species data ########################
      h4("Welche Art?"), #, align="center"
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
      tabsetPanel(
        tabPanel("Karte",
                     
          # interactive map
          leafletOutput(outputId = "map"),
          
          # output table 1
          hr(),
          h4("Neobiota/IAS im Landkreis"),
          DTOutput(outputId = "ListeNeobiota"),

          # output table 2
          br(),
          hr(),
          h4("Neobiota/IAS mit hohem Potenzial zur Etablierung im Landkreis"),
          DTOutput(outputId = "table")
        ),
        tabPanel("über NaVI", includeHTML("NaVIdetails.html"))
      )
    )
  )
)


# server function (server <- function(input, output){})
server <- function(input, output){

  ## select base data set (all neobiota or IAS)
  subdata <- reactiveValues(data = NULL)
  subdata$region_lists <- region_lists # default
  
  mapdata <- reactiveValues(data = NULL)
  mapdata$map_simp <- map_simp # default
  
  observeEvent(input$dataNeobiota, {
    subdata$region_lists <- region_lists
    mapdata$map_simp <- map_simp
  })
  
  observeEvent(input$dataEUIAS, {
    subdata$region_lists <- region_lists_eu
    mapdata$map_simp <- map_simp_eu
  })
  
  
  # render the output table ###########
  output$table <- renderDT({
    region_list_sub <- subdata$region_lists[RegionName%in%input$Kreise_Daten &  Art%in%all_pot_spec[[input$Kreise_Daten]],c("Art","Deutscher Artname","Habitateignung (0-1)")]
    region_list_sub <- region_list_sub[order(region_list_sub[,3], decreasing=TRUE)]
  })
  output$ListeNeobiota <- renderDT(# show only species in the selected municipality
    subdata$region_lists[RegionName%in%input$Kreise_Daten & Ist=="x", c("Art","Deutscher Artname")]
  )
  
  ## region download #######################
  data_regs <- reactive({
    subdata$region_lists[RegionName%in%input$Kreise_Daten & Ist=="x", c("RegionName","Art","Deutscher Artname")]
  })
  
  output$download_ist <- downloadHandler(
    filename = function() {
      paste0("ListeNeobiota_", input$Kreise_Daten, ".csv")
    },
    content = function(file) {
      write.table(data_regs(), file)
    }
  )
  
  ## pot species download ##################
  data_potspec <- reactive({
    subdata$region_lists[RegionName%in%input$Kreise_Daten &  Art%in%all_pot_spec[[input$Kreise_Daten]],c("NAME_2","Art","Deutscher Artname","Habitateignung (0-1)")]
  })

  output$download_pot <- downloadHandler(
    filename = function() {
      paste0("ListePotenzielleNeobiota_",input$Kreise_Daten, ".csv")
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
    if(input$Kreise_Daten == "Keine"){
      mapfiltered <- mapdata$map_simp
    } else {
      mapfiltered <- map_fine[which(map_fine$NAME_2 == input$Kreise_Daten), ]
    }
    
    # create leaflet
    pal <- colorBin("YlOrRd", domain = mapfiltered$nSpez, bins = 7)
    
    labels <- sprintf("%s: %g", mapfiltered$NAME_2, mapfiltered$nSpez) %>%
      lapply(htmltools::HTML)
    
    #plot the base map
    if (input$Kreise_Daten == "Keine"){
      leaflet(mapfiltered) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~ pal(nSpez),
          color = NA,
          dashArray = "3",
          fillOpacity = 0.7,
          label = labels) %>%
        leaflet::addLegend(
          pal = pal, values = ~nSpez,
          opacity = 0.7, title = "Anzahl Arten")
    } else {
      leaflet(mapfiltered) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~ pal(nSpez),
          color = NA,
          dashArray = "3",
          fillOpacity = 0.7,
          label = labels)
      # leaflet::addLegend(
      #   pal = pal, values = ~nSpez,
      #   opacity = 0.7, title = "Anzahl Neobiota")
    }
  })
  
  
  # add species occurrences #################
  observeEvent(input$Art_Daten, {
    
    spfiltered <- point_data[which(point_data$Taxon == input$Art_Daten), ]

    # select subset of records to increase performance
    if(input$Kreise_Daten == "Keine"){ # for Germany-wide presentation
      if (nrow(spfiltered)>1000){ # select only a random set of 1000 records
        spfiltered <- spfiltered[sample(1:nrow(spfiltered),1000)]
      }
    } else { # for selctions of individual regions select only records within a buffer distance of dist_buff
      map_sub <- map_fine[which(map_fine$NAME_2 == input$Kreise_Daten), ]
      ext <- st_bbox(st_buffer(map_sub,dist_buff))
      spfiltered <- subset(spfiltered, Laengengrad<ext$xmax & Laengengrad>ext$xmin & Breitengrad<ext$ymax & Breitengrad>ext$ymin)
    }

    leafletProxy("map", data = spfiltered) %>%
      clearMarkers() %>% 
      clearGroup(group="occurrences") %>%
      addCircleMarkers(
        lat = spfiltered$Breitengrad,
        lng = spfiltered$Laengengrad,
        group="occurrences",
        # color = "#000000",
        # fillColor = "#000000",
        weight = 5,
        radius = 5
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
