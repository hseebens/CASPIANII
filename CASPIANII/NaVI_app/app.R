###########################################################################################################
#
# Shiny App "NaVI - Neobiota an Verkehrswegen Informationssystem"
#
# Das Skript laedt und startet die Shiny App NaVI. 
#
# Die Shiny App NaVI dient der Darstellung und Bereitstellung von Datensaetzen und Karten von 
# gebietsfremden und invasiven Arten. Die zugrundliegenden Datensaetze können mit der Funktion
# 'DataPreparationShiny.R' erzeugt werden.
#
# Author: Hanno Seebens, Senckenberg Gesellschaft fuer Naturforschung
##########################################################################################################




## Bereinigen der R Umgebung
rm(list=ls())



## R Pakete der shiny app
library(shiny)
library(shinybusy)
library(leaflet)

## R Pakete zur Datenbearbeitung
library(sf) 
library(DT) 
library(dplyr)
library(tidyr)
library(data.table)
library(units)


# setwd(file.path("..","NaVI_app"))



## load data----

dist_buff <<- 100
units(dist_buff) <- as_units("km")
dist_buff <<- dist_buff # make it a global variable

# polygon data sets
map_fine <<- st_read("AnzahlNeobiota_ist_Kreise.shp")
uni_regs <<- sort(unique(map_fine$NAME_2))
all_regs <- unique(cbind.data.frame(map_fine$NAME_2,map_fine$CC_2))
colnames(all_regs) <- c("RegionName","CC_2")
all_regs <<- all_regs # make it a global variable

map_simp <<- st_read("AnzahlNeobiota_ist_Kreise_simpl.shp")
map_simp_eu <<- st_read("AnzahlNeobiota_ist_Kreise_simpl_EUIAS.shp")
map_simp_han <<- st_read("AnzahlNeobiota_ist_Kreise_simpl_HanL.shp")
map_simp_beo <<- st_read("AnzahlNeobiota_ist_Kreise_simpl_BeoL.shp")
map_simp_man <<- st_read("AnzahlNeobiota_ist_Kreise_simpl_ManL.shp")
map_simp_akt <<- st_read("AnzahlNeobiota_ist_Kreise_simpl_AktL.shp")


## Species lists by region #####################################################

## All neobiota ###############
spec_istpot <- fread("Neobiota_IstPot_Liste_Kreise.gz", 
                     colClasses=c("character","character","character","character","character","character","numeric"))
setorderv(spec_istpot,cols=c("NAME_2","HabitatEignung"),order=-1)
colnames(spec_istpot) <- c("NAME_2","CC_2","Art","Gruppe","Ist","Pot","Habitateignung (0-1)")
spec_istpot_regs <- merge(spec_istpot,all_regs, by="CC_2", all=T)


## EU IAS list ################
spec_istpot_eu <- fread("Neobiota_IstPot_Liste_Kreise_EUIAS.gz", 
                        colClasses=c("character","character","character","character","character","character","numeric"))
setorderv(spec_istpot_eu,cols=c("NAME_2","HabitatEignung"),order=-1)
colnames(spec_istpot_eu) <- c("NAME_2","CC_2","Art","Gruppe","Ist","Pot","Habitateignung (0-1)")
spec_istpot_regs_eu <- merge(spec_istpot_eu,all_regs, by="CC_2", all=T)

## Handlungsliste ################
spec_istpot_han <- fread("Neobiota_IstPot_Liste_Kreise_HanL.gz", 
                         colClasses=c("character","character","character","character","character","character","numeric"))
setorderv(spec_istpot_han,cols=c("NAME_2","HabitatEignung"),order=-1)
colnames(spec_istpot_han) <- c("NAME_2","CC_2","Art","Gruppe","Ist","Pot","Habitateignung (0-1)")
spec_istpot_regs_han <- merge(spec_istpot_han,all_regs, by="CC_2", all=T)

## Beobachtungsliste ################
spec_istpot_beo <- fread("Neobiota_IstPot_Liste_Kreise_BeoL.gz", 
                         colClasses=c("character","character","character","character","character","character","numeric"))
setorderv(spec_istpot_beo,cols=c("NAME_2","HabitatEignung"),order=-1)
colnames(spec_istpot_beo) <- c("NAME_2","CC_2","Art","Gruppe","Ist","Pot","Habitateignung (0-1)")
spec_istpot_regs_beo <- merge(spec_istpot_beo,all_regs, by="CC_2", all=T)

## Aktionsliste ################
spec_istpot_akt <- fread("Neobiota_IstPot_Liste_Kreise_AktL.gz", 
                         colClasses=c("character", "character","character","character","character","character","numeric"))
setorderv(spec_istpot_akt,cols=c("NAME_2","HabitatEignung"),order=-1)
colnames(spec_istpot_akt) <- c("NAME_2","CC_2","Art","Gruppe","Ist","Pot","Habitateignung (0-1)")
spec_istpot_regs_akt <- merge(spec_istpot_akt,all_regs, by="CC_2", all=T)

## Managementliste ################
spec_istpot_man <- fread("Neobiota_IstPot_Liste_Kreise_ManL.gz", 
                         colClasses=c("character", "character", "character", "character", "character", "character", "numeric"))
setorderv(spec_istpot_man,cols=c("NAME_2","HabitatEignung"),order=-1)
colnames(spec_istpot_man) <- c("NAME_2", "CC_2", "Art", "Gruppe", "Ist", "Pot", "Habitateignung (0-1)")
spec_istpot_regs_man <- merge(spec_istpot_man,all_regs, by="CC_2", all=T)



# species occurrences
point_data <- fread("Vorkommen_alleArten.gz")
point_data$DBcols <- as.numeric(as.factor(point_data$Datenbank))
point_data <<- point_data # make it a global variable

uni_spec <<- unique(point_data$Taxon)
all_cols <<- c('royalblue3','darkgreen','skyblue1')
all_DBs  <<- sort(unique(point_data$Datenbank))


## potential new arrivals
all_pot_spec <<- readRDS("Liste_PotSpez_Kreise.rds")

## common names
common_names <- fread("Tabelle_DeutscheName.csv")
colnames(common_names) <- c("Deutscher Artname", "Taxon_wissensch")

region_lists <<- merge(spec_istpot_regs,common_names,by.x="Art", by.y="Taxon_wissensch")
region_lists_eu <<- merge(spec_istpot_regs_eu,common_names,by.x="Art", by.y="Taxon_wissensch")
region_lists_han <<- merge(spec_istpot_regs_han,common_names,by.x="Art", by.y="Taxon_wissensch")
region_lists_beo <<- merge(spec_istpot_regs_beo,common_names,by.x="Art", by.y="Taxon_wissensch")
region_lists_akt <<- merge(spec_istpot_regs_akt,common_names,by.x="Art", by.y="Taxon_wissensch")
region_lists_man <<- merge(spec_istpot_regs_man,common_names,by.x="Art", by.y="Taxon_wissensch")





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
      actionButton("dataEUIAS","Unionsliste"),
      actionButton("dataBeoL","Beobachtungsliste"),
      actionButton("dataHanL","Handlungsliste"),
      actionButton("dataManL","Managementliste"),
      actionButton("dataAktL","Aktionsliste"),
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





server <- function(input, output){
  
  ## prepare data and maps depending on selections ##############################
  
  ## select base data set (all neobiota or IAS) and make those reactive
  subdata <- reactiveValues(data = NULL)
  subdata$region_lists <- region_lists # default
  
  mapdata <- reactiveValues(data = NULL)
  mapdata$map_simp <- map_simp # default
  mapdata$map_fine <- map_fine # default
  
  ## select map depending on the outcome of action buttons to subset the species list
  
  ## observe action button 'dataNeobiota'
  observeEvent(input$dataNeobiota, {
    subdata$region_lists <- region_lists
    mapdata$map_simp <- map_simp
  })
  
  ## observe action button 'dataEUIAS'
  observeEvent(input$dataEUIAS, {
    subdata$region_lists <- region_lists_eu
    mapdata$map_simp <- map_simp_eu
  })
  
  ## observe action button 'dataHanL'
  observeEvent(input$dataHanL, {
    subdata$region_lists <- region_lists_han
    mapdata$map_simp <- map_simp_han
  })
  
  ## observe action button 'dataBeoL'
  observeEvent(input$dataBeoL, {
    subdata$region_lists <- region_lists_beo
    mapdata$map_simp <- map_simp_beo
  })
  
  ## observe action button 'dataManL'
  observeEvent(input$dataManL, {
    subdata$region_lists <- region_lists_man
    mapdata$map_simp <- map_simp_man
  })
  
  ## observe action button 'dataAktL'
  observeEvent(input$dataAktL, {
    subdata$region_lists <- region_lists_akt
    mapdata$map_simp <- map_simp_akt
  })
  
  
  ## prepare (render) the output table ###########
  
  ## for potentially establishing species
  pot_spec_tab <- reactive({ # make table reactive that the user can select from the table directly the map points
    region_list_sub <- subdata$region_lists[RegionName%in%input$Kreise_Daten &  
                                              Art%in%all_pot_spec[[input$Kreise_Daten]],c("Art", "Deutscher Artname", "Gruppe", "Habitateignung (0-1)")]
    region_list_sub <- region_list_sub[order(region_list_sub[,3], decreasing=TRUE)]
  })
  output$table <- renderDT({ # render DT table from reactive object
    df <- pot_spec_tab()
    DT::datatable(df)
  }) 
  
  ## for occurring species
  occ_spec_tab <- reactive({ # make table reactive that the user can select from the table directly the map points
    subdata$region_lists[RegionName%in%input$Kreise_Daten & 
                           Ist=="x", c("Art","Deutscher Artname", "Gruppe")]
  })
  output$ListeNeobiota <- renderDT({ # render DT table from reactive object
    df <- occ_spec_tab()
    DT::datatable(df)
  })
  
  
  ## prepare data for region download ##########################################
  
  data_regs <- reactive({ # make reactive that the table can be modified when making a selection
    subdata$region_lists[RegionName%in%input$Kreise_Daten & 
                           Ist=="x", c("RegionName", "Art", "Deutscher Artname", "Gruppe")]
  })
  
  ## for all species
  output$download_ist <- downloadHandler(
    filename = function() {
      paste0("ListeNeobiota_", input$Kreise_Daten, ".csv")
    },
    content = function(file) {
      write.table(data_regs(), file)
    }
  )
  
  ## for potential species 
  data_potspec <- reactive({
    subdata$region_lists[RegionName%in%input$Kreise_Daten & 
                           Art%in%all_pot_spec[[input$Kreise_Daten]],c("NAME_2","Art","Deutscher Artname", "Gruppe","Habitateignung (0-1)")]
  })
  
  ## add to download
  output$download_pot <- downloadHandler(
    filename = function() {
      paste0("ListePotenzielleNeobiota_",input$Kreise_Daten, ".csv")
    },
    content = function(file) {
      write.table(data_potspec(), file)
    }
  )
  
  
  ## prepare data for species download ##########################################
  
  data_spec <- reactive({ # make reactive
    subset(point_data, Taxon==input$Art_Daten)
  })
  
  # output$preview <- renderTable({
  # head(data_spec())
  # })
  
  ## add to download
  output$download_spec <- downloadHandler(
    filename = function() {
      paste0(input$Art_Daten, ".csv")
    },
    content = function(file) {
      write.table(data_spec(), file)
    }
  )
  
  
  ## generate map and data points depending on selections ######################
  
  ## generate colour palette for distinguishing data sources for point records
  pal <- colorNumeric(
    palette = all_cols,
    domain = sort(unique(point_data$DBcols))
  )
  
  ## render main map ########################
  
  output$map <- renderLeaflet({
    
    # choose map (either full map with simplified geometries or zoom on regions)
    if(input$Kreise_Daten == "Keine"){
      mapfiltered <- mapdata$map_simp # simplified geometries
      
      ## text for mouse-over
      labels <- sprintf("%s: %g", mapfiltered$NAME_2, mapfiltered$nSpez) %>%
        lapply(htmltools::HTML)
      
    } else {
      
      mapfiltered <- mapdata$map_simp # full map
      mapfiltered <- mapfiltered[which(mapfiltered$NAME_2 == input$Kreise_Daten), ] #
      
      ## text for mouse-over
      labels <- sprintf("%s: %g", mapfiltered$NAME_2, mapfiltered$nSpez) %>%
        lapply(htmltools::HTML)
    }
    
    ## create colours for legend
    pal_nSpec <- colorBin("YlOrRd", domain = mapfiltered$nSpez, bins = 7)
    
    ## plot the base map
    if (input$Kreise_Daten == "Keine"){ # if no region is selected
      leaflet(mapfiltered) %>%
        addTiles() %>%
        addPolygons( # polygons of species numbers 
          fillColor = ~ pal_nSpec(nSpez),
          color = NA,
          dashArray = "3",
          fillOpacity = 0.7,
          label = labels) %>%
        leaflet::addLegend(
          pal = pal_nSpec, 
          values = ~nSpez,
          opacity = 0.7, 
          title = "Anzahl Arten",
          na.label="Keine Arten")
    } else { # if a region has been selected
      leaflet(mapfiltered) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~ pal_nSpec(nSpez),
          color = NA,
          dashArray = "3",
          fillOpacity = 0.7,
          label = labels) #%>%
      # leaflet::addLegend(
      #   pal = pal, values = ~nSpez,
      #   opacity = 0.7, title = "Anzahl Neobiota")
    }
  })
  
  
  ## add points of species occurrences #################
  
  observeEvent(input$Art_Daten, { # observe whether a species has been selected
    
    spfiltered <- point_data[which(point_data$Taxon == input$Art_Daten), ] # subset to focal species
    
    uni_col_DB <- unique(cbind.data.frame(spfiltered$Datenbank,pal(spfiltered$DBcols))) # text for legend
    
    ## select subset of records to increase performance
    if(input$Kreise_Daten == "Keine"){ # for Germany-wide presentation
      if (nrow(spfiltered)>10000){ # select only a random set of 10000 records
        
        spfiltered <- spfiltered[sample(1:nrow(spfiltered),10000)]
        
        ## produce a warning that not all records are shown
        NotID <- showNotification("Warnung: Großer Datensatz. Nicht alle Daten werden angezeigt.", duration=15)
      }
    } else { # for selections of individual regions select only records within a buffer distance of dist_buff
      
      map_sub <- map_fine[which(map_fine$NAME_2 == input$Kreise_Daten), ] # subset to focal region
      ext <- st_bbox(st_buffer(map_sub,dist_buff)) # identify box around the focal region
      spfiltered <- subset(spfiltered, Laengengrad<ext$xmax & Laengengrad>ext$xmin & Breitengrad<ext$ymax & Breitengrad>ext$ymin) # subset to records in the vicinity
    }
    
    ## add the point records to the map
    leafletProxy("map", data = spfiltered) %>%
      clearMarkers() %>%
      clearGroup(group="occurrences") %>%
      # clearControls() %>%
      addCircleMarkers(
        lat = spfiltered$Breitengrad,
        lng = spfiltered$Laengengrad,
        group="occurrences",
        # color = "#000000",
        # color="royalblue3", #pal(spfiltered$DBcols),
        fillOpacity = 0.5,
        fillColor = NA,
        weight = 1,
        radius = 5
      ) #%>%
    # addLegend("bottomright", colors=uni_col_DB[,2] ,labels=uni_col_DB[,1],
    #           title = "Datenbank",
    #           opacity = 1
    # )
  })
  
  ## add records of species selected in table of all species ################################
  ## observe input to table of existing neobiota and plot records of selected species on map
  
  observe({
    
    selRow <- occ_spec_tab()[input$ListeNeobiota_rows_selected,] # keep row of selection
    # print(selRow$Art)
    
    spfiltered <- point_data[which(point_data$Taxon == selRow$Art), ] # subset records to focal species
    
    uni_col_DB <- unique(cbind.data.frame(spfiltered$Datenbank,pal(spfiltered$DBcols))) # text for legend
    
    ## subset to records in vicinity to increase performance
    map_sub <- map_fine[which(map_fine$NAME_2 == input$Kreise_Daten), ] # subset to region
    ext <- st_bbox(st_buffer(map_sub,dist_buff))
    spfiltered <- subset(spfiltered, Laengengrad<ext$xmax & Laengengrad>ext$xmin & Breitengrad<ext$ymax & Breitengrad>ext$ymin)
    ## if still too many points select randomly
    if (nrow(spfiltered)>10000){ # select only a random set of 1000 records
      spfiltered <- spfiltered[sample(1:nrow(spfiltered),10000)]
      NotID <- showNotification("Warnung: Großer Datensatz. Nicht alle Daten werden angezeigt.", duration=15)
    }
    
    ## add the point records to the map
    leafletProxy("map", data = spfiltered) %>%
      clearMarkers() %>%
      clearGroup(group="occurrences") %>%
      # clearControls() %>%
      addCircleMarkers(
        lat = spfiltered$Breitengrad,
        lng = spfiltered$Laengengrad,
        group="occurrences",
        # color = "#000000",
        # color="royalblue3", #pal(spfiltered$DBcols),
        fillOpacity = 0.5,
        fillColor = NA,
        weight = 1,
        radius = 5
      ) #%>%
    # addLegend("bottomright", colors=uni_col_DB[,2] ,labels=uni_col_DB[,1],
    #           title = "Datenbank",
    #           opacity = 1
    # )
  })
  
  
  ## add records of species selected in table of potential species ################################
  ## observe input to table of existing neobiota and plot records of selected species on map
  observe({
    
    selRow <- pot_spec_tab()[input$table_rows_selected,] # keep row of selection
    # print(selRow$Art)
    
    spfiltered <- point_data[which(point_data$Taxon == selRow$Art), ] # subset records to focal species
    
    uni_col_DB <- unique(cbind.data.frame(spfiltered$Datenbank,pal(spfiltered$DBcols))) # text for legend
    
    ## subset to records in vicinity
    map_sub <- map_fine[which(map_fine$NAME_2 == input$Kreise_Daten), ]
    ext <- st_bbox(st_buffer(map_sub,dist_buff))
    spfiltered <- subset(spfiltered, Laengengrad<ext$xmax & Laengengrad>ext$xmin & Breitengrad<ext$ymax & Breitengrad>ext$ymin)
    ## if still too many points select randomly and spill out warning message
    if (nrow(spfiltered)>10000){ # select only a random set of 1000 records
      spfiltered <- spfiltered[sample(1:nrow(spfiltered),10000)]
      NotID <- showNotification("Warnung: Großer Datensatz. Nicht alle Daten werden angezeigt.", duration=15)
    }
    
    ## add point records to map
    leafletProxy("map", data = spfiltered) %>%
      clearMarkers() %>%
      clearGroup(group="occurrences") %>%
      # clearControls() %>%
      addCircleMarkers(
        lat = spfiltered$Breitengrad,
        lng = spfiltered$Laengengrad,
        group="occurrences",
        # color = "#000000",
        # color="royalblue3", #pal(spfiltered$DBcols),
        fillOpacity = 0.5,
        fillColor = NA,
        weight = 1,
        radius = 5
      ) #%>%
    # addLegend("bottomright", colors=uni_col_DB[,2] ,labels=uni_col_DB[,1],
    #           title = "Datenbank",
    #           opacity = 1
    # )
  })
}


## Lade Shiny App
shinyApp(ui = ui, server = server)

