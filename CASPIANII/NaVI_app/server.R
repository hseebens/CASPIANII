################################################################################
#
# Shiny App NaVI - Funktion 'server'
#
# Die server Funktion stellt das 'backend' der Shiny App dar und liefert die 
# Inhalte, die in der Shiny App zur Verfuegung gestellt werden.
#
# Die Shiny App muss über die Funktion ... aufgerufen werden, die wiederum die 
# Server Funktion aufruft. Die Daten, die in der Shiny App zur Verfuegung 
# gestellt werden, werden mit der Funktion 'DataPreparationShiny.R' aufbereitet.
#
# Author: Hanno Seebens, Senckenberg Gesellschaft für Naturforschung
################################################################################




server <- function(input, output){
  
  ## prepare data and maps depending on selections #############################
  
  ## select base data set (all neobiota or IAS) and make those reactive
  subdata <- reactiveValues(data = NULL)
  subdata$region_lists <- region_lists # default
  
  mapdata <- reactiveValues(data = NULL)
  mapdata$map_simp <- map_simp # default
  mapdata$map_fine <- map_fine # default
  
  ## select map depending on the outcome of action buttons to subset species list
  
  # output$about <- renderUI({
  #   tags$iframe(src="NaVIdetails.html")
  # })
  
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
  # make table reactive that user can select from the table directly the map points
  pot_spec_tab <- reactive({
    region_list_sub <- subdata$region_lists[RegionName%in%input$Kreise_Daten & Art%in%all_pot_spec[[input$Kreise_Daten]], c("Art", "Deutscher Artname", "Gruppe", "Habitateignung")]
    region_list_sub <- region_list_sub[order(region_list_sub$Habitateignung, decreasing=TRUE)]
  })
  output$table <- renderDT({ # render DT table from reactive object
    df <- pot_spec_tab()
    DT::datatable(df)
  }) 
  
  ## for occurring species
  # make table reactive that user can select from the table directly the map points
  occ_spec_tab <- reactive({
    subdata$region_lists[RegionName%in%input$Kreise_Daten & Ist=="x", c("Art","Deutscher Artname", "Gruppe")]
  })
  output$ListeNeobiota <- renderDT({ # render DT table from reactive object
    df <- occ_spec_tab()
    DT::datatable(df)
  })
  
  
  ## prepare data for region download ##########################################
  
  # make reactive that the table can be modified when making a selection
  data_regs <- reactive({
    subdata$region_lists[RegionName%in%input$Kreise_Daten & 
                           Ist=="x", c("RegionName", "Art", "Deutscher Artname", 
                                       "Gruppe")]
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
                           Art%in%all_pot_spec[[input$Kreise_Daten]],c("NAME_2",
                          "Art","Deutscher Artname", "Gruppe","Habitateignung")]
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
  
  
  ## prepare data for species download #########################################
  
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
      mapfiltered <- mapfiltered[which(mapfiltered$NAME_2 == input$Kreise_Daten), ]
      
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
        NotID <- showNotification("Warnung: Grosser Datensatz. Nicht alle Daten werden angezeigt.", 
                                  duration=15)
      }
    } else { # for selections of individual regions select only records within a buffer distance of dist_buff
      
      map_sub <- map_fine[which(map_fine$NAME_2 == input$Kreise_Daten), ] # subset to focal region
      ext <- st_bbox(st_buffer(map_sub,dist_buff)) # identify box around the focal region
      spfiltered <- subset(spfiltered, Laengengrad<ext$xmax & 
                             Laengengrad>ext$xmin & Breitengrad<ext$ymax & 
                             Breitengrad>ext$ymin) # subset to records in the vicinity
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
  
  ## add records of species selected in table of all species ###################
  ## observe input to table of existing neobiota and plot records of selected 
  ## species on map
  
  observe({
    
    selRow <- occ_spec_tab()[input$ListeNeobiota_rows_selected,] # keep row of selection
    # print(selRow$Art)
    
    spfiltered <- point_data[which(point_data$Taxon == selRow$Art), ] # subset records to focal species
    
    uni_col_DB <- unique(cbind.data.frame(spfiltered$Datenbank,pal(spfiltered$DBcols))) # text for legend
    
    ## subset to records in vicinity to increase performance
    map_sub <- map_fine[which(map_fine$NAME_2 == input$Kreise_Daten), ] # subset to region
    ext <- st_bbox(st_buffer(map_sub,dist_buff))
    spfiltered <- subset(spfiltered, Laengengrad<ext$xmax & 
                           Laengengrad>ext$xmin & 
                           Breitengrad<ext$ymax & 
                           Breitengrad>ext$ymin)
    
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
  
  
  ## add records of species selected in table of potential species #############
  ## observe input to table of existing neobiota and plot records of selected 
  ## species on map
  
  observe({
    
    selRow <- pot_spec_tab()[input$table_rows_selected,] # keep row of selection
    # print(selRow$Art)
    
    spfiltered <- point_data[which(point_data$Taxon == selRow$Art), ] # subset records to focal species
    
    uni_col_DB <- unique(cbind.data.frame(spfiltered$Datenbank,pal(spfiltered$DBcols))) # text for legend
    
    ## subset to records in vicinity
    map_sub <- map_fine[which(map_fine$NAME_2 == input$Kreise_Daten), ]
    ext <- st_bbox(st_buffer(map_sub,dist_buff))
    spfiltered <- subset(spfiltered, Laengengrad<ext$xmax & 
                           Laengengrad>ext$xmin & 
                           Breitengrad<ext$ymax & 
                           Breitengrad>ext$ymin)
    
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
        fillOpacity = 0.5,
        fillColor = NA,
        weight = 1,
        radius = 5
      ) #%>%
  })
}
