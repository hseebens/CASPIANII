################################################################################
#
# Shiny App NaVI - Funktion 'server'
#
# Die server Funktion stellt das 'backend' der Shiny App dar und liefert die 
# Inhalte, die in der Shiny App zur Verfuegung gestellt werden.
#
# Die Daten, die in der Shiny App zur Verfügung gestellt werden, werden mit der 
# Funktion 'DataPreparationShiny.R' aufbereitet.
#
# Author: Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 10.12.25
################################################################################




server <- function(input, output){
  
  ## prepare data and maps depending on selections #############################

  ## select data depending on the selection of species lists and taxon group
  
  filtered_data <- reactive({
    
    if (input$Artlisten=="Unionsliste"){
      
      data_sub <- subset(all_reg_data, EU_Anliegen=="x")
      
      if (input$Artengruppe=="Alle Taxa"){
        data_out <- data_sub
      } else {
        data_out <- subset(data_sub, Artengruppe %in% input$Artengruppe)
      }
      
    } else {
      
      data_sub <- all_reg_data
      
      if (input$Artlisten=="Alle Neobiota" & input$Artengruppe=="Alle Taxa"){
        data_out <- data_sub
      } else if (input$Artlisten=="Alle Neobiota" & input$Artengruppe!="Alle Taxa"){
        data_out <- subset(data_sub, Artengruppe %in% input$Artengruppe)
      } else if (input$Artlisten!="Alle Neobiota" & input$Artengruppe=="Alle Taxa") {
        data_out <- subset(data_sub, BfNlisten %in% input$Artlisten)
      } else {
        data_out <- subset(data_sub, BfNlisten %in% input$Artlisten & Artengruppe %in% input$Artengruppe)
      }
    }
    data_out
  })
  
  
  ## create download
  output$download_reg_list <- downloadHandler(
    filename = function() {
      paste0("Liste_Neobiota_", input$Kreise_Daten, "_",input$Artlisten, "_", input$Artengruppe, ".csv")
    },
    content = function(file) {
      write.table(filtered_data(), file)
    }
  )
  
  
  ## select map depending on the selection of species lists and taxon group
  
  filtered_map <- reactive({
    
    if (input$Artlisten=="Unionsliste"){ # 'Unionsliste' has to treated differently as it is a different column than other lists
      
      data_sub <- subset(all_reg_data, EU_Anliegen=="x")
      
      if (input$Artengruppe=="Alle Taxa"){
        data_out <- data_sub
      } else {
        data_out <- subset(data_sub, Artengruppe %in% input$Artengruppe)
      }
      
    } else {
      
      data_sub <- all_reg_data
      
      if (input$Artlisten=="Alle Neobiota" & input$Artengruppe=="Alle Taxa"){
        data_out <- data_sub
      } else if (input$Artlisten=="Alle Neobiota" & input$Artengruppe!="Alle Taxa"){
        data_out <- subset(data_sub, Artengruppe %in% input$Artengruppe)
      } else if (input$Artlisten!="Alle Neobiota" & input$Artengruppe=="Alle Taxa") {
        data_out <- subset(data_sub, BfNlisten %in% input$Artlisten)
      } else {
        data_out <- subset(data_sub, BfNlisten %in% input$Artlisten & Artengruppe %in% input$Artengruppe)
      }
    }
    data_out <- subset(data_out, Status=="Ist")
    
    nSpec_reg <- data_out[, .N, by=CC_2]
    nSpec_reg$CC_2 <- as.integer(nSpec_reg$CC_2)
    
    spatial_out <- merge(regions, nSpec_reg, by="CC_2", all=T)

    if (input$Kreise_Daten!="Alle Kreise"){
      
      spatial_out <- spatial_out[which(spatial_out$RegionName == input$Kreise_Daten), ]
      
    }
    spatial_out
  })
  

  
    
  ## prepare (render) the output table to make them interactive ###########
  
  ## for potentially establishing species
  # make table reactive that user can select from the table directly the map points
  pot_spec_tab <- reactive({
    region_list_sub <- filtered_data()[RegionName %in% input$Kreise_Daten & 
                                        Taxon %in% subset(all_reg_data, Status=="Pot" & 
                                        RegionName==input$Kreise_Daten)$Taxon, 
                                       c("Taxon", "Deutscher Artname", "Artengruppe", "Invasionspotenzial")]
    region_list_sub <- region_list_sub[order(region_list_sub$Invasionspotenzial, decreasing=TRUE)]
  })
  
  output$table <- renderDT({ # render DT table from reactive object
    df <- pot_spec_tab()
    DT::datatable(df)
  }) 
  
  ## for actually occurring species
  # make table reactive that user can select from the table directly the map points
  occ_spec_tab <- reactive({
    filtered_data()[RegionName%in%input$Kreise_Daten & Status=="Ist", 
                    c("Taxon","Deutscher Artname", "Artengruppe")]
  })
  
  output$ListeNeobiota <- renderDT({ # render DT table from reactive object
    df <- occ_spec_tab()
    DT::datatable(df)
  })
  
  
  ## prepare data for region download ##########################################
  
  # make reactive that the table can be modified when making a selection
  data_regs <- reactive({
    filtered_data()[RegionName%in%input$Kreise_Daten & 
                           Status=="Ist", c("RegionName", "Taxon", "Deutscher Artname", 
                                       "Artengruppe")]
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
    filtered_data()[RegionName%in%input$Kreise_Daten & 
                           Taxon%in%subset(all_reg_data, Status=="Pot" & 
                                             RegionName==input$Kreise_Daten)$Taxon
                    ,c("RegionName","Taxon","Deutscher Artname", "Artengruppe","Invasionspotenzial")]
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
  
  
  ## prepare occurrence data for species download #########################################
  
  data_spec <- reactive({ # make reactive
    subset(point_data, Taxon==input$Art_Daten)
  })

  ## add to download
  output$download_spec <- downloadHandler(
    filename = function() {
      paste0(input$Art_Daten, ".csv")
    },
    content = function(file) {
      write.table(data_spec(), file)
    }
  )
  
  
  ##############################################################################
  ## generate map and data points depending on selections ######################
  ##############################################################################

  ## render main map ########################
  
  output$map <- renderLeaflet({

    ## plot the base map
    if (input$Kreise_Daten == "Alle Kreise"){ # if no region is selected
      
      ## create colours for legend
      pal_nSpec <- colorBin("YlOrRd", domain = filtered_map()$N, bins = 7)
      
      labels <- sprintf("%s: %s", filtered_map()$RegionName, filtered_map()$N) %>% 
        lapply(htmltools::HTML)
      
      leaflet(filtered_map()) %>%
        addTiles() %>%
        addPolygons( # polygons of species numbers 
          fillColor = ~ pal_nSpec(filtered_map()$N),
          stroke=TRUE,
          color = "darkgrey",
          weight=1,
          dashArray = "1",
          fillOpacity = 0.9,
          label = labels) %>%
        leaflet::addLegend(
          pal = pal_nSpec, 
          values = ~N,
          opacity = 0.7, 
          title = "Anzahl Arten",
          na.label="Keine Arten")
    } else { # if a region has been selected
      leaflet(filtered_map()) %>%
        addTiles() %>%
        addPolygons(
          fillColor = "orange",
          color = "darkgrey",
          weight=1,
          dashArray = "1",
          fillOpacity = 0.9) #%>%
    }
  })
  
  
  ## add points of species occurrences depending on dropdown menu #################
  
  observeEvent(input$Art_Daten, { # observe whether a species has been selected
    
    spfiltered <- point_data[which(point_data$Taxon == input$Art_Daten), ] # subset to focal species
 
    ## select subset of records to increase performance
    if(input$Kreise_Daten == "Alle Kreise"){ # for Germany-wide presentation
      if (nrow(spfiltered)>10000){ # select only a random set of 10000 records
        
        spfiltered <- spfiltered[sample(1:nrow(spfiltered),10000)]
        
        ## produce a warning that not all records are shown
        NotID <- showNotification("Warnung: Grosser Datensatz. Nicht alle Daten werden angezeigt.", 
                                  duration=15)
      }
    } else { # for selections of individual regions select only records within a buffer distance of dist_buff
      
      map_sub <- regions[which(regions$RegionName == input$Kreise_Daten), ] # subset to focal region
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
        fillOpacity = 0.5,
        fillColor = NA,
        weight = 1,
        radius = 5
      ) 
  })
  
  ## add occurrences of species depending on table selection #################
  ## observe input to table of existing neobiota and plot records of selected 
  ## species on map
  
  observe({
    
    selRow <- occ_spec_tab()[input$ListeNeobiota_rows_selected,] # keep row of selection
    # print(selRow$Taxon)
    
    if (is.null(selRow)) return()  # nothing selected yet
    
    spfiltered <- point_data[which(point_data$Taxon == selRow$Taxon), ] # subset records to focal species
    

    ## subset to records in vicinity to increase performance
    map_sub <- regions[which(regions$RegionName == input$Kreise_Daten), ] # subset to region
    ext <- st_bbox(st_buffer(map_sub ,dist_buff))
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
        fillOpacity = 0.5,
        fillColor = NA,
        weight = 1,
        radius = 5
      )
  })
  
  
  ## add potential occurrences of species depending on table selection #################
  ## observe input to table of existing neobiota and plot records of selected 
  ## species on map
  
  observe({
    
    selRow <- pot_spec_tab()[input$table_rows_selected,] # keep row of selection
    # print(selRow$Taxon)
    
    spfiltered <- point_data[which(point_data$Taxon == selRow$Taxon), ] # subset records to focal species
    
    ## subset to records in vicinity
    map_sub <- regions[which(regions$RegionName == input$Kreise_Daten), ]
    ext <- st_bbox(st_buffer(map_sub, dist_buff))
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
      ) 
  })
}
