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
# Author: Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 04.02.26
################################################################################




server <- function(input, output, session){
  
  ## prepare data and maps depending on selections #############################

  ## function to subset species groups
  filter_species <- function(data, artlisten, artengruppe) {
    
    if (artlisten == "Unionsliste") {
      data <- data[EU_Anliegen == "x"]
    } else if (artlisten != "Alle Neobiota") {
      data <- data[BfNlisten %in% artlisten]
    }
    
    if (artengruppe != "Alle Taxa") {
      data <- data[Artengruppe %in% artengruppe]
    }
    data
  }
  
  ## apply species function after input
  filtered_data <- eventReactive(
    list(input$Artlisten, input$Artengruppe),
    {
      filter_species(all_reg_data, input$Artlisten, input$Artengruppe)
    }
  )
  
  ## create filtered map 
  filtered_map <- reactive({
    data_out <- filter_species(all_reg_data, input$Artlisten, input$Artengruppe)
    data_out <- data_out[Status == "Ist"]
    
    nSpec_reg <- data_out[, .N, by=CC_2]
    nSpec_reg$CC_2 <- as.integer(nSpec_reg$CC_2)
    
    spatial_out <- merge(regions, nSpec_reg, by="CC_2", all=T)
    
    if (input$Kreise_Daten!="Alle Kreise"){
      spatial_out <- spatial_out[which(spatial_out$RegionName == input$Kreise_Daten), ]
    }
    spatial_out
  })
  
  
  ## function to subset regions
  filter_region <- function(data, region) {
    if (region == "Alle Kreise") {
      data
    } else {
      data[RegionName == region]
    }
  }
  
  ## load data points depending on species selection only to improve performance
  observe({
    updateSelectizeInput(
      session,
      inputId = "Art_Daten",
      choices = sort(unique(all_reg_data$Taxon)),
      server = TRUE
    )
  })
  

  ## prepare (render) the output table to make them interactive ###########
  
  ## for potentially establishing species ##########
  
  # make table reactive that user can select from the table directly the map points
  pot_spec_tab <- reactive({
    reg_data <- filter_region(filtered_data(), input$Kreise_Daten)
    
    pot_taxa <- all_reg_data[
      Status == "Pot" & RegionName == input$Kreise_Daten,
      unique(Taxon)
    ]
    
    out <- reg_data[Taxon %in% pot_taxa,
                    c("Taxon","Deutscher Artname","Artengruppe","Invasionspotenzial")]
    
    out[order(Invasionspotenzial, decreasing = TRUE)]
  })
  
  ## render DT table from reactive object
  output$table <- renderDT({ 
    df <- pot_spec_tab()
    DT::datatable(df)
  }) 
  
  ## for actually occurring species #############
  
  # make table reactive that user can select from the table directly the map points
  occ_spec_tab <- reactive({
    filter_region(filtered_data(), input$Kreise_Daten)[
      Status == "Ist",
      c("Taxon","Deutscher Artname","Artengruppe")
    ]
  })
  
  ## render DT table from reactive object
  output$ListeNeobiota <- renderDT({ 
    df <- occ_spec_tab()
    DT::datatable(df)
  })
  
  
  ## prepare data for region download ##########################################
  
  # make reactive that the table can be modified when making a selection
  data_regs <- reactive({
    filter_region(filtered_data(), input$Kreise_Daten)[
      Status == "Ist",
      c("RegionName","Taxon","Deutscher Artname","Artengruppe")
    ]
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
  
  ## make point dataset reactive
  data_spec <- reactive({ 
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
      pal_nSpec <- colorBin("Spectral", domain = filtered_map()$N, bins = 5, right=TRUE, reverse=TRUE)
      
      labels <- sprintf("%s: %s", filtered_map()$RegionName, filtered_map()$N) %>% 
        lapply(htmltools::HTML)
      
      leaflet(filtered_map()) %>%
        addWMSTiles(
          baseUrl = "https://sgx.geodatenzentrum.de/wms_topplus_open?",
          layers = "web_light", group = "TopPlusOpen",
          options = WMSTileOptions(format = "image/png",
                                   transparent = TRUE),
          attribution = paste0("BKG (", strftime(Sys.Date(), "%Y"),
                               "), <a href=\"https://sgx.geodatenzentrum",
                               ".de/web_public/gdz/datenquellen/Datenque",
                               "llen_TopPlusOpen.html\">",
                               "data source"), "</a>") %>%
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
          opacity = 1, 
          title = "Anzahl Arten",
          na.label="Keine Arten")
    } else { # if a region has been selected
      leaflet(filtered_map()) %>%
        addWMSTiles(
          baseUrl = "https://sgx.geodatenzentrum.de/wms_topplus_open?",
          layers = "web_light", group = "TopPlusOpen",
          options = WMSTileOptions(format = "image/png",
                                   transparent = TRUE),
          attribution = paste0("BKG (", strftime(Sys.Date(), "%Y"),
                               "), <a href=\"https://sgx.geodatenzentrum",
                               ".de/web_public/gdz/datenquellen/Datenque",
                               "llen_TopPlusOpen.html\">",
                               "data source"), "</a>") %>%
        addPolygons(
          fillColor = "orange",
          color = "darkgrey",
          weight=1,
          dashArray = "1",
          fillOpacity = 0.9) #%>%
    }
  })
  
  
  ### add points of species occurrences depending on dropdown menu #################

  ## basic function for plotting
  plot_species_points <- function(taxon) {
    
    ## return nothing if no taxon and no region seleted
    if (is.null(taxon) || all(taxon == "Keine") || input$Kreise_Daten=="Alle Kreise") return(NULL)
    
    spfiltered <- point_data[Taxon %in% taxon]
    
    if (input$Kreise_Daten != "Alle Kreise") {
      map_sub <- subset(regions, RegionName == input$Kreise_Daten) 
      ext <- st_bbox(st_buffer(map_sub, dist_buff))
      spfiltered <- spfiltered[
        Laengengrad > ext$xmin & Laengengrad < ext$xmax &
          Breitengrad  > ext$ymin & Breitengrad  < ext$ymax
      ]
    }
    
    if (nrow(spfiltered) > 10000) {
      spfiltered <- spfiltered[sample(.N, 10000)]
      showNotification("Warnung: Großer Datensatz.", duration = 15)
    }
    
    leafletProxy("map") %>%
      clearGroup("occurrences") %>%
      addCircleMarkers(
        data = spfiltered,
        lng = ~Laengengrad,
        lat = ~Breitengrad,
        group = "occurrences",
        radius = 5,
        fillOpacity = 0.5
      )
  }
  
  ## define points to add to plot
  observeEvent(input$Art_Daten, {
    req(input$Art_Daten != "Keine")
    plot_species_points(input$Art_Daten)
  })
  
  observeEvent(input$ListeNeobiota_rows_selected, {
    sel <- occ_spec_tab()[input$ListeNeobiota_rows_selected]
    plot_species_points(sel$Taxon)
  })
  
  observeEvent(input$table_rows_selected, {
    sel <- pot_spec_tab()[input$table_rows_selected]
    plot_species_points(sel$Taxon)
  })
}
