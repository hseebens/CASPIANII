

ermittle_vorkommen <- function(TaxonName=TaxonName,
                               Datenbank=NULL,
                               Ausschnitt=NULL,
                               eigeneDaten=NULL,
                               identifier=identifier,
                               plot_map=T){
  
  cat(paste0("\n*** Ermittle Vorkommensdaten für ",TaxonName," *** \n") ) # notification for the user
  
  Vorkommen_alle <- list()
  x <- 0
  if (!is.null(eigeneDaten)){
    if (!any(c( "Taxon", "Laengengrad", "Breitengrad")%in%colnames(eigeneDaten))){
      
      stop("Spalten fehlen! Bitte ergänzen.")
      
    } else {
      
      eigeneDaten <- eigeneDaten[,c("Taxon", "Laengengrad", "Breitengrad")]
      eigeneDaten$Vorkommen <- 1
      eigeneDaten$Zeitpunkt <- NA
      eigeneDaten$Datenbank <- "eigene Daten"
      
      x <- x + 1
      
      Vorkommen_alle[[x]] <- eigeneDaten
    }
  }
  
  if (!is.null(Datenbank)){
    
    cat("\nDer Download von Daten kann einige Minuten in Anspruch nehmen.\n") # notification for user
    
    Vorkommen <- suppressMessages(suppressWarnings(get_occurrence_records(TaxonName=TaxonName,Datenbank=Datenbank,Ausschnitt=Ausschnitt)))
    
    
    ## clean records
    cat("\n Bereinigen der Vorkommensdaten...\n")
    
    country_borders <- readOGR(dsn=file.path("Data","Input","Shapefiles"),layer="ne_50m_land",stringsAsFactors=F)
    Vorkommen_cleaned <- suppressMessages(suppressWarnings(clean_coordinates(Vorkommen, lon = "Laengengrad",
                                                                    lat = "Breitengrad",
                                                                    value ="clean",
                                                                    # countries = "countryCode",
                                                                    species = "Taxon",
                                                                    country_ref=country_borders
                                                                    ))) 
    
    ## prepare output
    x <- x + 1
    Vorkommen_alle[[x]] <- Vorkommen_cleaned
    
  }
  
  Vorkommen_alle <- do.call("rbind",Vorkommen_alle)
  
  
  if (nrow(Vorkommen_alle) < 100) { # check, whether the number of occurrences is sufficient
    # print("NOTE: The number of occurrences is < 50. SDMs might not yield reliable results.") }
    warning("\nWarnung: Die Anzahl an Datenpunkten ist <100. SDMs können unzuverlässige Ergebnisse liefern.") 
  } 
  
  ## Plot records on a map 
  # if (plot_map){
  # 
  #   records <- Vorkommen_alle
  #   records$col <- as.numeric(as.factor(records$Datenbank))
  #   all_cols <- c('red', 'orange','black', 'blue')
  #   all_DBs <- sort(unique(records$Datenbank))
  #   
  #   pal <- colorFactor(
  #     palette = all_cols,
  #     domain = records$col
  #   )
  #   
  #   uni_col_DB <- unique(cbind.data.frame(records$Datenbank,pal(records$col)))
  # 
  #   map_records <- leaflet(records) %>% addTiles() %>% 
  #     clearBounds() %>%
  #     addCircleMarkers(lng=~Laengengrad, lat=~Breitengrad,radius = 2, weight = 1,color=~pal(col),fillOpacity = 0.5) %>%
  #     addLegend("bottomright", colors=uni_col_DB[,2] ,labels=uni_col_DB[,1],
  #               title = "Datenbank",
  #               opacity = 1
  #     )
  #   map_records
  # }


  ## save data to disk
  fwrite(Vorkommen_alle, file.path("Data","Input",paste0("Vorkommen_",TaxonName,"_",identifier,".csv"))) # stores the final occurrence file on the users computer

  cat(paste0("\nVorkommensdaten wurden als 'Vorkommen_",TaxonName,".csv' im Verzeichnis 'Data/Input' gespeichert.\n") ) # notification for the user
  
  return(Vorkommen_alle)
}

