

ermittleVorkommen <- function(TaxonName=TaxonName,
                               Datenbank=NULL,
                               Ausschnitt=NULL,
                               eigeneDaten=NULL,
                               identifier=identifier,
                               max_limit=20000){
  
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
    
    cat("\n Der Download von Daten kann einige Minuten in Anspruch nehmen.\n") # notification for user
    
    Vorkommen <- suppressMessages(suppressWarnings(
      sammleVorkommenOnline(TaxonName=TaxonName,
                            Datenbank=Datenbank,
                            Ausschnitt=Ausschnitt,
                            max_limit=max_limit)))
    
    # remove wrong coordinates
    ind <- (Vorkommen$Laengengrad>90 | Vorkommen$Laengengrad< -90) |  (Vorkommen$Breitengrad>180 | Vorkommen$Breitengrad< -180)
    Vorkommen <- Vorkommen[!ind,]
    
    # remove inprecise coordinates
    ind <- nchar(sub('[0-9]+\\.', '', Vorkommen$Laengengrad))<3
    Vorkommen <- Vorkommen[!ind,]
    ind <- nchar(sub('[0-9]+\\.', '', Vorkommen$Breitengrad))<3
    Vorkommen <- Vorkommen[!ind,]
    
    if (nrow(Vorkommen)>0){
      
      ## clean records
      cat("\n Bereinigen der Vorkommensdaten...\n")
      
      country_borders <- st_read(dsn=file.path("SDM","Data","Input","Shapefiles"),layer="ne_50m_land",quiet=T)
      Vorkommen_cleaned <- suppressMessages(suppressWarnings(clean_coordinates(Vorkommen, lon = "Laengengrad",
                                                                               lat = "Breitengrad",
                                                                               value ="clean",
                                                                               # countries = "countryCode",
                                                                               species = "Taxon",
                                                                               tests = c( "centroids", "equal", "gbif", "institutions", "outliers",
                                                                                         "zeros"), # remove "capitals" and "seas"
                                                                               country_ref=country_borders
      ))) 
      
      ## prepare output
      x <- x + 1
      Vorkommen_alle[[x]] <- Vorkommen_cleaned
    }
  }
  
  Vorkommen_alle <- do.call("rbind",Vorkommen_alle)
  
  if (!is.null(Vorkommen_alle)){
    
    if (nrow(Vorkommen_alle) < 100) { # check, whether the number of occurrences is sufficient
      # print("NOTE: The number of occurrences is < 50. SDMs might not yield reliable results.") }
      warning("\nWarnung: Die Anzahl an Datenpunkten ist <100. SDMs können unzuverlässige Ergebnisse liefern.") 
    } 

    ## save data to disk
    fwrite(Vorkommen_alle, file.path("SDM","Data","Input",paste0("Vorkommen_",TaxonName,"_",identifier,".csv"))) # stores the final occurrence file on the users computer
    
    cat(paste0("\n Vorkommensdaten wurden als 'Vorkommen_",TaxonName,".csv' im Verzeichnis 'Data/Input' gespeichert.\n") ) # notification for the user
    
    return(Vorkommen_alle)
    
  } else {
    
    cat(paste("\n Nach Bereinigung keine ausreichenden Eintraege fuer",TaxonName,"vorhanden.\n")) 
    
  }
}

