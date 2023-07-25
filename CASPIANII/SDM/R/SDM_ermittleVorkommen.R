

ermittleVorkommen <- function(TaxonName=NULL,
                              Datenbank=NULL,
                              Ausschnitt=NULL,
                              eigeneDaten=NULL,
                              identifier=NULL,
                              max_limit=20000){
  
  ## load status file for reporting 
  status_species <- read.xlsx(file.path("SDM","Data","Output",paste0("StatusModellierung",identifier,".xlsx",sep="")),sheet=1)
  ind_species <- which(status_species$Taxon==TaxonName)
  
  ######################################################################################
  ## identify pure marine species for which the workflow does not work
  entry <- try(wm_records_names(name = TaxonName,marine_only=F),silent=T)[[1]] # WoRMS
  
  ## check if species is purely marine (x=1), otherwise set x=0
  x <- 0
  if (!any(grepl("Error",entry))){
    if (all(!is.na(entry$isMarine))){
      if (all(entry$isMarine==1)){
        x <- 1 # set x to 1 if habitat is marine
      }
    }
    if (all(!is.na(entry$isTerrestrial))){
      if (all(entry$isTerrestrial==1)){
        x <- 0 # set x to 0 if habitat is terrestrial
      }
    }
    if (all(!is.na(entry$isFreshwater))){
      if (all(entry$isFreshwater==1)){
        x <- 0 # set x to 0 if habitat is freshwater
      }
    }
  }
  
  if (x==1){
    
    cat(paste0("\n*** ",TaxonName," ist eine rein marine Art, fuer die keine Modellierung durchgefuehrt werden kann. ***\n") ) # notification for the user

    ## write status to log file
    status_species$Status[ind_species] <- "Keine Habitatmodellierung, da es sich um eine marine Art handelt."
    
    ## export status of species list
    write.xlsx(status_species,file=file.path("SDM","Data","Output",paste0("StatusModellierung",identifier,".xlsx",sep="")))
    
    return("Keine Daten: Marine species")
    
  } else { # if not a marine species, obtain occurrence data

    ##################################################################################################
    
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
      
      if (nrow(Vorkommen_alle) < 50) { # check, whether the number of occurrences is sufficient

        cat(paste("\nWarnung: Die Anzahl an Datenpunkten ist <50. Es kann keine Habitatmodellierung fuer",TaxonName,"durchgeführt werden.")) 
        
        ## write status to log file
        status_species$Status[ind_species] <- "Nach Bereinigung Datenmenge zu gering zur Habitatmodellierung."
        
        ## export status of species list
        write.xlsx(status_species,file=file.path("SDM","Data","Output",paste0("StatusModellierung",identifier,".xlsx",sep="")))
        
        return("Keine ausreichende Datenmenge zur Habitatmodellierung")
        
      } else {
        
        ## save data to disk
        fwrite(Vorkommen_alle, file.path("SDM","Data","Input",paste0("Vorkommen_",TaxonName,identifier,".csv"))) # stores the final occurrence file on the users computer
        
        cat(paste0("\n Vorkommensdaten wurden als 'Vorkommen_",TaxonName,".csv' im Verzeichnis 'Data/Input' gespeichert.\n") ) # notification for the user
        
        ## write status to log file
        status_species$Status[ind_species] <- "Genügend Daten zur Habitatmodellierung vorhanden"
        
        ## export status of species list
        write.xlsx(status_species,file=file.path("SDM","Data","Output",paste0("StatusModellierung",identifier,".xlsx",sep="")))
        
        return(Vorkommen_alle)
        
      }
    } else {
      
      cat(paste("\nEs liegen keine Daten zum Vorkommen von",TaxonName,"vor. Die Habitatmodellierung kann nicht durchgefuehrt werden.")) 
      
      ## write status to log file
      status_species$Status[ind_species] <- "Nach Bereinigung Datenmenge zu gering zur Habitatmodellierung."
      
      ## export status of species list
      write.xlsx(status_species,file=file.path("SDM","Data","Output",paste0("StatusModellierung",identifier,".xlsx",sep="")))
      
      return("Keine ausreichende Datenmenge zur Habitatmodellierung")
      
    }
  }
}

