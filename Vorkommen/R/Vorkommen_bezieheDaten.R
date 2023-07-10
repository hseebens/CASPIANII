##################### Get occurrence records ########################################################
# 
# This scripts returns the occurrence records of the requested taxon available at GBIF, sMon,
# iNaturalist and OBIS in any combination if specified in option 'database' and in 'EigeneDaten_Dateiname'. 
# Available records are standardised, merged and returned.
# 
# databases: GBIF, iNat, sMon, OBIS, NA (if NA, no records are extracted and only EigeneDaten_Dateiname used)
#
# Minimum required input: TaxonName (name of taxon); database or EigeneDaten_Dateiname
# Function output: List of coordinates and date of record
#
# Project: CASPIAN II
# 
# Senckenberg Gesellschaft fuer Naturforschung, 28.11.22
###############################################################################################################


Vorkommen_bezieheDaten <- function(TaxonName=TaxonName,
                                   EigeneDaten_Verzeichnis=NA,
                                   EigeneDaten_Dateiname=NA,
                                   Datenbank=NA,
                                   sMon_Verzeichnis=NA,
                                   max_limit=10000,
                                   Ausschnitt=NULL){
  
  ## get occurrence records from individual sources ##############################
  
  all_records <- list()
  x <- 0
  
  if (is.null(Ausschnitt)){
    Ausschnitt <- "POLYGON ((5 45, 15 45, 15 58, 5 58, 5 45))"
  } else {
    Ausschnitt <- paste("POLYGON ((",Ausschnitt[1],Ausschnitt[2],",",
                                      Ausschnitt[3], Ausschnitt[2],",", 
                                      Ausschnitt[3], Ausschnitt[4],",",
                                      Ausschnitt[1], Ausschnitt[4],",", 
                                      Ausschnitt[1], Ausschnitt[2],
                                      "))")
  }
  
  if (all(is.na(Datenbank)) & length(EigeneDaten_Dateiname)==1){
    
    warning("Keine Datenbank ausgewaehlt oder eigene Daten bereit gestellt.")
    
  }
  
  ## sMon occurrences ###############################################################
  
  if ("sMon"%in%Datenbank){
    
    cat("\n** Bearbeitung von sMon ******************\n")

    if (is.na(sMon_Verzeichnis)){
      
      warning("Kein Verzeichnis fuer sMon Daten angegeben. Bitte als Variable 'sMon_Verzeichnis' angeben.")
      
    } else {
        
      sMon_taxa <- fread(file.path("WP1","Data","AlienSpecies_in_sMon.csv"))
      
      if (TaxonName%in%sMon_taxa$Taxon){ # check if TaxonName is a synonym in sMon
        
        # cat(paste(TaxonName,"found in sMon database as",sMon_taxa$Taxon_orig[sMon_taxa$Taxon==TaxonName],"\n"))
        cat(paste(TaxonName,"in sMon Datenbank gefunden als",sMon_taxa$Taxon_orig[sMon_taxa$Taxon==TaxonName],"\n"))
        
        TaxonName_new <- sMon_taxa$Taxon_orig[sMon_taxa$Taxon==TaxonName]
        
        ## get occurrence records #####
        occ_dat <- get_sMon_occurrences(TaxonName_new,sMon_Verzeichnis)
        
        ## prepare output #############
        occ_dat <- occ_dat[,c("TaxonName","Longitude","Latitude","Period")]
        occ_dat$Datenbank <- "sMon"
        
        colnames(occ_dat) <- c("Taxon","Laengengrad","Breitengrad","Zeitpunkt","Datenbank")
        
        x <- x + 1
        all_records[[x]] <- occ_dat
        
      } else if (TaxonName%in%sMon_taxa$Taxon_orig){ # take direct match
        
        # cat(paste(TaxonName,"found in sMon database"))
        cat(paste(TaxonName,"in sMon Datenbank gefunden"))
        
        ## get occurrence records #####
        occ_dat <- get_sMon_occurrences(TaxonName,sMon_Verzeichnis)
        
        ## prepare output #############
        occ_dat <- occ_dat[,c("TaxonName","Longitude","Latitude","Period")]
        occ_dat$Datenbank <- "sMon"
        
        colnames(occ_dat) <- c("Taxon","Laengengrad","Breitengrad","Zeitpunkt","Datenbank")
        
        x <- x + 1
        all_records[[x]] <- occ_dat
      } 

      if (x==0){
        # cat(paste("No records found in sMon for",TaxonName,"\n"))
        cat(paste("Keine Eintraege in sMon fuer",TaxonName,"\n"))
      } else {
        
        cat(paste(nrow(occ_dat),"Eintraege von",TaxonName,"in sMon gefunden\n"))
        
      }
    }
  } 

  ## GBIF occurrences ###################################################################
    
  if ("GBIF"%in%Datenbank){
    
    cat("\n** Bearbeitung von GBIF ******************\n")
    
    xx <- name_backbone(name=TaxonName)$usageKey
    nrecords <- occ_count(xx,georeferenced=T,country="DE") # number of available records
    
    if (nrecords>max_limit){
      # warning(paste0("\nNumber of available records (n=",nrecords,") exceeds limit (",max_limit,")!\n You may either increase limit or download from website."))
      warning(paste0("\nAnzahl der verfuegbaren Eintraege (n=",nrecords,") ueberschreitet Limit (",max_limit,")!\n Entweder sollte max_limit erhoeht werden (Warnung: iNaturalist erlaubt keine hohen downloads) oder Daten sollten direkt von Webseite geladen werden."))
    }
    
    occ_dat <- occ_data(scientificName = TaxonName,geometry = Ausschnitt,hasCoordinate=T,limit=max_limit)[[2]]
    

    ## prepare output #############
    if (!is.null(occ_dat)){
        
      cat(paste(TaxonName,"in GBIF gefunden als",unique(occ_dat$scientificName),"\n"))
      
      if (nrow(occ_dat)==max_limit){
        # cat("Maximum limit of records per GBIF request reached. Either increase \n the limit within this function or download directly from GBIF.")
        cat("\nMaximum Limit von Eintraegen fuer GBIF Anfrage erreicht. Entweder Limit (max_limit) erhoehen oder Daten direkt von der GBIF Webseite laden.\n")
      }
      
      cat(paste(nrow(occ_dat),"Eintraege von",TaxonName,"in GBIF gefunden\n"))

      if ("eventDate"%in%colnames(occ_dat)){
        occ_dat <- occ_dat[,c("species","decimalLongitude","decimalLatitude","eventDate")]
      } else {
        occ_dat <- occ_dat[,c("species","decimalLongitude","decimalLatitude")]
        occ_dat$eventDate <- NA
      }
      occ_dat$Datenbank <- "GBIF"
      
      colnames(occ_dat) <- c("Taxon","Laengengrad","Breitengrad","Zeitpunkt","Datenbank")
      
      x <- x + 1
      all_records[[x]] <- as.data.table(occ_dat)
      
    } else { ## no output
      # cat(paste("No records found for",TaxonName,"in GBIF\n"))
      cat(paste("Keine Eintraege in GBIF fuer",TaxonName,"\n"))
    }
  } 
  
  ## iNaturalist occurrences ############################################################
  
  if ("iNat"%in%Datenbank){
    
    # cat("\n** Working on iNaturalist ****************** \n")
    cat("\n** Bearbeitung von iNaturalist ****************** \n")
    
    out <- occ(query = TaxonName, from = c('inat')
               # ,ebirdopts = ebirdopts
               ,geometry = Ausschnitt # bounding box of Germany roughly
               ,limit = max_limit
               ,has_coords=T
    )
    occ_dat <- occ2df(out)
    
    if (nrow(occ_dat)>0){
      
      cat(paste(nrow(occ_dat),"Eintraege von",TaxonName,"in iNaturalist gefunden\n"))
      
      ## prepare output #############
      occ_dat <- occ_dat[,c("name","longitude","latitude","date")]
      occ_dat$Datenbank <- "iNat"
      occ_dat$date <- as.character(occ_dat$date)
      
      colnames(occ_dat) <- c("Taxon","Laengengrad","Breitengrad","Zeitpunkt","Datenbank")
      
      x <- x + 1
      all_records[[x]] <- as.data.table(occ_dat)
      
    } else {
      # cat(paste("No records found in iNaturalist for",TaxonName,"\n"))
      cat(paste("Keine Eintraege in iNaturalist fuer",TaxonName,"\n"))
    }
  }
  
  ## OBIS occurrences ############################################################
  
  if ("OBIS"%in%Datenbank){
    
    # cat("\n** Working on OBIS ****************** \n")
    cat("\n** Bearbeitung von OBIS ****************** \n")
    
    occ_dat <- robis::occurrence(scientificname = TaxonName
               ,geometry = Ausschnitt # bounding box of Germany roughly
               # ,limit = max_limit
               # ,has_coords=T
    )
    
    if (!is.null(nrow(occ_dat))) {
      
      # occ_dat <- occ2df(out)
      
      if (nrow(occ_dat)>0){
        
        # cat(paste(nrow(occ_dat),"records of",TaxonName,"found in OBIS\n"))
        cat(paste(nrow(occ_dat),"Eintraege von",TaxonName,"in OBIS gefunden\n"))
        
        ## prepare output #############
        if ("date"%in%colnames(occ_dat)){
          occ_dat <- occ_dat[,c("scientificName","decimalLongitude","decimalLatitude","date_start")]
        } else {
          occ_dat <- occ_dat[,c("scientificName","decimalLongitude","decimalLatitude")]
          occ_dat$date <- NA
        }
        occ_dat$Datenbank <- "OBIS"
        occ_dat$date <- as.character(occ_dat$date)
        
        colnames(occ_dat) <- c("Taxon","Laengengrad","Breitengrad","Zeitpunkt","Datenbank")
        
        x <- x + 1
        all_records[[x]] <- as.data.table(occ_dat)
        
      } else {
        # cat(paste("No records found in OBIS for",TaxonName,"\n"))
        cat(paste("Keine Eintraege in OBIS fuer",TaxonName,"\n"))
      }
    }
  }
  
  ## add own records ####################################################################
  
  if (!is.na(EigeneDaten_Dateiname) & !is.na(EigeneDaten_Verzeichnis)){
    
    own_records <- fread(file.path(paste0(EigeneDaten_Verzeichnis,EigeneDaten_Dateiname)))
    
    if (nrow(own_records)>0){
      
      if (any(!c("Taxon","Laengengrad","Breitengrad","Zeitpunkt")%in%colnames(own_records))){
        # stop("Column names in 'own_records' not correct. Please change to Taxon, Longitude, Latitude, Period and Database.")
        stop(paste0("Spaltennamen in ",EigeneDaten_Dateiname," nicht korrekt. Bitte aendern in Taxon, Laengengrad, Breitengrad und Zeitpunkt."))
      }
      
      own_records$Datenbank <- "Eigene Daten"
      
      x <- x + 1
      
      all_records[[x]] <- as.data.table(own_records)
    }
  }
  
  ## output ##############################################################################
  
  all_output <- rbindlist(all_records,use.names = T,fill=T)
  all_output$Laengengrad <- as.numeric(all_output$Laengengrad)
  all_output$Breitengrad <- as.numeric(all_output$Breitengrad)
  
  if (!is.null(all_output)){
    
    if (nrow(all_output) < 100) { # check, whether the number of occurrences is sufficient
      warning("\nWarnung: Die Anzahl an Datenpunkten ist <100. SDMs können unzuverlässige Ergebnisse liefern.") 
    } 
    
    ## save data to disk
    fwrite(all_output, file.path("WP1","Data",paste0("Vorkommen_",TaxonName,".csv"))) # stores the final occurrence file on the users computer
    
    cat(paste0("\nVorkommensdaten wurden als 'Vorkommen_",TaxonName,".csv' im Verzeichnis 'WP1/Data' gespeichert.\n") ) # notification for the user
    
    return(all_output)
    
  } else {
    
    cat(paste("\nKeine ausreichenden Eintraege fuer",TaxonName,"gefunden.\n")) 
    
  }
}

