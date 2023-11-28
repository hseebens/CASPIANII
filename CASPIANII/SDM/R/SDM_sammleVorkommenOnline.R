##################### Get occurrence records ########################################################
# 
#
# Project: CASPIAN II
# 
# Senckenberg Gesellschaft fuer Naturforschung, 28.11.22
###############################################################################################################


sammleVorkommenOnline <- function(TaxonName=TaxonName,
                                   Datenbank=NA,
                                   max_limit=max_limit,
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
  
  if (all(is.na(Datenbank))){
    
    stop("Keine Datenbank ausgewaehlt.")
    
  }
  

  ## GBIF occurrences ###################################################################
  
  if ("GBIF"%in%Datenbank){
    
    cat("\n** Bearbeitung von GBIF ******************\n")
    
    taxonKey <- name_backbone(name=TaxonName)$usageKey
    nrecords <- occ_search(taxonKey=taxonKey, limit=0, country="DE", hasCoordinate=TRUE)$meta$count # number of available records
    
    if (nrecords>max_limit){
      
      # warning(paste0("\nNumber of available records (n=",nrecords,") exceeds limit (",max_limit,")!\n You may either increase limit or download from website."))
      cat(paste0("\n Anzahl der verfuegbaren Eintraege in GBIF (n=",nrecords,") ueberschreitet Limit (n=",max_limit,"). Vorkommensdaten sollten mit alternativen Weg mit 'SDM_bezieheHoheDatenmengen.R' oder Daten direkt von Webseite heruntergeladen werden."))
      
    } else {
      
      occ_dat <- try(occ_data(scientificName = TaxonName,geometry = Ausschnitt,hasCoordinate=TRUE,limit=max_limit)[[2]], silent=TRUE)
      
      
      ## prepare output #############
      if (!is.null(occ_dat)){
        
        cat(paste(" ",TaxonName,"in GBIF gefunden als",unique(occ_dat$scientificName),"\n"))
        
        if (nrow(occ_dat)==max_limit){
          # cat("Maximum limit of records per GBIF request reached. Either increase \n the limit within this function or download directly from GBIF.")
          cat("\n Maximum Limit von Eintraegen fuer GBIF Anfrage erreicht. Entweder Limit (max_limit) erhoehen oder Daten direkt von der GBIF Webseite laden.\n")
        }
        
        cat(paste(" ",nrow(occ_dat),"Eintraege von",TaxonName,"in GBIF gefunden.\n"))
        
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
        cat(paste(" Keine Eintraege in GBIF fuer",TaxonName,"\n"))
      }
      
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
    occ_dat <- subset(occ_dat,name==TaxonName)
    
    if (nrow(occ_dat)>0){
      
      cat(paste(" ",nrow(occ_dat),"Eintraege von",TaxonName,"in iNaturalist gefunden.\n"))
      
      ## prepare output #############
      occ_dat <- occ_dat[,c("name","longitude","latitude","date")]
      occ_dat$Datenbank <- "iNat"
      occ_dat$date <- as.character(occ_dat$date)
      
      colnames(occ_dat) <- c("Taxon","Laengengrad","Breitengrad","Zeitpunkt","Datenbank")
      
      x <- x + 1
      all_records[[x]] <- as.data.table(occ_dat)
      
    } else {
      # cat(paste("No records found in iNaturalist for",TaxonName,"\n"))
      cat(paste(" Keine Eintraege in iNaturalist fuer",TaxonName,"\n"))
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
        cat(paste(" ",nrow(occ_dat),"Eintraege von",TaxonName,"in OBIS gefunden.\n"))
        
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
        cat(paste(" Keine Eintraege in OBIS fuer",TaxonName,"\n"))
      }
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
    
    return(all_output)
    
  }
}

