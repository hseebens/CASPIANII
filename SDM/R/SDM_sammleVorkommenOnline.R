##################### Get occurrence records ###################################
# 
#
# Skript ist Teil des Workflows "SDM" zur Habitatmodellierung von Neobiota in 
# Deutschland. Es wird mit dem Skript SDM_ermittleVorkommen.R aufgerufen.
#
# Das Skript extrahiert für eine Art Vorkommensdaten von verschiedenen online 
# Platformen und fügt diese zu einer Tabelle zusammen.
# 
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 08.12.25
################################################################################


sammleVorkommenOnline <- function(TaxonName=TaxonName,
                                   Datenbank=NA,
                                   max_limit=max_limit,
                                   Ausschnitt=NULL){
  
  ## get occurrence records from individual sources ##############################
  
  all_records <- list()
  x <- 0
  
  if (is.null(Ausschnitt)){ 
    Ausschnitt <- "POLYGON ((5 48, 15 48, 15 56, 5 56, 5 48))"
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
    nrecords <- occ_search(taxonKey=taxonKey, limit=0, geometry = Ausschnitt, hasCoordinate=TRUE)$meta$count # number of available records
    
    if (nrecords>max_limit){
      cat(paste0("\n Anzahl der verfügbaren Einträge in GBIF (n=",nrecords,") überschreitet Limit (n=",max_limit,"). Vorkommensdaten sollten mit alternativen Weg mit 'SDM_bezieheHoheDatenmengen.R' oder Daten direkt von Webseite heruntergeladen werden."))
      cat(paste0("\n Download wird auf n=",max_limit," Vorkommensdaten begrenzt."))
    } 
    
    occ_dat <- try(occ_data(scientificName = TaxonName, geometry = Ausschnitt, hasCoordinate=TRUE, 
                            limit=max_limit)[[2]], silent=TRUE)

    # remove non-living, non-free-ranging specimen
    occ_dat <- occ_dat[!occ_dat$basisOfRecord%in%c("PRESERVED_SPECIMEN", "LIVING_SPECIMEN", 
                                                   "FOSSIL_SPECIMEN", "MATERIAL_SAMPLE", "MATERIAL_CITATION"), ]

    ## prepare output #############
    if (!is.null(occ_dat)){
      
      cat(paste(" ",TaxonName,"in GBIF gefunden als",unique(occ_dat$scientificName),"\n"))
      
      if (nrow(occ_dat)>max_limit*0.95){
        # cat("Maximum limit of records per GBIF request reached. Either increase \n the limit within this function or download directly from GBIF.")
        cat("\n Maximum Limit von Einträgen fuer GBIF Anfrage erreicht. Entweder Limit (max_limit) erhöhen oder Daten direkt von der GBIF Webseite laden.\n")
      }
      
      ## check for gridded data (with regular coordinates) and remove if detected ########
      ## gridded datasets are identified by calculating the euclidean distance 
      ## between consecutive coordinates and identifying regularities in distances
      ## (e.g., frequent similar distances between coordinates). The parameters
      ## were selected based on experience for a good match.
      ## it does not identify all but the majority of datasets and remove those
      ## records from the list of coordinates.
      
      # check only datasets with many records
      tab <- sort(table(occ_dat$datasetKey), decreasing=T)  
      ds_names <- names(tab)[tab>20]
      
      if (length(ds_names)>1){ # is at least one dataset mentioned?
        for (i in 1:length(ds_names)){
          dat <- subset(occ_dat, datasetKey==ds_names[i]) # single dataset

          ## order of lat/lon matters
          dat_sort <- dat[order(dat$decimalLatitude, dat$decimalLongitude), ] # order of coordinates matters
          dat_eucl <- sqrt( diff(dat_sort$decimalLatitude)^2 + diff(dat_sort$decimalLongitude)^2 ) # euclidean distance between consecutive records 
          dat_eucl <- dat_eucl[dat_eucl<1] # remove large distances
          dat_rnd <- round(dat_eucl, 4) # round values to avoid very small differences
          dat_rnd <- dat_rnd[dat_rnd!=0] # remove zeros (due to same coordinates)
          tab_diffs_latlon <- sort(table(dat_rnd), decreasing=T) # count similar coordinate differences
          if (length(dat_rnd)<10) next # move on if there are just a few remaining coordinates
          
          dat_sort <- dat[order(dat$decimalLongitude, dat$decimalLatitude), ] # order of coordinates matters
          dat_eucl <- sqrt( diff(dat_sort$decimalLatitude)^2 + diff(dat_sort$decimalLongitude)^2 ) # euclidean distance between consecutive records 
          dat_eucl <- dat_eucl[dat_eucl<1]
          dat_rnd <- round(dat_eucl, 4) # round values to avoid very small differences
          dat_rnd <- dat_rnd[dat_rnd!=0] # remove zeros (due to same coordinates)
          tab_diffs_lonlat <- sort(table(dat_rnd), decreasing=T) # count similar coordinate differences
          if (length(dat_rnd)<10) next # move on if there are just a few remaining coordinates
          
          # remove dataset with >40% records with regular distances (approximation through counting similar differences between coordinates)
          if (sum(tab_diffs_lonlat[1:3], na.rm=T)/sum(tab_diffs_lonlat) > 0.4 |
              sum(tab_diffs_latlon[1:3], na.rm=T)/sum(tab_diffs_latlon) > 0.4){
            # print(i)
            # x11()
            # plot(dat$decimalLongitude, dat$decimalLatitude, main=paste("Gridded", TaxonName, i))
            occ_dat <- subset(occ_dat, datasetKey!=ds_names[i])
          } 
        }
      }
      
      
      cat(paste(" ",nrow(occ_dat),"Einträge von",TaxonName,"von GBIF bezogen.\n"))
      
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
      cat(paste(" Keine Eintraege in GBIF fuer",TaxonName,"\n"))
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
      occ_dat <- subset(occ_dat,name==TaxonName) # iNaturalists includes all kinds of sub-species
    }
    
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
    
    cat("\n** Bearbeitung von OBIS ****************** \n")
    
    occ_dat <- robis::occurrence(scientificname = TaxonName
                                 ,geometry = Ausschnitt # bounding box of Germany roughly
                                 # ,limit = max_limit
                                 # ,has_coords=T
    )
    
    if (!is.null(nrow(occ_dat))) {
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

