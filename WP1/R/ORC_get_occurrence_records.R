##################### Get occurrence records ########################################################
# 
# This scripts returns the occurrence records of the requested taxon available at GBIF, sMon,
# iNaturalist and OBIS in any combination if specified in option 'database' and in 'own_records'. 
# Available records are standardised, merged and returned.
# 
# databases: GBIF, iNat, sMon, OBIS, NA (if NA, no records are extracted and only own_records used)
#
# Minimum required input: taxon_name (name of taxon); database or own_records
# Function output: List of coordinates and date of record
#
# Project: CASPIAN II
# 
# Hanno Seebens, 18.07.22
###############################################################################################################


get_occurrence_records <- function(taxon_name=taxon_name,
                                   own_records=NA,
                                   database=NA,
                                   sMon_folder=NA,
                                   max_limit=10000,
                                   bounding_box=NULL){
  
  ## get occurrence records from individual sources ##############################
  
  all_records <- list()
  x <- 0
  
  if (is.null(bounding_box)){
    bounding_box <- "POLYGON ((5 45, 15 45, 15 58, 5 58, 5 45))"
  } else {
    bounding_box <- paste("POLYGON ((",bounding_box[1],bounding_box[2],",",
                                      bounding_box[3], bounding_box[2],",", 
                                      bounding_box[3], bounding_box[4],",",
                                      bounding_box[1], bounding_box[4],",", 
                                      bounding_box[1], bounding_box[2],
                                      "))")
  }
  
  ## sMon occurrences ###############################################################

  if (all(is.na(database)) & length(own_records)==1){
    warning("No databases selected and no own records provided.")
  }
  
  if ("sMon"%in%database){
    
    cat("\n** Working on sMon ******************\n")

    if (is.na(sMon_folder)){
      
      warning("No folder provided for sMon data. Please provide variable 'sMon_folder'.")
      
    } else {
        
      sMon_taxa <- fread(file.path("WP1","Data","AlienSpecies_in_sMon.csv"))
      
      if (taxon_name%in%sMon_taxa$Taxon){ # check if taxon_name is a synonym in sMon
        
        cat(paste(taxon_name,"found in sMon database as",sMon_taxa$Taxon_orig[sMon_taxa$Taxon==taxon_name],"\n"))
        
        taxon_name_new <- sMon_taxa$Taxon_orig[sMon_taxa$Taxon==taxon_name]
        
        ## get occurrence records #####
        occ_dat <- get_sMon_occurrences(taxon_name_new,sMon_folder)
        
        ## prepare output #############
        occ_dat <- occ_dat[,c("TaxonName","Longitude","Latitude","Period")]
        occ_dat$database <- "sMon"
        
        x <- x + 1
        all_records[[x]] <- occ_dat
        
      } else if (taxon_name%in%sMon_taxa$Taxon_orig){ # take direct match
        
        cat(paste(taxon_name,"found in sMon database"))
        
        ## get occurrence records #####
        occ_dat <- get_sMon_occurrences(taxon_name,sMon_folder)
        
        ## prepare output #############
        occ_dat <- occ_dat[,c("TaxonName","Longitude","Latitude","Period")]
        occ_dat$database <- "sMon"
        
        colnames(occ_dat) <- c("Taxon","Longitude","Latitude","Period","Database")
        
        x <- x + 1
        all_records[[x]] <- occ_dat
      } 

      if (x==0){
        cat(paste("No records found in sMon for",taxon_name,"\n"))
      }
    }
  } 

  ## GBIF occurrences ###################################################################
    
  if ("GBIF"%in%database){
    
    cat("\n** Working on GBIF ******************\n")
    
    xx <- name_backbone(name=taxon_name)$usageKey
    nrecords <- occ_count(xx,georeferenced=T,country="DE") # number of available records
    
    if (nrecords>max_limit){
      warning(paste0("\nNumber of available records (n=",nrecords,") exceeds limit (",max_limit,")!\n You may either increase limit or download from website."))
    }
    
    occ_dat <- occ_data(scientificName = taxon_name,geometry = bounding_box,hasCoordinate=T,limit=max_limit)[[2]]
    
    if (nrow(occ_dat)>0){
      cat(paste(taxon_name,"found in GBIF as",unique(occ_dat$scientificName),"\n"))
    }
    if (nrow(occ_dat)==max_limit){
      cat("Maximum limit of records per GBIF request reached. Either increase \n the limit within this function or download directly from GBIF.")
    }
    
    ## prepare output #############
    if (nrow(occ_dat)>0){
        
      cat(paste(nrow(occ_dat),"records of",taxon_name,"found in GBIF\n"))
      
      occ_dat <- occ_dat[,c("species","decimalLongitude","decimalLatitude","eventDate")]
      # occ_dat$Period <- NA
      occ_dat$database <- "GBIF"
      
      colnames(occ_dat) <- c("Taxon","Longitude","Latitude","Period","Database")
      
      x <- x + 1
      all_records[[x]] <- as.data.table(occ_dat)
      
    } else { ## no output
      cat(paste("No records found for",taxon_name,"in GBIF\n"))
    }
  } 
  
  ## iNaturalist occurrences ############################################################
  
  if ("iNat"%in%database){
    
    cat("\n** Working on iNaturalist ****************** \n")

    out <- occ(query = taxon_name, from = c('inat')
               # ,ebirdopts = ebirdopts
               ,geometry = bounding_box # bounding box of Germany roughly
               ,limit = max_limit
               ,has_coords=T
    )
    occ_dat <- occ2df(out)
    
    if (nrow(occ_dat)>0){
      
      cat(paste(nrow(occ_dat),"records of",taxon_name,"found in iNaturalist\n"))
      
      ## prepare output #############
      occ_dat <- occ_dat[,c("name","longitude","latitude","date")]
      occ_dat$database <- "iNat"
      occ_dat$date <- as.character(occ_dat$date)
      
      colnames(occ_dat) <- c("Taxon","Longitude","Latitude","Period","Database")
      
      x <- x + 1
      all_records[[x]] <- as.data.table(occ_dat)
      
    } else {
      cat(paste("No records found in iNaturalist for",taxon_name,"\n"))
    }
  }
  
  ## OBIS occurrences ############################################################
  
  if ("OBIS"%in%database){
    
    cat("\n** Working on OBIS ****************** \n")
    
    out <- occ(query = taxon_name, from = c('obis')
               # ,ebirdopts = ebirdopts
               ,geometry = bounding_box # bounding box of Germany roughly
               ,limit = max_limit
               ,has_coords=T
    )
    occ_dat <- occ2df(out)
    
    if (nrow(occ_dat)>0){
      
      cat(paste(nrow(occ_dat),"records of",taxon_name,"found in OBIS\n"))
      
      ## prepare output #############
      if ("date"%in%colnames(occ_dat)){
        occ_dat <- occ_dat[,c("name","longitude","latitude","date")]
      } else {
        occ_dat <- occ_dat[,c("name","longitude","latitude")]
        occ_dat$date <- NA
      }
      occ_dat$database <- "OBIS"
      occ_dat$date <- as.character(occ_dat$date)
      
      colnames(occ_dat) <- c("Taxon","Longitude","Latitude","Period","Database")
      
      x <- x + 1
      all_records[[x]] <- as.data.table(occ_dat)
      
    } else {
      cat(paste("No records found in OBIS for",taxon_name,"\n"))
    }
  }
  
  ## add own records ####################################################################
  
  if (length(own_records)>1){
    if (nrow(own_records)>0){
      
      if (any(!c("Taxon","Longitude","Latitude","Period","Database")%in%colnames(own_records))){
        stop("Column names in 'own_records' not correct. Please change to Taxon, Longitude, Latitude, Period and Database.")
      }
      
      x <- x + 1
      
      all_records[[x]] <- as.data.table(own_records)
    }
  }
  
  ## output ##############################################################################
  
  all_output <- rbindlist(all_records,use.names = T,fil=T)
  all_output$Longitude <- as.numeric(all_output$Longitude)
  all_output$Latitude <- as.numeric(all_output$Latitude)
  
  return(all_output)
}

