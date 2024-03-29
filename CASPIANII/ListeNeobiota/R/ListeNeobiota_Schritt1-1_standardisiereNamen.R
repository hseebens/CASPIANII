
####### Standardisation of taxon names #################################################
#
# Standardisation of taxon names
# check and replace species names using 'rgibf' GBIF taxonomy
#
# Senckenberg Gesellschaft für Naturforschung, 04.11.22
#########################################################################################

CheckGBIFTax <- function(dat){
  
  if ("scientificName"%in%colnames(dat)) colnames(dat)[colnames(dat)=="scientificName"] <- "scientificName_orig"
  if ("Taxon"%in%colnames(dat)) colnames(dat)[colnames(dat)=="Taxon"] <- "Taxon_orig"
  if ("Wissenschaftlicher_Name"%in%colnames(dat)) colnames(dat)[colnames(dat)=="Wissenschaftlicher_Name"] <- "scientificName_orig"

  if (!"scientificName"%in%colnames(dat)) dat$scientificName <- NA
  if ("scientificName_orig"%in%colnames(dat)) dat$scientificName <- dat$scientificName_orig
  if (!"Taxon"%in%colnames(dat)) dat$Taxon <- NA
  if ("Taxon_orig"%in%colnames(dat)) dat$Taxon <- dat$Taxon_orig
  if ("scientificName_orig"%in%colnames(dat)) dat$Taxon <- dat$scientificName_orig # overwrites Taxon!
  if (!"GBIFstatus"%in%colnames(dat)) dat$GBIFstatus <- "Missing"
  if (!"GBIFmatchtype"%in%colnames(dat)) dat$GBIFmatchtype <- NA
  if (!"GBIFnote"%in%colnames(dat)) dat$GBIFnote <- NA
  if (!"GBIFstatus_Synonym"%in%colnames(dat)) dat$GBIFstatus_Synonym <- NA
  if (!"species"%in%colnames(dat)) dat$species <- NA
  if (!"genus"%in%colnames(dat)) dat$genus <- NA
  if (!"family"%in%colnames(dat)) dat$family <- NA
  if (!"order"%in%colnames(dat)) dat$order <- NA
  if (!"class"%in%colnames(dat)) dat$class <- NA
  if (!"phylum"%in%colnames(dat)) dat$phylum <- NA
  if (!"kingdom"%in%colnames(dat)) dat$kingdom <- NA 
  if (!"GBIFtaxonRank"%in%colnames(dat)) dat$GBIFtaxonRank <- NA
  if (!"GBIFusageKey"%in%colnames(dat)) dat$GBIFusageKey <- NA
  
  if (any(colnames(dat)=="kingdom_user")){
    taxlist_lifeform <- unique(dat[,c("Taxon","kingdom_user")])
    taxlist <- taxlist_lifeform$Taxon
  } else if (any(colnames(dat)=="Author")){
    taxlist <- unique(paste(dat$Taxon,dat$Author))
  } else if (!all(is.na(dat$scientificName))){
    taxlist <- unique(dat$scientificName)
  } else {
    taxlist <- unique(dat$Taxon)
  }
  n_taxa <- length(taxlist)

  #setup progress bar
  pb <- txtProgressBar(min=0, max=n_taxa, initial=0,style = 3)
  
  options(warn=-1) # the use of 'tibbles' data frame generates warnings as a bug; if solved this options() should be turned off
  
  mismatches <- data.frame(Taxon=NA,status=NA,matchType=NA)
  for (j in 1:n_taxa){# loop over all species names; takes some hours...
    
    # select species name and download taxonomy
    ind_tax <- which(dat$Taxon==taxlist[j])
    
    db_all <- name_backbone_verbose(taxlist[j],strict=T) # check for names and synonyms
    db <- db_all[["data"]]
    alternatives <- db_all$alternatives
    
    if (any(db$status=="ACCEPTED" & db$matchType=="EXACT" & colnames(db)=="canonicalName")){ 
      
      ### EXACT MATCHES: select only accepted names and exact matches ##############################################
      
      dat$Taxon[ind_tax]      <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$canonicalName[1]
      dat$scientificName[ind_tax] <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$scientificName[1]
      dat$GBIFstatus[ind_tax]      <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$status[1]
      dat$GBIFmatchtype[ind_tax]   <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$matchType[1]
      dat$GBIFtaxonRank[ind_tax]        <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$rank[1]
      dat$GBIFusageKey[ind_tax]        <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$usageKey[1]
      
      try(dat$species[ind_tax]     <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$species[1],silent=T)
      try(dat$genus[ind_tax]       <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$genus[1],silent=T)
      try(dat$family[ind_tax]      <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$family[1],silent=T)
      try(dat$class[ind_tax]       <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$class[1],silent=T)
      try(dat$order[ind_tax]       <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$order[1],silent=T)
      try(dat$phylum[ind_tax]      <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$phylum[1],silent=T)
      try(dat$kingdom[ind_tax]     <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$kingdom[1],silent=T)
      
      next # jump to next taxon
      
    } else if (any(db$status=="SYNONYM" & db$matchType=="EXACT" & colnames(db)=="species")) { # select synonyms
      
      ## SYNONYMS #################################################################################
      
      ## flag that it is a synonym
      dat$GBIFstatus[ind_tax] <- db[db$status=="SYNONYM" & db$matchType=="EXACT",]$status[1]
      dat$GBIFmatchtype[ind_tax] <- db[db$status=="SYNONYM" & db$matchType=="EXACT",]$matchType[1]
      dat$GBIFtaxonRank[ind_tax]     <- db[db$status=="SYNONYM" & db$matchType=="EXACT",]$rank[1]
      dat$GBIFusageKey[ind_tax]     <- db[db$status=="SYNONYM" & db$matchType=="EXACT",]$usageKey[1]
      
      ## check if accepted name is provided in 'alternatives'
      if (any(alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT")){
        
        if (nrow(alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT",])>1) {
          dat$GBIFnote[ind_tax] <- "Multiple accepted names for synonym in GBIF"
        } 
        
        dat$scientificName[ind_tax] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT",]$scientificName[1]
        dat$Taxon[ind_tax]          <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT",]$canonicalName[1]
        
        try(dat$species[ind_tax]     <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT",]$species[1],silent=T)
        try(dat$genus[ind_tax]       <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT",]$genus[1],silent=T)
        try(dat$family[ind_tax]      <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT",]$family[1],silent=T)
        try(dat$class[ind_tax]       <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT",]$class[1],silent=T)
        try(dat$order[ind_tax]       <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT",]$order[1],silent=T)
        try(dat$phylum[ind_tax]      <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT",]$phylum[1],silent=T)
        try(dat$kingdom[ind_tax]     <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT",]$kingdom[1],silent=T)
        
        next # jump to next taxon
        
      } else if (db$rank=="SPECIES"){  ## try to get author name of synonym (not provided in 'db')(works only for species)

        dat$Taxon[ind_tax]    <- db[db$status=="SYNONYM" & db$matchType=="EXACT",]$species[1]
        dat$GBIFstatus[ind_tax]    <- db[db$status=="SYNONYM" & db$matchType=="EXACT",]$status[1]
        dat$GBIFmatchtype[ind_tax] <- db[db$status=="SYNONYM" & db$matchType=="EXACT",]$matchType[1]
        dat$GBIFtaxonRank[ind_tax]      <- db[db$status=="SYNONYM" & db$matchType=="EXACT",]$rank[1]
        dat$GBIFusageKey[ind_tax]      <- db[db$status=="SYNONYM" & db$matchType=="EXACT",]$usageKey[1]
        
        db_all_2 <- name_backbone_verbose(dat$Taxon[ind_tax][1],strict=T) # get scientific name
        db_2 <- db_all_2[["data"]]

        if (db_2$matchType=="EXACT"){ # exact matches
          dat$scientificName[ind_tax]  <- db_2[db_2$matchType=="EXACT",]$scientificName[1]
          dat$GBIFstatus_Synonym[ind_tax]<- db_2[db_2$matchType=="EXACT",]$status[1]
          try(dat$species[ind_tax]     <- db_2[db_2$matchType=="EXACT",]$species[1],silent=T)
          try(dat$genus[ind_tax]       <- db_2[db_2$matchType=="EXACT",]$genus[1],silent=T)
          try(dat$family[ind_tax]      <- db_2[db_2$matchType=="EXACT",]$family[1],silent=T)
          try(dat$class[ind_tax]       <- db_2[db_2$matchType=="EXACT",]$class[1],silent=T)
          try(dat$order[ind_tax]       <- db_2[db_2$matchType=="EXACT",]$order[1],silent=T)
          try(dat$phylum[ind_tax]      <- db_2[db_2$matchType=="EXACT",]$phylum[1],silent=T)
          try(dat$kingdom[ind_tax]     <- db_2[db_2$matchType=="EXACT",]$kingdom[1],silent=T)
        }
      }
      next
      
    } else if (any(db$status=="ACCEPTED" & db$matchType=="FUZZY" & db$confidence>=97 & colnames(db)=="canonicalName")) { 

      ## FUZZY MATCHES #################################################################################
      
      dat$Taxon[ind_tax]      <- db[db$status=="ACCEPTED" & db$matchType=="FUZZY",]$canonicalName[1]
      dat$scientificName[ind_tax] <- db[db$status=="ACCEPTED" & db$matchType=="FUZZY",]$scientificName[1]
      dat$GBIFstatus[ind_tax]      <- db[db$status=="ACCEPTED" & db$matchType=="FUZZY",]$status[1]
      dat$GBIFmatchtype[ind_tax]   <- db[db$status=="ACCEPTED" & db$matchType=="FUZZY",]$matchType[1]
      dat$GBIFtaxonRank[ind_tax]        <- db[db$status=="ACCEPTED" & db$matchType=="FUZZY",]$rank[1]
      dat$GBIFusageKey[ind_tax]        <- db[db$status=="ACCEPTED" & db$matchType=="FUZZY",]$usageKey[1]
      
      dat$scientificName[ind_tax] <- db[db$status=="ACCEPTED" & db$matchType=="FUZZY",]$scientificName[1]
      try(dat$species[ind_tax] <-db[db$status=="ACCEPTED" & db$matchType=="FUZZY",]$species[1],silent=T)
      try(dat$genus[ind_tax]   <-  db[db$status=="ACCEPTED" & db$matchType=="FUZZY",]$genus[1],silent=T)
      try(dat$family[ind_tax]  <- db[db$status=="ACCEPTED" & db$matchType=="FUZZY",]$family[1],silent=T)
      try(dat$class[ind_tax]   <- db[db$status=="ACCEPTED" & db$matchType=="FUZZY",]$class[1],silent=T)
      try(dat$order[ind_tax]   <- db[db$status=="ACCEPTED" & db$matchType=="FUZZY",]$order[1],silent=T)
      try(dat$phylum[ind_tax]  <- db[db$status=="ACCEPTED" & db$matchType=="FUZZY",]$phylum[1],silent=T)
      try(dat$kingdom[ind_tax] <- db[db$status=="ACCEPTED" & db$matchType=="FUZZY",]$kingdom[1],silent=T)
      
      next # jump to next taxon
      
    } else if (any(alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT") & any(colnames(alternatives)=="species")){

      ## HOMONYMS #################################################################################
      ## check for alternative names because of e.g. multiple entries for different taxonomic groups in GBIF...

      ## check information of kingdom provided by user and selected respective author
      if (any(colnames(dat)=="kingdom_user")) {
        if (length(unique(alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom==taxlist_lifeform[j,2],]$family))>1) print(paste(taxlist[j],"name occurrs in more than one family! To resolve this, you may provide information about author in original database, or kingdom or taxonomic group in DatabaseInfo.xlsx."))
        
        dat$Taxon[ind_tax] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT",]$species[1]
        
        dat$scientificName[ind_tax] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom==taxlist_lifeform[j,2],]$scientificName[1]
        dat$GBIFstatus[ind_tax]      <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom==taxlist_lifeform[j,2],]$status[1]
        dat$GBIFmatchtype[ind_tax]   <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom==taxlist_lifeform[j,2],]$matchType[1]
        dat$GBIFtaxonRank[ind_tax]        <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom==taxlist_lifeform[j,2],]$rank[1]
        dat$GBIFusageKey[ind_tax]        <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom==taxlist_lifeform[j,2],]$usageKey[1]
        dat$GBIFnote[ind_tax]        <- "Homonym in GBIF"

        try(dat$species[ind_tax]     <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom==taxlist_lifeform[j,2],]$species[1],silent=T)
        try(dat$genus[ind_tax]       <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom==taxlist_lifeform[j,2],]$genus[1],silent=T)
        try(dat$family[ind_tax]      <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom==taxlist_lifeform[j,2],]$family[1],silent=T)
        try(dat$class[ind_tax]       <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom==taxlist_lifeform[j,2],]$class[1],silent=T)
        try(dat$order[ind_tax]       <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom==taxlist_lifeform[j,2],]$order[1],silent=T)
        try(dat$phylum[ind_tax]      <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom==taxlist_lifeform[j,2],]$phylum[1],silent=T)
        try(dat$kingdom[ind_tax]     <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom==taxlist_lifeform[j,2],]$kingdom[1],silent=T)
        
        next
      }

      ## select entries from cross-taxonomic databases from certain taxa
      if (any(colnames(dat)=="Taxon_group")){
        if (unique(dat$Taxon_group)!="All"){ # check if 'Taxon_group' provides useful information
          if (grepl("Vascular plants",unique(dat$Taxon_group))){ # case of vascular plants
            
            dat$Taxon[ind_tax] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT",]$species[1]
            
            dat$scientificName[ind_tax] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom=="Plantae",]$scientificName[1]
            dat$GBIFstatus[ind_tax]      <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom=="Plantae",]$status[1]
            dat$GBIFmatchtype[ind_tax]   <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom=="Plantae",]$matchType[1]
            dat$GBIFtaxonRank[ind_tax]        <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom=="Plantae",]$rank[1]
            dat$GBIFusageKey[ind_tax]        <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom=="Plantae",]$usageKey[1]
            dat$GBIFnote[ind_tax]        <- "Homonym in GBIF"
            
            try(dat$species[ind_tax]     <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom=="Plantae",]$species[1],silent=T)
            try(dat$genus[ind_tax]       <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom=="Plantae",]$genus[1],silent=T)
            try(dat$family[ind_tax]      <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom=="Plantae",]$family[1],silent=T)
            try(dat$class[ind_tax]       <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom=="Plantae",]$class[1],silent=T)
            try(dat$order[ind_tax]       <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom=="Plantae",]$order[1],silent=T)
            try(dat$phylum[ind_tax]      <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom=="Plantae",]$phylum[1],silent=T)
            try(dat$kingdom[ind_tax]     <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom=="Plantae",]$kingdom[1],silent=T)
          }
          if (grepl("Reptiles",unique(dat$Taxon_group))){
            
            dat$Taxon[ind_tax] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT",]$species[1]
            
            dat$scientificName[ind_tax] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Reptilia",]$scientificName[1]
            dat$GBIFstatus[ind_tax]      <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Reptilia",]$status[1]
            dat$GBIFmatchtype[ind_tax]   <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Reptilia",]$matchType[1]
            dat$GBIFtaxonRank[ind_tax]            <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Reptilia",]$rank[1]
            dat$GBIFusageKey[ind_tax]            <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Reptilia",]$usageKey[1]
            dat$GBIFnote[ind_tax] <- "Homonym in GBIF"
            
            try(dat$species[ind_tax]     <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Reptilia",]$species[1],silent=T)
            try(dat$genus[ind_tax]       <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Reptilia",]$genus[1],silent=T)
            try(dat$family[ind_tax]      <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Reptilia",]$family[1],silent=T)
            try(dat$class[ind_tax]       <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Reptilia",]$class[1],silent=T)
            try(dat$order[ind_tax]       <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Reptilia",]$order[1],silent=T)
            try(dat$phylum[ind_tax]      <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Reptilia",]$phylum[1],silent=T)
            try(dat$kingdom[ind_tax]     <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Reptilia",]$kingdom[1],silent=T)
          }
          if (grepl("Amphibians",unique(dat$Taxon_group))){
            
            dat$Taxon[ind_tax] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT",]$species[1]
            
            dat$scientificName[ind_tax] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Amphibia",]$scientificName[1]
            dat$GBIFstatus[ind_tax]      <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Amphibia",]$status[1]
            dat$GBIFmatchtype[ind_tax]   <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Amphibia",]$matchType[1]
            dat$GBIFtaxonRank[ind_tax]            <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Amphibia",]$rank[1]
            dat$GBIFusageKey[ind_tax]            <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Amphibia",]$usageKey[1]
            dat$GBIFnote[ind_tax] <- "Homonym in GBIF"
            
            try(dat$species[ind_tax]     <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Amphibia",]$species[1],silent=T)
            try(dat$genus[ind_tax]       <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Amphibia",]$genus[1],silent=T)
            try(dat$family[ind_tax]      <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Amphibia",]$family[1],silent=T)
            try(dat$class[ind_tax]       <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Amphibia",]$class[1],silent=T)
            try(dat$order[ind_tax]       <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Amphibia",]$order[1],silent=T)
            try(dat$phylum[ind_tax]      <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Amphibia",]$phylum[1],silent=T)
            try(dat$kingdom[ind_tax]     <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Amphibia",]$kingdom[1],silent=T)
          }
          if (grepl("Birds",unique(dat$Taxon_group))){
            
            dat$Taxon[ind_tax] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT",]$species[1]
            
            dat$scientificName[ind_tax] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Aves",]$scientificName[1]
            dat$GBIFstatus[ind_tax]      <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Aves",]$status[1]
            dat$GBIFmatchtype[ind_tax]   <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Aves",]$matchType[1]
            dat$GBIFtaxonRank[ind_tax]        <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Aves",]$rank[1]
            dat$GBIFusageKey[ind_tax]        <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Aves",]$usageKey[1]
            dat$GBIFnote[ind_tax]        <- "Homonym in GBIF"
            
            try(dat$species[ind_tax]     <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Aves",]$species[1],silent=T)
            try(dat$genus[ind_tax]       <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Aves",]$genus[1],silent=T)
            try(dat$family[ind_tax]      <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Aves",]$family[1],silent=T)
            try(dat$class[ind_tax]       <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Aves",]$class[1],silent=T)
            try(dat$order[ind_tax]       <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Aves",]$order[1],silent=T)
            try(dat$phylum[ind_tax]      <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Aves",]$phylum[1],silent=T)
            try(dat$kingdom[ind_tax]     <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Aves",]$kingdom[1],silent=T)
          }
          if (grepl("Insects",unique(dat$Taxon_group))){
            
            dat$Taxon[ind_tax] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT",]$species[1]
            
            dat$scientificName[ind_tax] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Insecta",]$scientificName[1]
            dat$GBIFstatus[ind_tax]      <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Insecta",]$status[1]
            dat$GBIFmatchtype[ind_tax]   <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Insecta",]$matchType[1]
            dat$GBIFtaxonRank[ind_tax]        <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Insecta",]$rank[1]
            dat$GBIFusageKey[ind_tax]        <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Insecta",]$usageKey[1]
            dat$GBIFnote[ind_tax]        <- "Homonym in GBIF"
            
            try(dat$species[ind_tax]     <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Insecta",]$species[1],silent=T)
            try(dat$genus[ind_tax]       <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Insecta",]$genus[1],silent=T)
            try(dat$family[ind_tax]      <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Insecta",]$family[1],silent=T)
            try(dat$class[ind_tax]       <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Insecta",]$class[1],silent=T)
            try(dat$order[ind_tax]       <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Insecta",]$order[1],silent=T)
            try(dat$phylum[ind_tax]      <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Insecta",]$phylum[1],silent=T)
            try(dat$kingdom[ind_tax]     <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Insecta",]$kingdom[1],silent=T)
          }
          if (grepl("Mammals",unique(dat$Taxon_group))){
            
            dat$Taxon[ind_tax] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT",]$species[1]
            
            dat$scientificName[ind_tax] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Mammalia",]$scientificName[1]
            dat$GBIFstatus[ind_tax]      <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Mammalia",]$status[1]
            dat$GBIFmatchtype[ind_tax]   <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Mammalia",]$matchType[1]
            dat$GBIFtaxonRank[ind_tax]        <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Mammalia",]$rank[1]
            dat$GBIFusageKey[ind_tax]        <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Mammalia",]$usageKey[1]
            dat$GBIFnote[ind_tax]        <- "Homonym in GBIF"
            
            try(dat$species[ind_tax]     <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Mammalia",]$species[1],silent=T)
            try(dat$genus[ind_tax]       <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Mammalia",]$genus[1],silent=T)
            try(dat$family[ind_tax]      <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Mammalia",]$family[1],silent=T)
            try(dat$class[ind_tax]       <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Mammalia",]$class[1],silent=T)
            try(dat$order[ind_tax]       <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Mammalia",]$order[1],silent=T)
            try(dat$phylum[ind_tax]      <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Mammalia",]$phylum[1],silent=T)
            try(dat$kingdom[ind_tax]     <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Mammalia",]$kingdom[1],silent=T)
          }
        }
      }
    } else if (any(alternatives$status=="SYNONYM" & alternatives$matchType=="EXACT" & any(colnames(alternatives)=="species"))) { # check for synonyms in 'alternatives'

      ## check alternative names #################################################################################
      
      if (nrow(alternatives[alternatives$status=="SYNONYM" & alternatives$matchType=="EXACT",])>1) { # check if multiple synonyms are provided; if so leave to next taxon
        dat$GBIFnote[ind_tax] <- "Multiple synonyms in GBIF"
        next # not possible to identify correct name
      } 
      
      dat$Taxon[ind_tax]       <- alternatives[alternatives$status=="SYNONYM" & alternatives$matchType=="EXACT",]$species[1]
      dat$GBIFstatus[ind_tax]       <- alternatives[alternatives$status=="SYNONYM" & alternatives$matchType=="EXACT",]$status[1]
      dat$GBIFmatchtype[ind_tax]   <- alternatives[alternatives$status=="SYNONYM" & alternatives$matchType=="EXACT",]$matchType[1]
      dat$GBIFtaxonRank[ind_tax]            <- alternatives[alternatives$status=="SYNONYM" & alternatives$matchType=="EXACT",]$rank[1]
      dat$GBIFusageKey[ind_tax]            <- alternatives[alternatives$status=="SYNONYM" & alternatives$matchType=="EXACT",]$usageKey[1]
      dat$GBIFnote[ind_tax] <- "Homonym in GBIF"
      
      ## try to get author name of synonym (not provided in 'db')
      db_all_2 <- name_backbone_verbose(dat$Taxon[ind_tax][1])
      db_2 <- db_all_2[["data"]]
      if (db_2$matchType=="EXACT"){
        
        # if (length(unique(db_2[db_2$status=="ACCEPTED" & db_2$matchType=="EXACT",]$family))>1) cat(paste0("\n Warning: Multiple entries of ",dat$scientificName[ind_tax]," found in GBIF! Add author to species name or add kingdom information to original database or check GBIF. \n"))
        if (length(unique(db_2[db_2$status=="ACCEPTED" & db_2$matchType=="EXACT",]$family))>1){
          cat(paste0("\n Warnung: Mehrfache Einträge für ",dat$scientificName[ind_tax]," in GBIF gefunden! Ergänze Autor zum Artnamen oder füge Informationen zum Reich der Art in Rohdatensatz oder überprüfe GBIF Eintrag. \n"))
        } 
        
        dat$scientificName[ind_tax] <- db_2[db_2$status=="ACCEPTED" & db_2$matchType=="EXACT",]$scientificName[1]
        
        try(dat$species[ind_tax]     <- db_2[db_2$status=="ACCEPTED" & db_2$matchType=="EXACT",]$species[1],silent=T)
        try(dat$genus[ind_tax]       <- db_2[db_2$status=="ACCEPTED" & db_2$matchType=="EXACT",]$genus[1],silent=T)
        try(dat$family[ind_tax]      <- db_2[db_2$status=="ACCEPTED" & db_2$matchType=="EXACT",]$family[1],silent=T)
        try(dat$class[ind_tax]       <- db_2[db_2$status=="ACCEPTED" & db_2$matchType=="EXACT",]$class[1],silent=T)
        try(dat$order[ind_tax]       <- db_2[db_2$status=="ACCEPTED" & db_2$matchType=="EXACT",]$order[1],silent=T)
        try(dat$phylum[ind_tax]      <- db_2[db_2$status=="ACCEPTED" & db_2$matchType=="EXACT",]$phylum[1],silent=T)
        try(dat$kingdom[ind_tax]     <- db_2[db_2$status=="ACCEPTED" & db_2$matchType=="EXACT",]$kingdom[1],silent=T)
      }
      
      next # jump to next taxon

    } else {
      mismatches <- rbind(mismatches,c(taxlist[j],NA,NA))
      try(mismatches$status[nrow(mismatches)] <- db$status,silent = T)
      try(mismatches$matchType[nrow(mismatches)] <- db$matchType,silent = T)
    }

    #update progress bar
    info <- sprintf("%d%% done", round((j/n_taxa)*100))
    setTxtProgressBar(pb, j, label=info)
  }
  close(pb)

  options(warn=0) # the use of 'tibbles' data frame generates warnings as a bug; if solved this options() should be turned off
  
  # dat <- dat[!is.na(dat$GBIFstatus),] # remove species not resolved in GBIF

  out <- list()
  out[[1]] <- dat
  out[[2]] <- mismatches

  return(out)
}
