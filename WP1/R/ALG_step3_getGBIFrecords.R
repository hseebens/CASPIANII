##################### get number of GBIF records ########################################################
# 
# This scripts returns the number of records per taxon available at GBIF
# 
# Project: CASPIAN II
# 
# Hanno Seebens, 29.06.22
###############################################################################################################



get_GBIFrecords <- function(dat){

  dat <- read.xlsx(file.path("Data","ListeGebietsfremderArten_gesamt_standardisiert.xlsx"),sheet=1)
  
  ## get list of taxon names
  dat$SpecNames <- dat$scientificName
  dat$SpecNames[is.na(dat$SpecNames)] <- dat$Taxon[is.na(dat$SpecNames)]
  SpecNames <- dat$SpecNames
  
  #######################################################################################
  ### get GBIF keys for all species (necessary to apply 'occ_count' #####################
  
  cat("\n Get GBIF keys for taxa \n")
  
  GBIF_speclist <- list()
  x <- 0
  for (i in 1:length(SpecNames)){# loop over all species
    
    specname <- name_backbone(SpecNames[i], limit = 10, strict = TRUE)      # overrides the max limit to increase speed
    if (all(colnames(specname)!="species")) next
    
    x <- x + 1
    GBIF_speclist[[x]] <- c(specname$speciesKey,specname$matchType,SpecNames[i])
    
    if (x%%1000==0) print(x)
  }
  GBIF_species <- as.data.frame(do.call("rbind",GBIF_speclist),stringsAsFactors = F)
  colnames(GBIF_species) <- c("speciesKey","matchType","Orig_name")

  #######################################################################################
  ### Get the number of GBIF records per species ########################################

  cat("\n Get number of records per taxon from GBIF \n")
  
  # remove entries with duplicated GBIF keys (e.g. synonyms)
  ind <- !duplicated(GBIF_species$speciesKey)
  GBIF_species <- GBIF_species[ind,]
  
  GBIF_species$nGBIFRecords_DE <- 0
  GBIF_species$nGBIFRecords_All <- 0
  for (i in 1:length(GBIF_species$speciesKey)){
    
    nRecords_All <- try(occ_count(GBIF_species$speciesKey[i]))
    nRecords_DE <- try(occ_count(GBIF_species$speciesKey[i],country="DE"))
    
    if (class(nRecords_DE)=="try-error") next
    
    GBIF_species$nGBIFRecords_DE[i] <- nRecords_DE
    GBIF_species$nGBIFRecords_All[i] <- nRecords_All
    
    if (i%%1000==0) print(i)
  }
  
  ## merge record numbers with original input file 
  dat_out <- merge(dat,GBIF_species[,c("Orig_name","nGBIFRecords_DE","nGBIFRecords_All")],by.x="SpecNames",by.y="Orig_name",all.x=T)

  ## add comment to data availability and status of invasion
  dat_out$comment <- ""
  dat_out$comment[dat$nGBIFRecords_DE<1000] <- "geringe Datendichte"
  dat_out$comment[dat$database=="EASIN" & dat$nGBIFRecords_DE<100 & dat$phylum=="Chordata"] <- "möglicherweise nicht etabliert"
  dat_out$comment[dat$database=="EASIN" & dat$nGBIFRecords_DE<100 & dat$phylum=="Tracheophyta"] <- "möglicherweise nicht etabliert"
  dat_out$comment[dat$database=="GRIIS" & dat$nGBIFRecords_DE<100 & dat$phylum=="Chordata"] <- "möglicherweise nicht etabliert"
  dat_out$comment[dat$database=="GRIIS" & dat$nGBIFRecords_DE<100 & dat$phylum=="Tracheophyta"] <- "möglicherweise nicht etabliert"
  
  dat_out <- dat_out[order(dat_out$taxonGroup,dat_out$scientificName),] # sort output
  
  ## Create Workbook object and add worksheets for output
  wb <- createWorkbook()
  hs2 <- createStyle(halign = "center", valign = "center", textDecoration = "bold",border = "Bottom")
  
  addWorksheet(wb, "GesamtListeGebietsfremdeArten")
  writeData(wb,"GesamtListeGebietsfremdeArten",dat_out[,-which(colnames(dat_out)=="SpecNames")], headerStyle = hs2)
  
  ## export file (overrides existing file!) ##########################
  saveWorkbook(wb, file.path("Data","ListeGebietsfremderArten_gesamt_standardisiert.xlsx"), overwrite = T, returnValue = FALSE)
  
}

