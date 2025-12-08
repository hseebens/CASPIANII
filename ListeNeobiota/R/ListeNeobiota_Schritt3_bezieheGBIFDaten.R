##################################################################################
#
# Skript ist Teil des Workflows "ListeNeobiota" zur Erstellung einer 
# einheitlichen Liste von Neobiota in Deutschland. Es wird mit dem Skript 
# erstelleListeNeobiota_MAIN.R aufgerufen.
#
# Dieses Skript ermittelt die Anzahl an vorhandenen Einträgen zum Vorkommen der
# Neobiota auf GBIF. Die Anzahl der Vorkommensdaten für in der ListeNeobiota
# ergänzt.
# 
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 08.12.25
##################################################################################



bezieheGBIFDaten <- function(){

  dat <- read.xlsx(file.path("ListeNeobiota","Daten","Output","ListeGebietsfremderArten_gesamt_standardisiert.xlsx"),sheet=1)
  # dat <- dat[1:100,]
  
  ## get list of taxon names
  dat$SpecNames <- dat$wissenschaftlicherName
  dat$SpecNames[is.na(dat$SpecNames)] <- dat$Taxon[is.na(dat$SpecNames)]
  SpecNames <- dat$SpecNames
  
  #######################################################################################
  ### get GBIF keys for all species (necessary to apply 'occ_count' #####################
  
  # cat("\n Get GBIF keys for taxa \n")
  cat("\n Extrahiere GBIF keys fuer Taxa \n")

  #setup progress bar
  pb <- txtProgressBar(min=1, max=length(SpecNames), initial=0,style = 3)
  
  GBIF_speclist <- list()
  x <- 0
  for (i in 1:length(SpecNames)){# loop over all species  
    
    specname <- name_backbone(SpecNames[i], limit = 10, strict = TRUE)      # overrides the max limit to increase speed
    if (all(colnames(specname)!="species")) next
    
    x <- x + 1
    GBIF_speclist[[x]] <- c(specname$speciesKey,specname$matchType,SpecNames[i])
    
    # if (x%%200==0) cat(paste(" ",x,"\n"))
    setTxtProgressBar(pb, i, label=info)
    
  }
  close(pb)
  GBIF_species <- as.data.frame(do.call("rbind",GBIF_speclist),stringsAsFactors = F)
  colnames(GBIF_species) <- c("speciesKey","matchType","Orig_name")
  
  #######################################################################################
  ### Get the number of GBIF records per species ########################################
  
  # cat("\n Get number of records per taxon from GBIF \n")
  cat("\n Ermittle Anzahl Eintraege pro Art auf GBIF \n")
  
  # remove entries with duplicated GBIF keys (e.g. synonyms)
  ind <- !duplicated(GBIF_species$speciesKey)
  GBIF_species <- GBIF_species[ind,]
  
  # GBIF_species <- data.frame(Orig_name=SpecNames)
  GBIF_species$Eintraege_GBIF_DE <- 0
  GBIF_species$Eintraege_GBIF_Europa <- 0
  
  #setup progress bar
  # pb <- txtProgressBar(min=1, max=length(GBIF_species$speciesKey), initial=0,style = 3)
  
  for (i in 1:length(GBIF_species$speciesKey)){ #

    # nRecords_DE <- try(occ_count(scientificName=SpecNames[i], country="DE"))
    # nRecords_DE <- try(occ_search(scientificName=SpecNames[i], limit=0, country="DE")$meta$count)
    
    # nRecords_DE <- try(occ_count(speciesKey=GBIF_species$speciesKey[i], country="DE"))
    # nRecords_Europa <- try(occ_count(scientificName=SpecNames[i], continent="europe"))
    
    
    nRecords_DE <- try(occ_count(speciesKey=GBIF_species$speciesKey[i], country="DE"), silent=TRUE)
    x <- 1
    while (class(nRecords_DE)=="try-error"){ # to deal with server errors
      nRecords_DE <- try(occ_count(scientificName=SpecNames[i], country="DE"), silent=TRUE)
      x <- x + 1
      if (x==20) break
    }
    # print(paste(i,x))
    
    nRecords_Europa <- try(occ_count(speciesKey=GBIF_species$speciesKey[i], continent="europe"), silent=TRUE)
    x <- 1
    while (class(nRecords_Europa)=="try-error"){ # to deal with server errors
      nRecords_Europa <- try(occ_count(speciesKey=GBIF_species$speciesKey[i], continent="europe"), silent=TRUE)
      x <- x + 1
      if (x==20) break
    }
    
    if (class(nRecords_DE)=="try-error") next
    
    GBIF_species$Eintraege_GBIF_DE[i] <- nRecords_DE
    GBIF_species$Eintraege_GBIF_Europa[i] <- nRecords_Europa
    
    # if (i%%100==0) cat(paste(" ",i,"\n"))
    
    setTxtProgressBar(pb, i, label=info)
  }
  close(pb)

  ## merge record numbers with original input file 
  dat_out <- merge(dat,GBIF_species[,c("Orig_name","Eintraege_GBIF_DE","Eintraege_GBIF_Europa")],by.x="SpecNames",by.y="Orig_name",all.x=T)
  dat_out$Eintraege_GBIF_DE[is.na(dat_out$Eintraege_GBIF_DE)] <- 0
  dat_out$Eintraege_GBIF_Europa[is.na(dat_out$Eintraege_GBIF_Europa)] <- 0
  
  dat_out <- dat_out[order(dat_out$Artengruppe,dat_out$wissenschaftlicherName),] # sort output
  
  ## Create Workbook object and add worksheets for output
  wb <- createWorkbook()
  hs2 <- createStyle(halign = "center", valign = "center", textDecoration = "bold",border = "Bottom")
  
  addWorksheet(wb, "GesamtListeGebietsfremdeArten")
  writeData(wb,"GesamtListeGebietsfremdeArten",dat_out[,-which(colnames(dat_out)=="SpecNames")], headerStyle = hs2)
  
  ## export file (overrides existing file!) ##########################
  saveWorkbook(wb, file.path("ListeNeobiota","Daten","Output","ListeGebietsfremderArten_gesamt_standardisiert.xlsx"), overwrite = T, returnValue = FALSE)
  
}

