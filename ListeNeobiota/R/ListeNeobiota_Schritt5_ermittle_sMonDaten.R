##################################################################################
#
# Skript ist Teil des Workflows "ListeNeobiota" zur Erstellung einer 
# einheitlichen Liste von Neobiota in Deutschland. Es wird mit dem Skript 
# erstelleListeNeobiota_MAIN.R aufgerufen.
#
# Dieses Skript ermittelt für jede Art die Anzahl an Einträgen in der Datenbank
# sMon. sMon enthält Vorkommensdaten von Pflanzenarten in Deutschland zu unter-
# schiedlichen Zeitpunkt. sMon Dateien müssen lokal vorliegen. 
#
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 08.12.25
##################################################################################



ermittlesMonDaten <- function(sMon_Verzeichnis=NULL){
  
  ## read species lists from sMon #############
  
  ## set working directory temporally to sMon folder
  working_directory <- getwd()
  setwd(sMon_Verzeichnis)
  
  # cat("Load sMon data sets\n")
  cat(" Lade sMon Datensaetze\n")
  
  specNames_all1 <- fread("1875_9_1875_2_Modelled_OPs_incl_sd_pt_1.csv",select="TaxonName")
  nRecords_sMon1 <- specNames_all1[,.N,by="TaxonName"]
  specNames1 <- unique(specNames_all1)
  
  specNames_all2 <- fread("1875_9_1875_2_Modelled_OPs_incl_sd_pt_2.csv",select="TaxonName")
  nRecords_sMon2 <- specNames_all2[,.N,by="TaxonName"]
  specNames2 <- unique(specNames_all2)
  
  specNames_all3 <- fread("1875_9_1875_2_Modelled_OPs_incl_sd_pt_3.csv",select="TaxonName")
  nRecords_sMon3 <- specNames_all3[,.N,by="TaxonName"]
  specNames3 <- unique(specNames_all3)
  
  specNames_all4 <- fread("1875_9_1875_2_Modelled_OPs_incl_sd_pt_4.csv",select="TaxonName")
  nRecords_sMon4 <- specNames_all4[,.N,by="TaxonName"]
  specNames4 <- unique(specNames_all4)
  
  setwd(working_directory) # resset working directory
  
  ## combine data sets
  # specNames_sMon <- unique(rbind(specNames1,specNames2,specNames3,specNames4))
  # colnames(specNames_sMon) <- "Taxon"
  
  nRecords_sMon <- unique(rbind(nRecords_sMon1,nRecords_sMon2,nRecords_sMon3,nRecords_sMon4))

  ## remove wrong symbols (correct scientific names will be provided through GBIF next line)
  nRecords_sMon$TaxonName <- gsub("�", "", nRecords_sMon$TaxonName)
  
  ## standardise taxon names
  cat(" Standardisiere sMon Taxonnamen\n")
  specNames_sMon_standardised <- CheckGBIFTax(nRecords_sMon)[[1]]

  ## replace original names with standardised ones 
  nRecords_sMon$TaxonName <- specNames_sMon_standardised$Taxon
  
  ## sum records across taxa
  nRecords_sMon <- aggregate(N ~ TaxonName, FUN=sum, data=nRecords_sMon)
  colnames(nRecords_sMon) <- c("Taxon","Eintraege_sMon")

  
  
  ## get alien species list #######################################
  
  alienspecies <- read.xlsx(file.path("ListeNeobiota","Daten","Output","ListeGebietsfremderArten_gesamt_standardisiert.xlsx"),sheet=1)
  # alienspecies <- alienspecies[,-which(colnames(alienspecies)=="Eintraege_sMon")] # in case column exist from former runs
  alienspecies <- unique(alienspecies)
  
  ## standardise sMon species names for comparison ################
  
  alien_sMon_translation <- specNames_sMon_standardised[,c("Taxon_orig","Taxon")] # orig and GBIF names
  sMon_spec <- specNames_sMon_standardised$Taxon
  
  # subset(specNames_sMon_standardised,Taxon=="Ammocalamagrostis baltica")
  
  
  ## alien species in sMon ########################################
  aliens_in_sMon <- alienspecies$Taxon[alienspecies$Taxon%in%sMon_spec]
  
  ## export alien species list
  fwrite(subset(alien_sMon_translation,Taxon%in%aliens_in_sMon),file.path("ListeNeobiota","Daten","Output","AlienSpecies_in_sMon.csv"))
  
  nRecords_sMon_standardised <- merge(nRecords_sMon,specNames_sMon_standardised,by.x="Taxon",by.y="Taxon_orig")
  nRecords_aliens_in_sMon <- nRecords_sMon_standardised[,c("Taxon.y","Eintraege_sMon")]
  colnames(nRecords_aliens_in_sMon) <- c("Taxon","Eintraege_sMon")
  
  
  ## Generate output file by adding sMon records to the main list #################
  
  alienspecies_nRecords <- merge(alienspecies,nRecords_aliens_in_sMon,by="Taxon",all.x=T)
  alienspecies_nRecords$Eintraege_sMon[is.na(alienspecies_nRecords$Eintraege_sMon)] <- 0
  
  
  ## add comment to data availability and status of invasion ###################################
  alienspecies_nRecords$Kommentar <- ""
  
  # ## invasion status
  # alienspecies_nRecords$Kommentar[alienspecies_nRecords$database=="EASIN" & alienspecies_nRecords$Eintraege_GBIF_DE<100 & alienspecies_nRecords$phylum=="Chordata"] <- "moeglicherweise nicht etabliert"
  # alienspecies_nRecords$Kommentar[alienspecies_nRecords$database=="EASIN" & alienspecies_nRecords$Eintraege_GBIF_DE<100 & alienspecies_nRecords$phylum=="Tracheophyta"] <- "moeglicherweise nicht etabliert"
  # alienspecies_nRecords$Kommentar[alienspecies_nRecords$database=="GRIIS" & alienspecies_nRecords$Eintraege_GBIF_DE<100 & alienspecies_nRecords$phylum=="Chordata"] <- "moeglicherweise nicht etabliert"
  # alienspecies_nRecords$Kommentar[alienspecies_nRecords$database=="GRIIS" & alienspecies_nRecords$Eintraege_GBIF_DE<100 & alienspecies_nRecords$phylum=="Tracheophyta"] <- "moeglicherweise nicht etabliert"
  
  ## data availability
  alienspecies_nRecords$Kommentar[alienspecies_nRecords$Eintraege_GBIF_DE<100] <- "geringe GBIF Datendichte"
  alienspecies_nRecords$Kommentar[alienspecies_nRecords$Eintraege_GBIF_DE<1000 & alienspecies_nRecords$Eintraege_GBIF_DE>=100] <- "mittlere GBIF Datendichte"
  alienspecies_nRecords$Kommentar[alienspecies_nRecords$Eintraege_GBIF_DE>=1000 ] <- "gute GBIF Datendichte"

  # alienspecies_nRecords$Kommentar[alienspecies_nRecords$Eintraege_sMon > 10000] <- paste(alienspecies_nRecords$Kommentar[alienspecies_nRecords$Eintraege_sMon > 10000],"gute sMon Datendichte",sep="; ")
  # alienspecies_nRecords$Kommentar[alienspecies_nRecords$Eintraege_sMon < 100] <- paste(alienspecies_nRecords$Kommentar[alienspecies_nRecords$Eintraege_sMon < 100],"geringe sMon Datendichte",sep="; ")
  # alienspecies_nRecords$Kommentar[alienspecies_nRecords$Eintraege_sMon > 100 & alienspecies_nRecords$Eintraege_sMon < 10000] <- paste(alienspecies_nRecords$Kommentar[alienspecies_nRecords$Eintraege_sMon > 100 & alienspecies_nRecords$Eintraege_sMon < 10000],"mittlere sMon Datendichte",sep="; ")
  # alienspecies_nRecords$Kommentar[alienspecies_nRecords$Eintraege_GBIF_DE > 1000 & alienspecies_nRecords$Eintraege_sMon > 10000] <- "gute Datendichte in GBIF und sMon"
  # alienspecies_nRecords$Kommentar[alienspecies_nRecords$Eintraege_GBIF_DE < 100 & alienspecies_nRecords$Eintraege_sMon < 100] <- "geringe Datendichte in GBIF und sMon"
  
  # alienspecies_nRecords$Kommentar <- gsub("^; ","",alienspecies_nRecords$Kommentar)
  # alienspecies_nRecords$Kommentar <- gsub("^;","",alienspecies_nRecords$Kommentar)
  
  alienspecies_nRecords <- alienspecies_nRecords[,c("Taxon","wissenschaftlicherName","Artengruppe","EU_Anliegen","Status","Erstnachweis","Pfad","BfNlisten","Gattung","Familie","Ordnung","Klasse","Phylum","Reich","Eintraege_GBIF_DE","Eintraege_GBIF_Europa","Eintraege_sMon","Kommentar","Datenbank")]

  ## Create Workbook object and add worksheets for output ##############################################
  wb <- createWorkbook()
  hs2 <- createStyle(halign = "center", valign = "center", textDecoration = "bold",border = "Bottom")
  
  addWorksheet(wb, "GesamtListeGebietsfremdeArten")
  writeData(wb,"GesamtListeGebietsfremdeArten",alienspecies_nRecords, headerStyle = hs2)
  
  ## export file (overrides existing file!) ##########################
  saveWorkbook(wb, file.path("ListeNeobiota","Daten","Output","ListeGebietsfremderArten_gesamt_standardisiert.xlsx"), overwrite = T, returnValue = FALSE)
}
