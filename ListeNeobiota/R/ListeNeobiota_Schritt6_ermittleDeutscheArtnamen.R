##################################################################################
#
# Skript ist Teil des Workflows "ListeNeobiota" zur Erstellung einer 
# einheitlichen Liste von Neobiota in Deutschland. Es wird mit dem Skript 
# erstelleListeNeobiota_MAIN.R aufgerufen.
#
# Dieses Skript extrahiert deutsche Artnamen für die Arten der ListeNeobiota von
# GBIF. Für jeden wissenschaftlichen Namen wird auf GBIF nach einem deutschen
# Artnamen gesucht und, falls vorhanden, der häufigste Namen in die ListeNeobiota
# integriet. Hierfür muss eine Internetverbindung bestehen. 
#
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 08.12.25
##################################################################################


ermittleDeutscheNamen <- function(){
  
  dat <- read.xlsx(file.path("ListeNeobiota","Daten","Output","ListeGebietsfremderArten_gesamt_standardisiert.xlsx"),sheet=1)
  if (any(colnames(dat)=="Artname")) dat <- dat[, -which(colnames(dat)=="Artname")]

  ## Ermittle deutsche Namen von GBIF ##########################################
  
  cat("\n Ermittle deutsche Namen von GBIF\n\n")
  
  #setup progress bar
  uni_spec <- unique(dat$Taxon)
  pb <- txtProgressBar(min=0, max=length(uni_spec), initial=0,style = 3)
  
  options(warn=-1) # turn of warnings (will pop up if no German names provided)
  
  all_common_names <- list()
  for (i in 1:length(uni_spec)){
    spec_dat <- name_backbone(uni_spec[i])
    try({
      all_names <- name_usage(spec_dat$usageKey, data="vernacularNames")
      all_germans <- all_names$data[all_names$data$language=="deu",]
      common_name <- names(sort(table(all_germans$vernacularName), decreasing=T)[1])
      if (is.null(common_name)){
        all_common_names[[i]] <- NA
      } else {
        all_common_names[[i]] <- common_name
      }
    }, silent=TRUE)
    # if (i%%100==0) print(paste(round(i/length(uni_spec)*100),"%"))
    
    #update progress bar
    info <- sprintf("%d%% done", round((i/length(uni_spec))*100))
    setTxtProgressBar(pb, i) #, label=info
  }
  close(pb)
  
  options(warn=0) # turn on warnings again
  
  
  ind_miss <- unlist(lapply(all_common_names,length))==0
  all_common_names[ind_miss] <- NA
  scientific_common <- unique(cbind.data.frame(uni_spec, unlist(all_common_names)))
  colnames(scientific_common) <- c("Taxon","Artname")
  
  ## Export
  fwrite(scientific_common,file.path("ListeNeobiota", "Daten", "Output", "Tabelle_DeutscheName.csv"))
  
  ## generate output ######################################################################
  
  dat_out <- merge(dat, scientific_common, by="Taxon", all.x=T)
  
  dat_out <- dat_out[order(dat_out$Artengruppe,dat_out$wissenschaftlicherName),] # sort output

  dat_out <- dat_out[,c("Taxon","wissenschaftlicherName","Artname","Artengruppe","EU_Anliegen","Status","Erstnachweis","Pfad","BfNlisten","Gattung","Familie","Ordnung","Klasse","Phylum","Reich","Eintraege_GBIF_DE","Eintraege_GBIF_Europa","Eintraege_sMon","Kommentar","Datenbank")]
  
  ## Create Workbook object and add worksheets for output
  wb <- createWorkbook()
  hs2 <- createStyle(halign = "center", valign = "center", textDecoration = "bold",border = "Bottom")
  
  addWorksheet(wb, "GesamtListeGebietsfremdeArten")
  writeData(wb,"GesamtListeGebietsfremdeArten",dat_out, headerStyle = hs2)
  
  ## export file (overrides existing file!) ##########################
  saveWorkbook(wb, file.path("ListeNeobiota","Daten","Output","ListeGebietsfremderArten_gesamt_standardisiert.xlsx"), overwrite = T, returnValue = FALSE)
  
}
