##################################################################################
#
# Skript ist Teil des Workflows "ListeNeobiota" zur Erstellung einer 
# einheitlichen Liste von Neobiota in Deutschland. Es wird mit dem Skript 
# erstelleListeNeobiota_MAIN.R aufgerufen.
#
# In diesem Skript werden einheimische Pflanzenarten mit Hilfe des BfN Skripts 519
# identifiziert und entfernt.
#
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 08.12.25
##################################################################################



bereinigeListe <- function(){

  ## Lade Liste einheimischer Pflanzenarten
  Artenliste <- read.xlsx(file.path("ListeNeobiota","Daten","Input","skript519_checkliste.xlsx"),sheet=1)
  
  einheimisch <- subset(Artenliste,FLOR!="E") # waehle einheimische Pflanzenarten
  einheimisch_stand <- einheimisch
  colnames(einheimisch_stand)[colnames(einheimisch_stand)=="VOLLNAME"] <- "Taxon"
  einheimisch_stand <- CheckGBIFTax(einheimisch_stand)
  
  # table(grepl("\\(E\\)",einheimisch$VOLLNAME))
  
  alienspecies <- read.xlsx(file.path("ListeNeobiota","Daten","Output","ListeGebietsfremderArten_gesamt_standardisiert.xlsx"),sheet=1)
  
  ## entferne einheimische Pflanzenarten aus Liste
  alienspecies <- alienspecies[!alienspecies$Taxon%in%einheimisch_stand[[1]]$Taxon,]
  # alienspecies$wissenschaftlicherName[alienspecies$wissenschaftlicherName%in%einheimisch$VOLLNAME]
  
  ## entferne andere einheimische Arten aus Liste
  alienspecies <- alienspecies[!alienspecies$Taxon=="Capra ibex",] # Alpen-Steinbock
  alienspecies <- alienspecies[!alienspecies$Taxon=="Chrysoperla carnea",] # 
  alienspecies <- alienspecies[!alienspecies$Taxon=="Labia minor",] # 
  alienspecies <- alienspecies[!alienspecies$Taxon=="Anser anser",] # 
  alienspecies <- alienspecies[!alienspecies$Taxon=="Anser brachyrhynchus",] # 
  alienspecies <- alienspecies[!alienspecies$Taxon=="Aythya ferina",] # 
  alienspecies <- alienspecies[!alienspecies$Taxon=="Cygnus olor",] # 
  
  alienspecies <- unique(alienspecies)
  
  # alienspecies[alienspecies$Taxon%in%einheimisch_stand$WISS_NAME,]

  ## Create Workbook object and add worksheets for output ##############################################
  wb <- createWorkbook()
  hs2 <- createStyle(halign = "center", valign = "center", textDecoration = "bold",border = "Bottom")
  
  addWorksheet(wb, "GesamtListeGebietsfremdeArten")
  writeData(wb,"GesamtListeGebietsfremdeArten",alienspecies, headerStyle = hs2)
  
  ## export file (overrides existing file!) ##########################
  saveWorkbook(wb, file.path("ListeNeobiota","Daten","Output","ListeGebietsfremderArten_gesamt_standardisiert.xlsx"), overwrite = T, returnValue = FALSE)
  
}

  
  