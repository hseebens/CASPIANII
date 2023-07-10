##################### get available records from sMon ########################################################
# 

# 
# Project: CASPIAN II
# 
# Senckenberg Gesellschaft fuer Naturforschung, 04.11.22
###############################################################################################################



bereinigeListe <- function(){

  ## Lade Liste einheimischer Pflanzenarten
  Artenliste <- read.xlsx(file.path("ListeNeobiota","Data","Input","skript519_checkliste.xlsx"),sheet=1)
  
  einheimisch <- subset(Artenliste,FLOR!="E") # waehle einheimische Pflanzenarten
  einheimisch_stand <- einheimisch
  colnames(einheimisch_stand)[colnames(einheimisch_stand)=="VOLLNAME"] <- "Taxon"
  einheimisch_stand <- CheckGBIFTax(einheimisch_stand)
  
  # table(grepl("\\(E\\)",einheimisch$VOLLNAME))
  
  alienspecies <- read.xlsx(file.path("ListeNeobiota","Data","Output","ListeGebietsfremderArten_gesamt_standardisiert.xlsx"),sheet=1)
  
  ## entferne einheimische Pflanzenarten aus Liste
  alienspecies <- alienspecies[!alienspecies$Taxon%in%einheimisch_stand[[1]]$Taxon,]
  # alienspecies$wissenschaftlicherName[alienspecies$wissenschaftlicherName%in%einheimisch$VOLLNAME]
  
  # alienspecies[alienspecies$Taxon%in%einheimisch_stand$WISS_NAME,]
  
  ## Create Workbook object and add worksheets for output ##############################################
  wb <- createWorkbook()
  hs2 <- createStyle(halign = "center", valign = "center", textDecoration = "bold",border = "Bottom")
  
  addWorksheet(wb, "GesamtListeGebietsfremdeArten")
  writeData(wb,"GesamtListeGebietsfremdeArten",alienspecies, headerStyle = hs2)
  
  ## export file (overrides existing file!) ##########################
  saveWorkbook(wb, file.path("ListeNeobiota","Data","Output","ListeGebietsfremderArten_gesamt_standardisiert.xlsx"), overwrite = T, returnValue = FALSE)
  
}

  
  