
prepareOccurrencesCalibration <- function(TaxonName,
                                          database=c("sMon","iNat") # can be sMon, GBIF or iNat
                                          ){

  # Name des Taxons  
  # TaxonName <- "Impatiens glandulifera"
  
  # lade Daten
  tryCatch(occ_data <- fread(file.path("Vorkommen","Daten",paste0("Vorkommen_",TaxonName,".csv"))), silent = TRUE)
  
  if (nrow(occ_data)==0){
    cat("\n ERROR: Keine Daten zum Vorkommen gefunden. Daten muessen als Vorkommen_[TaxonName].csv in Vorkommen/Daten gespeichert sein\n")
  }
  
  occ_data <- as.data.frame(occ_data)
  
  occ_data <- occ_data[occ_data$Datenbank%in%database,]
  
  # extrahiere Jahr entsprendend der verwendeten Datenbank
  occ_data$Jahr <- NA
  
  if (any(occ_data$Datenbank=="sMon")){
    ind_sMon <- occ_data$Datenbank=="sMon"
    occ_data$Jahr[ind_sMon] <- as.numeric(str_sub(occ_data$Zeitpunkt[ind_sMon], -4))
  }
  if (any(occ_data$Datenbank!="sMon")){
    ind_DB <- occ_data$Datenbank!="sMon"
    occ_data$Jahr[ind_DB] <- year(as.Date(occ_data$Zeitpunkt[ind_DB]))
  }
  occ_data <- occ_data[!is.na(occ_data$Jahr),]
  
  # weise eine Aufenthaltswahrscheinlichkeit zu sofern noch nicht vorhanden (presence-only wird zu 1)
  if (all(colnames(occ_data)!="Aufenthaltsw")){
    occ_data$Aufenthaltsw <- 1
  }
  if (any(is.na(occ_data$Aufenthaltsw))){
    ind_NA <- is.na(occ_data$Aufenthaltsw)
    occ_data$Aufenthaltsw[ind_NA] <- 1
  }

  occ_data <- subset(occ_data, Aufenthaltsw >= 0.9) # verwende nur Aufenthaltswahrscheinlichkeiten >0.9 (von sMon) für die Initialisierung
  
  # erstelle output
  occ_data <- occ_data[!is.na(occ_data$Jahr),]
  
  fwrite(occ_data,file.path("CASPIAN","Daten",paste0("Vorkommen_",TaxonName,"_Kalibrierung.csv")))
  
  cat(paste0("\n Vorkommen von ",TaxonName," im Verzeichnis CASPIAN/Daten gespeichert.\n"))
  
  return(occ_data)
}

