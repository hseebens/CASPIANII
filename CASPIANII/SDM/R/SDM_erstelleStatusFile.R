##################################################################################
#
# Dieses erstellt eine Tabelle zum Status der Modellierung der Arten.
#
# Hanno Seebens, 17.07.2023
##################################################################################


erstelleStatusFile <- function(Name_Artenliste=NULL,
                               identifier=identifier,
                               Min_Anzahl_GBIF_DE=50,
                               Max_Anzahl_GBIF_DE=20000){
  
  ## check identifier separator
  if (strtrim(identifier,1)!="_"){
    identifier <- paste0("_",identifier)
  }
  
  status_species <- as.data.frame(read.xlsx(file.path("SDM","Data","Input",Name_Artenliste),sheet=1)[,c("Taxon","Eintraege_GBIF_DE")])
  
  status_species$Status <- "Bisher keine Habitatmodellierung durchgefuehrt."
  
  status_species$Status[status_species$Eintraege_GBIF_DE<Min_Anzahl_GBIF_DE] <- "Keine Habitatmodellierung, da die Datenmenge der Vorkommensdaten nicht ausreicht."
  status_species$Status[status_species$Eintraege_GBIF_DE>Max_Anzahl_GBIF_DE] <- "Datenmenge zu groß für diesen Workflow. Bitte alternativen Weg mit 'SDM_bezieheHoheDatenmengen.R' verwenden."
  
  ## speichere vorlaeufige Liste des Status aller Arten
  write.xlsx(status_species,file=file.path("SDM","Data","Output",paste0("StatusModellierung",identifier,".xlsx",sep="")))
}