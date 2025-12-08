################################################################################
#
# Skript ist Teil des Workflows "SDM" zur Habitatmodellierung von Neobiota in 
# Deutschland. Es wird mit dem Skript run_SDM_workflow.R aufgerufen.

# Dieses erstellt eine Tabelle zum Status der Modellierung der ausgewählten Arten.
#
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 08.12.25
################################################################################


erstelleStatusFile <- function(Name_Artenliste=NULL,
                               identifier=identifier,
                               ueberschreiben=FALSE){
  
  ## check identifier separator
  if (strtrim(identifier,1)!="_"){
    identifier <- paste0("_",identifier)
  }
  
  if (file.exists(file.path("SDM","Daten","Output",paste0("StatusModellierung",identifier,".xlsx",sep="")))){
    
    cat("\n Datei zum Status der Modellierung existiert bereits.")
    
    if (ueberschreiben){
      
      cat("\n Da ueberschreiben=TRUE wird Datei zum Status der Modellierung ueberschrieben.")
      
      status_species <- as.data.frame(read.xlsx(file.path("SDM","Daten","Input",Name_Artenliste),sheet=1)[,c("Taxon", "Eintraege_GBIF_DE", "Eintraege_GBIF_Europa")])
      status_species <- unique(status_species)
      
      status_species$Status <- "Bisher keine Habitatmodellierung durchgefuehrt."
      
      # status_species$Status[status_species$Eintraege_GBIF_Europa<=min_limit] <- "Keine Habitatmodellierung, da die Datenmenge der Vorkommensdaten nicht ausreicht."
      # status_species$Status[status_species$Eintraege_GBIF_Europa>=max_limit] <- "Große Datenmenge. Vorkommen sollten mit 'SDM_bezieheHoheDatenmengen.R' bezogen verwenden."
      
      ## speichere vorlaeufige Liste des Status aller Arten
      write.xlsx(status_species,file=file.path("SDM","Daten","Output",paste0("StatusModellierung",identifier,".xlsx",sep="")))
      
    } else {
      
      cat("\n Da ueberschreiben=FALSE wird Datei zum Status der Modellierung nicht ueberschrieben.")

    }
  } else {
    
    status_species <- as.data.frame(read.xlsx(file.path("SDM","Daten","Input",Name_Artenliste),sheet=1)[,c("Taxon", "Eintraege_GBIF_DE", "Eintraege_GBIF_Europa")])
    
    status_species$Status <- "Bisher keine Habitatmodellierung durchgefuehrt."
    
    # status_species$Status[status_species$Eintraege_GBIF_Europa<min_limit] <- "Keine Habitatmodellierung, da die Datenmenge der Vorkommensdaten nicht ausreicht."
    # status_species$Status[status_species$Eintraege_GBIF_Europa>max_limit] <- "Datenmenge zu groß für diesen Workflow. Bitte alternativen Weg mit 'SDM_bezieheHoheDatenmengen.R' verwenden."
    
    ## speichere vorlaeufige Liste des Status aller Arten
    write.xlsx(status_species,file=file.path("SDM","Daten","Output",paste0("StatusModellierung",identifier,".xlsx",sep="")))
    
  }
}