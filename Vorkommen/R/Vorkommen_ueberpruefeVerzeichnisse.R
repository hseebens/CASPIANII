##################### Ermittlung von Vorkommensdaten fuer einzelne Arten #######
# 
# Skript ist Teil des Workflows "Vorkommen" zur Darstellung der Vorkommen von
# Arten in Deutschland. Es wird mit dem Skript erstelleListeNeobiota_MAIN.R 
# aufgerufen.
#
# Dieses Skript ueberprueft die erforderliche Verzeichnisstruktur und ergaenzt
# fehlende Verzeichnisse.
# 
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 08.12.25
################################################################################


ueberpruefe_Datenverzeichnis <- function (){
  
  x <- 0 # dummy variable 
  
  ## create output folder #####
  if (!file.exists(file.path("Vorkommen","Daten"))){
    dir.create("Vorkommen","Daten")
    cat("\n Verzeichnis 'Vorkommen/Daten' erstellt.\n") # notification for the user
    x <- 1
  }
  
  if (x==0){
    cat("\n Alle Verzeichnisse sind bereits vorhanden.\n") # notification for the user
  }
}  