##################################################################################
#
# Dieses Skript ueberprueft die erforderliche Verzeichnisstruktur und ergaenzt
# fehlende Verzeichnisse.
#
# Hanno Seebens, 16.09.2023
##################################################################################


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