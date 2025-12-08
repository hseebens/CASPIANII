################################################################################
#
# Skript ist Teil des Workflows "SDM" zur Habitatmodellierung von Neobiota in 
# Deutschland. Es wird mit dem Skript run_SDM_workflow.R aufgerufen.

# Dieses Skript überprueft die erforderliche Verzeichnisstruktur und ergänzt
# fehlende Verzeichnisse.
#
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 08.12.25
################################################################################


ueberpruefeVerzeichnisse <- function (){
  
  x <- 0 # dummy variable 
  
  ## create output folder #####
  if (!file.exists(file.path("SDM","Grafiken"))){
    dir.create(file.path("SDM","Grafiken"))
    cat("\n Verzeichnis 'SDM/Grafiken' erstellt.\n") # notification for the user
    x <- 1
  }
  if (!file.exists(file.path("SDM","Daten"))){
    dir.create(file.path("SDM","Daten"))
    cat("\n Verzeichnis 'SDM/Daten' erstellt.\n") # notification for the user
    x <- 1
  }
  if (!file.exists(file.path("SDM","Daten","Output"))){
    dir.create(file.path("SDM","Daten","Output"))
    cat("\n Verzeichnis 'SDM/Daten/Output' erstellt.\n") # notification for the user
    x <- 1
  }
  if (!file.exists(file.path("SDM","Daten","Input"))){
    dir.create(file.path("SDM","Daten","Input"))
    cat("\n Verzeichnis 'SDM/Daten/Input' erstellt.\n") # notification for the user
    x <- 1
  }
  if (!file.exists(file.path("SDM","Daten","Input","Shapefiles"))){
    dir.create(file.path("SDM","Daten","Input","Shapefiles"))
    cat("\n Verzeichnis 'SDM/Daten/Input/Shapefiles' erstellt.\n") # notification for the user
    x <- 1
  }
  if (!file.exists(file.path("SDM","Daten","Input","WorldClim"))){
    dir.create(file.path("SDM","Daten","Input","WorldClim"))
    cat("\n Verzeichnis 'SDM/Daten/Input/WorldClim' erstellt.\n") # notification for the user
    x <- 1
  }
  if (!file.exists(file.path("SDM","Daten","Input","CorineLandcover"))){
    dir.create(file.path("SDM","Daten","Input","CorineLandcover"))
    cat("\n Verzeichnis 'SDM/Daten/Input/CorineLandcover' erstellt.\n") # notification for the user
    x <- 1
  }
  if (x==0){
    cat("\n Alle Verzeichnisse sind bereits vorhanden.\n") # notification for the user
  }
}  