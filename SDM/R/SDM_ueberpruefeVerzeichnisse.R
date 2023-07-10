##################################################################################
#
# Dieses Skript ueberprueft die erforderliche Verzeichnisstruktur und ergaenzt
# fehlende Verzeichnisse.
#
# Hanno Seebens, 20.06.2023
##################################################################################


ueberpruefe_Verzeichnisse <- function (){
  
  x <- 0 # dummy variable 
  
  ## create output folder #####
  if (!file.exists(file.path("SDM","Data"))){
    dir.create("SDM","Data")
    cat("\n Verzeichnis 'SDM/Data' erstellt.\n") # notification for the user
    x <- 1
  }
  if (!file.exists(file.path("SDM","Data","Output"))){
    dir.create(file.path("SDM","Data","Output"))
    cat("\n Verzeichnis 'SDM/Data/Output' erstellt.\n") # notification for the user
    x <- 1
  }
  if (!file.exists(file.path("SDM","Data","Output", "Grafiken"))){
    dir.create(file.path("SDM","Data","Output", "Grafiken"))
    cat("\n Verzeichnis 'SDM/Data/Output' erstellt.\n") # notification for the user
    x <- 1
  }
  if (!file.exists(file.path("SDM","Data","Input"))){
    dir.create(file.path("SDM","Data","Input"))
    cat("\n Verzeichnis 'SDM/Data/Input' erstellt.\n") # notification for the user
    x <- 1
  }
  if (!file.exists(file.path("SDM","Data","Input","Shapefiles"))){
    dir.create(file.path("SDM","Data","Input","Shapefiles"))
    cat("\n Verzeichnis 'SDM/Data/Input/Shapefiles' erstellt.\n") # notification for the user
    x <- 1
  }
  if (!file.exists(file.path("SDM","Data","Input","WorldClim"))){
    dir.create(file.path("SDM","Data","Input","WorldClim"))
    cat("\n Verzeichnis 'SDM/Data/Input/WorldClim' erstellt.\n") # notification for the user
    x <- 1
  }
  if (!file.exists(file.path("SDM","Data","Input","CorineLandcover"))){
    dir.create(file.path("SDM","Data","Input","CorineLandcover"))
    cat("\n Verzeichnis 'SDM/Data/Input/CorineLandcover' erstellt.\n") # notification for the user
    x <- 1
  }
  if (x==0){
    cat("\n Alle Verzeichnisse sind bereits vorhanden.\n") # notification for the user
  }
}  