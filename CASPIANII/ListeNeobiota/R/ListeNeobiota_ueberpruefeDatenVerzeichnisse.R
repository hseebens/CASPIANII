##################################################################################
#
# Dieses Skript ueberprueft die erforderliche Verzeichnisstruktur und ergaenzt
# fehlende Verzeichnisse.
#
# Hanno Seebens, 20.06.2023
##################################################################################


ueberpruefe_Datenverzeichnisse <- function (){
  
  x <- 0 # dummy variable 
  
  ## create output folder #####
  if (!file.exists(file.path("ListeNeobiota","Data"))){
    dir.create("SDM","Data")
    cat("\n Verzeichnis 'ListeNeobiota/Data' erstellt.\n") # notification for the user
    x <- 1
  }
  if (!file.exists(file.path("ListeNeobiota","Data","Input"))){
    dir.create(file.path("ListeNeobiota","Data","Input"))
    cat("\n Verzeichnis 'ListeNeobiota/Data/Input' erstellt.\n") # notification for the user
    x <- 1
  }
  if (!file.exists(file.path("ListeNeobiota","Data","Output"))){
    dir.create(file.path("ListeNeobiota","Data","Output"))
    cat("\n Verzeichnis 'ListeNeobiota/Data/Output' erstellt.\n") # notification for the user
    x <- 1
  }
  if (!file.exists(file.path("ListeNeobiota","Data","Shapefiles"))){
    dir.create(file.path("ListeNeobiota","Data","Shapefiles"))
    cat("\n Verzeichnis 'ListeNeobiota/Data/Shapefiles' erstellt.\n") # notification for the user
    x <- 1
  }

  if (x==0){
    cat("\n Alle Verzeichnisse sind bereits vorhanden.\n") # notification for the user
  }
  
  
  ### check data sets ######################
  
  y <- 0
  
  ## check availability of borders of federal states
  if (any(file.exists(file.path("ListeNeobiota","Data","Shapefiles","RegionsTerrMarine_160621_Germany.shp"))==FALSE)){
    stop(paste0("\n Datensatz RegionsTerrMarine_160621_Germany.shp fehlt im Verzeichnis",file.path("ListeNeobiota","Data","Shapefiles")))
    y <- 1
  }
  
  ## check availability of borders of federal states
  if (any(file.exists(file.path("ListeNeobiota","Data","Input","ListeGebietsfremderArten_einzelneDB_Rohdaten.xlsx"))==FALSE)){
    stop(paste0("\n Datensatz ListeGebietsfremderArten_einzelneDB_Rohdaten.xlsx fehlt im Verzeichnis",file.path("SDM","Data","Input")))
    y <- 1
  }
  
  ## check availability of borders of federal states
  if (any(file.exists(file.path("ListeNeobiota","Data","Input","List_IAS_union_concern.xlsx"))==FALSE)){
    stop(paste0("\n Datensatz List_IAS_union_concern.xlsx fehlt im Verzeichnis",file.path("SDM","Data","Input")))
    y <- 1
  }
  
  ## check availability of borders of federal states
  if (any(file.exists(file.path("ListeNeobiota","Data","Input","skript519_checkliste.xlsx"))==FALSE)){
    stop(paste0("\n Datensatz skript519_checkliste.xlsx fehlt im Verzeichnis",file.path("SDM","Data","Input")))
    y <- 1
  }
  
  ## check availability of borders of federal states
  if (any(file.exists(file.path("ListeNeobiota","Data","Input","PfadUebersetzung.xlsx"))==FALSE)){
    stop(paste0("\n Datensatz PfadUebersetzung.xlsx fehlt im Verzeichnis",file.path("SDM","Data","Input")))
    y <- 1
  }
  
  ## check availability of borders of federal states
  if (any(file.exists(file.path("ListeNeobiota","Data","Input","INTRODUCTION_PATHWAYS.csv"))==FALSE)){
    stop(paste0("\n Datensatz INTRODUCTION_PATHWAYS.csv fehlt im Verzeichnis",file.path("SDM","Data","Input")))
    y <- 1
  }
  
  if (x==0){
    cat("\n Alle notwendigen Datensaetze vorhanden.\n")
  }
  
}  