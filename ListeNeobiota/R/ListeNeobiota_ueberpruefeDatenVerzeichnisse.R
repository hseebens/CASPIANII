################################################################################
#
# Skript ist Teil des Workflows "ListeNeobiota" zur Erstellung einer 
# einheitlichen Liste von Neobiota in Deutschland. Es wird mit dem Skript 
# erstelleListeNeobiota_MAIN.R aufgerufen.
#
# Dieses Skript ueberprueft die erforderliche Verzeichnisstruktur und ergaenzt
# fehlende Verzeichnisse.
#
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 08.12.25
################################################################################



ueberpruefe_Datenverzeichnisse <- function (){
  
  x <- 0 # dummy variable 
  
  ## create output folder #####
  if (!file.exists(file.path("ListeNeobiota","Daten"))){
    dir.create("ListeNeobiota","Daten")
    cat("\n Verzeichnis 'ListeNeobiota/Daten' erstellt.\n") # notification for the user
    x <- 1
  }
  if (!file.exists(file.path("ListeNeobiota","Daten","Input"))){
    dir.create(file.path("ListeNeobiota","Daten","Input"))
    cat("\n Verzeichnis 'ListeNeobiota/Daten/Input' erstellt.\n") # notification for the user
    x <- 1
  }
  if (!file.exists(file.path("ListeNeobiota","Daten","Output"))){
    dir.create(file.path("ListeNeobiota","Daten","Output"))
    cat("\n Verzeichnis 'ListeNeobiota/Daten/Output' erstellt.\n") # notification for the user
    x <- 1
  }

  if (x==0){
    cat("\n Alle Verzeichnisse sind bereits vorhanden.\n") # notification for the user
  }
  
  
  ### check data sets ######################
  
  y <- 0

  ## check availability of borders of federal states
  if (any(file.exists(file.path("ListeNeobiota","Daten","Input","ListeGebietsfremderArten_einzelneDB_Rohdaten.xlsx"))==FALSE)){
    stop(paste0("\n Datensatz ListeGebietsfremderArten_einzelneDB_Rohdaten.xlsx fehlt im Verzeichnis",file.path("ListeNeobiota","Daten","Input")))
    y <- 1
  }
  
  ## check availability of borders of federal states
  if (any(file.exists(file.path("ListeNeobiota","Daten","Input","List_IAS_union_concern.xlsx"))==FALSE)){
    stop(paste0("\n Datensatz List_IAS_union_concern.xlsx fehlt im Verzeichnis",file.path("ListeNeobiota","Daten","Input")))
    y <- 1
  }
  
  ## check availability of borders of federal states
  if (any(file.exists(file.path("ListeNeobiota","Daten","Input","skript519_checkliste.xlsx"))==FALSE)){
    stop(paste0("\n Datensatz skript519_checkliste.xlsx fehlt im Verzeichnis",file.path("ListeNeobiota","Daten","Input")))
    y <- 1
  }
  
  ## check availability of borders of federal states
  if (any(file.exists(file.path("ListeNeobiota","Daten","Input","PfadUebersetzung.xlsx"))==FALSE)){
    stop(paste0("\n Datensatz PfadUebersetzung.xlsx fehlt im Verzeichnis",file.path("ListeNeobiota","Daten","Input")))
    y <- 1
  }
  
  ## check availability of borders of federal states
  if (any(file.exists(file.path("ListeNeobiota","Daten","Input","Einfuhrpfade.csv"))==FALSE)){
    stop(paste0("\n Datensatz Einfuhrpfade.csv fehlt im Verzeichnis",file.path("ListeNeobiota","Daten","Input")))
    y <- 1
  }
  
  if (x==0){
    cat("\n Alle notwendigen Datensaetze vorhanden.\n")
  }
  
}  