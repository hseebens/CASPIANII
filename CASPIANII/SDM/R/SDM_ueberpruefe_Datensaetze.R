##################################################################################
#
# Dieses Skript ueberprueft die erforderliche Datensaetze.
#
# Hanno Seebens, 17.07.2023
##################################################################################


ueberpruefe_Datensaetze <- function(Klima_var=NULL,
                                     Landbedeck_var=NULL,
                                     Name_Artenliste=NULL){

  x <- 0 
  
  ## check availability of climate files
  if (!is.null(Klima_var)){
    filenames <- paste0("wc2.1_2.5m_bio_",gsub("bio","",Klima_var),".tif") # generate file names
  
    if (any(file.exists(file.path("SDM","Data","Input","WorldClim",filenames))==FALSE)){
      ind <- which(file.exists(file.path("SDM","Data","Input","WorldClim",filenames))==FALSE)
      # cat("\n Datensatz ",filenames[ind]," fehlt im Verzeichnis",file.path("SDM","Data","Input","WorldClim"),". Bitte ergaenzen.\n")
      x <- 1
      stop(paste0("\n Datensatz ",filenames[ind]," fehlt im Verzeichnis",file.path("SDM","Data","Input","WorldClim"),". Bitte ergaenzen.\n"))
    }
  }
  
  ## check availability of land cover files
  if (!is.null(Landbedeck_var)){
    
    LC_files <- paste0(Landbedeck_var,".tif") # generate file names
    
    if (any(file.exists(file.path("SDM","Data","Input","CorineLandcover",LC_files))==FALSE)){
      ind <- which(file.exists(file.path("SDM","Data","Input","CorineLandcover",LC_files))==FALSE)
      stop(paste0("\n Datensatz ",LC_files[ind]," fehlt im Verzeichnis",file.path("SDM","Data","Input","CorineLandcover"),". Bitte ergaenzen.\n"))
      x <- 1
    }
  }
  
  ## check availability of species list
  if (any(file.exists(file.path("SDM","Data","Input",Name_Artenliste))==FALSE)){
    stop(paste0("\n Datensatz ",Name_Artenliste," fehlt im Verzeichnis",file.path("SDM","Data","Input")))
    x <- 1
  }

  ## check availability of country borders
  if (any(file.exists(file.path("SDM","Data","Input","Shapefiles","ne_50m_land.shp"))==FALSE)){
    stop(paste0("\n Datensatz ne_50m_land.shp fehlt im Verzeichnis",file.path("SDM","Data","Input","Shapefiles")))
    x <- 1
  }
  
  ## check availability of borders of federal states
  if (any(file.exists(file.path("SDM","Data","Input","Shapefiles","gadm41_DEU_1.shp"))==FALSE)){
    stop(paste0("\n Datensatz gadm41_DEU_1.shp fehlt im Verzeichnis",file.path("SDM","Data","Input","Shapefiles")))
    x <- 1
  }
  
  ## check availability of borders of municipalities
  if (any(file.exists(file.path("SDM","Data","Input","Shapefiles","gadm41_DEU_3.shp"))==FALSE)){
    stop(paste0("\n Datensatz gadm41_DEU_3 fehlt im Verzeichnis",file.path("SDM","Data","Input","Shapefiles")))
    x <- 1
  }
  
  if (x==0){
    cat("\n Alle notwendingen Datensaetze vorhanden.\n")
  }
  
}