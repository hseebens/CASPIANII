################################################################################
#
# Skript ist Teil des Workflows "SDM" zur Habitatmodellierung von Neobiota in 
# Deutschland. Es wird mit dem Skript run_SDM_workflow.R aufgerufen.

# Dieses Skript ueberprueft die erforderliche Datensaetze.
#
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 08.12.25
################################################################################


ueberpruefeDatensaetze <- function(Klima_var=NULL,
                                     Landbedeck_var=NULL,
                                     Name_Artenliste=NULL){

  x <- 0 
  
  ## check availability of climate files
  if (!is.null(Klima_var)){
    
    filenames <- paste0("wc2.1_30s_bio_",gsub("bio","",Klima_var),".tif") # generate file names
  
    if (any(file.exists(file.path("SDM","Daten","Input","WorldClim",filenames))==FALSE) &
        !file.exists(file.path("SDM","Daten","Input","WorldClim","WorldClim2.1_RasterStackEurope_bio_30s.tif"))){
      
      ind <- which(file.exists(file.path("SDM","Daten","Input","WorldClim",filenames))==FALSE)

      x <- 1
      stop(paste0("\n Datensatz ",filenames[ind]," oder WorldClim2.1_RasterStackEurope_bio_30s.tif fehlen im Verzeichnis",file.path("SDM","Daten","Input","WorldClim"),". Bitte ergaenzen.\n"))
    }
  }
  
  ## check availability of land cover files
  if (!is.null(Landbedeck_var)){
    if (any(file.exists(file.path("SDM","Daten","Input","CorineLandcover","LandCover_Corine_allclasses.tif"))==FALSE)){
      ind <- which(file.exists(file.path("SDM","Daten","Input","CorineLandcover","LandCover_Corine_allclasses.tif"))==FALSE)
      stop(paste0("\n Datensatz ",LC_files[ind]," fehlt im Verzeichnis",file.path("SDM","Daten","Input","CorineLandcover"),". Bitte ergaenzen.\n"))
      x <- 1
    }
  }
  
  ## check availability of species list
  if (any(file.exists(file.path("SDM","Daten","Input",Name_Artenliste))==FALSE)){
    stop(paste0("\n Datensatz ",Name_Artenliste," fehlt im Verzeichnis",file.path("SDM","Daten","Input")))
    x <- 1
  }

  ## check availability of country borders
  if (any(file.exists(file.path("SDM","Daten","Input","Shapefiles","ne_50m_land.shp"))==FALSE)){
    stop(paste0("\n Datensatz ne_50m_land.shp fehlt im Verzeichnis",file.path("SDM","Daten","Input","Shapefiles")))
    x <- 1
  }
  
  ## check availability of borders of federal states
  if (any(file.exists(file.path("SDM","Daten","Input","Shapefiles","gadm41_DEU_1.shp"))==FALSE)){
    stop(paste0("\n Datensatz gadm41_DEU_1.shp fehlt im Verzeichnis",file.path("SDM","Daten","Input","Shapefiles")))
    x <- 1
  }
  
  ## check availability of borders of municipalities
  if (any(file.exists(file.path("SDM","Daten","Input","Shapefiles","gadm41_DEU_3.shp"))==FALSE)){
    stop(paste0("\n Datensatz gadm41_DEU_3 fehlt im Verzeichnis",file.path("SDM","Daten","Input","Shapefiles")))
    x <- 1
  }
  
  if (x==0){
    cat("\n Alle notwendingen Datensaetze vorhanden.\n")
  }
  
}