################################################################################
#
# Skript ist Teil des Workflows "SDM" zur Habitatmodellierung von Neobiota in 
# Deutschland. Es wird mit dem Skript run_SDM_workflow.R aufgerufen.
#
# Das Skript extrahiert ausgewählte Umweltdaten zu Vorkommensdaten der gewählten 
# Art. 
#
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 08.12.25
################################################################################


ermittleUmweltdaten <- function(TaxonName=NULL, 
                                Vorkommen=NULL,
                                identifier=NULL,
                                Klima_var=NULL, 
                                Landbedeck_var=NULL, 
                                Ausschnitt=NULL,
                                min_limit=50,
                                max_limit=10000,
                                plot_predictors=FALSE,
                                check_corr=FALSE) { ## start of main function
  
  cat(paste0("\n*** Ermittle Umweltdaten für ",TaxonName," ***\n") ) # notification for the user

  ## load status file for reporting 
  status_species <- read.xlsx(file.path("SDM","Daten","Output",paste0("StatusModellierung",identifier,".xlsx",sep="")),sheet=1)
  ind_species <- which(status_species$Taxon==TaxonName)

  ## all variables  
  col_names <- c(Klima_var, Landbedeck_var)
  
  ## Extent of selection
  ext_stack <- ext(c(Ausschnitt[1],Ausschnitt[3],Ausschnitt[2],Ausschnitt[4]))

  ## generate file of environmental variables if existing; otherwise, generate it ########################
  if (!file.exists(file.path("SDM","Daten","Input",paste0("UmweltdatenRaster",identifier,".tif")))){
    
    cat("\n Erstelle Rasterdatensatz der Umweltvariablen:", file.path("SDM","Daten","Input","WorldClim", paste0("UmweltdatenRaster",identifier,".tif")),"\n")
    
    predictor_stack <- list()
    
    ## climate data
    if (!is.null(Klima_var)){
  
      if (file.exists(file.path("SDM","Daten","Input","WorldClim","WorldClim2.1_RasterStackEurope_bio_30s.tif"))){
        
        envstack <- rast(file.path("SDM","Daten","Input","WorldClim","WorldClim2.1_RasterStackEurope_bio_30s.tif")) 
        
        ## crop to extent
        envstack  <- crop(envstack, ext_stack) # crop the climate data to the extent of the land cover data (needed because the climate data has a global extent and the land cover data has an European extent)
        
        predictor_stack <- c(predictor_stack,envstack)
        
      } else {
        
        ## generate file names
        filenames <- paste0("wc2.1_30s_bio_",gsub("bio","",Klima_var),".tif")
        
        ## check availability of land cover files
        if (any(file.exists(file.path("SDM","Daten","Input","WorldClim",filenames))==FALSE)){
          ind <- which(file.exists(file.path("SDM","Daten","Input","WorldClim",filenames))==FALSE)
          cat("\n Datensatz ",filenames[ind]," fehlt im Verzeichnis",file.path("SDM","Daten","Input","WorldClim"),". Bitte ergänzen.\n")
        }
        
        envstack <- rast(file.path("SDM","Daten","Input","WorldClim",filenames)) 
        
        ## crop to extent
        envstack  <- crop(envstack, ext_stack) # crop the climate data to the extent of the land cover data (needed because the climate data has a global extent and the land cover data has an European extent)
        
        predictor_stack <- c(predictor_stack,envstack)
        
      }
    }
    
    ## land cover data
    if (!is.null(Landbedeck_var)){ # load the raster stack with the land cover data
  
      ## check availability of land cover files
      if (any(file.exists(file.path("SDM","Daten","Input","CorineLandcover","LandCover_Corine_allclasses.tif"))==FALSE)){
        cat("\n Datensatz LandCover_Corine_allclasses.tif fehlt im Verzeichnis",file.path("SDM","Daten","Input","CorineLandcover"),". Bitte ergänzen.\n")
        stop()
      }
      
      ## generate folder paths
      LCStack <- rast(file.path("SDM","Daten","Input","CorineLandcover","LandCover_Corine_allclasses.tif")) 
  
      LCStack <- LCStack[[as.numeric(gsub("LC","", Landbedeck_var))]]
      names(LCStack) <- Landbedeck_var # rename variables to allow the application of gam() later on
      
      ## crop to extent
      LCStack <- crop(LCStack, ext_stack) # crop the climate data to the extent of the land cover data (needed because the climate data has a global extent and the land cover data has an European extent)
      
      predictor_stack <- c(predictor_stack,LCStack)
      
      # (Note: cropping not 100% precise)
    }
  
    ## merge if climate and land cover variables provided
    if (!is.null(Landbedeck_var) & !is.null(Klima_var)){
      
      ## resample land cover to fit to climate raster (cropping for land cover not exact)
      new_raster <- LCStack[[1]]
      values(new_raster) <- NA
      envstack_2 <- list()
      
      for (j in 1:nlyr(envstack)){
        
        new_climate <- terra::resample(envstack[[j]],new_raster)
        new_climate[new_climate<0] <- 0 # method may produce negative proportions
        envstack_2[[j]] <- new_climate
      }
      envstack <- rast(envstack_2)
      
      predictor_stack <- c(envstack, LCStack) # stack climate and land cover layers
      
    } else {
      
      predictor_stack <- rast(predictor_stack)
      
    }
    
    ## save output to disk
    writeRaster(predictor_stack,file.path("SDM","Daten","Input",paste0("UmweltdatenRaster",identifier,".tif")), filetype = "GTiff",overwrite=TRUE)
    
  } else {
    
    predictor_stack <- rast(file.path("SDM","Daten","Input",paste0("UmweltdatenRaster",identifier,".tif")))

  }
  
  if (plot_predictors){
    dev.new()
    plot(predictor_stack) # plot the layers
  }
  
  if (check_corr & nlyr(predictor_stack)>1){
    
    cat("\n Teste Korrelationen der Umweltvariablen. Dies kann ein paar Minuten dauern...")
    
    corr <- layerCor(predictor_stack, 'pearson', use="complete.obs", maxcell=1000000) # correlation test of environmental variables 
    col_names <- names(predictor_stack)
    
    ## identify variable pairs with high correlation
    ind_highcorr <- which(corr$correlation>0.5,arr.ind=T)
    ind_highcorr <- ind_highcorr[ind_highcorr[,1]!=ind_highcorr[,2],]
    
    if (nrow(ind_highcorr)>0){
      ind_dupl <- duplicated(corr$correlation[ind_highcorr]) # remove lower triangle of corr matrix
      
      highcorr <- cbind.data.frame(col_names[ind_highcorr[,1]][ind_dupl],col_names[ind_highcorr[,2]][ind_dupl],
                                   round(corr$correlation[ind_highcorr[ind_dupl][2],ind_highcorr[ind_dupl][1]],2))
      colnames(highcorr) <- c("Var1","Var2","R2")
      
      cat("\n Folgende Variablen sind mit |r|>0.5 korreliert und einzelne Variablen könnten entfernt werden: \n")
      print(highcorr[!ind_dupl,])
    }
  }

  ## load species' occurrence records  
  if (is.null(Vorkommen)){ # check if occurrence data are provided; if not, load from disk
    Vorkommen <- fread(file.path("SDM","Daten","Input",paste0("Vorkommen_",TaxonName,identifier,".csv"))) # stores the final occurrence file on the users computer
  }
  
  
  ## check for large data sets and down-sampled those (workflow cannot handle very large files (>>20000 occurrence records))
  
  ## thinning of records (to avoid over-emphasis of highly sampled areas) ##################
  template <- predictor_stack[[1]]
  template <- disagg(template, fact=2)

  ## thinning to raster template
  occ_thinner <- thin_points(data=as.matrix(Vorkommen[, c("Laengengrad", "Breitengrad")]),
                             lon_col="Laengengrad",
                             lat_col="Breitengrad",
                             method="grid",
                             raster_obj=template,
                             trials=10)
  retained_records <- unlist(occ_thinner[["retained"]])
  
  # ind_rows <- sample(1:nrow(Vorkommen), max_limit)
  Vorkommen <- Vorkommen[retained_records, ]

  cat(paste0("\n Anzahl Vorkommen werden zur Modellierung für ",TaxonName, " auf ", sum(retained_records), " Einträge ausgedünnt. \n"))
  
  ## still too many records?
  if (nrow(Vorkommen)>max_limit){
    
    cat(paste0("\n Anzahl Vorkommen weiterhin zu hoch (n=",nrow(Vorkommen),") zur Modellierung für ",TaxonName,". Datensatz wird auf max_limit=", max_limit, " zufällig reduziert. \n"))
    
    ind_rows <- sample(1:nrow(Vorkommen), max_limit)
    Vorkommen <- Vorkommen[ind_rows, ]
    
  }
  
  ## extract environmental data for occurrence records
  if(nlyr(predictor_stack)==1) {
    occenv <- cbind(Vorkommen, pred_var = terra::extract(x = predictor_stack, y = data.frame(Vorkommen[,c('Laengengrad','Breitengrad')]), ID=FALSE))
    colnames(occenv) <- c(colnames(Vorkommen),col_names)
  } else {
    occenv <- cbind(Vorkommen, terra::extract(x = predictor_stack, y = data.frame(Vorkommen[,c('Laengengrad','Breitengrad')]), ID=FALSE))
  }

  ## remove occurrence points without environmental data
  occenv <- as.data.frame(occenv,stringsAsFactors=F)
  occenv <- occenv[complete.cases(occenv[,-which(colnames(occenv)=="Zeitpunkt")]),] 

  ## check for missing data
  if (nrow(occenv)<min_limit){
    
    warning("Keine ausreichende Überschneidung von Daten zum Vorkommen und Umweltvariablen. Kein output!")

    ## write status to log file
    status_species$Status[ind_species] <- "Keine Habitatmodellierung, da keine ausreichende Überschneidung von Daten zum Vorkommen und Umweltvariablen vorliegt."
    
    ## export status of species list
    write.xlsx(status_species,file=file.path("SDM","Daten","Output",paste0("StatusModellierung",identifier,".xlsx",sep="")))
    
    return()
    
  } else {
    
    if (any(colSums(occenv[,(dim(Vorkommen)[2]+1):dim(occenv)[2]])==0)){
      cat(" Für die folgenden Landbeckungsvariablen liegen keine Vorkommen der Art vor und sollten aus 'landcov' entfernt werden:")  
      # print("Note: The following land cover variables only take on the value 0 across the occurrences of your focal species. Please remove/replace these variables to avoid errors while model fitting due to uninformative predictors.") # notification for the user
      print(names(which(colSums(occenv[,(dim(Vorkommen)[2]+1):dim(occenv)[2]])==0)))
    }
    
    ## output  
    fwrite(occenv, file.path("SDM","Daten","Input",paste0("VorkommenUmweltdaten_",TaxonName,identifier,".csv"))) # stores the final occurrence file on the users computer
    
    ## export status of species list
    write.xlsx(status_species,file=file.path("SDM","Daten","Output",paste0("StatusModellierung",identifier,".xlsx",sep="")))
    
    return(occenv) 
  }
}  ## end of main function
