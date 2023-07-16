###########################################################################################################
#
# Function to automatically download WorldClim environmental variables, 
# test for their correlation and add them to the table with the occurrence records of the focal species.
# In addition, it tests for an association of the land cover data and the climate data
# This function is part of the SDM workflow. 
# This version is used, when land cover data is provided in the run script.
#
# Author: Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 23.06.23
##########################################################################################################


ermittleUmweltdaten <- function(TaxonName=NULL, 
                                Vorkommen=NULL,
                                identifier=NULL,
                                Klima_var=NULL, 
                                Landbedeck_var=NULL, 
                                Ausschnitt=NULL,
                                plot_predictors=TRUE) { ## start of main function
  
  cat(paste0("\n*** Ermittle Umweltdaten für ",TaxonName," ***\n") ) # notification for the user

  
  ## load status file for reporting 
  status_species <- read.xlsx(file.path("SDM","Data","Output","Status_Arten.xlsx"),sheet=1)
  ind_species <- which(status_species$Taxon==TaxonName)

  ## all variables  
  col_names <- c(Klima_var,Landbedeck_var)
  
  ## Extent of selection
  ext_stack <- extent(c(Ausschnitt[1],Ausschnitt[3],Ausschnitt[2],Ausschnitt[4]))
  
  predictor_stack <- list()
  
  ## climate data
  if (!is.null(Klima_var)){
    
    # # download the full set of bioclimatic variables from worldclim at 2.5 min resolution, result is a raster stack
    # fullenvir <- suppressMessages(suppressWarnings(raster::getData(name = "worldclim",var = "bio", res = 2.5)))
    # envstack <- subset(fullenvir, Klima_var) # subset the worldclim data to the environmental data of interest as specified by the user
    # rm(fullenvir)
    
    # ## get environmental data from disk at high resolution
    # filenames <- paste0("wc2.1_30s_bio_",gsub("bio","",Klima_var),".tif")
    # envstack <- rast(file.path("..","..","..","..","Storage_large","Climate",filenames))
    
    ## generate file names
    filenames <- paste0("wc2.1_2.5m_bio_",gsub("bio","",Klima_var),".tif")

    ## check availability of land cover files
    if (any(file.exists(file.path("SDM","Data","Input","WorldClim",filenames))==FALSE)){
      ind <- which(file.exists(file.path("SDM","Data","Input","WorldClim",filenames))==FALSE)
      cat("\n Datensatz ",filenames[ind]," fehlt im Verzeichnis",file.path("SDM","Data","Input","WorldClim"))
    }

    # envstack <- raster(file.path("SDM","Data","Input","WorldClim",filenames)) # workstation
    envstack <- rast(file.path("..","..","..","DATA","Environmental","WorldClim",filenames)) # local
    
    ## crop to extent
    envstack  <- crop(envstack, ext_stack) # crop the climate data to the extent of the land cover data (needed because the climate data has a global extent and the land cover data has an European extent)
    
    predictor_stack <- c(predictor_stack,envstack)
  }
  
  ## land cover data
  if (!is.null(Landbedeck_var)){ # load the raster stack with the land cover data

    ## all land cover file
    # LC_files <- c("LC1.tif","LC2.tif", "LC3.tif", "LC4.tif", "LC5.tif", "LC6.tif", "LC7.tif", "LC8.tif", "LC9.tif", "LC10.tif",
    #               "LC11.tif","LC12.tif", "LC13.tif", "LC14.tif", "LC15.tif", "LC16.tif", "LC17.tif", "LC18.tif", "LC19.tif", "LC20.tif",
    #               "LC21.tif","LC22.tif", "LC23.tif", "LC24.tif", "LC25.tif", "LC26.tif", "LC27.tif", "LC28.tif", "LC29.tif", "LC30.tif",
    #               "LC31.tif","LC32.tif", "LC33.tif", "LC34.tif", "LC35.tif", "LC36.tif", "LC37.tif", "LC38.tif", "LC39.tif", "LC40.tif",
    #               "LC41.tif","LC42.tif", "LC43.tif", "LC44.tif")
    LC_files <- paste0(Landbedeck_var,".tif")
    
    ## check availability of land cover files
    if (any(file.exists(file.path("SDM","Data","Input","CorineLandcover",LC_files))==FALSE)){
      ind <- which(file.exists(file.path("SDM","Data","Input","CorineLandcover",LC_files))==FALSE)
      cat("\n Datensatz ",LC_files[ind]," fehlt im Verzeichnis",file.path("SDM","Data","Input","CorineLandcover"))
    }
    
    ## generate folder paths
    LC_pathnames_all <- c()
    for (i in 1:length(LC_files)){
      LC_pathnames_all <- c(LC_pathnames_all,file.path("SDM","Data","Input","CorineLandcover",LC_files[i]))
    }
    LCStack <- rast(LC_pathnames_all) 
    
    # LCStack <- subset(LCStack, Landbedeck_var) # subset the land cover data to the environmental data of interest as specified by the user
    
    ## crop to extent
    LCStack <- crop(LCStack, ext_stack) # crop the climate data to the extent of the land cover data (needed because the climate data has a global extent and the land cover data has an European extent)
    
    predictor_stack <- c(predictor_stack,LCStack)
    
    # (Note: cropping not 100% precise)
  }

  ## merge if climate and land cover variables provided
  if (!is.null(Landbedeck_var) & !is.null(Klima_var)){
    
    ## resample land cover to fit to climate raster (cropping for land cover not exact)
    new_raster <- envstack[[1]]
    values(new_raster) <- NA
    LCStack_2 <- list()
    for (i in 1:nlyr(LCStack)){
      new_LC <- resample(LCStack[[i]],new_raster,method="bilinear")
      new_LC[new_LC<0] <- 0 # method may produce negative proportions
      LCStack_2[[i]] <- new_LC
    }
    LCStack <- rast(LCStack_2)
    
    predictor_stack <- c(envstack, LCStack) # stack climate and land cover layers
  } else {
    predictor_stack <- rast(predictor_stack)
  }

  
  if (plot_predictors){
    x11()
    plot(predictor_stack) # plot the layers
  }
  
  ## save output to disk
  writeRaster(predictor_stack,file.path("SDM","Data","Input",paste0("UmweltdatenRaster_",TaxonName,"_",identifier,".tif")), filetype = "GTiff",overwrite=TRUE)
  
  if (nlyr(predictor_stack)>1){
    corr <- layerCor(predictor_stack, 'pearson', na.rm=T) # correlation test of environmental variables of choice
    col_names <- names(predictor_stack)
    
    ## identify variable pairs with high correlation
    ind_highcorr <- which(corr$`pearson`>0.5,arr.ind=T)
    ind_highcorr <- ind_highcorr[ind_highcorr[,1]!=ind_highcorr[,2],]
    
    if (nrow(ind_highcorr)>0){
      ind_dupl <- duplicated(corr$`pearson`[ind_highcorr]) # remove lower triangle of corr matrix
      
      highcorr <- cbind.data.frame(col_names[ind_highcorr[,1]][ind_dupl],col_names[ind_highcorr[,2]][ind_dupl],
                                   round(corr$`pearson`[ind_highcorr[ind_dupl][2],ind_highcorr[ind_dupl][1]],2))
      colnames(highcorr) <- c("Var1","Var2","R2")
      
      cat("\n Folgende Variablen sind mit |r|>0.5 korreliert und einzelne Variablen koennten entfernt werden: \n")
      print(highcorr[!ind_dupl,])
    }
  }

  ## load species' occurrence records  
  if (is.null(Vorkommen)){ # check if occurrence data are provided; if not, load from disk
    Vorkommen <- fread(file.path("SDM","Data","Input",paste0("Vorkommen_",TaxonName,"_",identifier,".csv"))) # stores the final occurrence file on the users computer
  }
  
  ## check for large data sets and down-sampled those (workflow cannot handle very large files (>>20000 occurrence records))
  if (nrow(Vorkommen)>20000){
    
    warning(paste0("\n Anzahl Vorkommen zu hoch (n=",nrow(Vorkommen),"). Datensatz wird auf max 20000 Einträge reduziert. \n"))
    
    ind_rows <- sample(1:nrow(Vorkommen),20000)
    Vorkommen <- Vorkommen[ind_rows,]
  }
  
  ## extract environmental data for occurrence records
  if(nlyr(predictor_stack)==1) {
    occenv <- cbind(Vorkommen, pred_var = terra::extract(x = predictor_stack, y = data.frame(Vorkommen[,c('Laengengrad','Breitengrad')]), ID=FALSE))
    colnames(occenv) <- c(colnames(Vorkommen),col_names)
  } else {
    occenv <- cbind(Vorkommen, terra::extract(x = predictor_stack, y = data.frame(Vorkommen[,c('Laengengrad','Breitengrad')]), ID=FALSE))
  }

  ## remove occurrence points without environmental data
  occenv <- occenv[complete.cases(occenv),] 

  ## check for missing data
  if (nrow(occenv)<50){
    
    warning("Keine ausreichende Überschneidung von Daten zum Vorkommen und Umweltvariablen. Kein output!")

    ## write status to log file
    status_species$Status[ind_species] <- "Keine Habitatmodellierung, da keine ausreichende Überschneidung von Daten zum Vorkommen und Umweltvariablen vorliegt."
    
    return()
    
  } else {
    
    if (any(colSums(occenv[,(dim(Vorkommen)[2]+1):dim(occenv)[2]])==0)){
      cat(" Die folgenden Landbeckungsvariablen beinhalten nur 0 für die Vorkommen der Art und sollten aus 'landcov' entfernt werden:")  
      # print("Note: The following land cover variables only take on the value 0 across the occurrences of your focal species. Please remove/replace these variables to avoid errors while model fitting due to uninformative predictors.") # notification for the user
      print(names(which(colSums(occenv[,(dim(Vorkommen)[2]+1):dim(occenv)[2]])==0)))
    }
    
    ## output  
    fwrite(occenv, file.path("SDM","Data","Input",paste0("VorkommenUmweltdaten_",TaxonName,"_",identifier,".csv"))) # stores the final occurrence file on the users computer
    
    return(occenv) 
  }
  
  ## export status of species list
  write.xlsx(status_species,file=file.path("SDM","Data","Output","Status_Arten.xlsx"))
  
} ## end of main function
