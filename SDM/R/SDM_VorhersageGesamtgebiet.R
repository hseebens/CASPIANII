################################################################################
#
# Skript ist Teil des Workflows "SDM" zur Habitatmodellierung von Neobiota in 
# Deutschland. Es wird mit dem Skript run_SDM_workflow.R aufgerufen.
#
# Das Skript erstellt Vorhersage der SDMs für jede Art flächendeckend für 
# Deutschland.
#
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 08.12.25
################################################################################

# !!NOTE: the type of parallel processing that is used here might only work on Windows. If it is supposed to work for other systems, I probably need to write separate functions and build in some type of automated system detection in the run-script!!

VorhersageGesamtgebiet <- function(TaxonName=NULL,
                          Modelllaeufe=NULL,
                          Ausschnitt=NULL,
                          speichern=TRUE,
                          identifier=NULL) { ## start of main function
  
  cat(paste0("\n*** Extrapolation der Vorhersagen für ",TaxonName," ***\n") ) # notification for the user
  cat("\nDie Berechnung von Vorhersagen kann einige Minuten dauern.\n")

  ## load predictor variables 
  # allenvir <- stack(file.path("SDM","Daten","Input",paste0("UmweltdatenRaster_",TaxonName,identifier,".grd")))
  allenvir <- rast(file.path("SDM","Daten","Input",paste0("UmweltdatenRaster",identifier,".tif")))

  ## Extent of selection
  ext_stack <- terra::ext(c(Ausschnitt[1],Ausschnitt[3],Ausschnitt[2],Ausschnitt[4]))
  envstack  <- terra::crop(allenvir, ext_stack) # crop the climate data to the extent of the land cover data (needed because the climate data has a global extent and the land cover data has an European extent)
  
  ## prepare data frame to be used in the predictions
  envstack <- as.data.frame(envstack, xy = TRUE)
  # terra::xyFromCell(envstack)
  # terra::extract(envstack,xy=TRUE)
  # envstack <- rasterToPoints(envstack) 
  # envstack <- as.data.frame(envstack)
  envstack <- envstack[complete.cases(envstack),]
  
  coords <- envstack[,c("x","y")] # prepare data frame to store predictions
  
  ## function to be used in parallel processing below
  ## genertes predictions for each fitted model (each pseudo absences data set * each random split)

  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)
  
  all_preds <- foreach(i=1:length(Modelllaeufe), .packages=c("mgcv"), .errorhandling = "remove") %dopar% {
  # for (i in 1:length(Modelllaeufe)){  
    
    single_run <- Modelllaeufe[[i]]
      
    if (!is.na(single_run$TSS)){
      if (single_run$TSS>0.2) { # remove models with a poor TSS
        
        # predict suitabilities (this part of the function is slowest)
        testpred <- as.data.frame(predict(single_run$mod,newdata=envstack,type="response",se.fit=FALSE)) 
        colnames(testpred) <- paste0("PredSuit_", single_run$PAblock,"_",single_run$RndSub)
        
        # add predictions to environmental data frame
        out <- testpred
        
      } else {
        out <- NA # end of if clause
      }
    } else { # end of if clause
      out <- NA
    }
    return(out)
  } ## end of sub function
  
  ## stop cluster  
  stopCluster(cl)
  
  names(all_preds) <- paste0("Modellauf_",unlist(lapply(Modelllaeufe,"[[",1)))
  
  ## inform about the number of model runs used for model projection
  # perc_usedruns <- length(unlist(lapply(all_preds, nrow))) / length(Modelllaeufe) * 100
  cat(paste0("\n Von ",length(Modelllaeufe), " Modelläufen haben ", length(unlist(lapply(all_preds, nrow))), " eine ausreichende Qualität (TSS>0.2) für Vorhersagen \n") ) # notification for the user
  
  if (length(unlist(lapply(all_preds, nrow)))==0){
    
    cat(paste0("\n Aufgrund mangelnder Qualität der Modelle keine Habitatmodellierung für ", TaxonName, " ausgführt. \n") ) # notification for the user
    
    ## update status for species in log file #################################################
    status_species <- read.xlsx(file.path("SDM","Daten","Output",paste0("StatusModellierung",identifier,".xlsx",sep="")),sheet=1)
    ind_species <- which(status_species$Taxon==TaxonName)
    
    status_species$Status[ind_species] <- "Kein Modell mit ausreichender Qualität ermittelt. Keine Habitatmodellierung ausgefuehrt."
    
    ## export status of species list
    write.xlsx(status_species, file=file.path("SDM","Daten","Output",paste0("StatusModellierung",identifier,".xlsx",sep="")))
    
    ## return NULL
    return(NULL)
    
  } else {

    ## calculate mean predicted values
    all_preds_df <- as.data.frame(do.call("cbind",all_preds),stringsAsFactors=F)
    mean_preds   <- rowMeans(all_preds_df)
    sd_preds     <- apply(all_preds_df,1,sd)
    
    final_preds <- cbind.data.frame(envstack,HabitatEignung_mittel=mean_preds,HabitatEignung_std=sd_preds)
    
    ## output 
    all_preds <- c(list(final_preds),all_preds)
    all_preds_singlefile <- do.call("cbind",all_preds)
    
    ## save list with suitability predictions for the different model runs
    if (speichern){
      if (is.null(TaxonName) | is.null(identifier)){
        warning("Angaben 'TaxonName' und/oder 'identifier' fehlen. Bitte ergänzen.")
      }
      fwrite(all_preds_singlefile, file=file.path("SDM","Daten","Output", paste0("HabitatEignung_",TaxonName,identifier,".gz")))
    }
    
    ## return final product
    return(all_preds_singlefile)
    
  }
  
} ## end of main function
