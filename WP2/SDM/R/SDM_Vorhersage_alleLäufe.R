###########################################################################################################
#
# Function to predict environmental suitability for the different model runs with parallelization
# This function is part of the SDM workflow.
#
# Author: Larissa Nowak
##########################################################################################################

# !!NOTE: the type of parallel processing that is used here might only work on Windows. If it is supposed to work for other systems, I probably need to write separate functions and build in some type of automated system detection in the run-script!!

Vorhersage_alleLäufe <- function(TaxonName,
                        Modellläufe,
                        Ausschnitt=NULL,
                        speichern=T,
                        identifier=NULL) { ## start of main function
  
  # print("Note: Predicting environmental suitabilities might take several minutes. If predicted over the entire world it can take up to 30 minutes or more depending on your machine.")
  cat(paste0("\n*** Extrapolation der Vorhersagen für ",TaxonName," ***\n") ) # notification for the user
  cat("Die Berechnung von Vorhersagen kann einige Zeit in Anspruch nehmen.")
  
  ## load predictor variables 
  allenvir <- stack(file.path("Data","Input",paste0("RasterDatenUmwelt_",TaxonName,"_",identifier,".grd")))

  ## Extent of selection
  ext_stack <- extent(c(Ausschnitt[1],Ausschnitt[3],Ausschnitt[2],Ausschnitt[4]))
  envstack  <- crop(allenvir, ext_stack) # crop the climate data to the extent of the land cover data (needed because the climate data has a global extent and the land cover data has an European extent)
  
  ## prepare data frame to be used in the predictions
  envstack <- rasterToPoints(envstack) 
  envstack <- as.data.frame(envstack)
  envstack <- envstack[complete.cases(envstack),]
  
  coords <- envstack[,c("x","y")] # prepare data frame to store predictions
  
  ## function to be used in parallel processing below
  ## genertes predictions for each fitted model (each pseudo absences data set * each random split)

  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)
  
  all_preds <- foreach(i=1:length(Modellläufe), .packages=c("mgcv")) %dopar% {
  # for (i in 1:length(Modellläufe)){  
    
    single_run <- Modellläufe[[i]]
      
    if (!is.na(single_run$AUC)){
      if (single_run$AUC>0.7) { # remove models with a poor AUC
        
        # predict suitabilities (this part of the function is slowest)
        testpred <- as.data.frame(predict(single_run$mod,newdata=envstack,type="response",se.fit=FALSE)) 
        colnames(testpred) <- paste0("PredSuit_", single_run$PAblock,"_",single_run$RndSub)
        
        # add predictions to environmental data frame
        out <- testpred
        
      } else {
        out <- NA ## end of if clause
      }
    } else {
      out <- NA
    }
    return(out)
  } ## end of sub function
  
  ## stop cluster  
  stopCluster(cl)
  
  names(all_preds) <- paste0("Modellauf_",1:length(Modellläufe))
  
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
      warning("Angaben 'TaxonName' und/oder 'identifier' fehlt. Bitte ergänzen.")
    }
    save(all_preds_singlefile, file=file.path("Data","Output", paste0("Suitability_",TaxonName,"_",identifier,".RData"))) 
  }

  ## return final product
  return(all_preds_singlefile)
  
} ## end of main function
