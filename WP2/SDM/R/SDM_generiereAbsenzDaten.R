###########################################################################################################
#
# Function to randomly sample five sets of pseudo absences (PA), extract environmental and land cover data 
# for them and combine them with the occurrence data of the focal species
# With parallelization
# This function is part of the SDM workflow.
# This version is used, when land cover data is provided in the run script.
#
# Author: Larissa Nowak
##########################################################################################################

# !!NOTE: the type of parallel processing that is used here might only work on Windows. If it is supposed to work for other systems, I probably need to write separate functions and build in some type of automated system detection in the run-script!!

generiereAbsenzDaten <- function(TaxonName=NULL,
                                 VorkommenUmwelt=NULL, 
                                 n_AbsenzDaten=5,
                                 speichern=T,
                                 identifier=NULL) { ## start of main function

  cat(paste0("\n*** Generiere Absenzdaten für ",TaxonName,". ***\n") ) # notification for the user
  
  ## load predictor variables
  predictor_stack <- stack(file.path("Data","Input",paste0("RasterDatenUmwelt_",TaxonName,"_",identifier,".grd")))
  
  col_names_pred <- names(predictor_stack)
  
  ## extract and prepare occurrence records
  occ_data <- as.data.frame(VorkommenUmwelt,stringsAsFactors=F)
  # occ_data$Zeitpunkt <- suppressWarnings( as.numeric(occ_data$Zeitpunkt) ) ## adjust data set for easier matching
  occ_data$Zeitpunkt <- 1
  colnames(occ_data)[colnames(occ_data)=="Zeitpunkt"] <- "Praesenz"
  occ_data$Taxon <- NULL
  occ_data$Datenbank <- NULL
  
  ## prepare a template with all terretrial cells to sample from
  ## (simply take the spatial layer with most non-missing cells)
  n_cells <- c()
  for (i in 1:nlayers(predictor_stack)){
    n_cells <- c(n_cells,sum(!is.na(values(predictor_stack[[i]])))) # count number of non-missing cells
  }
  template <- predictor_stack[[which.max(n_cells)]] 

  ## number of alternative pseudo absence data sets
  PA_run <- 1:n_AbsenzDaten
  
  myfun <-  function(z){ # define function to be used in parallel processing
    
    ## generate n random pseoudo absences
    PA <- as.data.frame(dismo::randomPoints(mask=template, p = occ_data[,c("Laengengrad","Breitengrad")],  n = 10000)) 
    # the argument p gives the occurrence points from which sampling should not be done, 
    # n is the number of PAs that should be sampled. If in addition the argument prob is set true, the values of the mask are taken as probabilities (interesting, if a weighing of the potential absence cells is desired).
   
    colnames(PA) <- c("Laengengrad","Breitengrad")
    PA$Praesenz <- 0
    
    ## extract and add predictor variables to pseudo absence location
    PAenv <- cbind(PA, pred_var = raster::extract(x = predictor_stack, y = data.frame(PA[,c('Laengengrad','Breitengrad')])))
    colnames(PAenv) <- c("Laengengrad", "Breitengrad", "Praesenz", col_names_pred)

    ## output
    OccenvPA <- rbind(occ_data, PAenv) # combining PAs with occurrences

    return(OccenvPA)
  }
  
  # parallel processing: 
  no_cores <- parallel::detectCores(logical = TRUE) # get number of logical cores of the user's machine
  cl <- parallel::makeCluster(no_cores-1)  # provide clusters to R
  doParallel::registerDoParallel(cl) # register to these clusters
  parallel::clusterExport(cl,list('myfun','PA_run', 'occ_data', 'predictor_stack', 'template' , 'col_names_pred'), envir=environment()) 
  # export all objects and functions to the clusters #envir() indicates that these functions and objects can be found within the environment of the main function
  PAlist <- c(parallel::parLapply(cl,PA_run,fun=myfun)) # apply function in parallel
  stopCluster(cl)
  
  ## save results to disk
  if (speichern){
    if (is.null(TaxonName) | is.null(identifier)){
      warning("Angabe 'TaxonName' oder 'identifier' fehlt. Bitte ergänzen.")
    }
    save(PAlist, file=file.path("Data","Input", paste0("PAlist_",TaxonName,"_",identifier,".RData")))
    # load(file=file.path("Data","Input", "PAlist_Acer saccharinum_run.071122.test.RData"))
  }
  
  return(PAlist)
  
} ## end of main function
