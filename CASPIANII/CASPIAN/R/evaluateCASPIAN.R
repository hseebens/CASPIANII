#Goodness of Fit function for CASPIAN. Not designed to be a standalone, as requires only the parameter values. 
# Other arguments need to be provided externally, such as through calibrateCASPIAN()
# Based on maximum likelihood Estimate, not on presence/absence evaluation, and considers only invaded links, not links that are not invaded. 

evaluateCASPIAN <- function(pars) {
  
  cat("\n",format(Sys.time(),usetz = TRUE),"\n") 
  
  #format parameters
  if (is.vector(pars))  pars = matrix(pars, nrow = 1)
  p <- matrix(NA, nrow = nrow(pars), ncol = length(defaultValues))
  
  #mix parameters to be evaluated with the defaults
  for (i in 1:nrow(pars)){ 
    p[i, ] <- suppressWarnings(createMixWithDefaults(pars[i,1:ncol(pars)], defaultValues, parSel))
  }
  colnames(p) <- colnames(defaultValues)
  
  #load initialization file
  load(file.path(new_dir,file_init))
  
  # Start parameter evaluation and print parameters to be evaluated
  cat("\n New Simulation. Parameters: \n") 
  
  #read configFile
  tx <- readLines(file.path(new_dir,calib_ConfigFile))
  
  #replace parameter values in the configFile with the ones to be evaluated
  for (i in 1:length(pars)){
    tx[grep(parNames[i],tx)]<-paste0("par_",parNames[i]," <- ",pars[i])
    cat("\n", tx[grep(parNames[i],tx)],"\n")
  }
  
  # write the updated configFile
  writeLines(tx, con=file.path(new_dir,calib_ConfigFile))
  
  # run CASPIAN
  old_eval_dirs<-list.dirs() # store existing folders (new folder produced by runCASPIAN will be deleted below)
  invisible(capture.output(
    outputCASPIAN <- runCASPIAN(configFile=file.path(new_dir,calib_ConfigFile),path2data=path2data)
  ))
  outputCASPIAN <- outputCASPIAN[[1]]
  assign(x = "outputCASPIAN",value = outputCASPIAN, envir = .GlobalEnv)
  
  # match CASPIAN results with data
  nruns <- 1
  MLEs <- c()
  
  for (nparticles in 1:nruns) {
    MLE <- c()
    
    for (nsteps in length(outputCASPIAN)){
      
      predicted <- outputCASPIAN[[nsteps]][,c("ID","Pinv")]
      observed <- InvasionData[[nsteps]]
      llobj <- merge(observed, predicted, all = FALSE,
                     by = "ID")
      colnames(llobj)<-c("ID","Observed","CASPIAN")
      
      #remove links invaded as starting points
      if (networkType=="aquatic"){
        InitLinks<-which(llobj$ID%in%init_obj$init_segm_water)
      }
      if (networkType=="terrestrial"){
        InitLinks<-which(llobj$ID%in%init_obj$init_segm)
      }
      # if (length(InitLinks)!=0) { llobj<-llobj[-InitLinks,]}
      
      #Calculate goodness of fit based on PCC and presence/absence
      # # options(warn = -1)
      # singleAUC<-presence.absence.accuracy(llobj, threshold = 0.01, find.auc = TRUE, st.dev = FALSE, which.model=1,na.rm = T)$PCC
      # AUC<- sum(AUC,singleAUC,na.rm = T)
      # AUC<- -(AUC-1)
      
      #Calculate goodness of fit based on MLE
      res <- llobj$CASPIAN - llobj$Observed
      err <- llobj$Observed*0.2 # consider 0.2 standard deviation of the likelihood. Could impact on the final results.
      singleMLE <- dnorm(res, sd = err, log = T)
      
      MLE <- sum(MLE,singleMLE,na.rm = TRUE)
      assign(x = "llobj", value=llobj,envir=.GlobalEnv)
      
    }
    
    MLEs[nparticles] <- MLE
  }
  #remove new folder with CASPIAN results
  new_eval_dirs <- list.dirs()
  new_eval_dir <- new_eval_dirs[which(new_eval_dirs %in% old_eval_dirs == FALSE)]
  ind_dir <- which(list.dirs()%in%new_eval_dir)
  unlink(list.dirs()[ind_dir],recursive = TRUE)

  # print goodness of fit
  cat("\n goodness-of-fit = ", MLEs,"\n")
  
  #return goodness of fit value
  return(MLEs)
  
}
