###########################################################################################################
#
# Function to fit five generalized additive models (GAM) over five different random 70-30 data splits
# and computes the area under the curve (AUC) for each model
# Parts of this function are adapted from https://github.com/christianhof/BioScen1.5_SDM/blob/master/R
# This function is part of the SDM workflow.
# This version is used, when land cover data is provided in the run script.
#
# Author: Hanno Seebens (with support by Larissa Nowak), Senckenberg Gesellschaft für Naturforschung
##########################################################################################################


fit_SDMs <- function(TaxonName=NULL,
                     VorkommenUmweltPA,
                     n_Modelllaeufe=5,
                     identifier=NULL) { ## start of main function
  
  ## check identifier separator
  if (strtrim(identifier,1)!="_"){
    identifier <- paste0("_",identifier)
  }
  
  load(file=file.path("SDM","Data","Input", paste0("PAlist_",TaxonName,identifier,".RData")))
  
  VorkommenUmweltPA <- PAlist
    
  cat(paste0("\n*** Fit Modell für ",TaxonName," ***\n") ) # notification for the user
  cat("\nDas Fitten des Modells an Daten kann einige Zeit (Minuten bis Stunden) in Anspruch nehmen.\n")
  
  ## fit GAM model for different sets of pseudo absences and different random selections of 30/70 splits
  ## For example, 5 sets of pseudo absences and 5 random selections results in 25 model runs.
  
  ## set GAM model
  
  ## prepare data to be modelled
  data_occ <- VorkommenUmweltPA[[1]]
  data_occ <- data_occ[complete.cases(data_occ),]
  data_occ <- cbind.data.frame(ID=1:nrow(data_occ),data_occ,stringsAsFactors=F)
  
  ## select the names of the predictor variables; 1 is the index, 2 and 3 are long, lat and 4 is the presence
  preds <- colnames(data_occ[(which(colnames(data_occ)=="Praesenz")+1):dim(data_occ)[2]]) 
  
  ## create formula for GAMs; k defines the number of basic functions and thus the degree of allowed smoothing in s()
  formula_model <- as.formula(paste("Praesenz ~ ",paste(sapply(preds,function(x,...) paste0("s(",x,", k=10",")")),collapse="+",sep=" ")))
  
  ## distribution family of the residuals; binomial because the dependent variable is presence absence data (0/1)
  family_model <- "binomial" 
  
  ## generate random samples of data for training and testing
  
  data_all_runs <- list()
  x <- 0
  for (i in 1:length(VorkommenUmweltPA)){
    
    ## prepare data to be modelled
    data_occ <- VorkommenUmweltPA[[i]]
    data_occ <- data_occ[complete.cases(data_occ),]
    data_occ <- cbind.data.frame(ID=1:nrow(data_occ),data_occ,stringsAsFactors=F)
    
    for (j in 1:n_Modelllaeufe){
      
      x <- x + 1
      
      # split data 30/70 for training and testing
      smp_size <- floor(0.3 * nrow(data_occ)) # 70-30 split is done here
      train_ind <- sample(seq_len(nrow(data_occ)), size = smp_size)
      fit.blocks <- data_occ[-train_ind, ]
      test.block <- data_occ[train_ind, ]
      
      out_single <- list(PAblock=i,RndSub=j,fit_data=fit.blocks,test_data=test.block,formula_model=formula_model,family_model=family_model)
      data_all_runs[[x]] <- out_single
    }
  }
  
  #### FITTING ######################################################################################
  ## parallelised loop to fit GAM model for each case
  
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)
  
  modelruns <- foreach(i=1:length(data_all_runs), .packages=c("mgcv","PresenceAbsence"), .errorhandling = "remove") %dopar% {
  # modelruns <- list()
  # for (i in 1:length(data_all_runs)){
    
    data_single_run <- data_all_runs[[i]]

    formula_model <- data_single_run$formula_model
    family_model <- data_single_run$family_model
    fit_data <- data_single_run$fit_data
    test_data <- data_single_run$test_data

    ## fit model to training block
    possibleError <- tryCatch(model1 <- gam(formula_model, family=family_model, data=fit_data, method = "REML"), 
                              error=function(e) e)
    
    if(!inherits(possibleError, "error")){ # check if gam fit worked
      
      ## apply model on test block for cross-validation
      project_suit <- as.data.frame(predict(model1,newdata=test_data,type="response",se.fit=FALSE)) 
      
      ## calculate AUC for model evaluation
      colnames(project_suit) <- paste("pred",data_single_run$PAblock,data_single_run$RndSub,sep=".")
      eval.data.auc <- cbind(test_data[,c("ID","Praesenz")],project_suit)
      
      AUC <- round(auc(eval.data.auc,st.dev=FALSE,na.rm=T),4) # computes AUC; requires 1. ID, 2. presence/absence, 3. predicted suitability
      
      ## output
      return(list(run=i,PAblock=data_single_run$PAblock,RndSub=data_single_run$RndSub,AUC=AUC,mod=model1))
      
    } else {
      
      return()
    }
  } # end of parallelised loop

  ## stop cluster  
  stopCluster(cl)

  ## save output to disk
  save(modelruns,file=file.path("SDM","Data","Output", paste0("ModelFit_",TaxonName,identifier,".RData")))
  # load(file=file.path("Data","Output", paste0("ModelFit_",TaxonName,identifier,".RData")))
  
  ## evaluate model fit (export mean AUC and R2)
  out_eval <- list()
  x <- 0
  for (i in 1:length(modelruns)){
    dat <- modelruns[[i]]
    if (!is.null(dat)){
      x <- x + 1
      out_eval[[x]] <- c(dat$PAblock,dat$RndSub,dat$AUC,summary(dat$mod)$r.sq)
    }
  }
  out_eval <- as.data.frame(do.call("rbind",out_eval),stringsAsFactors=F)
  
  cat(paste(" Anzahl an Modellläufen:",x,"\n"))
  cat(paste(" Mittelwert AUC:",round(mean(out_eval$V3,na.rm=T),2),"\n"))
  cat(paste(" Mittelwert R2:",round(mean(out_eval$V4,na.rm=T),2),"\n"))
  
  # output
  return(modelruns)
  
} ## end of main function