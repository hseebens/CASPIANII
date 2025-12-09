################################################################################
#
# Skript ist Teil des Workflows "SDM" zur Habitatmodellierung von Neobiota in 
# Deutschland. Es wird mit dem Skript run_SDM_workflow.R aufgerufen.
#
# Diese Funktion kalibriert die Modellparameter durch den Vergleich der 
# Modellergebnisse des SDMs mit Vorkommensdaten (auch "anfitten" genannt). Die 
# Modellergebnisse wird durch eine "cross-validation" getestet, um das beste 
# Modell zu ermitteln.
#
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 08.12.25
################################################################################


fitSDMs <- function(TaxonName=NULL,
                    VorkommenUmweltPA=NULL,
                    n_Modelllaeufe=5,
                    identifier=NULL,
                    timeout = 1200) { ## start of main function
  
  # load(file=file.path("SDM","Daten","Input", paste0("PAlist_",TaxonName,identifier,".RData")))
  # VorkommenUmweltPA <- PAlist
  
  cat(paste0("\n*** Fit Modell für ",TaxonName," ***\n") ) # notification for the user
  cat("\nDas Fitten des Modells an Daten kann einige Minuten dauern.\n")
  
  ## load status file for reporting 
  status_species <- read.xlsx(file.path("SDM","Daten","Output",paste0("StatusModellierung",identifier,".xlsx",sep="")),sheet=1)
  ind_species <- which(status_species$Taxon==TaxonName)
  
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
  formula_model <- as.formula(paste("Praesenz ~ ",paste(sapply(preds,function(x,...) paste0("s(",x,", k=7",")")),collapse="+",sep=" ")))

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
  cl <- makeCluster(cores[1]-1) # -1 to avoid overloading your computer
  registerDoParallel(cl)
  
  modelruns <- foreach(i=1:length(data_all_runs), .packages=c("mgcv","PresenceAbsence","R.utils"), .errorhandling = "remove") %dopar% {
  # modelruns <- list()
  # for (i in 1:length(data_all_runs)){
    
    data_single_run <- data_all_runs[[i]]

    formula_model <- data_single_run$formula_model
    family_model <- data_single_run$family_model
    fit_data <- data_single_run$fit_data
    test_data <- data_single_run$test_data

    ## fit model to training block
    possibleError <- tryCatch(withTimeout(model1 <- gam(formula_model, family=family_model, data=fit_data, method = "REML", select=TRUE),
                                          timeout = timeout),  # set maximum time to 20 minutes each
                              error=function(e) e)
    
    if(!inherits(possibleError, "error")){ # check if gam fit worked
      
      ## apply model on test block for cross-validation
      project_suit <- as.data.frame(predict(model1,newdata=test_data,type="response",se.fit=FALSE)) 
      
      ## calculate AUC for model evaluation
      colnames(project_suit) <- paste("pred",data_single_run$PAblock,data_single_run$RndSub,sep=".")
      eval.data.auc <- cbind(test_data[,c("ID","Praesenz")],project_suit)
      
      ## Performance measures
      AUC <- round(auc(eval.data.auc,st.dev=FALSE,na.rm=T),4) # computes AUC; requires 1. ID, 2. presence/absence, 3. predicted suitability
      PCC <- round(pcc(cmx(eval.data.auc),st.dev=FALSE),4)
      sens <- sensitivity(cmx(eval.data.auc), st.dev = FALSE)
      spec <- specificity(cmx(eval.data.auc), st.dev = FALSE)
      TSS <- sens + spec - 1
      
      ## output
      return(list(run=i,PAblock=data_single_run$PAblock,RndSub=data_single_run$RndSub,AUC=AUC,mod=model1, PCC=PCC, TSS=TSS))
      
    } else {
      
      return()
    }
  } # end of parallelised loop

  ## stop cluster  
  stopCluster(cl)
  
  ## no results
  if (is.null(unlist(modelruns))){
    
    cat("\n Warnung: Fitten der Modelle abgebrochen, da die maximale Zeit überschritten wurde. Keine Modellergebenisse. \n")
    
    ## write status to log file
    status_species$Status[ind_species] <- "Keine Habitatmodellierung, da keine Modelle gefittet werden konnten."
    
    ## export status of species list
    write.xlsx(status_species,file=file.path("SDM","Daten","Output",paste0("StatusModellierung",identifier,".xlsx",sep="")))
    
    return()
  }
  
  ## save output to disk
  save(modelruns,file=file.path("SDM","Daten","Output", paste0("ModelFit_",TaxonName,identifier,".RData")))
  # load(file=file.path("Daten","Output", paste0("ModelFit_",TaxonName,identifier,".RData")))
  
  ## evaluate model fit (export mean AUC and R2)
  out_eval <- list()
  x <- 0
  for (i in 1:length(modelruns)){
    dat <- modelruns[[i]]
    if (!is.null(dat)){
      x <- x + 1
      out_eval[[x]] <- c(dat$PAblock,dat$RndSub,dat$AUC,summary(dat$mod)$r.sq, dat$PCC, dat$TSS)
    }
  }
  out_eval <- as.data.frame(do.call("rbind",out_eval),stringsAsFactors=F)
  colnames(out_eval) <- c("PAblock", "RndSub", "AUC", "R2", "PCC", "TSS")
  
  cat(paste(" Anzahl an erfolgreichen Modellläufen:",x,"\n"))
  cat(paste(" Anzahl an Vorkommensdaten:",sum(VorkommenUmweltPA[[1]]$Praesenz==1),"\n"))
  cat(paste(" Mittelwert AUC:",round(mean(out_eval$AUC,na.rm=T),2),"\n"))
  cat(paste(" Mittelwert R2:",round(mean(out_eval$R2,na.rm=T),2),"\n"))
  cat(paste(" Mittelwert PCC:",round(mean(out_eval$PCC,na.rm=T),2),"\n"))
  cat(paste(" Mittelwert TSS:",round(mean(out_eval$TSS,na.rm=T),2),"\n"))
  
  # output
  return(modelruns)
  
} ## end of main function