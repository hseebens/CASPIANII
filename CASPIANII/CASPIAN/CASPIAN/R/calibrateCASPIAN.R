# utility fuction to calibrate CASPIAN. Reads data, formats them, and optimizes the model based on the speficic arguments given
# Uses constrOptim() with Nelder-Mead algorithm as optimizer.
# Calls evaluateCASPIAN() to calculate the goodness-of-fit

calibrateCASPIAN<- function( path2data, configFile, speciesData, thresholdData=0.9,
                             networkType, #either "aquatic" or "terrestrial". No default
                             yearToCalibrate, ParametersToCalibrate, 
                             ParameterRange= data.frame(name=c("S_att0", "R_att0","att1","att2","att3","S_air0","R_air0", "air1","air2","nat1","nat2","estT",
                                                               "nat_riverside1","nat_riverside2",
                                                               "nat_a","nat_b","ball1","beta","c3","estW"),
                                                        min= c(0,0,0,-0.176874,0, 0,0,-1.22,0.38, 0,0.0001,0.005,
                                                               1,0.01,
                                                               6,0.4, 10^3, 3.15 * 10^-4, 2.3,0.7),
                                                        max= c(10^-5,10^-4,1,-0.000110, 1,0.1,0.1,2.35, 0.8, 5, 1, 0.99,
                                                               150,50,
                                                               13,3.5, 10^5,3.15,3,0.99)
                                                        ),
                             trace=6,maxit=500,lmm=10,ndeps=rep(10,length(ParametersToCalibrate)),fnscale=c(-1)  # constrOptim() options
#                              ...
) {# add more options
  
  #delete existing CASPIAN results 
  unlink(dir(mainDir, pattern = "CASPIAN_", full.names = TRUE), recursive = TRUE)
  
  #get current subfolders, needed to retrieve the initialization file once CASPIAN is initialized
  Old_dirs<-list.dirs()
  
  # The following section reads the data from the specified speciesData file 
  # and formats them in a way easier to match with CASPIAN output.
  
  # reading data and first formatting
  Impatient_data<-read.table(speciesData,sep=",",header=T)
  Impatient_data<-Impatient_data[,c(1,4,5,7,8)]
  Impatient_data$Zeitpunkt<-str_sub(Impatient_data$Period,-4)
  Impatient_data$Zeitpunkt<-as.numeric(Impatient_data$Zeitpunkt)
  Impatient_data<-as.data.table(Impatient_data)
  
  # Set Threshold probability for sMon dataset
  Impatient_data<-Impatient_data[OP>=thresholdData,]
  
  #get initial simulation year
  init_year<-min(Impatient_data$Zeitpunkt)
  
  #get coordinates for invaded locations and match them to time periods and probabilities
  data_Locations<-Impatient_data[Zeitpunkt>=init_year,
                                 c("Zeitpunkt","Longitude","Latitude","OP")]
  
  
  #calculate numbers of iterations for CASPIAN on a monthly time step
  IterToEvaluate<-(data_Locations$Zeitpunkt-init_year)*12
  
  # associate the correct number of iterations to invaded locations
  data_Locations[,iter:=IterToEvaluate]
  
  #get model iteration to evaluate for calibration
  stepToCalibrate<-which(unique(data_Locations$Zeitpunkt) %in% yearToCalibrate)
  
  ################## gathering data part to be changed ends here
  
  
  
  #rename columns and set starting iteration to 1
  data_coords<- data_Locations[,c("Longitude","Latitude","iter","OP")]
  data_coords$iter[data_coords$iter==0]<-1
  assign(x="data_coords", value = data_coords, envir = .GlobalEnv)
  
  #Next section: get the corresponding network links for the invaded locations
  
  #create empty list to store invaded links (matching the list output of CASPIAN)
  CompleteInvasionData<-list()
  
  #reading in starting configuration file to provide network data and other information to initialize CASPIAN
  source(configFile)
  
  #build land cover species preference matrix
  species_preferences<- data.table(LC_cat_ID= 1:5,Species_preferences=c(Urban_areas,Arable_land,Pastures,Forests,Wetlands))
  
  #Calculate invaded links for each time step
  if (runAquaticModel==TRUE) max_dist<-max_dist_W
  if (runTerrestrialModel==TRUE) max_dist<-max_dist_T
  if (networkType=="aquatic") netwToUse<-Water_netw_data
  if (networkType=="terrestrial")  netwToUse<-Terrestrial_netw_data
  
  for (i in sort(unique(data_coords$iter))) {
    cat("\n", i ,"\n")
    #calculate invaded links as in the CASPIAN initialization
    invisible(capture.output(
      ID<-getNeighbourSegmCoord(netwToUse,init_coords = data_coords[iter==i,1:2],max_dist = max_dist)
    ))
    # associate data probability to invaded links as maximum of the location probabilities
    linkProb<-data.frame(Pinv_obs=max(data_coords$OP[data_coords$iter<=i]),ID)
    #add to the empty list
    CompleteInvasionData[[as.character(i)]]<-linkProb
  }
  
  # select list element(s) on which the calibration should be performed
  InvasionData<-CompleteInvasionData[stepToCalibrate]
  
  # initialize CASPIAN to avoid repeating the process for every model evaluation
  
  cat("\n Initializing CASPIAN \n")
  invisible(capture.output(
    init_data<-runCASPIAN(configFile=configFile,path2data=path2data)
  ))
  rm(init_data)
  
  # get current subfolders, identify the new one with CASPIAN results, and copy the initialization file to the main working directory to be used at a later stage
  cat("\n Copying initialization file to current working directory \n")
  New_dirs<-list.dirs()
  file.copy(from = file.path(New_dirs[which(New_dirs %in% Old_dirs == FALSE)], file_init),to=file_init,overwrite = TRUE)
  
  #save progress so far in .rData file
  cat("\n Saving Pre-Calibration file \n")
  save.image("PreCalibration.Rdata")
  
  #######################################
  # build general calibration configFile HERE. Changes plot options, number of iterations, and initialization options.
  tx  <- readLines(configFile)
  tx[grep("makeplot",tx)]<-"makeplot <- FALSE"
  tx[grep("save_plot",tx)]<-"save_plot <- FALSE"
  tx[grep("initialize",tx)]<-"initialize <- FALSE"
  tx[grep("save_init",tx)]<-"save_init <- FALSE"
  if (runTerrestrialModel==TRUE){
  tx[grep("num_iter_T",tx)]<-paste0("num_iter_T <- ",as.numeric(as.character(unique(data_coords$iter)))[stepToCalibrate])
  tx[grep("iter_save_T",tx)]<-paste0("iter_save_T <- ",as.numeric(as.character(unique(data_coords$iter)))[stepToCalibrate])
  }
  if (runAquaticModel==TRUE){
    tx[grep("num_iter_W",tx)]<-paste0("num_iter_W <- ",as.numeric(as.character(unique(data_coords$iter)))[stepToCalibrate])
    tx[grep("iter_save_W",tx)]<-paste0("iter_save_W <- ",as.numeric(as.character(unique(data_coords$iter)))[stepToCalibrate])
  }
  
  # assign name to optimized config file and write to file
  calib_ConfigFile<-"Calibration_configFile.R"
  writeLines(tx, con=calib_ConfigFile)
  
  #######################################
  
  cat("\n Beginning calibration \n")
  
  # build parameter matrix
  parameters<-ParMatrix(par_att0_Roads,par_att0_Railways,par_att1,par_att2,par_att3,
                        par_air0_Roads,par_air0_Railways,par_air1,par_air2,
                        par_nat1,par_nat2,par_nat_riverside1,par_nat_riverside2,par_est_T,par_cont,par_pall,
                        par_nat_a,par_nat_b,par_ball,
                        par_hull0,par_a,par_c1,par_g,par_c2,par_b,par_c3,par_est_W)
  
  # get default values
  defaultValues <- parameters
  
  #select which parameters to calibrate
  parNames <- ParametersToCalibrate
  
  # identify parameters to calibrate indexes and their default parameter names and values
  parSel <- c()
  for (i in parNames) parSel <- c(parSel, grep(i, colnames(defaultValues)))
  parValues <- c(defaultValues[parSel])
  
  # get number of parameters to calibrate
  npar <- length(parValues)
  
  # define calibrated parameters range
  parRange<-ParameterRange[ParameterRange$name %in% ParametersToCalibrate,]
  
  #### setup constrOptim()
  
  #Convert the parameter ranges to  ui and ci matrices
  n <- nrow(parRange)
  opt_ui <- rbind( diag(n), -diag(n) )
  opt_ci <- c( parRange[,2], - parRange[,3] )
  
  #export objects to global environment
  assign(x="calib_ConfigFile", value=calib_ConfigFile, envir = .GlobalEnv)
  assign(x="InvasionData", value=InvasionData, envir = .GlobalEnv)
  assign(x="defaultValues", value=defaultValues, envir = .GlobalEnv)
  assign(x="parSel", value=parSel, envir = .GlobalEnv)
  assign(x="opt_ui", value=opt_ui, envir = .GlobalEnv)
  assign(x="opt_ci", value=opt_ci, envir = .GlobalEnv)
  assign(x="file_init", value=file_init, envir = .GlobalEnv)
  assign(x="parNames", value=parNames, envir = .GlobalEnv)
  
  #run optimizer
  sink("constroptim_log.txt")
  opt_Nelder2<-constrOptim(theta=parValues, f=evaluateCASPIAN, grad=NULL, ui=opt_ui,ci=opt_ci
                           ,method="Nelder-Mead"
                           # ,method = "L-BFGS-B"
                           # ,lower = parRange[,2], upper = parRange[,3]
                           ,control=list(trace=trace,maxit=maxit,lmm=lmm,ndeps=ndeps,fnscale=fnscale))
  
  sink()
  
  sink("Optimizer_Results.txt")
  opt_Nelder2
  sink()
  
  ### calculate model output with best parameter values
  
  #read configFile and replace default values with optimized ones
  for (i in 1:length(opt_Nelder2$par)){
    tx[grep(parNames[i],tx)]<-paste0("par_",parNames[i]," <- ",opt_Nelder2$par[i])
    cat("\n", tx[grep(parNames[i],tx)],"\n")
  }
  #alter additional options: plots, initialization, number of steps in the simulation
  tx  <- readLines(configFile)
  tx[grep("makeplot",tx)]<-"makeplot <- TRUE"
  tx[grep("save_plot",tx)]<-"save_plot <- TRUE"
  tx[grep("initialize",tx)]<-"initialize <- FALSE"
  tx[grep("save_init",tx)]<-"save_init <- FALSE"
  if (runTerrestrialModel==TRUE){
    tx[grep("num_iter_T",tx)]<-paste0("num_iter_T <- ",as.numeric(as.character(unique(data_coords$iter))))
    tx[grep("max_iter_T",tx)]<-paste0("max_iter_T <- ",max(as.numeric(as.character(unique(data_coords$iter)))))
  }
  if (runAquaticModel==TRUE){
    tx[grep("num_iter_W",tx)]<-paste0("num_iter_W <- ",as.numeric(as.character(unique(data_coords$iter))))
    tx[grep("max_iter_W",tx)]<-paste0("max_iter_W <- ",max(as.numeric(as.character(unique(data_coords$iter)))))
  }
  # assign name to optimized config file and write to file
  opt_ConfigFile<-"Optimized_configFile.R"
  writeLines(tx, con=opt_ConfigFile)
  
  #run CASPIAN with optimized parameter values
  optOutputCASPIAN<-runCASPIAN(configFile=opt_ConfigFile,path2data=path2data)
  
  #save .Rdata file with the entire session
  save.image("PostCalibration.rData")
  
  cat("\n Done \n")
  
}