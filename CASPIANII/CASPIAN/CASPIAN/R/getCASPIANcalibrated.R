# Calibration Script for CASPIAN
# Example species: I. glandulifera
rm(list=ls())
graphics.off()

#set repository as working directory and Load Scripts and packages
setwd("C:/Users/JLU-SU/Documents/GitHub/CASPIANII/CASPIANII/CASPIAN/CASPIAN")
source(file.path("LadeSkripte_CASPIAN.R")) 
source(file.path("..","..","LadePakete.R"))
LadePakete()

#set pre-uploaded configuration file, specific for calibration
configFile <- file.path(getwd(),"inst","extdata","PreCalibration_configFile.R")

#set directory with data as working directory
path2data <- file.path("C:/Users/JLU-SU/Desktop/CASPIAN/TestFunction/")

mainDir<-path2data
setwd(mainDir)

#set file with species data
speciesData <- file.path(mainDir,"Vorkommen_Impatiens_glandulifera_sMon.csv") #file with Species invasion data

#set parameters to calibrate
ParametersToCalibrate <- c("nat_riverside1","nat_riverside2") #names of the parameters to be calibrated

#call calibration function
calibrateCASPIAN(path2data = path2data, 
                 configFile = configFile, speciesData = speciesData,networkType="aquatic",
                 ParametersToCalibrate = ParametersToCalibrate,
                 yearToCalibrate=1996, maxit=100)

