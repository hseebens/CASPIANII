graphics.off()
rm(list=ls())

setwd("/home/hanno/Bioinvasion/CASPIANII/CASPIANII/CASPIAN/CASPIAN")

source(file.path("LadeSkripte_CASPIAN.R"))

source(file.path("..","..","LadePakete.R"))
LadePakete()

path2data <- file.path("..","..","..","..","EBAspread","Data","FinalDataFiles")

configFile <- file.path("inst","extdata","configFile.R")
modelResults <- runCASPIAN(configFile=configFile,path2data=path2data)

