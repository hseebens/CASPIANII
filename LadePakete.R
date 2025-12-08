
LadePakete <- function(){
  
  # list of required packages
  packages <- c("terra", "R.utils", "rgbif", "CoordinateCleaner", "maps", "ggplot2","data.table", #, "dismo"
                "viridis","mgcv", "PresenceAbsence", "doParallel","openxlsx","robis","spocc",
                "leaflet","mapsf","geosphere","sf","SearchTrees","worrms","stringr", "units", "GeoThinneR") 
  
  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])] # check which of them is not yet installed
  if(length(new.packages)) install.packages(new.packages); rm(new.packages) # install them
  
  l <- sapply(packages, function(s) suppressMessages(suppressWarnings(require(s, quietly=T, character.only = TRUE)))); rm(packages, l) # load all required packages
  
}
