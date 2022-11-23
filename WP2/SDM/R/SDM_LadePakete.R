
LadePakete <- function(){
  
  # list of required packages
  packages <- c("raster", "rgbif", "CoordinateCleaner", "maps", "dismo", "ggplot2","data.table", 
                "rgdal", "viridis","mgcv", "PresenceAbsence", "doParallel","openxlsx","robis","spocc",
                "leaflet") 
  
  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])] # check which of them is not yet installed
  if(length(new.packages)) install.packages(new.packages); rm(new.packages) # install them
  
  l <- sapply(packages, require, character.only = TRUE); rm(packages, l) # load all required packages
  
}
