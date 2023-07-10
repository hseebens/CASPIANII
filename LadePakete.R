
LadePakete <- function(){
  
  # list of required packages
  packages <-
    c(
      "raster",
      "rgbif",
      "rlang",
      "CoordinateCleaner",
      "maps",
      "dismo",
      "ggplot2",
      "data.table",
      "rgdal",
      "viridis",
      "mgcv",
      "PresenceAbsence",
      "doParallel",
      "openxlsx",
      "robis",
      "spocc",
      "leaflet",
      "mapsf",
      "geosphere",
      "sf",
      "SearchTrees",
      "worrms"
    )
  
  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])] # check which of them is not yet installed
  if(length(new.packages)) install.packages(new.packages); rm(new.packages) # install them
  
  l <- sapply(packages, function(s) suppressMessages(suppressWarnings(require(s, quietly=T, character.only = TRUE)))); rm(packages, l) # load all required packages
  
}
