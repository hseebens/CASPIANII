###########################################################################################################
#
# Function to plot the average predicted environmental suitabilities
# This function is part of the SDM workflow.
#
# Author: Hanno Seebens (with support by Larissa Nowak), Senckenberg Gesellschaft für Naturforschung
##########################################################################################################

erstelleKarteHabitatEignung <- function(HabitatEignung, Vorkommen) { ## start of main function
  
  # print("Note: If the same plot should be plotted and stored again, make sure the pdf-file with the respective name is closed on your computer. Otherwise, R will be unable to overwrite the file and yield an error, when running this step.") # notification for the user
  
  GermanShapefile <- NULL
  if (file.exists(file.path("SDM","Data","Input","Shapefiles","gadm41_DEU_1.shp"))){
    # GermanShapefile <- readOGR(dsn=file.path("SDM","Data","Input","Shapefiles"),layer="gadm41_DEU_1",verbose=F) # optional: loads a shapefile of the shape of Germany to be used for cropping the suitability plot to the extent and shape of Germany; set to NULL if this is not desired!
    GermanShapefile <- st_read(dsn=file.path("SDM","Data","Input","Shapefiles"),layer="gadm41_DEU_1") # optional: loads a shapefile of the shape of Germany to be used for cropping the suitability plot to the extent and shape of Germany; set to NULL if this is not desired!
  }
  
  ## transform suitability predictions into raster
  # meanSuit <- HabitatEignung[[length(HabitatEignung)]] # last entry contains mean predictions
  meanSuit_coords <- HabitatEignung[,c("x", "y", "HabitatEignung_mittel")] # prepare raster file with the mean predictions for plotting
  rastpreds <- rast(meanSuit_coords, type="xyz")
  # coordinates(meanSuit_coords) <- ~ x + y
  # gridded(meanSuit_coords) <- T
  # rastpreds <- raster(meanSuit_coords)

  Vorkommen <- st_as_sf(Vorkommen, coords=c("Laengengrad","Breitengrad"),crs = 4326)
  st_set_crs(Vorkommen, st_crs("+proj=longlat +datum=WGS84"))
  # coordinates(Vorkommen)=~Laengengrad+Breitengrad # prepare occurrence file for plotting, transform it into a shapefile
  # proj4string(Vorkommen)<- CRS("+proj=longlat +datum=WGS84")
  

  ## make plot #################################
  
  # ## instant plot
  # x11()
  # plot(rastpreds, col=viridis(100))#, xlim=xlim, ylim=ylim) # plot the predicted probabilities
  # if (!is.null(GermanShapefile)) plot(GermanShapefile,add=T,border=gray(0.7))
  # points(Vorkommen, pch=1, cex=0.5)
  
  # store plots on local computer:
  # pdf(file.path("Grafiken", paste0("HabitatEignung_",TaxonName,"_",identifier,".pdf"))) # plot without occurrences
  # png(file.path("Grafiken", paste0("KarteHabitatEignung_",TaxonName,"_",identifier,".png"))) # plot without occurrences
  # plot(rastpreds, col=viridis(100))
  # if (!is.null(GermanShapefile)) plot(GermanShapefile,add=T,border=gray(0.7))
  # dev.off()
 
  # pdf(file.path("Grafiken", paste0("KarteHabitatEignung+Vorkommen_",TaxonName,"_",identifier,".pdf"))) # plot with occurrences
  png(file.path("SDM","Grafiken", paste0("KarteHabitatEignung+Vorkommen_",TaxonName,"_",identifier,".png")),units="in",res=300,width=8,height=8) # plot with occurrences
  plot(rastpreds, col=viridis(100))
  if (!is.null(GermanShapefile)) plot(st_geometry(GermanShapefile),add=T,border=gray(0.7))
  points(Vorkommen, pch=1, cex=0.5)
  dev.off()
  
  
  ## update status for species in log file #################################################
  status_species <- read.xlsx(file.path("SDM","Data","Output",paste0("StatusModellierung_",identifier,".xlsx",sep="")),sheet=1)
  ind_species <- which(status_species$Taxon==TaxonName)
  
  status_species$Status[ind_species] <- "Habitatmodellierung ausgefuehrt."
  
  ## export status of species list
  write.xlsx(status_species,file=file.path("SDM","Data","Output",paste0("StatusModellierung_",identifier,".xlsx",sep="")))
  
  return(rastpreds)
  
} ## end of main function

