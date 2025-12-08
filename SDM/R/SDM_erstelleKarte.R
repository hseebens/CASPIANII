################################################################################
#
# Skript ist Teil des Workflows "SDM" zur Habitatmodellierung von Neobiota in 
# Deutschland. Es wird mit dem Skript run_SDM_workflow.R aufgerufen.
#
# Das Skript erstellt eine Karte der Vorhersage des SDMs für eine Art.
#
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 08.12.25
################################################################################

erstelleKarteHabitatEignung <- function(HabitatEignung=NULL, 
                                        Vorkommen=NULL,
                                        identifier=NULL) { ## start of main function

  if (is.null(HabitatEignung)){
    
    cat(paste0("\n*** Keine Daten zur Habitatmodellierung fuer Kartendarstellung gefunden. *** \n") ) # notification for the user
    
    return()
    
  } else {
    
    GermanShapefile <- NULL
    if (file.exists(file.path("SDM","Daten","Input","Shapefiles","gadm41_DEU_1.shp"))){
      GermanShapefile <- st_read(dsn=file.path("SDM","Daten","Input","Shapefiles"),layer="gadm41_DEU_1",quiet=TRUE) # optional: loads a shapefile of the shape of Germany to be used for cropping the suitability plot to the extent and shape of Germany; set to NULL if this is not desired!
    }
    
    ## transform suitability predictions into raster
    meanSuit_coords <- HabitatEignung[,c("x", "y", "HabitatEignung_mittel")] # prepare raster file with the mean predictions for plotting
    rastpreds <- rast(meanSuit_coords, type="xyz", digits=6)
    
    Vorkommen <- st_as_sf(Vorkommen, coords=c("Laengengrad","Breitengrad"),crs = 4326)
    Vorkommen <- st_set_crs(Vorkommen, st_crs("+proj=longlat +datum=WGS84"))
    
    ## make plot #################################
    
    # pdf(file.path("Grafiken", paste0("KarteHabitatEignungVorkommen_",TaxonName,identifier,".pdf"))) # plot with occurrences
    png(file.path("SDM","Grafiken", paste0("KarteHabitatEignungVorkommen_",TaxonName,identifier,".png")),units="in",res=300,width=8,height=8) # plot with occurrences
    plot(rastpreds, col=rev(terrain.colors(100))) #rev(hcl.colors(10, pal="Mint")))
    if (!is.null(GermanShapefile)) plot(st_geometry(GermanShapefile), add=T, border=gray(0.4))
    points(Vorkommen, pch=1, cex=0.5)
    dev.off()
    
    ## update status for species in log file #################################################
    status_species <- read.xlsx(file.path("SDM","Daten","Output",paste0("StatusModellierung",identifier,".xlsx",sep="")),sheet=1)
    ind_species <- which(status_species$Taxon==TaxonName)
    
    status_species$Status[ind_species] <- "Habitatmodellierung ausgefuehrt."
    
    ## export status of species list
    write.xlsx(status_species,file=file.path("SDM","Daten","Output",paste0("StatusModellierung",identifier,".xlsx",sep="")))
    
    cat(paste0("\n*** Karte der Habitateignung für ",TaxonName," erstellt. *** \n") ) # notification for the user
    
    return(rastpreds)
    
  }
} ## end of main function

