################################################################################
# 
# Dieses Skript fuehrt den workflow "Ausbreitung" aus, mit dem das Potenzial
# jeder Art zum Erreichen weiterer Standorte (dem Invasionspotenzial) ermittelt
# wird. Das Skript diese Berechnung für jede Art mit einer Datei 
# "HabitatEignung_[TaxonName]_[identifier].gz" im Verzeichnis SDM/Daten/Output/
# aus. 
#
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 08.12.25
################################################################################

graphics.off()
rm(list=ls())


## Laden der R Pakete ##########################################################
library(data.table) # effizientes Einlesen großer Datensaetze
library(sf) # geografische Polygondaten
library(terra) # geografische Rasterdaten
library(units) # Setzen geografischer Einheiten
library(gdistance) # Berechnung der Routen
library(raster) # (veraltet) notwendig zum Laufen von gdistance
library(openxlsx) # Einlesen von Exceldaten
library(GeoThinneR) # effizientes Ausduennen von Koordinaten



################################################################################
## Konfiguration des Workflows #################################################
################################################################################


################################################################################
## Arbeitsverzeichnis ##########################################################

## Gegebenenfalls muss das Arbeitsverzeichnis angepasst werden. Das Arbeits-
## verzeichnis beinhaltet alle notwendigen Verzeichnis wie /Ausbreitung, /SDM und
## /ListeNeobiota. Es kann durch Laden des RStudio Projekts "CASPIANII.Rproj" oder
## manuell in der folgenden Zeile gesetzt werden.

# setwd(file.path("C:", "path", "to", "files"))
setwd(file.path("C:","Hanno","Bioinvasion","CASPIANII","CASPIANII"))


## Verzeichnisse der Datensaetze des Workflows "Habitateignung" ################

SDM_input_folder <- file.path("SDM","Daten","Input")
SDM_output_folder <- file.path("SDM","Daten","Output")
# SDM_input_folder <- file.path("E:","Bioinvasion","CASPIAN II","Files 230925","Daten","Input")
# SDM_output_folder <- file.path("E:","Bioinvasion","CASPIAN II","Files 230925","Daten","Output")


################################################################################
## ID des Modelllaufs (wie in Dateinamen der Habitateignung angegeben)
identifier <- "_230925"



################################################################################
## Ende: Konfiguration der Habitatmodellierung #################################
################################################################################



################################################################################
###### Berechnung des Invasionpotentials ##############################################

################################################################################
## Lade Datensaetze ############################################################

# Grenzen der Landkreise
regions2 <- st_read(dsn=file.path("SDM","Daten","Input","Shapefiles"), layer="gadm41_DEU_2")

## Grosse Fliessgewaesser in Deutschland 
waterbodies <- rast(file.path("Ausbreitung", "Daten", "GrosseFliessgewaesser_Deutschland.tif"))
## Tranformation der Vorkommen von Gewaessern in 0 und 1
no_waterbodies <- waterbodies + 1
no_waterbodies[no_waterbodies==2] <- 0

## Liste der Neobiota und taxonomische Information
tax_dat <- read.xlsx(file.path( "SDM", "Daten", "Input","ListeGebietsfremderArten_gesamt_standardisiert.xlsx"))


################################################################################
## Voreinstellungen ############################################################

## Umgrenzung (bounding box) von Deutschlnad
crop_extent <- c(5,47,16,55.5) 

## Umkreis um Zielgebiet (z.B. Landkreise) fuer Vorkommen einer Art
dist_buff <- 100 # Angaben in km
units(dist_buff) <- as_units("km")


################################################################################
## Durchfuehrung ###############################################################


## Identifizierung aller vorhandenden SDM Projektionen im Verzeichnis
all_files <- list.files(SDM_output_folder)
all_files <- all_files[grep(identifier,all_files)]
all_files <- all_files[grep("HabitatEignung_",all_files)]
all_files <- all_files[grep(paste0(identifier,"\\.gz"),all_files)]

## Liste der Taxa mit SDM Projektionen
all_files <- gsub(identifier, "", all_files)
all_files <- gsub("HabitatEignung_", "" , all_files)
all_species <- gsub("\\.gz", "", all_files)


# Schleife ueber alle Taxa und Zielgebiete
output_all <- list()
for (s in 1:length(all_species)){ # Schleife ueber alle Taxa

  species <- all_species[s]

  org_tax <- tax_dat[tax_dat$Taxon==species,]
  
  if (nrow(org_tax)==1){

    print(paste0("Working on ", species, " (", s, "/", length(all_species), ")"))
    ptm <- proc.time()
    
    ## suitabilities ##############################
    SDM_proj_all <- fread(file.path(SDM_output_folder, paste0("HabitatEignung_", species, identifier, ".gz")))
    
    ## transform to spatial object using the averaged model only
    SDM_proj_mean <- SDM_proj_all[,c("x", "y", "HabitatEignung_mittel")]
    SDM_proj <- rast(SDM_proj_mean, type="xyz", crs=terra::crs(regions2))
    
    prob_establ <- extract(SDM_proj, regions2, fun=function(x) mean(x, na.rm=T), method="bilinear")
    if (nrow(prob_establ)!=nrow(regions2)) print("Warning: Number of regions mis-matches")

    ## SDM projections for shortest path analysis
    SDM_proj_spread <- SDM_proj
    
    # values(SDM_proj_spread)[is.na(values(SDM_proj_spread))] <- 0 # allow shortest paths even through unsuitable habitats
    
    ## restrict aquatic species (fish and mussels) to water bodies
    if (org_tax$Klasse %in% c("Bivalvia", "Copepoda", "Ostracoda") | 
        org_tax$Ordnung %in% c("Cypriniformes", "Acipenseriformes", "Clupeiformes", 
                               "Siluriformes", "Anguilliformes", "Perciformes", "Salmoniformes", 
                               "Gasterosteiformes", "Cyprinodontiformes", "Esociformes", 
                               "Decapoda", "Amphipoda") ){
      
      waterbodies_species <- terra::resample(waterbodies, SDM_proj_spread)
      no_waterbodies_species <- terra::resample(no_waterbodies, SDM_proj_spread)
      
      SDM_proj_spread[waterbodies_species & SDM_proj_spread<0.8] <- 0.8
      SDM_proj_spread[no_waterbodies_species & SDM_proj_spread>0.1] <- 0.1
    }
    
    ## no dispersal restrictions for birds
    if (org_tax$Klasse=="Aves" & !is.na(org_tax$Klasse)){
      SDM_proj_spread[SDM_proj_spread>0] <- 1
    }
    
    # SDM_proj_spread <- ((1-exp(-1*SDM_proj_spread)) / (1-exp(-1)))
    
    ## generate transition layer (resistance map) from SDM projection
    trans_layer <- transition(raster::raster(SDM_proj_spread), transitionFunction = mean, directions = 8)
    trans_layer <- geoCorrection(trans_layer, type = "c", scl=TRUE)
    
    ## species occurrences ########################
    occurrences <- fread(file.path(SDM_input_folder, paste0("Vorkommen_", species, identifier, ".csv")))
    
    ## spatial thinning of occurrence records to save computation time and avoid over-crowded cells #####
    ## resolution of thinning is twice is high as the SDM raster projection x
    
    ## subset to bounding box (eg Germany)
    occ_germ <- subset(occurrences, Laengengrad>crop_extent[1] & Laengengrad<crop_extent[3] &
                         Breitengrad>crop_extent[2] & Breitengrad<crop_extent[4])
    
    ## prepare template raster for thinning (resolution twice as high as in the SDM projection raster)
    template <- SDM_proj
    template <- disagg(template, fact=2)
    
    ## thinning to raster template
    occ_thinner <- thin_points(data=as.matrix(occ_germ[, c("Laengengrad", "Breitengrad")]), 
                               lon_col="Laengengrad",
                               lat_col="Breitengrad",
                               method="grid",
                               raster_obj=template,
                               trials=10)
    
    ## get final coordinates
    all_coords <- occ_germ[occ_thinner$retained[[1]] ,c("Laengengrad", "Breitengrad", "Taxon")] # prepare raster file with the mean predictions for plotting
    
    ## skip if no record remain
    if (nrow(all_coords)==0) next
    
    # Transform to sf object
    coords_sf <- st_as_sf(all_coords, coords=c("Laengengrad","Breitengrad"), crs=st_crs(regions2))
 
    # x11()
    # plot(SDM_proj)
    # plot(coords_sf, add=T, pch=16)
    
    ## create output file
    output <- data.frame(Taxon=species, prob_establ=prob_establ[,2])
    output$focal_region <- NA
    output$prob_intro <- NA
    output$prob_invasion <- NA
    
    pb = txtProgressBar(min = 0, max = nrow(regions2), initial = 0, style = 3)
    
    graphics.off()
    for (r in 1:nrow(regions2)){ # loop over all regions 
      
      ## select a target region
      name_region <- regions2$NAME_2[r]
      # name_region <- "Offenbach am Main"
      focal_region <- regions2[regions2$NAME_2==name_region,]
      centroid_reg <- st_centroid(focal_region) # region centroid
      output[r, "focal_region"] <- focal_region$NAME_2 # add to output
      
      ## crop raster and occurrences 200km around centroid
      region_buffered <- st_buffer(st_geometry(centroid_reg) , dist=dist_buff)
      focal_raster <- crop(SDM_proj_spread, region_buffered) # crop region
      focal_coords <- st_crop(coords_sf, focal_raster) # crop occurrence points
      if (nrow(focal_coords)==0) next
      if (all(is.na(extract(SDM_proj, focal_coords)[, 2]))) next # all coordinates outside raster
      
      ## skip if species is already recorded inside focal region
      occ_in_reg <- st_join(focal_region, focal_coords)
      if (!any(is.na(occ_in_reg$Taxon))) next
      
      ## compute shortest paths ##################
      
      # calculate cost of shortest paths
      focal_coords$target <- focal_region$NAME_2
      focal_coords$cost <- costDistance(trans_layer,
                                        fromCoords = as_Spatial(st_geometry(focal_coords)),
                                        toCoords = as_Spatial(st_geometry(centroid_reg))
      )
      focal_coords$cost[focal_coords$cost<1] <- 1 # avoid zeros and small values to avoid values >1 for the prob_intro
      focal_coords$prob_intro <- 1/focal_coords$cost

      # # calculate actual shortest paths
      # if (any(focal_coords$cost!="Inf")){
      #   short_paths <- shortestPath(x = trans_layer,
      #                               origin = as_Spatial(st_geometry(centroid_reg)),
      #                               goal = as_Spatial(st_geometry(focal_coords[focal_coords$cost!="Inf",])),
      #                               output = "SpatialLines"
      #   )
      # }
      # 
      # ## plot everything ###########################
      # x11()
      # # png(file.path("Ausbreitung", "Grafiken", paste0("Routen_", species, name_region, ".png")), unit="in", width=6, height=6, res=300)
      # op <- par(cex=1)
      # plot(focal_raster, main=paste(species, "-", name_region)) # raster
      # # plot(focal_raster, main=paste("Elwes-Schneeglöckchen", "-", name_region)) # raster
      # plot(st_geometry(focal_region),border="red", lwd=1,add=T) # focal region
      # plot(st_geometry(focal_coords),pch=16,add=T,col="black", cex=1) # starting points
      # # plot(short_paths, col = "black", lwd = focal_coords$prob_intro*20, add=T) # routes
      # plot(short_paths, col = "black", lwd = 1, add=T) # routes
      # plot(st_geometry(centroid_reg),pch=16,add=T,col="red", cex=1) # destination
      # par(op)
      # # dev.off()
      
      ## output ####################################
      prob_intro <- 1-prod(1- (0.1 * focal_coords$prob_intro) )# baseline weighting factor for all species
      
      # if (org_tax$Reich=="Plantae") {
      #   prob_intro <- 1-prod(1- (0.01 * focal_coords$prob_intro) )# weighting factor for plants other than those below
      # }
      # if (org_tax$Reich=="Animalia") {
      #   prob_intro <- 1-prod(1- (0.01 * focal_coords$prob_intro) )# weighting factor for animals other than those below
      # }
      if (!is.na(org_tax$Ordnung) & 
          (org_tax$Ordnung %in% c("Acipenseriformes","Clupeiformes","Cypriniformes",  ## fishes
                                                           "Cyprinodontiformes","Esociformes","Gasterosteiformes","Perciformes",
                                                           "Salmoniformes","Siluriformes") | 
           org_tax$Klasse %in% c("Mammalia", "Aves"))) {
        prob_intro <- 1-prod(1- (1 * focal_coords$prob_intro) )# weighting factor for vertebrates other than those below
      }
      # if (org_tax$Phylum=="Arthropoda") {
      #   prob_intro <- 1-prod(1- (0.01 * focal_coords$prob_intro) )# weighting factor to keep values sensible
      # }
      if (!is.na(org_tax$Reich) & org_tax$Reich=="Plantae") {
        prob_intro <- 1-prod(1- (0.01 * focal_coords$prob_intro) )# weighting factor to keep values sensible
      }
      # if (org_tax$Phylum=="Amphibia") {
      #   prob_intro <- 1-prod(1- (0.01 * focal_coords$prob_intro) )# weighting factor to keep values sensible
      # }
      # if (!is.na(org_tax$Klasse) & org_tax$Klasse=="Aves") {
      #   prob_intro <- 1-prod(1- (1 * focal_coords$prob_intro) )# weighting factor to keep values sensible
      # }

      prob_invasion <- prob_intro * prob_establ[r, 2]
      
      output[r, "prob_intro"] <- prob_intro
      output[r, "prob_invasion"] <- prob_invasion
      
      setTxtProgressBar(pb, r)
      # print(paste(round(r/nrow(regions2), 4)*100, "% done"))
    }
    close(pb)
    print(proc.time() - ptm)
    
    output_all[[s]] <- output
    names(output_all)[s] <- species
    
    saveRDS(output_all, file=file.path("Ausbreitung", "Daten", paste0("InvasionsRisiken_Landkreise", identifier, "_", s, ".rds")))
    
    if (s>0 & file.exists(file.path("Ausbreitung", "Daten", paste0("InvasionsRisiken_Landkreise", identifier, "_", s, ".rds")))){
      file.remove(file.path("Ausbreitung", "Daten", paste0("InvasionsRisiken_Landkreise", identifier, "_", s-1, ".rds")))
      file.remove(file.path("Ausbreitung", "Daten", paste0("InvasionsRisiken_Landkreise", identifier, "_", s-2, ".rds")))
      file.remove(file.path("Ausbreitung", "Daten", paste0("InvasionsRisiken_Landkreise", identifier, "_", s-3, ".rds")))
    }
  }
}

all_data <- do.call("rbind", output_all)

fwrite(all_data, file=file.path("Ausbreitung", "Daten", paste0("InvasionsRisiken_Landkreise", identifier, "_all.csv")))



