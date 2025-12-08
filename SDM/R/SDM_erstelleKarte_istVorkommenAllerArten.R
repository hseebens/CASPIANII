################################################################################
#
# Skript ist Teil des Workflows "SDM" zur Habitatmodellierung von Neobiota in 
# Deutschland. Es wird mit dem Skript run_SDM_workflow.R aufgerufen.
#
# Die Funktion liest alle vorhandenen Dateien zum Vorkommen von Arten eines SDM 
# Durchlaufs ("identifier") ein und erstellt Karten mit allen Arten akkumuliert.
#
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 08.12.25
################################################################################



erstelleKarte_istVorkommenAlle <- function(VorkommenVerzeichnis=VorkommenVerzeichnis,
                                           identifier=identifier,
                                           Name_Artenliste=Name_Artenliste,
                                           Ausschnitt=Ausschnitt_Extrapolation,
                                           exportiereKarte=FALSE,
                                           max_nTaxa_raster=100,
                                           max_nTaxa_gemeinden=300,
                                           max_nTaxa_kreise=700,
                                           Artgruppe=NULL){

  cat("\n Integriere tatsächliches Vorkommen aller Arten.\n\n")
  
  ## Load data #########################################################
  
  ## load maps of Germany
  regions3 <- st_read(dsn=file.path("SDM","Daten","Input","Shapefiles"),layer="gadm41_DEU_3", quiet=TRUE)
  regions2 <- st_read(dsn=file.path("SDM","Daten","Input","Shapefiles"),layer="gadm41_DEU_2", quiet=TRUE)
  germany_border <- st_read(dsn=file.path("SDM","Daten","Input","Shapefiles"),layer="gadm41_DEU_1", quiet=TRUE)

  ## read all files of occurrences
  all_files <- list.files(VorkommenVerzeichnis)
  all_files <- all_files[grep(identifier,all_files)]
  all_files <- all_files[grep("Vorkommen_",all_files)]
  all_files <- all_files[grep("\\.csv",all_files)]
  
  if (length(all_files)==0){
    cat("\n ERROR: Keine Dateien gefunden. Kontrolliere Verzeichnispfad 'VorkommenVerzeichnis' und Vorkommensdateien darin.")
  }
  
  ## subset for certain taxa
  if (!is.null(Artgruppe)){
    
    neobiota <- read.xlsx(file.path("SDM","Daten","Input",Name_Artenliste),sheet=1)
    Arten_Gruppe <- subset(neobiota,ArtGruppe==Artgruppe)$Taxon
    
    all_files <- all_files[grepl(paste(Arten_Gruppe,collapse="|"),all_files)]
  }
  
  ## loop over all available files of occurrences #######################
  
  all_sites_CC3 <- all_sites_CC2 <- really_all_coords <- list()
  
  ## template for collecting records
  ext_stack <- ext(c(Ausschnitt[1],Ausschnitt[3],Ausschnitt[2],Ausschnitt[4]))
  all_rasters <- new_raster <- rast(ncols=500,nrows=500,extent=ext_stack) # set resolution for map
  values(all_rasters) <- 0
  values(new_raster) <- 0
  
  #setup progress bar
  n_species <- length(all_files)
  pb <- txtProgressBar(min=0, max=n_species, initial=0,style = 3)
  
  for (i in 1:n_species){ # loop over occurrence files length(all_files)
    
    # print(paste(round(i/n_species*100,2),"%"))
    
    ## load occurrences
    Vorkommen <- fread(file=file.path(VorkommenVerzeichnis, all_files[i])) 
    
    ## get taxon name from file name
    TaxonName <- gsub("Vorkommen_|.csv","",all_files[i])
    TaxonName <- gsub(identifier,"",TaxonName)
    TaxonName <- gsub("_","",TaxonName)
    
    # if (!is.na(neobiota[neobiota$Taxon==TaxonName,]$Reich) & neobiota[neobiota$Taxon==TaxonName,]$Reich!="Plantae") next
    
    if (all(is.na(Vorkommen$Laengengrad))) next
    
    ## transform to spatial object
    all_coords <- Vorkommen[,c("Laengengrad", "Breitengrad", "Taxon")] # prepare raster file with the mean predictions for plotting
    
    # Transform to sf object
    coords_sf <- st_as_sf(all_coords,coords=c("Laengengrad","Breitengrad"),crs=st_crs(regions2))
    
    ## get occurrences per polygon (municipality)
    pts_regs3 <- st_join(coords_sf,regions3)
    polys3 <- pts_regs3$CC_3[!is.na(pts_regs3$CC_3)]
    
    ## get occurrences per polygon (Kreise)
    pts_regs2 <- st_join(coords_sf,regions2)
    polys2 <- pts_regs2$CC_2[!is.na(pts_regs2$CC_2)]

    if (length(polys2)==0) next
    
    # generate rasterized map
    raster_aliens <- terra::rasterize(as.matrix(all_coords[,c("Laengengrad","Breitengrad")]),new_raster)
    values(raster_aliens)[!is.na(values(raster_aliens))] <- 1
    values(raster_aliens)[is.na(values(raster_aliens))] <- 0
    all_rasters <- all_rasters + raster_aliens
    
    if (length(polys)==0){
      cat(paste0(" Keine Daten zum Vorkommen fuer ",TaxonName,"\n"))
      next
    } 
 
    ## prepare output

    really_all_coords[[i]] <- all_coords
    
    all_sites_CC3[[i]] <- unique(cbind.data.frame(TaxonName,polys3))
    all_sites_CC2[[i]] <- unique(cbind.data.frame(TaxonName,polys2))
    
    # update progress bar
    try(info <- sprintf("%d%% done", round((i/n_species)*100, 2)), silent = TRUE)
    setTxtProgressBar(pb, i, label = info)
    
  }
  close(pb)

  
  all_sites_CC3_df <- do.call("rbind",all_sites_CC3)
  all_sites_CC2_df <- do.call("rbind",all_sites_CC2)
  all_coords_df <- do.call("rbind",really_all_coords)

  colnames(all_sites_CC3_df) <- c("Taxon","CC_3")
  colnames(all_sites_CC2_df) <- c("Taxon","CC_2")
  
  if (!is.null(Artgruppe)){
    fwrite(all_sites_CC3_df,file.path("SDM","Daten","Output", paste0("istVorkommenAlleArten_Gemeinden_",Artgruppe,identifier,".gz")))
    fwrite(all_sites_CC2_df,file.path("SDM","Daten","Output", paste0("istVorkommenAlleArten_Kreise_",Artgruppe,identifier,".gz")))
    fwrite(all_coords_df,file.path("SDM","Daten","Output", paste0("istVorkommenAlleArten_Koordinaten_",Artgruppe,identifier,".gz")))
    terra::writeRaster(all_rasters,file.path("SDM","Daten","Output",paste0("istVorkommenAlleArten_Raster_",Artgruppe,identifier,".tif")), overwrite=TRUE, filetype = "GTiff")
  } else {
    fwrite(all_sites_CC3_df,file.path("SDM","Daten","Output", paste0("istVorkommenAlleArten_Gemeinden",identifier,".gz")))
    fwrite(all_sites_CC2_df,file.path("SDM","Daten","Output", paste0("istVorkommenAlleArten_Kreise",identifier,".gz")))
    fwrite(all_coords_df,file.path("SDM","Daten","Output", paste0("istVorkommenAlleArten_Koordinaten",identifier,".gz")))
    writeRaster(all_rasters,file.path("SDM","Daten","Output",paste0("istVorkommenAlleArten_Raster",identifier,".tif")), overwrite=TRUE, filetype="GTiff")
    # all_sites_df <- fread(file.path("SDM","Daten","Output", paste0("VorkommenAlleArten_GADM3",identifier,".gz")))
    # all_rasters <- raster(file.path("Grafiken","RasterAllOccurrences_191222"))
  }
  # all_sites_CC3_df <- fread(file.path("SDM","Daten","Output", paste0("istVorkommenAlleArten_Gemeinden",identifier,".gz")), colClasses=c("character","character"))
  # all_sites_CC2_df <- fread(file.path("SDM","Daten","Output", paste0("istVorkommenAlleArten_Kreise",identifier,".gz")), colClasses=c("character","character"))
  # # all_coords_df <- fread(file.path("SDM","Daten","Output", paste0("istVorkommenAlleArten_Koordinaten",identifier,".gz")))
  # all_rasters <- rast(file.path("SDM","Daten","Output",paste0("istVorkommenAlleArten_Raster",identifier,".tif")))

  cat(paste0(" Vorkommen von ",length(unique(all_sites_CC3_df$Taxon)), " Arten integriert."))

  
  if (exportiereKarte){
    
    ## Plot raster map ################################################################################
    
    germany  <- terra::crop(all_rasters, ext_stack) # crop the climate data to the extent of the land cover data (needed because the climate data has a global extent and the land cover data has an European extent)

    aliens_masked <- terra::mask(germany,germany_border)
    if (!is.null(max_nTaxa_raster)){
      values(aliens_masked)[values(aliens_masked)>max_nTaxa_raster] <- max_nTaxa_raster
    }
    
    if (!is.null(Artgruppe)){
      png(file.path("SDM","Grafiken",paste0("KarteDeutschland_VorkommenAlle_Raster_",Artgruppe,identifier,".png")),unit="in",width=8,height=8,res=300)
    } else {
      png(file.path("SDM","Grafiken",paste0("KarteDeutschland_VorkommenAlle_Raster",identifier,".png")),unit="in",width=8,height=8,res=300)
    }
    plot(aliens_masked,col=rev(hcl.colors(12,pal="Mint")))
    plot(st_geometry(germany_border),add=TRUE,lwd=0.5)
    # text(">",x=17.9,y=53.08,xpd=NA)
    dev.off()

    ## plot all all_sites_df summed up #################################################################

    ## Gemeinden (municipalities):
    
    all_sites_agg_CC3 <- aggregate(Taxon ~ CC_3, data=all_sites_CC3_df, length)
    
    regions3 <- merge(regions3, all_sites_agg_CC3, by="CC_3")
    if (!is.null(max_nTaxa_gemeinden)){
      regions3$Taxon[regions3$Taxon>max_nTaxa_gemeinden] <- max_nTaxa_gemeinden
    }
    
    if (!is.null(Artgruppe)){
      png(file.path("SDM","Grafiken",paste0("KarteDeutschland_VorkommenAlle_Gemeinden_",Artgruppe,identifier,".png")),unit="in",width=8,height=8,res=300)
    } else {
      png(file.path("SDM","Grafiken",paste0("KarteDeutschland_VorkommenAlle_Gemeinden",identifier,".png")),unit="in",width=8,height=8,res=300)
    }
    mf_choro(regions3,var="Taxon",leg_title="Anzahl Neobiota",border=NA, breaks = "equal", pal=rev(hcl.colors(15,pal="Mint"))) #,breaks="pretty"
    plot(st_geometry(germany_border),add=TRUE,lwd=0.5)
    # text(">",x=17.85,y=53,xpd=NA)
    dev.off()

    ## Kreise :
    
    all_sites_agg_CC2 <- aggregate(Taxon ~ CC_2, data=all_sites_CC2_df, length)
    
    regions2 <- merge(regions2, all_sites_agg_CC2, by="CC_2")
    if (!is.null(max_nTaxa_kreise)){
      regions2$Taxon[regions2$Taxon>max_nTaxa_kreise] <- max_nTaxa_kreise
    }
    
    if (!is.null(Artgruppe)){
      png(file.path("SDM","Grafiken",paste0("KarteDeutschland_VorkommenAlle_Kreise_",Artgruppe,identifier,".png")),unit="in",width=8,height=8,res=300)
    } else {
      png(file.path("SDM","Grafiken",paste0("KarteDeutschland_VorkommenAlle_Kreise",identifier,".png")),unit="in",width=8,height=8,res=300)
    }
    mf_choro(regions2, var="Taxon",leg_title="Anzahl Neobiota",border=NA, pal=rev(hcl.colors(15,pal="Mint"))) #,breaks="pretty"
    plot(st_geometry(germany_border),add=TRUE,lwd=0.5)
    # text(">",x=17.85,y=53,xpd=NA)
    dev.off()
    
  }
}





