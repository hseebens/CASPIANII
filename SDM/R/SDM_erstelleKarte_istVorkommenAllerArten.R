


erstelleKarte_istVorkommenAlle <- function(VorkommenVerzeichnis=VorkommenVerzeichnis,
                                           identifier=identifier,
                                           exportiereKarte=F,
                                           Taxa=NULL){

  cat("\n Integriere tatsächliches Vorkommen aller Arten.\n\n")
  
  ## Load data #########################################################
  ## load maps of Germany
  # regions <- st_read(dsn=file.path("SDM","Data","Input","Shapefiles"),layer="gadm41_DEU_4",quiet=T)
  regions <- st_read(dsn=file.path("SDM","Data","Input","Shapefiles"),layer="gadm41_DEU_3")
  germany_border <- st_read(dsn=file.path("SDM","Data","Input","Shapefiles"),layer="gadm41_DEU_1",quiet=T)
  # regions$ID <- 1:nrow(regions)
  
  ## read all files of occurrences
  all_files <- list.files(VorkommenVerzeichnis)
  all_files <- all_files[grep(identifier,all_files)]
  all_files <- all_files[grep("Vorkommen_",all_files)]
  all_files <- all_files[grep("\\.csv",all_files)]
  
  ## subset for certain taxa
  if (!is.null(Taxa)){
    
    neobiota <- read.xlsx(file.path("SDM","Data","Input","ListeGebietsfremderArten_gesamt_standardisiert.xlsx"),sheet=1)
    Arten_Gruppe <- subset(neobiota,ArtGruppe==Taxa)$Taxon
    
    all_files <- all_files[grepl(paste(Arten_Gruppe,collapse="|"),all_files)]
  }
  
  ## loop over all available files of occurrences #######################
  
  all_sites <- really_all_coords <- list()
  
  ## template for collecting records
  all_rasters <- new_raster <- raster(ncols=150,nrows=150,xmx=16,xmn=5,ymn=47,ymx=56) #
  values(all_rasters) <- 0
  values(new_raster) <- 0
  
  n_species <- length(all_files) ### note: check value of i (environment) when debugging manually! 
  for (i in 1:n_species){ # loop over occurrence files length(all_files)
    print(paste("Species",i , "of", n_species,"=", round(i/n_species*100,2),"%"))
    
    ## load occurrences
    Vorkommen <- fread(file=file.path(VorkommenVerzeichnis, all_files[i])) 
    
    ## get taxon name from file name
    ### note: take care not to conflate internal TaxonName variable with TaxonName variable in environment
    ### note: could add check if all species in variable artenliste have a corresponding file; may use artenliste[i]? 
    TaxonName <- gsub("Vorkommen_|.csv","",all_files[i])
    TaxonName <- gsub(identifier,"",TaxonName)
    TaxonName <- gsub("_","",TaxonName)
    
    if (all(is.na(Vorkommen$Laengengrad))) next # check if no NA in Vorkommen$Laengengrad; is this faster than if(any(is.na()))?
    
    ## transform to spatial object
    all_coords <- Vorkommen[,c("Laengengrad", "Breitengrad", "Taxon")] # prepare raster file with the mean predictions for plotting
    
    # Transform to sf object
    coords_sf <- st_as_sf(all_coords,coords=c("Laengengrad","Breitengrad"),crs=st_crs(regions))
    
    ## get occurrences per polygon
    pts_regs <- st_join(coords_sf,regions)
    # polys <- pts_regs[!is.na(pts_regs$ID),]
    polys <- pts_regs$CC_3[!is.na(pts_regs$CC_3)] ### note: polys() is an alias for lines{terra}, var should be renamed
    
    # generate rasterized map
    raster_aliens <- rasterize(all_coords[,c("Laengengrad","Breitengrad")],new_raster) ### will convert to {terra} later
    values(raster_aliens)[!is.na(values(raster_aliens))] <- 1
    values(raster_aliens)[is.na(values(raster_aliens))] <- 0
    all_rasters <- all_rasters + raster_aliens
    
    if (length(polys)==0){
      cat(paste0(" Keine Daten zum Vorkommen fuer ",TaxonName,"\n"))
      next
    } 
    
    ## prepare output
    
    # all_TaxonName[[i]] <- strsplit(all_files[i],"_")[[1]][[2]]
    
    # really_all_coords[[i]] <- all_coords
    
    all_sites[[i]] <- unique(cbind.data.frame(TaxonName,polys)) ### note: cbind may cause multiple binds when debugging manually.
  } ## end for-loop over occurence files ### note: depending on the number of files, parallelise later in dev?
  
  all_sites_df <- do.call("rbind",all_sites) ### note: rbind may cause multiple binds when debugging manually.
  ### note: all_sites_df <- data.table::rbindlist(all_sites) may be more efficient for large lists.
  # all_coords_df <- do.call("rbind",really_all_coords)

  colnames(all_sites_df) <- c("Taxon","CC_3") ### note: concatenate may cause multiple binds when debugging manually. 
  
  if (!is.null(Taxa)){
    fwrite(all_sites_df,file.path("SDM","Data","Output", paste0("istVorkommenAlleArten_GADM3_",Taxa,"_",identifier,".gz")))
    writeRaster(all_rasters,file.path("SDM","Data","Output",paste0("istVorkommenAlleArten_Raster_",Taxa,"_",identifier)),overwrite=T,format="GTiff")
  } else {
    fwrite(all_sites_df,file.path("SDM","Data","Output", paste0("istVorkommenAlleArten_GADM3_",identifier,".gz")))
    writeRaster(all_rasters,file.path("SDM","Data","Output",paste0("istVorkommenAlleArten_Raster_",identifier)),overwrite=T,format="GTiff")
    # all_sites_df <- fread(file.path("SDM","Data","Output", paste0("VorkommenAlleArten_GADM3_",identifier,".gz")))
    # all_rasters <- raster(file.path("Grafiken","RasterAllOccurrences_191222"))
  }
  
  cat(paste0(" Vorkommen von ",length(unique(all_sites_df$Taxon)), " Arten integriert."))

  
  if (exportiereKarte){
    
    ## Plot raster map ################################################################################
    
    Ausschnitt <- (c(3,47,17,55.1)) # crop to Germany  # lower left and upper right corner (long-lat)
    # ext_stack <- terra::ext(Ausschnitt) ## not needed, but equivalent result as old code
    germany  <- terra::crop(all_rasters, Ausschnitt) # crop the climate data to the extent of the land cover data (needed because the climate data has a global extent and the land cover data has an European extent)
    # values(germany)[values(germany)>100] <- 100
    
    # # nSpec_germany <- rasterize(all_coords_df[,c("Laengengrad","Breitengrad")],new_raster)
    # # germany2  <- crop(nSpec_germany, ext_stack) # crop the climate data to the extent of the land cover data (needed because the climate data has a global extent and the land cover data has an European extent)
<<<<<<< HEAD
    aliens_masked <- terra::mask(germany,germany_border)
=======
    aliens_masked <- terra::mask(germany2,germany_border)
>>>>>>> 76e64e5d0bf16b07d736467ef54ab38c519991a0
    values(aliens_masked)[values(aliens_masked)>300] <- 300
    
    if (!is.null(Taxa)){
      png(file.path("SDM","Data","Output","Grafiken",paste0("KarteDeutschland_VorkommenAlle_raster150_",Taxa,"_",identifier,".png")),unit="in",width=8,height=8,res=300)
    } else {
      png(file.path("SDM","Data","Output","Grafiken",paste0("KarteDeutschland_VorkommenAlle_raster150_",identifier,".png")),unit="in",width=8,height=8,res=300)
    }
    plot(aliens_masked,col=rev(hcl.colors(10,pal="Mint")))
    plot(st_geometry(germany_border),add=T,lwd=0.5)
    text(">",x=17.9,y=53.08,xpd=NA)
    dev.off()

    germany2 <-  terra::mask(germany,germany_border)
    values(germany2) <- log10(values(germany2)+1)
    
    if (!is.null(Taxa)){
      png(file.path("SDM","Data","Output","Grafiken",paste0("KarteDeutschland_VorkommenAlle_raster150_log10_",Taxa,"_",identifier,".png")),unit="in",width=8,height=8,res=300)
    } else {
      png(file.path("SDM","Data","Output","Grafiken",paste0("KarteDeutschland_VorkommenAlle_raster150_log10_",identifier,".png")),unit="in",width=8,height=8,res=300)
    }
    plot(germany2,col=rev(hcl.colors(10,pal="Mint")))
    plot(st_geometry(germany_border),add=T,lwd=0.5)
    dev.off()

    
    ## plot all all_sites_df summed up #################################################################

    cat("\n Erstelle Karten...\n")
    
    all_sites_agg <- aggregate(Taxon ~ CC_3, data=all_sites_df,length) ### note:  aggregate::stats() could be masked if {terra} or {sf} are loaded.
    regions <- terra::merge(regions,all_sites_agg,by="CC_3") ### note: merge may cause multiple binds when debugging manually.
    regions$Taxon[regions$Taxon>300] <- 300
    
    if (!is.null(Taxa)){
      png(file.path("SDM","Data","Output","Grafiken",paste0("KarteDeutschland_VorkommenAlle_GADM3_max100_",Taxa,"_",identifier,".png")),unit="in",width=8,height=8,res=300)
    } else {
      png(file.path("SDM","Data","Output","Grafiken",paste0("KarteDeutschland_VorkommenAlle_GADM3_max100_",identifier,".png")),unit="in",width=8,height=8,res=300)
    }
    if(is.numeric(regions$Taxon) && length(unique(regions$Taxon)) > 1){
    mf_choro(regions,var="Taxon",leg_title="Anzahl Neobiota",border=NA,breaks="pretty") #,breaks="pretty" ### note: this does not break pretty. ^__^ 
    text(">",x=17.85,y=53,xpd=NA)}
    dev.off()

  } ## end if(exportiereKarte)


}




