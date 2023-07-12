




erstelleKarte_potVorkommenAlle <- function(VorhersageVerzeichnis=parent.frame()$VorhersageVerzeichnis,
                                           identifier=parent.frame()$identifier,
                                           exportiereKarte=TRUE,
                                           Taxa=NULL){

  cat("\n Integriere potentielles Vorkommen aller Arten.\n\n")
  
  ## Load data #########################################################
  ## load maps of Germany
  # regions <- st_read(dsn=file.path("SDM","Data","Input","Shapefiles"),layer="gadm41_DEU_4",quiet=T)
  regions <- st_read(dsn=file.path("SDM","Data","Input","Shapefiles"),layer="gadm41_DEU_3")
  germany_border <- st_read(dsn=file.path("SDM","Data","Input","Shapefiles"),layer="gadm41_DEU_1",quiet=T)
  # regions$ID <- 1:nrow(regions)
  
  ## read all files of occurrences
  all_files <- list.files(VorhersageVerzeichnis)
  all_files <- all_files[grep(identifier,all_files)]
  all_files <- all_files[grep("HabitatEignung_",all_files)]
  all_files <- all_files[grep("\\.gz",all_files)]
 
  if (!is.null(Taxa)){
    
    neobiota <- read.xlsx(file.path("SDM","Data","Input","ListeGebietsfremderArten_gesamt_standardisiert.xlsx"),sheet=1)
    Arten_Gruppe <- subset(neobiota,ArtGruppe==Taxa)$Taxon
    
    all_files <- all_files[grepl(paste(Arten_Gruppe,collapse="|"),all_files)]
  }
  
  ## loop over all available predictor files #######################
  
  ## template for collecting records
  all_rasters <- new_raster <- raster(ncols=150,nrows=150,xmx=16,xmn=5,ymn=47,ymx=56) #
  values(all_rasters) <- 0
  values(new_raster) <- 0
  
  all_TaxonName <- all_maxSuits <- all_meanSuits <- list()
  n_species <- length(all_files)
  for (i in 1:n_species){ #length(all_files)

    print(paste(round(i/n_species*100,2),"%"))

    ## load suitabilities
    HabitatEignung <- fread(file=file.path(VorhersageVerzeichnis, all_files[i]))
    
    if (all(is.na(HabitatEignung$HabitatEignung_mittel))) next
    
    ## transform to spatial object
    meanSuit_coords <- HabitatEignung[,c("x", "y", "HabitatEignung_mittel")]
    
    ## generate empty raster for the first loop as a data container    
    if (i==1){
      rasterData_all <- rasterFromXYZ(meanSuit_coords)
      values(rasterData_all)[!is.na(values(rasterData_all))] <- 0
      rasterData_max07 <- rasterData_all
    }
    
    ## get taxon name
    TaxonName <- gsub("HabitatEignung_|.gz","",all_files[i])
    TaxonName <- gsub(identifier,"",TaxonName)
    TaxonName <- gsub("_","",TaxonName)

    ## extract mean values per polygon
    meanSuit_rast <- terra::rast(meanSuit_coords,type="xyz")
    meanSuits_regs <- terra::extract(meanSuit_rast,regions,fun=mean)
    meanSuits_regs$CC_3 <- regions$CC_3
    meanSuits_regs <- meanSuits_regs[,c("CC_3","HabitatEignung_mittel")]

    ## subset to suitable habitats
    maxSuitSpec <- terra::subset(meanSuits_regs,HabitatEignung_mittel>0.7)
    # maxSuitSpec <- subset(meanSuits_regs,HabitatEignung_mittel>0.9) # max
    
    if (nrow(maxSuitSpec)==0){
      cat(paste0(" Keine ausreichenden Daten zur Vorhersage fuer ",TaxonName,"\n"))
      next
    }
    
    ## collect data as raster cells
    ## suitability >0.7
    raster_aliens <- terra::rasterize(meanSuit_coords[,c("x","y")],rasterData_max07, field = meanSuit_coords$HabitatEignung_mittel)
    terra::values(raster_aliens)[terra::values(raster_aliens)>0.7] <- 1
    terra::values(raster_aliens)[terra::values(raster_aliens)<0.7] <- 0
    rasterData_max07 <- rasterData_max07 + raster_aliens
    
    ## all values
    raster_aliens <- terra::rasterize(meanSuit_coords[,c("x","y")],rasterData_all,field=meanSuit_coords$HabitatEignung_mittel)
    rasterData_all <- rasterData_all + raster_aliens

    ## prepare output polygon data
    meanSuits_regs$HabitatEignung_mittel <- round(meanSuits_regs$HabitatEignung_mittel,3)
    meanSuits_regs$Taxon <- TaxonName
    colnames(meanSuits_regs) <- c("CC_3","HabitatEignung","Taxon")

    maxSuitSpec$HabitatEignung_mittel <- round(maxSuitSpec$HabitatEignung_mittel,3)
    maxSuitSpec$Taxon <- TaxonName
    colnames(maxSuitSpec) <- c("CC_3","HabitatEignung","Taxon")

    all_TaxonName[[i]] <- strsplit(all_files[i],"_")[[1]][[2]]

    all_meanSuits[[i]] <- meanSuits_regs

    all_maxSuits[[i]] <- maxSuitSpec
  }

  all_maxSuits_df <- do.call("rbind",all_maxSuits)
  all_meanSuits_df <- do.call("rbind",all_meanSuits)
  
  all_maxSuits_df <- all_maxSuits_df[all_maxSuits_df$CC_3!="NA",]
  all_meanSuits_df <- all_meanSuits_df[all_meanSuits_df$CC_3!="NA",]
  
  ## crop raster to German borders
  rasterData_all_masked <- terra::mask(rasterData_all,germany_border)
  rasterData_max07_masked <- terra::mask(rasterData_max07,germany_border)
  
  
  ## output ############################################################
  all_maxSuits_df <- all_maxSuits_df[order(all_maxSuits_df$CC_3,all_maxSuits_df$HabitatEignung,decreasing = T),]

  if (!is.null(Taxa)){
    fwrite(all_maxSuits_df,file.path(getwd(),VorhersageVerzeichnis,  paste0("potVorkommen_",Taxa,"_maxEignung_GADM3_",identifier,".gz")))
    fwrite(all_meanSuits_df,file.path(getwd(),VorhersageVerzeichnis, paste0("potVorkommen_",Taxa,"_mittlereEignung_GADM3_",identifier,".gz")))
    
    writeRaster(rasterData_all_masked,file.path(getwd(),VorhersageVerzeichnis, paste0("potVorkommen_",Taxa,"_mittlereEignung_raster_",identifier)),overwrite=T,format="GTiff")
    writeRaster(rasterData_max07_masked,file.path(getwd(),VorhersageVerzeichnis, paste0("potVorkommen_",Taxa,"_maxEignung_raster_",identifier)),overwrite=T,format="GTiff")
    
  } else {
    fwrite(all_maxSuits_df,file.path(getwd(),VorhersageVerzeichnis, paste0("potVorkommen_alleArten_maxEignung_GADM3_",identifier,".gz")))
    fwrite(all_meanSuits_df,file.path(getwd(),VorhersageVerzeichnis, paste0("potVorkommen_alleArten_meanEignung_GADM3_",identifier,".gz")))
    
    writeRaster(rasterData_all_masked,file.path("SDM","Data","Output",paste0("potVorkommen_mittlereEignung_raster_",identifier)),overwrite=T,format="GTiff")
    writeRaster(rasterData_max07_masked,file.path("SDM","Data","Output",paste0("potVorkommen_maxEignung_raster_",identifier)),overwrite=T,format="GTiff")
  }

  # all_maxSuits_df <- fread(file=file.path("SDM","Data","Output", paste0("potVorkommen_alleArten_maxEignung_GADM3_",identifier,".gz")))
  # all_meanSuits_df <- fread(file=file.path("SDM","Data","Output", paste0("potVorkommen_alleArten_meanEignung_GADM3_",identifier,".gz")))
  
  ## calculate distance beteen neighbouring points
  # library(fields)
  # rdist.earth(x1=HabitatEignung[10000,c("x","y")],x2=HabitatEignung[10001,c("x","y")],miles=F)
  
  ## plot all taxa with suitable habitats ##################################
  all_maxSuits_split <- terra::split(all_maxSuits_df,f=all_maxSuits_df$CC_3)
  nSpec <- unlist(lapply(all_maxSuits_split,nrow))
  regs_spec <- cbind.data.frame(names(all_maxSuits_split),as.integer(nSpec))
  colnames(regs_spec) <- c("CC_3","potAnzahlNeobiota")
  # hist(unlist(nSpec),50)

  
  ## Plot maps ##########################################
  if (exportiereKarte){
    
    cat("\n Erstelle Karten...\n")
    
    regions_plot <- terra::merge(regions,regs_spec,by="CC_3",all=T)

    ## plot presence-absence (suit > 0.7) ##############
    if (!is.null(Taxa)){
      png(file.path(getwd(),VorhersageVerzeichnis,"Grafiken", paste0("KarteDeutschland_PotNArten07_GADM3_pretty_",Taxa,"_",identifier,".png")),unit="in",width=8,height=8,res=300)
    } else {
      png(file.path(getwd(),VorhersageVerzeichnis,"Grafiken",paste0("KarteDeutschland_PotNArten07_GADM3_pretty_",identifier,".png")),unit="in",width=8,height=8,res=300)
    }
    if(is.numeric(regions_plot$potAnzahlNeobiota) && length(regions$potAnzahlNeobiota) > 1){
    mf_choro(regions_plot,var="potAnzahlNeobiota",leg_title="Pot. Anzahl Neobiota",border=NA,breaks="pretty")}
    dev.off()
    
    
    ## plot all suitabilities summed up #########

    all_meanSuits_agg <- aggregate(HabitatEignung ~ CC_3, data=all_meanSuits_df,sum)
    
    # regions$ID <- 1:nrow(regions)
    regions_plot <- terra::merge(regions,all_meanSuits_agg,by="CC_3")
    
    if (!is.null(Taxa)){
      png(file.path(getwd(),VorhersageVerzeichnis,"Grafiken", paste0("KarteDeutschland_SumEignungAlle_GADM3_pretty_",Taxa,"_",identifier,".png")),unit="in",width=8,height=8,res=300) # plot without occurrences
    } else {
      png(file.path(getwd(),VorhersageVerzeichnis,"Grafiken", paste0("KarteDeutschland_SumEignungAlle_GADM3_pretty_",identifier,".png")),unit="in",width=8,height=8,res=300) # plot without occurrences
    }
    if(is.numeric(regions_plot$HabitatEignung) && length(regions$HabitatEignung) > 1){
      mf_choro(regions_plot,var="HabitatEignung",leg_title="Habitateignung",border=NA,breaks="pretty")} #
    dev.off()
    
    
    ## plots raster data #####################
    
    if (!is.null(Taxa)){
      png(file.path(getwd(),VorhersageVerzeichnis,"Grafiken",paste0("KarteDeutschland_SumEignungAlle_",Taxa,"_raster150",identifier,".png")),unit="in",width=8,height=8,res=300)
    } else {
      png(file.path(getwd(),VorhersageVerzeichnis,"Grafiken",paste0("KarteDeutschland_SumEignungAlle_raster150",identifier,".png")),unit="in",width=8,height=8,res=300)
    }
    plot(rasterData_all_masked, col=rev(hcl.colors(10,pal="Mint")))
    plot(st_geometry(germany_border),add=T,lwd=0.5)
    # text(">",x=17.9,y=53.08,xpd=NA)
    dev.off()
    
    ## presence/absence ###########
    if (!is.null(Taxa)){
      png(file.path(getwd(),VorhersageVerzeichnis,"Grafiken",paste0("KarteDeutschland_PotNArten07_",Taxa,"_raster150",identifier,".png")),unit="in",width=8,height=8,res=300)
    } else {
      png(file.path(getwd(),VorhersageVerzeichnis,"Grafiken",paste0("KarteDeutschland_PotNArten07_raster150_",identifier,".png")),unit="in",width=8,height=8,res=300)
    }
    plot(rasterData_all, col=rev(hcl.colors(10,pal="Mint")))
    plot(st_geometry(germany_border),add=T,lwd=0.5)
    # text(">",x=17.9,y=53.08,xpd=NA)
    dev.off()

  }
}




