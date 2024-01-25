




erstelleKarte_potVorkommenAlle <- function(VorhersageVerzeichnis=VorhersageVerzeichnis,
                                           identifier=identifier,
                                           Name_Artenliste=Name_Artenliste,
                                           Ausschnitt=Ausschnitt_Extrapolation,
                                           exportiereKarte=TRUE,
                                           Artgruppe=NULL){

  cat("\n Integriere potentielles Vorkommen aller Arten.\n\n")
  
  ## Load data #########################################################
  ## load maps of Germany
  # regions <- st_read(dsn=file.path("SDM","Data","Input","Shapefiles"),layer="gadm41_DEU_4",quiet=T)
  regions3 <- st_read(dsn=file.path("SDM","Data","Input","Shapefiles"),layer="gadm41_DEU_3", quiet=TRUE)
  regions2 <- st_read(dsn=file.path("SDM","Data","Input","Shapefiles"),layer="gadm41_DEU_2", quiet=TRUE)
  regions2$CC_2[regions2$NAME_2=="Bodensee"] <- "001" # missing
  germany_border <- st_read(dsn=file.path("SDM","Data","Input","Shapefiles"),layer="gadm41_DEU_1",quiet=T)
  # regions$ID <- 1:nrow(regions)
  
  ## read all files of occurrences
  all_files <- list.files(VorhersageVerzeichnis)
  all_files <- all_files[grep(identifier,all_files)]
  all_files <- all_files[grep("HabitatEignung_",all_files)]
  all_files <- all_files[grep("\\.gz",all_files)]
 
  if (!is.null(Artgruppe)){
    
    neobiota <- read.xlsx(file.path("SDM","Data","Input",Name_Artenliste),sheet=1)
    Arten_Gruppe <- subset(neobiota,ArtGruppe==Artgruppe)$Taxon
    
    all_files <- all_files[grepl(paste(Arten_Gruppe,collapse="|"),all_files)]
  }
  
  ## loop over all available predictor files #######################
  
  ## template for collecting records
  ext_stack <- ext(c(Ausschnitt[1],Ausschnitt[3],Ausschnitt[2],Ausschnitt[4]))
  all_rasters <- new_raster <- rast(ncols=150,nrows=150,extent=ext_stack) #
  values(all_rasters) <- 0
  values(new_raster) <- 0
  
  all_TaxonName <- all_meanSuits_CC2 <- all_meanSuits_CC3 <- all_maxSuits_CC2 <- all_maxSuits_CC3 <- list()
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
      # rasterData_all <- rasterFromXYZ(meanSuit_coords)
      rasterData_all <- rast(meanSuit_coords, type="xyz")
      values(rasterData_all)[!is.na(values(rasterData_all))] <- 0
      rasterData_max07 <- rasterData_all
    }
    
    ## get taxon name
    TaxonName <- gsub("HabitatEignung_|.gz","",all_files[i])
    TaxonName <- gsub(identifier,"",TaxonName)
    TaxonName <- gsub("_","",TaxonName)

    ## extract mean values per polygon
    meanSuit_rast <- terra::rast(meanSuit_coords,type="xyz")
    meanSuits_regs2 <- terra::extract(meanSuit_rast,regions2,fun=mean,na.rm=T)
    meanSuits_regs2$CC_2 <- regions2$CC_2
    meanSuits_regs2 <- meanSuits_regs2[,c("CC_2","HabitatEignung_mittel")]
    meanSuits_regs3 <- terra::extract(meanSuit_rast,regions3,fun=mean,na.rm=T)
    meanSuits_regs3$CC_3 <- regions3$CC_3
    meanSuits_regs3 <- meanSuits_regs3[,c("CC_3","HabitatEignung_mittel")]

    ## subset to suitable habitats
    maxSuitSpec2 <- subset(meanSuits_regs2,HabitatEignung_mittel>0.7)
    maxSuitSpec3 <- subset(meanSuits_regs3,HabitatEignung_mittel>0.7)
    # maxSuitSpec <- subset(meanSuits_regs,HabitatEignung_mittel>0.9) # max
    
    if (nrow(maxSuitSpec3)==0 | nrow(maxSuitSpec2)==0){
      cat(paste0(" Keine ausreichenden Daten zur Vorhersage fuer ",TaxonName,"\n"))
      next
    }
    
    ## collect data as raster cells
    ## suitability >0.7
    raster_aliens <- terra::rasterize(x=as.matrix(meanSuit_coords[,c("x","y")]), y=rasterData_max07, values=meanSuit_coords$HabitatEignung_mittel)
    values(raster_aliens)[values(raster_aliens)>0.7] <- 1
    values(raster_aliens)[values(raster_aliens)<0.7] <- 0
    rasterData_max07 <- rasterData_max07 + raster_aliens
    
    ## all values
    raster_aliens <- terra::rasterize(as.matrix(meanSuit_coords[,c("x","y")]), rasterData_all, values=meanSuit_coords$HabitatEignung_mittel)
    rasterData_all <- rasterData_all + raster_aliens

    ## prepare output polygon data
    meanSuits_regs2$HabitatEignung_mittel <- round(meanSuits_regs2$HabitatEignung_mittel,3)
    meanSuits_regs3$HabitatEignung_mittel <- round(meanSuits_regs3$HabitatEignung_mittel,3)
    meanSuits_regs2$Taxon <- TaxonName
    meanSuits_regs3$Taxon <- TaxonName
    colnames(meanSuits_regs2) <- c("CC_2","HabitatEignung","Taxon")
    colnames(meanSuits_regs3) <- c("CC_3","HabitatEignung","Taxon")
    
    # if (any(is.na(meanSuits_regs2$CC_2))) stop()
    # if (any(is.na(meanSuits_regs2$HabitatEignung))) stop()
    
    maxSuitSpec2$HabitatEignung_mittel <- round(maxSuitSpec2$HabitatEignung_mittel,3)
    maxSuitSpec3$HabitatEignung_mittel <- round(maxSuitSpec3$HabitatEignung_mittel,3)
    maxSuitSpec2$Taxon <- TaxonName
    maxSuitSpec3$Taxon <- TaxonName
    colnames(maxSuitSpec2) <- c("CC_2","HabitatEignung","Taxon")
    colnames(maxSuitSpec3) <- c("CC_3","HabitatEignung","Taxon")
    
    all_TaxonName[[i]] <- strsplit(all_files[i],"_")[[1]][[2]]

    all_meanSuits_CC2[[i]] <- meanSuits_regs2
    all_meanSuits_CC3[[i]] <- meanSuits_regs3
    
    all_maxSuits_CC2[[i]] <- maxSuitSpec2
    all_maxSuits_CC3[[i]] <- maxSuitSpec3
  }

  all_maxSuits_df_CC2 <- do.call("rbind",all_maxSuits_CC2)
  all_meanSuits_df_CC2 <- do.call("rbind",all_meanSuits_CC2)
  all_maxSuits_df_CC3 <- do.call("rbind",all_maxSuits_CC3)
  all_meanSuits_df_CC3 <- do.call("rbind",all_meanSuits_CC3)
  
  all_maxSuits_df_CC2 <- all_maxSuits_df_CC2[all_maxSuits_df_CC2$CC_2!="NA",]
  all_meanSuits_df_CC2 <- all_meanSuits_df_CC2[all_meanSuits_df_CC2$CC_2!="NA",]
  all_maxSuits_df_CC3 <- all_maxSuits_df_CC3[all_maxSuits_df_CC3$CC_3!="NA",]
  all_meanSuits_df_CC3 <- all_meanSuits_df_CC3[all_meanSuits_df_CC3$CC_3!="NA",]
  
  ## crop raster to German borders
  terra::crs(rasterData_all) <- "epsg:4326"
  terra::crs(rasterData_max07) <- "epsg:4326"
  rasterData_all_masked <- terra::mask(rasterData_all,germany_border)
  rasterData_max07_masked <- terra::mask(rasterData_max07,germany_border)
  
  
  ## output ############################################################
  all_maxSuits_df_CC2 <- all_maxSuits_df_CC2[order(all_maxSuits_df_CC2$CC_2,all_maxSuits_df_CC2$HabitatEignung,decreasing = T),]
  all_meanSuits_df_CC2 <- all_meanSuits_df_CC2[order(all_meanSuits_df_CC2$CC_2,all_meanSuits_df_CC2$HabitatEignung,decreasing = T),]
  all_maxSuits_df_CC3 <- all_maxSuits_df_CC3[order(all_maxSuits_df_CC3$CC_3,all_maxSuits_df_CC3$HabitatEignung,decreasing = T),]
  all_meanSuits_df_CC3 <- all_meanSuits_df_CC3[order(all_meanSuits_df_CC3$CC_3,all_meanSuits_df_CC3$HabitatEignung,decreasing = T),]
  
  if (!is.null(Artgruppe)){
    
    fwrite(all_maxSuits_df_CC2,file.path("SDM","Data","Output",  paste0("potVorkommen_",Artgruppe,"_Eignung07_Kreise_",identifier,".gz")))
    fwrite(all_meanSuits_df_CC2,file.path("SDM","Data","Output", paste0("potVorkommen_",Artgruppe,"_EignungSumme_Kreise_",identifier,".gz")))
    
    fwrite(all_maxSuits_df_CC3,file.path("SDM","Data","Output",  paste0("potVorkommen_",Artgruppe,"_Eignung07_Gemeinden_",identifier,".gz")))
    fwrite(all_meanSuits_df_CC3,file.path("SDM","Data","Output", paste0("potVorkommen_",Artgruppe,"_EignungSumme_Gemeinden_",identifier,".gz")))
    
    terra::writeRaster(rasterData_all_masked,file.path("SDM","Data","Output",paste0("potVorkommen_",Artgruppe,"_EignungSumme_raster_",identifier,".tif")),overwrite=T,filetype="GTiff")
    terra::writeRaster(rasterData_max07_masked,file.path("SDM","Data","Output",paste0("potVorkommen_",Artgruppe,"_Eignung07_raster_",identifier,".tif")),overwrite=T,filetype="GTiff")
    
  } else {
    
    fwrite(all_maxSuits_df_CC2,file.path("SDM","Data","Output",  paste0("potVorkommen_alleArten_Eignung07_Kreise_",identifier,".gz")))
    fwrite(all_meanSuits_df_CC2,file.path("SDM","Data","Output", paste0("potVorkommen_alleArten_EignungSumme_Kreise_",identifier,".gz")))
    
    fwrite(all_maxSuits_df_CC3,file.path("SDM","Data","Output",  paste0("potVorkommen_alleArten_Eignung07_Gemeinden_",identifier,".gz")))
    fwrite(all_meanSuits_df_CC3,file.path("SDM","Data","Output", paste0("potVorkommen_alleArten_EignungSumme_Gemeinden_",identifier,".gz")))
    
    terra::writeRaster(rasterData_all_masked,file.path("SDM","Data","Output",paste0("potVorkommen_EignungSumme_Raster_",identifier,".tif")),overwrite=T,filetype="GTiff")
    terra::writeRaster(rasterData_max07_masked,file.path("SDM","Data","Output",paste0("potVorkommen_Eignung07_Raster_",identifier,".tif")),overwrite=T,filetype="GTiff")
  }

  # all_maxSuits_df <- fread(file=file.path("SDM","Data","Output", paste0("potVorkommen_alleArten_Eignung07_GADM3_",identifier,".gz")))
  # all_meanSuits_df <- fread(file=file.path("SDM","Data","Output", paste0("potVorkommen_alleArten_meanEignung_GADM3_",identifier,".gz")))
  
  ## calculate distance beteen neighbouring points
  # library(fields)
  # rdist.earth(x1=HabitatEignung[10000,c("x","y")],x2=HabitatEignung[10001,c("x","y")],miles=F)
  
  ## plot all taxa with suitable habitats ##################################
  all_maxSuits_split_CC3 <- split(all_maxSuits_df_CC3,f=all_maxSuits_df_CC3$CC_3)
  all_maxSuits_split_CC2 <- split(all_maxSuits_df_CC2,f=all_maxSuits_df_CC2$CC_2)
  nSpec2 <- unlist(lapply(all_maxSuits_split_CC2,nrow))
  nSpec3 <- unlist(lapply(all_maxSuits_split_CC3,nrow))
  regs_spec2 <- cbind.data.frame(names(all_maxSuits_split_CC2),as.integer(nSpec2))
  regs_spec3 <- cbind.data.frame(names(all_maxSuits_split_CC3),as.integer(nSpec3))
  colnames(regs_spec2) <- c("CC_2","potAnzahlNeobiota")
  colnames(regs_spec3) <- c("CC_3","potAnzahlNeobiota")
  # hist(unlist(nSpec),50)

  
  ## Plot maps ##########################################
  if (exportiereKarte){
    
    cat("\n Erstelle Karten...\n")
    
    ## Gemeinden (municipalities)
    regions_plot3 <- merge(regions3,regs_spec3,by="CC_3",all=T)
    regions_plot3$potAnzahlNeobiota[is.na(regions_plot3$potAnzahlNeobiota)] <- 0
    
    ## plot presence-absence (suit > 0.7) ##############
    if (!is.null(Artgruppe)){
      png(file.path("SDM","Grafiken",paste0("KarteDeutschland_PotNArten07_Gemeinden_",Artgruppe,identifier,".png")),unit="in",width=8,height=8,res=300)
    } else {
      png(file.path("SDM","Grafiken",paste0("KarteDeutschland_PotNArten07_Gemeinden",identifier,".png")),unit="in",width=8,height=8,res=300)
    }
    mf_choro(regions_plot3,var="potAnzahlNeobiota",leg_title="Pot. Anzahl Neobiota",border=NA,breaks="pretty",pal=rev(hcl.colors(12,pal="OrYel")))
    plot(st_geometry(germany_border),add=T,lwd=0.5)
    dev.off()

    
    ## Landkreise:
    
    regions_plot2 <- merge(regions2,regs_spec2,by="CC_2",all=T)
    regions_plot2$potAnzahlNeobiota[is.na(regions_plot2$potAnzahlNeobiota)] <- 0
    
    ## plot presence-absence (suit > 0.7) ##############
    if (!is.null(Artgruppe)){
      png(file.path("SDM","Grafiken",paste0("KarteDeutschland_PotNArten07_Kreise_",Artgruppe,identifier,".png")),unit="in",width=8,height=8,res=300)
    } else {
      png(file.path("SDM","Grafiken",paste0("KarteDeutschland_PotNArten07_Kreise",identifier,".png")),unit="in",width=8,height=8,res=300)
    }
    mf_choro(regions_plot2,var="potAnzahlNeobiota",leg_title="Pot. Anzahl Neobiota",border=NA,breaks="pretty",pal=rev(hcl.colors(12,pal="OrYel")))
    plot(st_geometry(germany_border),add=T,lwd=0.5)
    dev.off()
    
    
    
    ## plot all suitabilities summed up #########
    
    ## Gemeinden (municipalities)
    
    all_meanSuits_agg3 <- aggregate(HabitatEignung ~ CC_3, data=all_meanSuits_df_CC3,sum,na.rm=T)
    
    # regions$ID <- 1:nrow(regions)
    regions_plot3 <- merge(regions3,all_meanSuits_agg3,by="CC_3")
    
    if (!is.null(Artgruppe)){
      png(file.path("SDM","Grafiken", paste0("KarteDeutschland_HabitateignungSumme_Gemeinden_",Artgruppe,identifier,".png")),unit="in",width=8,height=8,res=300) # plot without occurrences
    } else {
      png(file.path("SDM","Grafiken", paste0("KarteDeutschland_HabitateignungSumme_Gemeinden",identifier,".png")),unit="in",width=8,height=8,res=300) # plot without occurrences
    }
    mf_choro(regions_plot3,var="HabitatEignung",leg_title="Habitateignung",border=NA,breaks="pretty",pal=rev(hcl.colors(12,pal="OrYel"))) #
    plot(st_geometry(germany_border),add=T,lwd=0.5)
    dev.off()
    
    
    ## Kreise:
    
    all_meanSuits_agg2 <- aggregate(HabitatEignung ~ CC_2, data=all_meanSuits_df_CC2,function(x) sum(x,na.rm=T))
    
    # regions$ID <- 1:nrow(regions)
    regions_plot2 <- merge(regions2,all_meanSuits_agg2,by="CC_2")
    
    if (!is.null(Artgruppe)){
      png(file.path("SDM","Grafiken", paste0("KarteDeutschland_HabitateignungSumme_Kreise_",Artgruppe,identifier,".png")),unit="in",width=8,height=8,res=300) # plot without occurrences
    } else {
      png(file.path("SDM","Grafiken", paste0("KarteDeutschland_HabitateignungSumme_Kreise",identifier,".png")),unit="in",width=8,height=8,res=300) # plot without occurrences
    }
    mf_choro(regions_plot2,var="HabitatEignung",leg_title="Habitateignung",border=NA,breaks="pretty",pal=rev(hcl.colors(12,pal="OrYel"))) #
    plot(st_geometry(germany_border),add=T,lwd=0.5)
    dev.off()
    
    
    ## plots ratster data #####################
    
    if (!is.null(Artgruppe)){
      png(file.path("SDM","Grafiken",paste0("KarteDeutschland_HabitateignungSumme_",Artgruppe,"_Raster",identifier,".png")),unit="in",width=8,height=8,res=300)
    } else {
      png(file.path("SDM","Grafiken",paste0("KarteDeutschland_HabitateignungSumme_Raster",identifier,".png")),unit="in",width=8,height=8,res=300)
    }
    plot(rasterData_all_masked,col=rev(hcl.colors(12,pal="OrYel")))
    plot(st_geometry(germany_border),add=T,lwd=0.5)
    # text(">",x=17.9,y=53.08,xpd=NA)
    dev.off()
    
    ## presence/absence ###########
    if (!is.null(Artgruppe)){
      png(file.path("SDM","Grafiken",paste0("KarteDeutschland_PotNArten07_",Artgruppe,"_Raster",identifier,".png")),unit="in",width=8,height=8,res=300)
    } else {
      png(file.path("SDM","Grafiken",paste0("KarteDeutschland_PotNArten07_Raster",identifier,".png")),unit="in",width=8,height=8,res=300)
    }
    plot(rasterData_max07_masked,col=rev(hcl.colors(12,pal="OrYel")))
    plot(st_geometry(germany_border),add=T,lwd=0.5)
    # text(">",x=17.9,y=53.08,xpd=NA)
    dev.off()

    cat("\n Karten erstellt und in SDM->Grafiken gespeichert.\n")
    
  }
}



