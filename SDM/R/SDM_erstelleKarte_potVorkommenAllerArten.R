################################################################################
#
# Skript ist Teil des Workflows "SDM" zur Habitatmodellierung von Neobiota in 
# Deutschland. Es wird mit dem Skript run_SDM_workflow.R aufgerufen.
#
# Dieses Skript liest alle Dateien mit SDM Vorhersagen eines SDM Durchlaufs 
# ("identifier") ein und erstellt verschiedene Karten mit allen Arten akkumuliert.
#
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 08.12.25
################################################################################



erstelleKarte_potVorkommenAlle <- function(VorhersageVerzeichnis=VorhersageVerzeichnis,
                                           identifier=identifier,
                                           Name_Artenliste=Name_Artenliste,
                                           Ausschnitt=Ausschnitt_Extrapolation,
                                           exportiereKarte=TRUE,
                                           max_nTaxa_raster=100,
                                           max_nTaxa_gemeinden=500,
                                           max_nTaxa_kreise=400,
                                           Artgruppe=NULL){

  cat("\n Integriere potentielles Vorkommen aller Arten.\n\n")
  
  ## Load data #################################################################
  
  ## load maps of Germany
  regions3 <- st_read(dsn=file.path("SDM","Daten","Input","Shapefiles"),layer="gadm41_DEU_3", quiet=TRUE)

  regions2 <- st_read(dsn=file.path("SDM","Daten","Input","Shapefiles"),layer="gadm41_DEU_2", quiet=TRUE)
  regions2$CC_2[regions2$NAME_2=="Bodensee"] <- "001" # missing
  germany_border <- st_read(dsn=file.path("SDM","Daten","Input","Shapefiles"),layer="gadm41_DEU_1",quiet=T)

  ## read all files of occurrences
  all_files <- list.files(VorhersageVerzeichnis)
  all_files <- all_files[grep(identifier,all_files)]
  all_files <- all_files[grep("HabitatEignung_",all_files)]
  all_files <- all_files[grep("\\.gz",all_files)]
  
  if (length(all_files)==0){
    cat("\n ERROR: Keine Dateien gefunden. Kontrolliere Verzeichnispfad 'VorhersageVerzeichnis' und Vorhersagedateien darin.")
  }
  
  if (!is.null(Artgruppe)){
    
    neobiota <- read.xlsx(file.path("SDM","Daten","Input",Name_Artenliste),sheet=1)
    Arten_Gruppe <- subset(neobiota,ArtGruppe==Artgruppe)$Taxon
    
    all_files <- all_files[grepl(paste(Arten_Gruppe,collapse="|"),all_files)]
  }
  
  ## loop over all available predictor files #######################
  
  ## template for collecting records
  ext_stack <- ext(c(Ausschnitt[1],Ausschnitt[3],Ausschnitt[2],Ausschnitt[4]))
  all_rasters <- new_raster <- rast(ncols=150,nrows=150,extent=ext_stack) #
  values(all_rasters) <- 0
  values(new_raster) <- 0
  
  #setup progress bar
  n_species <- length(all_files)
  pb <- txtProgressBar(min=0, max=n_species, initial=0,style = 3)
  
  all_TaxonName <- all_meanSuits_CC2 <- all_meanSuits_CC3 <- all_maxSuits_CC2 <- all_maxSuits_CC3 <- list()
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

    
    ### Raster data ###########################
    
    ## transform suitability >0.7 into presence/absence
    raster_aliens <- terra::rasterize(x=as.matrix(meanSuit_coords[,c("x","y")]), y=rasterData_max07, values=meanSuit_coords$HabitatEignung_mittel)
    values(raster_aliens)[values(raster_aliens)>0.7] <- 1
    values(raster_aliens)[values(raster_aliens)<0.7] <- 0
    rasterData_max07 <- rasterData_max07 + raster_aliens # count number of species with suitable habitats
    
    ## sum all suitability values across all species
    raster_aliens <- terra::rasterize(as.matrix(meanSuit_coords[,c("x","y")]), rasterData_all, values=meanSuit_coords$HabitatEignung_mittel)
    rasterData_all <- rasterData_all + raster_aliens # sum suitability values
    
    
    #### Polyon data #######################
    
    ## extract mean values per polygon
    meanSuit_rast <- terra::rast(meanSuit_coords,type="xyz")
    meanSuits_regs2 <- terra::extract(meanSuit_rast,regions2,fun=mean,na.rm=T)
    meanSuits_regs2$CC_2 <- regions2$CC_2
    meanSuits_regs2 <- meanSuits_regs2[,c("CC_2","HabitatEignung_mittel")]
    meanSuits_regs3 <- terra::extract(meanSuit_rast,regions3,fun=mean,na.rm=T)
    meanSuits_regs3$CC_3 <- regions3$CC_3
    meanSuits_regs3 <- meanSuits_regs3[,c("CC_3","HabitatEignung_mittel")]

    ## prepare output polygon data
    meanSuits_regs2$HabitatEignung_mittel <- round(meanSuits_regs2$HabitatEignung_mittel,3)
    meanSuits_regs3$HabitatEignung_mittel <- round(meanSuits_regs3$HabitatEignung_mittel,3)
    meanSuits_regs2$Taxon <- TaxonName
    meanSuits_regs3$Taxon <- TaxonName
    colnames(meanSuits_regs2) <- c("CC_2","HabitatEignung","Taxon")
    colnames(meanSuits_regs3) <- c("CC_3","HabitatEignung","Taxon")
    
    ## output: all available input files
    all_TaxonName[[i]] <- strsplit(all_files[i],"_")[[1]][[2]]
    
    ## output: mean suitability per polygon
    all_meanSuits_CC2[[i]] <- meanSuits_regs2
    all_meanSuits_CC3[[i]] <- meanSuits_regs3
    
    # update progress bar
    try(info <- sprintf("%d%% done", round((i/n_species)*100, 2)), silent = TRUE)
    setTxtProgressBar(pb, i, label = info)
    
  }
  close(pb)
  
  all_meanSuits_df_CC2 <- do.call("rbind",all_meanSuits_CC2)
  all_meanSuits_df_CC3 <- do.call("rbind",all_meanSuits_CC3)
  
  all_meanSuits_df_CC2 <- all_meanSuits_df_CC2[all_meanSuits_df_CC2$CC_2!="NA",]
  all_meanSuits_df_CC3 <- all_meanSuits_df_CC3[all_meanSuits_df_CC3$CC_3!="NA",]
  
  ## crop raster to German borders
  terra::crs(rasterData_all) <- "epsg:4326"
  terra::crs(rasterData_max07) <- "epsg:4326"
  rasterData_all_masked <- terra::mask(rasterData_all,germany_border)
  rasterData_max07_masked <- terra::mask(rasterData_max07,germany_border)
  
  
  ## output ############################################################
  all_meanSuits_df_CC2 <- all_meanSuits_df_CC2[order(all_meanSuits_df_CC2$CC_2,all_meanSuits_df_CC2$HabitatEignung,decreasing = T),]
  all_meanSuits_df_CC3 <- all_meanSuits_df_CC3[order(all_meanSuits_df_CC3$CC_3,all_meanSuits_df_CC3$HabitatEignung,decreasing = T),]
  
  if (!is.null(Artgruppe)){
    
    fwrite(all_meanSuits_df_CC2,file.path("SDM","Daten","Output", paste0("potVorkommen_",Artgruppe,"_EignungSumme_Kreise",identifier,".gz")))
    
    fwrite(all_meanSuits_df_CC3,file.path("SDM","Daten","Output", paste0("potVorkommen_",Artgruppe,"_EignungSumme_Gemeinden",identifier,".gz")))
    
    terra::writeRaster(rasterData_all_masked,file.path("SDM","Daten","Output",paste0("potVorkommen_",Artgruppe,"_EignungSumme_raster",identifier,".tif")),overwrite=T,filetype="GTiff")
    terra::writeRaster(rasterData_max07_masked,file.path("SDM","Daten","Output",paste0("potVorkommen_",Artgruppe,"_Eignung07_raster",identifier,".tif")),overwrite=T,filetype="GTiff")
    
  } else {
    
    fwrite(all_meanSuits_df_CC2,file.path("SDM","Daten","Output", paste0("potVorkommen_alleArten_Kreise",identifier,".gz")))
    
    fwrite(all_meanSuits_df_CC3,file.path("SDM","Daten","Output", paste0("potVorkommen_alleArten_Gemeinden",identifier,".gz")))
    
    terra::writeRaster(rasterData_all_masked,file.path("SDM","Daten","Output",paste0("potVorkommen_EignungSumme_Raster",identifier,".tif")),overwrite=T,filetype="GTiff")
    terra::writeRaster(rasterData_max07_masked,file.path("SDM","Daten","Output",paste0("potVorkommen_Eignung07_Raster",identifier,".tif")),overwrite=T,filetype="GTiff")
  }

    
  ## get number of taxa with suitable habitats per region ##################################
  all_maxSuits_df_CC2 <- subset(all_meanSuits_df_CC2, HabitatEignung>0.5) # select suitable habitats
  all_maxSuits_df_CC3 <- subset(all_meanSuits_df_CC3, HabitatEignung>0.5)
  
  all_maxSuits_split_CC3 <- split(all_maxSuits_df_CC3, f=all_maxSuits_df_CC3$CC_3) # split by region
  all_maxSuits_split_CC2 <- split(all_maxSuits_df_CC2, f=all_maxSuits_df_CC2$CC_2)
  nSpec2 <- unlist(lapply(all_maxSuits_split_CC2,nrow)) # count number of species per region
  nSpec3 <- unlist(lapply(all_maxSuits_split_CC3,nrow))
  regs_spec2 <- cbind.data.frame(names(all_maxSuits_split_CC2),as.integer(nSpec2)) # build a data.frame
  regs_spec3 <- cbind.data.frame(names(all_maxSuits_split_CC3),as.integer(nSpec3))
  colnames(regs_spec2) <- c("CC_2","potAnzahlNeobiota")
  colnames(regs_spec3) <- c("CC_3","potAnzahlNeobiota")

  ## Plot maps ##########################################
  if (exportiereKarte){
    
    cat("\n Erstelle Karten...\n")
    
    ## Gemeinden (municipalities)
    regions_plot3 <- merge(regions3, regs_spec3, by="CC_3", all=T)
    regions_plot3$potAnzahlNeobiota[is.na(regions_plot3$potAnzahlNeobiota)] <- 0
    if (!is.null(max_nTaxa_gemeinden)){
      regions_plot3$potAnzahlNeobiota[regions_plot3$potAnzahlNeobiota>max_nTaxa_gemeinden] <- max_nTaxa_gemeinden
    }
    
    
    ## plot presence-absence (suit > 0.5) ##############
    if (!is.null(Artgruppe)){
      png(file.path("SDM","Grafiken",paste0("KarteDeutschland_PotNArten05_Gemeinden_",Artgruppe,identifier,".png")),unit="in",width=6,height=6,res=300)
    } else {
      png(file.path("SDM","Grafiken",paste0("KarteDeutschland_PotNArten05_Gemeinden",identifier,".png")),unit="in",width=6,height=6,res=300)
    }
    mf_choro(regions_plot3, var="potAnzahlNeobiota", leg_title="Pot. Anzahl Neobiota", 
             border=NA, breaks="pretty", pal="Mint") 
    plot(st_geometry(germany_border),add=T,lwd=0.5, xpd=T)
    dev.off()

    ## Landkreise:
    
    regions_plot2 <- merge(regions2,regs_spec2,by="CC_2",all=T)
    regions_plot2$potAnzahlNeobiota[is.na(regions_plot2$potAnzahlNeobiota)] <- 0
    if (!is.null(max_nTaxa_gemeinden)){
      regions_plot2$potAnzahlNeobiota[regions_plot2$potAnzahlNeobiota>max_nTaxa_kreise] <- max_nTaxa_kreise
    }
    
    ## plot presence-absence (suit > 0.7) ##############
    if (!is.null(Artgruppe)){
      png(file.path("SDM","Grafiken",paste0("KarteDeutschland_PotNArten05_Kreise_",Artgruppe,identifier,".png")),unit="in",width=6,height=6,res=300)
    } else {
      png(file.path("SDM","Grafiken",paste0("KarteDeutschland_PotNArten05_Kreise",identifier,".png")),unit="in",width=6,height=6,res=300)
    }
    mf_choro(regions_plot2, var="potAnzahlNeobiota", leg_title="Pot. Anzahl Neobiota", 
             border=NA, breaks="pretty", pal="Mint")
    plot(st_geometry(germany_border), add=T, lwd=0.5, xpd=T)
    dev.off()

    
    ## plot all suitabilities summed up #########
    
    ## Gemeinden (municipalities)
    
    all_meanSuits_agg3 <- aggregate(HabitatEignung ~ CC_3, data=all_meanSuits_df_CC3,sum,na.rm=T)
    
    regions_plot3 <- merge(regions3, all_meanSuits_agg3,by="CC_3")
    
    if (!is.null(Artgruppe)){
      png(file.path("SDM","Grafiken", paste0("KarteDeutschland_HabitateignungSumme_Gemeinden_",Artgruppe,identifier,".png")),unit="in",width=6,height=6,res=300) # plot without occurrences
    } else {
      png(file.path("SDM","Grafiken", paste0("KarteDeutschland_HabitateignungSumme_Gemeinden",identifier,".png")),unit="in",width=6,height=6,res=300) # plot without occurrences
    }
    mf_choro(regions_plot3, var="HabitatEignung", leg_title="Habitateignung", border=NA, breaks="pretty", pal="Mint") #
    plot(st_geometry(germany_border),add=T,lwd=0.5, xpd=T)
    dev.off()
    
    
    ## Kreise:
    
    all_meanSuits_agg2 <- aggregate(HabitatEignung ~ CC_2, data=all_meanSuits_df_CC2,function(x) sum(x,na.rm=T))
    
    # regions$ID <- 1:nrow(regions)
    regions_plot2 <- merge(regions2,all_meanSuits_agg2,by="CC_2")
    
    if (!is.null(Artgruppe)){
      png(file.path("SDM","Grafiken", paste0("KarteDeutschland_HabitateignungSumme_Kreise_",Artgruppe,identifier,".png")),unit="in",width=6,height=6,res=300) # plot without occurrences
    } else {
      png(file.path("SDM","Grafiken", paste0("KarteDeutschland_HabitateignungSumme_Kreise",identifier,".png")),unit="in",width=6,height=6,res=300) # plot without occurrences
    }
    mf_choro(regions_plot2,var="HabitatEignung",leg_title="Habitateignung",border=NA,breaks="pretty", pal="Mint") #OrYel
    plot(st_geometry(germany_border),add=T,lwd=0.5, xpd=T)
    dev.off()
    
    
    ## plots ratster data #####################
    
    if (!is.null(Artgruppe)){
      png(file.path("SDM","Grafiken",paste0("KarteDeutschland_HabitateignungSumme_",Artgruppe,"_Raster",identifier,".png")),unit="in",width=6,height=6,res=300)
    } else {
      png(file.path("SDM","Grafiken",paste0("KarteDeutschland_HabitateignungSumme_Raster",identifier,".png")),unit="in",width=6,height=6,res=300)
    }
    plot(rasterData_all_masked, col=rev(hcl.colors(12, pal="Mint")))
    plot(st_geometry(germany_border), add=T, lwd=0.5)
    # text(">",x=17.9,y=53.08,xpd=NA)
    dev.off()
    
    ## presence/absence ###########
    if (!is.null(Artgruppe)){
      png(file.path("SDM","Grafiken",paste0("KarteDeutschland_PotNArten07_",Artgruppe,"_Raster",identifier,".png")),unit="in",width=8,height=8,res=300)
    } else {
      png(file.path("SDM","Grafiken",paste0("KarteDeutschland_PotNArten07_Raster",identifier,".png")),unit="in",width=8,height=8,res=300)
    }
    plot(rasterData_max07_masked, col=rev(hcl.colors(12, pal="Mint")))
    plot(st_geometry(germany_border), add=T, lwd=0.5)
    # text(">",x=17.9,y=53.08,xpd=NA)
    dev.off()

    cat("\n Karten erstellt und in SDM->Grafiken gespeichert.\n")
    
  }
}



