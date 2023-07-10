InitSpreadTerrestrial<-function(Terrestrial_netw_data,Commodities_shape_data,
                                Water_netw_data=Water_netw_data,
                           Pallets_netw_data,Container_netw_data,env_terrestrial,
                           file_init="init_data.Rdata",save_init=TRUE,netw_type=c("all"),
                           init_coords,max_dist,save_dir,
                           species_preferences,traffic_type=c("all"),
                           incl_containers=F,Cont_threshold=0,
                           incl_pallets=F,Pall_threshold=0,
                           incl_riverside=T){

  ### load and format shapefiles (takes a while!) ######################################################
  tmp2 <- proc.time()

  ####################################################################
  ### load road and railway network file #############################

  cat("\n Loading network \n")

  roads_shp <- Terrestrial_netw_data

  if ("Env_suit"%in%colnames(roads_shp)) {
    colnames(roads_shp)[1:8] <- c("FromNode","ToNode","Type","Length","cargo","passengers", "ID", "Env_suit")
  } else {
    colnames(roads_shp)[1:7] <- c("FromNode","ToNode","Type","Length","cargo","passengers", "ID")
  }
  roads_shp$FromNode <- as.character(roads_shp$FromNode)
  roads_shp$ToNode   <- as.character(roads_shp$ToNode)
  roads_shp$Type     <- as.character(roads_shp$Type)
  roads_shp$ID       <- as.character(roads_shp$ID)

  if (all(netw_type!=c("all"))){
    roads_shp <- roads_shp[roads_shp$Type%in%netw_type,]
  }

  road_netw <- as.data.table(st_drop_geometry(roads_shp))
  road_netw[,Order:=c(1:nrow(roads_shp))]

  roads_shp$Length <- NULL # column will be added in modified form at the end below

  ## subsetting road/railway network according to traffic_type
  suppressWarnings(
    if (all(traffic_type!=c("all"))) {
      colTraffic<-which(colnames(road_netw)%in%traffic_type)
      road_netw[,Traffic:=rowSums(road_netw[, ..colTraffic])]
    } else {
      road_netw[,Traffic:=rowSums(cbind(cargo,passengers))]
    }
  )
  road_netw[,Traffic:=round((Traffic)*365/12,0)]
  set(road_netw, j=which(colnames(road_netw) %in% c("cargo","passengers")), value=NULL )

  if (any(road_netw[,Length==0])){ #assign length of 10m to segments with length 0
    options(warn=1)
    road_netw[Length==0,Length:=0.01]
    warning("Links of length 0 detected in Terrestrial_netw_data. Their length has been set to 10m. ")
    options(warn=0)
  }

  ## set initial values for spread
  road_netw[,newarrivals:=0]
  road_netw[,newarrivals:=as.numeric(newarrivals)]
  road_netw[,stateFromNode:=0]
  road_netw[,stateToNode:=0]


  ###############################################################
  ## Identify starting nodes ####################################

  cat("\n Initializing node states \n")

  ## Identifying initial invasion segments
  init_segm <- getNeighbourSegmCoord(shapeObj=roads_shp,init_coords=init_coords,max_dist=max_dist)

  ## Assigning initial state=1 to starting nodes ###################
  init_nodes <- road_netw[ID%in%init_segm,c(FromNode,ToNode)] # new
  road_netw[FromNode%in%init_nodes,stateFromNode:=1]
  road_netw[ToNode%in%init_nodes,stateToNode:=1]


  ####################################################################
  ## identify nodes in each cargo area if commodities are considered

  if  (incl_containers==TRUE | incl_pallets==TRUE) {

    cat("\n Initializing trade regions \n")

    CargoAreas<-Commodities_shape_data
    CargoAreas$ArCntnr <- as.character(CargoAreas$ArCntnr)
    CargoAreas$ArePllt <- as.character(CargoAreas$ArePllt)

    NodesCoords<-getNodesCoord(roads_shp)
    NodesCoords <- st_as_sf(NodesCoords, coords = c("Long","Lat"))
    st_crs(NodesCoords) <- st_crs(roads_shp)

    CargoAreas <- st_make_valid(CargoAreas)
    Nodes_CargoCell <- st_join(NodesCoords,CargoAreas)
    Nodes_CargoCell <- as.data.table(st_drop_geometry(Nodes_CargoCell))
    # Nodes_CargoCell <- as.data.table(cbind(Nodes_CargoCell,NodesCoords@data$nodeID,stringsAsFactors=F))
    setnames(Nodes_CargoCell,c("NodeID",names(Nodes_CargoCell)[2:3]))

    ## subsetting nodes according to subset of road_netw
    Nodes_CargoCell <- Nodes_CargoCell[NodeID%in%road_netw$ToNode]
  }

  ####################################################################
  ## identify Areas initialy invaded (for pallets only) from init_coords

  if (incl_pallets==TRUE){
    init_coords2<-init_coords
    init_coords2 <- st_as_sf(init_coords2, coords = c("Long","Lat"))
    st_crs(init_coords2) <- st_crs(roads_shp)
    # coordinates(init_coords2)<- ~ Long+Lat
    # proj4string(init_coords2)<-proj4string(CargoAreas)
    init_Areas <- st_join(init_coords2,CargoAreas)$ArePllt
  }

  ###############################################################
  ## Pallets flows ##############################################

  if  (incl_pallets==TRUE) {

    cat("\n Initializing pallets flows \n")

    #load pallet flow
    Pallets_netw<-as.data.table(Pallets_netw_data)#need to update to provide external file here. Perhaps also in network file?
    Pallets_netw$ToArea <- as.character(Pallets_netw$ToArea)
    Pallets_netw$FromArea <- as.character(Pallets_netw$FromArea)
    Pallets_netw <- Pallets_netw[FromArea!=ToArea,] #remove traffic from/to same area
    Pallets_netw <- Pallets_netw[ToArea%in%Nodes_CargoCell$ArePllt,] #subset, keep only areas where there are traffic nodes of the chosen netw_type

    # remove links with number of exchanged pallets per year < than Pall_threshold:
    if (Pall_threshold>0) {
      Pallets_netw<-Pallets_netw[numPallets>=Pall_threshold,]
    }

    Pallets_netw[,numPallets:=numPallets/12] #monthly scale
    #assign Area LinkIDs
    Pallets_netw[,AreaLinkID:=paste(FromArea,ToArea,sep="_")]

    # initialize state of Cargo Areas
    Pallets_netw[,stateFromArea:=0]
    Pallets_netw[,stateToArea:=0]
    Pallets_netw[,newarrivals:=0]

    # Update initial state of Cargo Areas
    Pallets_netw[FromArea%in%init_Areas,stateFromArea:=1]
    Pallets_netw[ToArea%in%init_Areas,stateToArea:=1]
  }

  ###############################################################
  ## Container flows ############################################

  if  (incl_containers==TRUE) {

    cat("\n Initializing containers flow \n")

    ## container data
    Container_netw<-as.data.table(Container_netw_data) #need to update to provide external file here. Perhaps also in network file?
    Container_netw$ToArea <- as.character(Container_netw$ToArea)
    Container_netw$FromArea <- as.character(Container_netw$FromArea)
    Container_netw<-Container_netw[FromArea!=ToArea,] #remove traffic from/to same area
    Container_netw<-Container_netw[ToArea%in%Nodes_CargoCell$ArCntnr,] #subset, keep only areas where there are traffic nodes of the chosen netw_type

    # remove areas with number of arriving containers per year < than Cont_threshold:
    if (Cont_threshold>0) {
      Container_netw <- Container_netw[numContainers>=Cont_threshold,]
    }

    Container_netw[,numContainers:=numContainers/12] #monthly scale
    Container_netw <- as.data.table(aggregate(numContainers ~ ToArea, Container_netw, sum))
    Nodes_ContCell <- Nodes_CargoCell[,c(1,3)]
    colnames(Nodes_ContCell) <- c("FromNode","ToArea")

    setkey(Container_netw,ToArea)
    setkey(Nodes_ContCell,ToArea)
    Container_netw <- merge(Container_netw,Nodes_ContCell,by="ToArea")

    #divide number of container per number of nodes in each area (assumes each nodes gets same number of containers)
    NodesPerArea <- as.data.table(table(Container_netw$ToArea))
    colnames(NodesPerArea) <- c("ToArea","numNodes")
    setkey(NodesPerArea,ToArea)
    Container_netw <- merge(Container_netw,NodesPerArea,by="ToArea")
    Container_netw[,numContainers:=numContainers/numNodes]
    set(Container_netw, j=which(colnames(Container_netw) %in% c("ToArea","numNodes")), value=NULL )
  }
  # setkey(road_netw,FromNode)

  ###############################################################
  ## Water ways #################################################

  if  (incl_riverside==TRUE) {

    cat("\n Initializing riverside \n")

    water_shp <- Water_netw_data
    water_netw <- as.data.table(st_drop_geometry(water_shp))

    water_netw$FromNode<-as.character(water_netw$FromNode)
    water_netw$ToNode<-as.character(water_netw$ToNode)
    water_netw$ID<-as.character(water_netw$ID)

    water_netw[,Order:=c(1:nrow(water_netw))]

    if (any(water_netw[,Length==0])){ #assign length of 10m to segments with length 0
      options(warn=1)
      water_netw[Length==0,Length:=0.01]
      warning("Links of length 0 detected in Water_netw_data. Their length has been set to 10m. ")
      options(warn=0)
    }

    ## Initializing node states ################################

    water_netw[,newarrivals:=0]
    water_netw[,newarrivals:=as.numeric(newarrivals)]
    water_netw[,stateFromNode:=0]
    water_netw[,stateToNode:=0]

    init_segm_water <- getNeighbourSegmCoord(shapeObj=water_shp,init_coords=init_coords,max_dist=max_dist)

    ## Identifying initial invasion segments ###################

    #Assigning initial state=1 to initially invaded nodes.
    init_nodes <- water_netw[ID%in%init_segm_water,c(FromNode,ToNode)] # new
    water_netw[FromNode%in%init_nodes,stateFromNode:=1]
    water_netw[ToNode%in%init_nodes,stateToNode:=1]

    ## Calculate suitability of terrestrial habitats #############
    if ("Env_suit"%in%colnames(water_netw)) {

      # cat("\n Suitability of habitats provided in network file \n")

    } else {

      # cat("\n Calculating suitability of habitats \n")

      water_netw[,Env_suit:=0.5]
      # LCdata<-as.data.table(env_terrestrial)
      # setkey(LCdata,LC_cat_ID)
      # setkey(species_preferences,LC_cat_ID)
      # LCdata<-LCdata[species_preferences]
      #
      # ### assign new land cover categories and species preferences
      # LCdata$LCprop <- LCdata$Proportion * LCdata$Species_preferences
      #
      # ## calculate suitability of habitats for each segment
      # LCdata <- as.data.table(LCdata)
      # road_segm_suit <- LCdata[,sum(LCprop),by=list(ID)]
      # road_segm_suit[V1>1,V1:=1]
      #
      # ## merge land cover suitability and road_netw
      # colnames(road_segm_suit) <- c("ID","Env_suit")
      #
      # setkey(road_segm_suit,ID)
      # setkey(road_netw,ID)
      # road_netw <- road_segm_suit[road_netw]
    }

    setkey(water_netw,Order)
    # water_shp@data<-water_netw
    water_shp <- merge(water_shp,water_netw[,c( "ID","newarrivals","stateFromNode","stateToNode","Env_suit")],by="ID")
  }

  ###############################################################
  ## Calculate suitability of terrestrial habitats #############
  if ("Env_suit"%in%colnames(road_netw)) {

    cat("\n Suitability of habitats provided in network file \n")

  } else {

    cat("\n Calculating suitability of habitats \n")

    LCdata<-as.data.table(env_terrestrial)
    setkey(LCdata,LC_cat_ID)
    setkey(species_preferences,LC_cat_ID)
    LCdata<-LCdata[species_preferences]

    ### assign new land cover categories and species preferences
    LCdata$LCprop <- LCdata$Proportion * LCdata$Species_preferences

    ## calculate suitability of habitats for each segment
    LCdata <- as.data.table(LCdata)
    road_segm_suit <- LCdata[,sum(LCprop),by=list(ID)]
    road_segm_suit[V1>1,V1:=1]

    ## merge land cover suitability and road_netw
    colnames(road_segm_suit) <- c("ID","Env_suit")

    setkey(road_segm_suit,ID)
    setkey(road_netw,ID)
    road_netw <- road_segm_suit[road_netw]
  }

  ###########################################################
  ## Combine all relevant data files ########################

  cat("\n Assembling initialization object \n")

  setkey(road_netw,Order)

  roads_shp <- merge(roads_shp,road_netw[,c( "ID","Length","Env_suit","Order","Traffic","newarrivals","stateFromNode","stateToNode")],by="ID")

  if (incl_pallets==FALSE & incl_containers==FALSE & incl_riverside==FALSE){
    init_obj <- list(roads_shp,init_segm)
    names(init_obj)<-c("roads_shp","init_segm")
  } else if (incl_pallets==FALSE & incl_containers==TRUE){
    init_obj <- list(roads_shp,init_segm,Nodes_CargoCell,Container_netw)
    names(init_obj)<-c("roads_shp","init_segm","Nodes_CargoCell","Container_netw")
  } else if (incl_pallets==TRUE & incl_containers==FALSE){
    init_obj <- list(roads_shp,init_segm,Nodes_CargoCell,Pallets_netw,init_Areas)
    names(init_obj)<-c("roads_shp","init_segm","Nodes_CargoCell","Pallets_netw","init_Areas")
  } else if (incl_pallets==TRUE & incl_containers==TRUE){
    init_obj <- list(roads_shp,init_segm,Nodes_CargoCell,Container_netw,Pallets_netw,init_Areas)
    names(init_obj)<-c("roads_shp","init_segm","Nodes_CargoCell","Container_netw","Pallets_netw","init_Areas")
  } else if (incl_riverside==TRUE){
    init_obj <- list(roads_shp,init_segm,water_shp,init_segm_water)
    names(init_obj)<-c("roads_shp","init_segm","water_shp","init_segm_water")
  }


  if (save_init) {
    cat("\n Saving initialization object \n")
    save(init_obj, file = file.path(save_dir,file_init))
  }
  print(proc.time() - tmp2)

  return(init_obj)
}
