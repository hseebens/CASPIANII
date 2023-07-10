#needs matrix of parameters. should call parameters by name (via colnames(matrix)).
#  Needs parallelization already? If so, folder structure needed to save results of each run separately.
# parallelization not needed yet for parameters, might be useful for networks at a later stage.

#ARGUMENTS:
# parameters: model parameter values. Must be provided in the same order
#   and same form shown in InitializationScript.R. Column names and order matter!
# internal_dataset: wheter to use the dataset internally provided with traffic data. Default TRUE.
# dir_data:if internal_dataset=FALSE, the folder where to look for traffic data. Otherwise ignored.
# netw_data: if internal_dataset=FALSE, the layer of shapefile to be imported. Otherwise ignored.
# Rdata_file: if internal_dataset=FALSE, .Rdata file with the traffic network. Otherwise ignored,
#   alternative to netw_data.
# initialize: Whether the model should be initialized. Default TRUE.
# save_init: if initialize=TRUE, should the initialization file be saved? Default TRUE.
# file_init: if initialize=TRUE, the name of the file to be created by InitializeSpread().
#   in the newly created folder (default  "init_data.rData" if save_init=TRUE). If initialize=FALSE, the FULL path
#   of the file to be read in (created by InitializeSpread() or ModelSpread() ). MUST BE an .Rdata file.
# init_coords: data.table object, with coordinates of invasion starting points and at which point
#   during the simulation they should be considered (set to 0 for consideration from the very beginning).
#   See InitializationScript.R for how to create it.
# num_iter: number of model iterations
# incl_attachment: if attachment to vehicles should be considered. Default TRUE.
# incl_airflow: if vehicle airstream should be considered. Default TRUE.
# incl_natural: if natural dispersal should be considered. Default TRUE.
# LandCoverID: IDs of Suitable Land Cover. See databases LClegend and LClist.
# max_dist: maximum distance (m) from initial coordinates for a segment to be considered infected.
# makeplot: should model results be plotted as maps (could be a long process)? Default FALSE.
# save_plot: logical. If TRUE, plots are created in the newly created folder as .png files.
#   If FALSE, an x11() device is opened. Only considered if makeplot=TRUE. if
# iter_save: which model iterations should be saved? By default=num_iter. If makeplot=TRUE, several maps
#   will be created, one for each saved iteration.
# export_results: Should results of the last iteration be exported in the newly created folder as a csv file?
#   Default FALSE.
# netw_type: the types of roads to be considered in the simulation. Default c("all").


CASPIANSpreadTerrestrial <- function(parameters,
                                     init_obj,
                                     # Terrestrial_netw_data,
                                     # water_shp,
                                     # Commodities_shape_data,
                                     # Pallets_netw_data,
                                     # Container_netw_data,init_coords,
                                     num_iter,
                                     incl_attachment=T,
                                     incl_airflow=T,
                                     incl_natural=T,
                                     incl_containers=T,
                                     # Cont_threshold=0,
                                     incl_pallets=T,
                                     # Pall_threshold=0,
                                     incl_riverside=T,
                                     # max_dist,
                                     iter_save=num_iter,
                                     plot_funct_rel=FALSE,
                                     export_results=F
                                     # netw_type=c("all"),
                                     # traffic_type=c("all")
                                     ){


  ####################################################################

  for(i in 1:length(init_obj)){
    assign(names(init_obj)[i], init_obj[[i]])
  }

  road_netw <- as.data.table(st_drop_geometry(init_obj[["roads_shp"]]))

  if (incl_riverside){
    water_netw <- as.data.table(st_drop_geometry(init_obj[["water_shp"]]))
  }

  ####################################################################
  ## Calculate probabilities for individual pathways

  for (nparset in 1:nrow(parameters)){ ## Loop over parameter settings

    ## plot functional relationship of probability
    if (plot_funct_rel==TRUE){
      x11(width=8,height=5)
      op <- par(mar=c(5,5,2.5,2.5),mfrow=c(2,3))
    }

    ## Calculate Pintro for container flows #####################
    if (incl_containers==TRUE){

      cat("\n Calculating Probability of introduction by container for each node \n")

      Container_netw[,Pi_container:=f_container(numContainers,parameters[nparset,"cont1"])]

      ## plot functional relationship of probability
      if (plot_funct_rel==TRUE){
        prob_varx <- seq(min(Container_netw$numContainers),max(Container_netw$numContainers),length.out=1000)
        plot(prob_varx,f_container(prob_varx,parameters[nparset,"cont1"]),main="Container dispersal",xlab="Number of containers",ylab="Probability")
      }
      Container_netw[,numContainers:=NULL]

      names(Container_netw)[1]<-c("ToNode")
      setkey(Container_netw,ToNode)
      setkey(road_netw,ToNode)
      road_netw<-merge(road_netw,Container_netw,by="ToNode",all.x=T) # adding container state of ToNode
      road_netw$ToNode <- as.character(road_netw$ToNode)
      # road_netw[,stateToNode:=Pi_container]

      #Filling NAs (some nodes are only from_nodes or to_nodes, or are not in the Cargo Areas)
      road_netw[is.na(Pi_container),Pi_container:=0]

      ## ERROR check for Pi_container
      if (is.numeric(road_netw[,Pi_container])==FALSE |
          any(road_netw[,Pi_container<0]) | any(road_netw[,Pi_container>1])) {
        assign(x = "road_netw",value = road_netw,envir = .GlobalEnv)
        stop ("Problem in probability calculations: Pi_container either non-numeric or not in 0:1 range")
      }
    } else {
      road_netw[,Pi_container:=0]
    }

    ## Calculate Pintro for pallet flows #####################
    if(incl_pallets==TRUE){

      cat("\n Calculating Probability of Introduction for each Cargo area \n")

      #Assigning probability to Cargo links for Pallets,
      #using same kernel than containers with different parameterization:
      Pallets_netw[,Pi_pallet:=f_container(numPallets,parameters[nparset,"pall1"])]
      Pallets_netw[is.na(Pi_pallet),Pi_pallet:=0]

      ## plot functional relationship of probability
      if (plot_funct_rel==TRUE){
        prob_varx <- seq(min(Pallets_netw$numPallets),max(Pallets_netw$numPallets),length.out=1000)
        plot(prob_varx,f_container(prob_varx,parameters[nparset,"pall1"]),main="Pallets dispersal",xlab="Number of pallets",ylab="Probability")
      }

      ## ERROR check for pi_Pallet
      if (is.numeric(Pallets_netw[,Pi_pallet])==FALSE |
          any(Pallets_netw[,Pi_pallet<0]) | any(Pallets_netw[,Pi_pallet>1])) {
        assign(x = "Pallets_netw",value = Pallets_netw,envir = .GlobalEnv)
        stop ("Problem in probability calculations: Pi_pallet either non-numeric or not in 0:1 range")
      }

      ## assign new Pintro for pallets to road/railway network
      setnames(Nodes_CargoCell,c("AreaContainer","ToArea","ToNode"))
      road_netw <- merge(road_netw,Nodes_CargoCell,by="ToNode",all.x=T)
    }

    ##################################################################
    # calculate Pintro (probability of introduction) for each network link

    cat("\n Calculating Probability of Introduction for each segment \n")

    ## natural dispersal ########
    if (incl_natural==TRUE){

      road_netw[,p_natural:=f_natural(Length,parameters[nparset,"nat1"],parameters[nparset,"nat2"] )]

      ## plot functional relationship of probability
      if (plot_funct_rel==TRUE){
        prob_varx <- seq(min(road_netw$Length),max(road_netw$Length),length.out=1000)
        plot(prob_varx,f_natural(prob_varx,parameters[nparset,"nat1"],parameters[nparset,"nat2"]),main="Natural dispersal",xlab="Length",ylab="Probability")
      }

      ## ERROR check for p_natural
      if (is.numeric(road_netw[,p_natural])==FALSE |
          any(road_netw[,p_natural<0]) | any(road_netw[,p_natural>1])) {
        assign(x = "road_netw",value = road_netw,envir = .GlobalEnv)
        stop ("Problem in probability calculations: Pi_container either non-numeric or not in 0:1 range")
      }
    } else {road_netw[,p_natural:= 0]}


    ## Calculate natural dispersal along riversides #####################
    if (incl_riverside==TRUE){

      water_netw[,p_riverside:=f_natural_riverside(Length,parameters[nparset,"nat_riverside1"],parameters[nparset,"nat_riverside2"])]

      # ## plot functional relationship of probability
      # if (plot_funct_rel==TRUE){
      #   prob_varx <- seq(min(water_netw$Length),max(water_netw$Length),length.out=1000)
      #   plot(prob_varx,f_natural_riverside(prob_varx,parameters[nparset,"nat1"],parameters[nparset,"nat2"]),main="Riverside dispersal",xlab="Length",ylab="Probability")
      # }
      #
      # ## ERROR check for f_natural_riverside
      # if (is.numeric(water_netw[,f_natural_riverside])==FALSE |
      #     any(water_netw[,f_natural_riverside<0]) | any(water_netw[,f_natural_riverside>1])) {
      #   assign(x = "water_netw",value = water_netw,envir = .GlobalEnv)
      #   stop ("Problem in probability calculations: Pi_container either non-numeric or not in 0:1 range")
      # }
      #

      ## ERROR check for f_natural_riverside
      if (is.numeric(water_netw[,p_riverside])==FALSE |
          any(water_netw[,p_riverside<0]) | any(water_netw[,p_riverside>1])) {
        assign(x = "water_netw",value = water_netw,envir = .GlobalEnv)
        stop ("Problem in probability calculations: f_natural_riverside either non-numeric or not in 0:1 range")
      }
    }

    ## attachment to vehicle ########
    if (incl_attachment==TRUE) {
      road_netw[,p_attach:=f_attach(Length,parameters[nparset,"att1"],parameters[nparset,"att2"],parameters[nparset,"att3"])]
      road_netw[grep("S",road_netw$ID),p_attach:= 1-(1-p_attach)^(Traffic*parameters[nparset,"S_att0"])]
      road_netw[grep("R",road_netw$ID),p_attach:= 1-(1-p_attach)^(Traffic*parameters[nparset,"R_att0"])]

      ## plot functional relationship of probability
      if (plot_funct_rel==TRUE){
        prob_varx <- seq(min(road_netw$Length),max(road_netw$Length),length.out=1000)
        plot(prob_varx,f_attach(prob_varx,parameters[nparset,"att1"],parameters[nparset,"att2"],parameters[nparset,"att3"]),
             main="Attachment",xlab="Length",ylab="Probability")
      }

      ## ERROR check for p_attach
      if (is.numeric(road_netw[,p_attach])==FALSE |
          any(road_netw[,p_attach<0]) | any(road_netw[,p_attach>1])) {
        assign(x = "road_netw",value = road_netw,envir = .GlobalEnv)
        stop ("Problem in probability calculations: p_attach either non-numeric or not in 0:1 range")
      }
    } else {road_netw[,p_attach:= 0]}

    ## airstream by motorvehicles and trains ########
    if (incl_airflow==TRUE) {
      road_netw[,p_airflow:=f_airflow(Length,parameters[nparset,"air2"],parameters[nparset,"air1"])] # parameters a and b are inverted on purpose, to accomodate an error in von der Lippe et al. 2013. See code for f_airflow
      road_netw[grep("S",road_netw$ID),p_airflow:= 1-(1-p_airflow)^(Traffic*parameters[nparset,"S_air0"])] # new
      road_netw[grep("R",road_netw$ID),p_airflow:= 1-(1-p_airflow)^(Traffic*parameters[nparset,"R_air0"])]

      ## plot functional relationship of probability
      if (plot_funct_rel==TRUE){
        prob_varx <- seq(min(road_netw$Length),max(road_netw$Length),length.out=1000)
        plot(prob_varx,f_airflow(prob_varx,parameters[nparset,"air2"],parameters[nparset,"air1"]),main="Airstream",xlab="Length",ylab="Probability")
        par(op)
      }

      ## ERROR check for p_airflow
      if (is.numeric(road_netw[,p_airflow])==FALSE |
          any(road_netw[,p_airflow<0]) | any(road_netw[,p_airflow>1])) {
        assign(x = "road_netw",value = road_netw,envir = .GlobalEnv)
        stop ("Problem in probability calculations: p_airflow either non-numeric or not in 0:1 range")
      }
    } else {road_netw[,p_airflow:= 0]}

    if (plot_funct_rel==TRUE){
      par(op)
    }

    road_netw[is.na(p_airflow),p_airflow:=0]
    road_netw[is.na(p_attach),p_attach:=0]
    road_netw[is.na(p_natural),p_natural:=0]

    if (incl_riverside==TRUE){
      water_netw[is.na(p_riverside),p_riverside:=0]
      water_netw[,Pe:=0.5] # still a random value
    }

    ## Calculation of Pintro (Pi_traffic) depending on pathways selection above ########
    road_netw[, Pi_traffic:=1-Reduce("*", 1-.SD), .SDcols=grep("p_",colnames(road_netw))] # new solution


    ##################################################################
    # calculate probability of establishment

    cat("\n Calculating Probability of Establishment for each segment \n")

    road_netw[,Pe:=parameters[nparset,"estT"]*Env_suit] # parameter for scaling down probability of establishment # new
    road_netw[is.na(Pe),Pe:=0]

    ## ERROR check for Pe
    if (is.numeric(road_netw[,Pe])==FALSE |
        any(road_netw[,Pe<0]) | any(road_netw[,Pe>1])) {
      assign(x = "road_netw",value = road_netw,envir = .GlobalEnv)
      stop ("Problem in probability calculations: Pe either non-numeric or not in 0:1 range")
    }

    # same for riverside
    if (incl_riverside==TRUE){

      water_netw[,Pe:=parameters[nparset,"estT"]*Env_suit] # parameter for scaling down probability of establishment # new
      water_netw[is.na(Pe),Pe:=0]

      ## ERROR check for Pe
      if (is.numeric(water_netw[,Pe])==FALSE |
          any(water_netw[,Pe<0]) | any(water_netw[,Pe>1])) {
        assign(x = "water_netw",value = water_netw,envir = .GlobalEnv)
        stop ("Problem in probability calculations: Pe either non-numeric or not in 0:1 range")
      }
    }


    ## set data.table key for road network (much faster)
    # And subset relevant information
    road_netw_details <- road_netw[,c("ID","Env_suit","Length","Traffic","p_natural","p_attach","p_airflow","Order")]
    set( road_netw, j=which(colnames(road_netw) %in% c("Env_suit","Length","Traffic","p_natural","p_attach","p_airflow","Order")), value=NULL ) # new
    setkey(road_netw,FromNode)

    # same for riverside
    if (incl_riverside==TRUE){
      water_netw_details <- water_netw[,c("ID","Env_suit","Length","p_riverside","Order")]
      set( water_netw, j=which(colnames(water_netw) %in% c("Env_suit","Length","Order")), value=NULL ) # new
      setkey(water_netw,FromNode)
    }

    ##################################################################
    ##### start simulation ###########################################
    ##################################################################

    cat("\n Ongoing Simulation \n")

    #setup progress bar
    pb <- txtProgressBar(title = "Simulation state", label = "0% done",min=0, max=num_iter, initial=0,style = 3)

    modelList<- list()
    for (it in 1:num_iter){#num_iter

      ################################################################
      ## Calculate spread for pallet flows between cargo regions

      if (incl_pallets==TRUE){

        ## Calculate link probabilities (Hanno) ######
        Pallets_netw[,newarrivals2:=newarrivals]
        ## Calculate Pintro for all links
        Pallets_netw[,newarrivals2:= 1-(stateFromArea * Pi_pallet ) ]
        ## Combine all Pintros to each ToNode
        Pallets_netw[,newarrivals2:= 1-prod(newarrivals2) ,by=ToArea]

        ## ERROR check for declining newarrivals
        if (any(Pallets_netw[,newarrivals2<newarrivals]) &
            any((Pallets_netw[newarrivals2<newarrivals,newarrivals-newarrivals2])>10^-15)) {
          assign(x = "Pallets_netw",value = Pallets_netw,envir = .GlobalEnv)
          stop ("Problem in commodities probability calculations: Decline in newarrivals")
        } else { Pallets_netw[,newarrivals:=newarrivals2] }
        Pallets_netw[,newarrivals2:=NULL]

        ## update nodes #####################
        ## Select unique ToNodes
        newstate_pall <- unique(Pallets_netw[,c("ToArea","stateToArea","newarrivals")]) # select single node (remove duplicates)
        ## update stateToAreas
        newstate_pall[,stateToArea2:=1-((1-stateToArea) * (1-newarrivals))] # update ToAreas with old and new state

        ## ERROR check for duplicated ToNodes (multiple probabilities assigned to the same node)
        if (any(duplicated(newstate_pall$ToArea))) {
          assign(x = "newstate_pall",value = newstate_pall,envir = .GlobalEnv)
          stop ("Problem in node probability calculations: ToNode(s) with multiple probabilities")
        }

        ## ERROR check for declining stateToArea #
        if (any(newstate_pall[,stateToArea2<stateToArea]) &
            any((newstate_pall[stateToArea2<stateToArea,stateToArea-stateToArea2])>10^-15)) {
          assign(x = "newstate_pall",value = newstate_pall,envir = .GlobalEnv)
          stop ("Problem in commodities probability calculations: Decline in stateToArea")
        } else { newstate_pall[,stateToArea:=stateToArea2]}
        newstate_pall[,c("newarrivals","stateToArea2"):=NULL]

        ## Merge new state nodes and network ####################
        ## Update stateToArea
        setnames(newstate_pall,c("ToArea","newstate"))# prepare file for merge (set names and key)
        ToArea_index <- match(Pallets_netw$ToArea,newstate_pall$ToArea)
        Pallets_netw[,stateToArea:=newstate_pall[ToArea_index,newstate]]

        ## ERROR check for wrong column merging
        if (!identical(Pallets_netw[,ToArea],newstate_pall[ToArea_index,ToArea])) {
          assign(x = "newstate_road",value = newstate_road,envir = .GlobalEnv)
          stop ("ERROR: Road network not sorted as ToArea file!")
        }

        ## Update stateFromArea
        setnames(newstate_pall,c("FromArea","newstate"))# prepare file for merge (set names and key)
        FromArea_index <- match(Pallets_netw$FromArea,newstate_pall$FromArea)
        noNA_index <- !is.na(FromArea_index) # identify ToAreas not in FromAreas
        Pallets_netw[noNA_index,newstate:=newstate_pall[FromArea_index[noNA_index],newstate]]
        Pallets_netw[is.na(newstate),newstate:=0]

        ## ERROR check for wrong column merging
        if (!identical(Pallets_netw[noNA_index,FromArea],newstate_pall[FromArea_index[noNA_index],FromArea])) {
          assign(x = "newstate_pall",value = newstate_pall,envir = .GlobalEnv)
          stop ("ERROR: Road network not sorted as FromArea file!")
        }

        ## ERROR check for declining stateFromArea
        if (any(Pallets_netw[newstate>0,newstate<stateFromArea]) &
            any((Pallets_netw[newstate>0 & newstate<stateFromArea,stateFromArea-newstate])>10^-15) ) {
          assign(x = "Pallets_netw",value = Pallets_netw,envir = .GlobalEnv)
          stop ("Problem in commodities probability calculations: Decline in stateFromArea")
        }

        Pallets_netw[newstate>0 ,stateFromArea:=newstate] # assigne new states to FromAreas
        Pallets_netw[,newstate:=NULL] # remove column to avoid columns with the same names

        #######################################################################################
        ### Assign pallet probability (state of node) to each nodes in each area, and update traffic network here:

        ## update stateToAreas in road network #################################
        PalletsToNetw <- unique(Pallets_netw[,c("ToArea","stateToArea")])

        ToArea_index <- match(road_netw$ToArea,PalletsToNetw$ToArea)
        noNA_index <- !is.na(ToArea_index) # identify cargo areas not in pallets network
        road_netw[noNA_index,stateToArea:=PalletsToNetw[ToArea_index[noNA_index],stateToArea]] # assign new state
        road_netw[is.na(stateToArea),stateToArea:=0]

        ## ERROR check for wrong column merging
        if (!identical(road_netw[noNA_index,ToArea],PalletsToNetw[ToArea_index[noNA_index],ToArea])) {
          assign(x = "road_netw",value = road_netw,envir = .GlobalEnv)
          stop ("ERROR: Road network not sorted as ToArea file!")
        }

      } else {
        road_netw[,stateToArea:=0]
      }

      ################################################################
      ## Calculate spread road and railway network

      if (incl_riverside==FALSE){ # for road+rail network

        road_netw[,newarrivals2:=newarrivals]

        ## Calculate Pintro for all links
        road_netw[,newarrivals2:=   1-(stateFromNode * Pi_traffic )] # calculate pintro for links # HANNO
        ## Combine all Pintros to each ToNode
        road_netw[,newarrivals2:=   1-prod(newarrivals2) ,by=ToNode]#}) # combining link pintros to newarrivals of ToNode

        ## ERROR check for declining newarrivals
        if (any(road_netw[,newarrivals2<newarrivals]) &
            any((road_netw[newarrivals2<newarrivals,newarrivals-newarrivals2])>10^-15)) {
          assign(x = "road_netw",value = road_netw,envir = .GlobalEnv)
          stop ("Problem in spread probability calculations: Decline in newarrivals")
        } else { road_netw[,newarrivals:=newarrivals2] }
        road_netw[,newarrivals2:=NULL]

        ## Select unique ToNodes
        newstate_road <- unique(road_netw[,c("ToNode","stateToNode","newarrivals","Pi_container","stateToArea")]) # extract new state of ToNodes to update FromNodes states
        ## Update state of nodes with old states, container and pallets
        newstate_road[,stateToNode2:=1-((1-stateToNode) * (1-newarrivals) * (1-Pi_container) * (1-stateToArea))] # update ToNodes with old and new state

        ## ERROR check for declining stateToNode
        if (any(newstate_road[,stateToNode2<stateToNode]) &
            any((newstate_road[stateToNode2<stateToNode,stateToNode-stateToNode2])>10^-15)) {
          assign(x = "road_netw",value = road_netw,envir = .GlobalEnv)
          stop ("Problem in spread probability calculations: Decline in stateToNode")
        } else { newstate_road[,stateToNode:=stateToNode2] }
        newstate_road[,c("newarrivals","Pi_container","stateToArea","stateToNode2"):=NULL]

        ## ERROR check for duplicated ToNodes (multiple probabilities assigned to the same node)
        if (any(duplicated(newstate_road$ToNode))) {
          assign(x = "newstate_road",value = newstate_road,envir = .GlobalEnv)
          stop ("Problem in node probability calculations: ToNode(s) with multiple probabilities")
        }

        ## Merge new node states with ToNodes of network file
        setnames(newstate_road,c("ToNode","newstate")) # prepare file for merge (set names and key)
        ToNode_index <- match(road_netw$ToNode,newstate_road$ToNode)
        road_netw[,stateToNode:=newstate_road[ToNode_index,newstate]]

        ## ERROR check for wrong column merging
        if (!identical(road_netw[,ToNode],newstate_road[ToNode_index,ToNode])) {
          assign(x = "newstate_road",value = newstate_road,envir = .GlobalEnv)
          stop ("ERROR: Road network not sorted as ToNode file!")
        }

        ## Merge new node states with FromNodes of network file
        setnames(newstate_road,c("FromNode","newstate")) # prepare file for merge (set names and key)

        FromNode_index <- match(road_netw$FromNode,newstate_road$FromNode)
        noNA_index <- !is.na(FromNode_index) # identify FromNodes not in ToNodes
        road_netw[noNA_index,newstate:=newstate_road[FromNode_index[noNA_index],newstate]]

        ## ERROR check for wrong column merging
        if (!identical(road_netw[noNA_index,FromNode],newstate_road[FromNode_index[noNA_index],FromNode])) {
          assign(x = "newstate_road",value = newstate_road,envir = .GlobalEnv)
          stop ("ERROR: Road network not sorted as FromNode file!")
        }

        ## ERROR check for declining stateFromNode
        if (any(road_netw[newstate>0,newstate<stateFromNode]) &
            any((road_netw[newstate>0 & newstate<stateFromNode,stateFromNode-newstate])>10^-15) ) {
          assign(x = "road_netw",value = road_netw,envir = .GlobalEnv)
          stop ("Problem in spread probability calculations: Decline in stateFromNode")
        }

        # road_netw <- newstate[road_netw] # merge road_netw and newstate to update FromNodes states
        road_netw[newstate>0 ,stateFromNode:=newstate] # assigne new states to FromNodes
        road_netw[,newstate:=NULL] # remove column to avoid columns with the same names

      }

      if (incl_riverside==TRUE){ # for water way network

        water_netw[,newarrivals2:=newarrivals]

        ## Calculate Pintro for all links
        water_netw[,newarrivals2:=   1-(stateFromNode * p_riverside )] # calculate pintro for links # HANNO
        ## Combine all Pintros to each ToNode
        water_netw[,newarrivals2:=   1-prod(newarrivals2) ,by=ToNode]#}) # combining link pintros to newarrivals of ToNode

        ## ERROR check for declining newarrivals
        if (any(water_netw[,newarrivals2<newarrivals]) &
            any((water_netw[newarrivals2<newarrivals,newarrivals-newarrivals2])>10^-15)) {
          assign(x = "water_netw",value = water_netw,envir = .GlobalEnv)
          stop ("Problem in spread probability calculations: Decline in newarrivals")
        } else { water_netw[,newarrivals:=newarrivals2] }
        water_netw[,newarrivals2:=NULL]

        ## Select unique ToNodes
        newstate_water <- unique(water_netw[,c("ToNode","stateToNode","newarrivals")]) # extract new state of ToNodes to update FromNodes states
        ## Update state of nodes with old states, container and pallets
        newstate_water[,stateToNode2:=1-((1-stateToNode) * (1-newarrivals))] # update ToNodes with old and new state

        ## ERROR check for declining stateToNode
        if (any(newstate_water[,stateToNode2<stateToNode]) &
            any((newstate_water[stateToNode2<stateToNode,stateToNode-stateToNode2])>10^-15)) {
          assign(x = "water_netw",value = water_netw,envir = .GlobalEnv)
          stop ("Problem in spread probability calculations: Decline in stateToNode")
        } else { newstate_water[,stateToNode:=stateToNode2] }
        newstate_water[,c("newarrivals","stateToNode2"):=NULL]

        ## ERROR check for duplicated ToNodes (multiple probabilities assigned to the same node)
        if (any(duplicated(newstate_water$ToNode))) {
          assign(x = "newstate_water",value = newstate_water,envir = .GlobalEnv)
          stop ("Problem in node probability calculations: ToNode(s) with multiple probabilities")
        }

        ## Merge new node states with ToNodes of network file
        setnames(newstate_water,c("ToNode","newstate")) # prepare file for merge (set names and key)
        ToNode_index <- match(water_netw$ToNode,newstate_water$ToNode)
        water_netw[,stateToNode:=newstate_water[ToNode_index,newstate]]

        ## ERROR check for wrong column merging
        if (!identical(water_netw[,ToNode],newstate_water[ToNode_index,ToNode])) {
          assign(x = "newstate_water",value = newstate_water,envir = .GlobalEnv)
          stop ("ERROR: water network not sorted as ToNode file!")
        }

        ## Merge new node states with FromNodes of network file
        setnames(newstate_water,c("FromNode","newstate")) # prepare file for merge (set names and key)

        FromNode_index <- match(water_netw$FromNode,newstate_water$FromNode)
        noNA_index <- !is.na(FromNode_index) # identify FromNodes not in ToNodes
        water_netw[noNA_index,newstate:=newstate_water[FromNode_index[noNA_index],newstate]]

        ## ERROR check for wrong column merging
        if (!identical(water_netw[noNA_index,FromNode],newstate_water[FromNode_index[noNA_index],FromNode])) {
          assign(x = "newstate_water",value = newstate_water,envir = .GlobalEnv)
          stop ("ERROR: water network not sorted as FromNode file!")
        }

        ## ERROR check for declining stateFromNode
        if (any(water_netw[newstate>0,newstate<stateFromNode]) &
            any((water_netw[newstate>0 & newstate<stateFromNode,stateFromNode-newstate])>10^-15) ) {
          assign(x = "water_netw",value = water_netw,envir = .GlobalEnv)
          stop ("Problem in spread probability calculations: Decline in stateFromNode")
        }

        # water_netw <- newstate[water_netw] # merge water_netw and newstate to update FromNodes states
        water_netw[newstate>0 ,stateFromNode:=newstate] # assigne new states to FromNodes
        water_netw[,newstate:=NULL] # remove column to avoid columns with the same names

      }


      ############################################################
      ## store results ###########################################

      if (it%in%iter_save) {

        if (incl_riverside==FALSE){ # for water way network

          setkey(road_netw,ID)
          setkey(road_netw_details,ID)
          road_netw_out <- road_netw_details[road_netw]
          road_netw_out[,p_link:=stateToNode] #calculate probability of link invasion: to be involved in Pinv instead of Pi, or it stays constant in time
          road_netw_out[,Pinv:=Pe*p_link] # calculate total probability for links
  #        road_netw_out[ID%in%init_segm,Pinv:=1] # assigning Pinv=1 for initial links. No effect on Traffic spread dynamics (use nodes).
          setkey(road_netw_out,Order)

           ## ERROR check for state probabilities
          if (is.numeric(road_netw_out[,stateFromNode])==FALSE | is.numeric(road_netw_out[,stateToNode])==FALSE |
              any(road_netw_out[,stateFromNode<0]) | any(road_netw_out[,stateFromNode>1]) |
              any(road_netw_out[,stateToNode<0]) | any(road_netw_out[,stateToNode>1])) {
            assign(x = "road_netw_out",value = road_netw_out,envir = .GlobalEnv)
            stop ("Problem in probability calculations: Probabilities either non-numeric or not in 0:1 range")
          }
          modelList[[as.character(it)]]<-road_netw_out
        }

        if (incl_riverside==TRUE){ # for water way network

          setkey(water_netw,ID)
          setkey(water_netw_details,ID)
          water_netw_out <- water_netw_details[water_netw]
          water_netw_out[,p_link:=stateToNode] #calculate probability of link invasion: to be involved in Pinv instead of Pi, or it stays constant in time
          water_netw_out[,Pinv:=Pe*p_link] # calculate total probability for links
          #        water_netw_out[ID%in%init_segm,Pinv:=1] # assigning Pinv=1 for initial links. No effect on Traffic spread dynamics (use nodes).
          setkey(water_netw_out,Order)

          ## ERROR check for state probabilities
          if (is.numeric(water_netw_out[,stateFromNode])==FALSE | is.numeric(water_netw_out[,stateToNode])==FALSE |
              any(water_netw_out[,stateFromNode<0]) | any(water_netw_out[,stateFromNode>1]) |
              any(water_netw_out[,stateToNode<0]) | any(water_netw_out[,stateToNode>1])) {
            assign(x = "water_netw_out",value = water_netw_out,envir = .GlobalEnv)
            stop ("Problem in probability calculations: Probabilities either non-numeric or not in 0:1 range")
          }
          modelList[[as.character(it)]]<-water_netw_out
        }
      }

      #update progress bar
      info <- sprintf("%d%% done", round((it/num_iter)*100))
      setTxtProgressBar(pb, it/(100)*100, label=info)
    }
  }
  close(pb)

  # roads_shp <- road_netw_out

  return(modelList)
}

