#ARGUMENTS:
# parameters: model parameter values. Must be provided in the same order
#   and same form shown in InitializationScript.R. Column names and order matter!
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
# max_dist: maximum distance (m) from initial coordinates for a segment to be considered infected.
# makeplot: should model results be plotted as maps (could be a long process)? Default FALSE.
# save_plot: logical. If TRUE, plots are created in the newly created folder as .png files.
#   If FALSE, an x11() device is opened. Only considered if makeplot=TRUE. if
# iter_save: which model iterations should be saved? By default=num_iter. If makeplot=TRUE, several maps
#   will be created, one for each saved iteration.

# netw_type: the types of roads to be considered in the simulation. Default c("all").



CASPIANSpreadAquatic <- function(parameters,init_obj,
                             Water_netw_data,
                             init_coords, num_iter,
                             incl_hullfouling=T,incl_natural_water=T,incl_ballast=TRUE,
                             max_dist,Port_time=NA,Paint_time=NA,
                             iter_save=num_iter,plot_funct_rel=FALSE,
                             export_results=F,
                             traffic_type=c("all")){

  ####################################################################

  for (i in 1:length(init_obj)){
    assign(names(init_obj)[i], init_obj[[i]])
  } 

  water_netw <- water_shp@data

  ####################################################################
  ## Calculate probabilities for individual pathways

  for (nparset in 1:nrow(parameters)){

    ## Calculate Pintro for ballast water #####################
    if (incl_ballast==TRUE){
      cat("\n Calculating Probability of introduction by ballast water for each node \n")
      water_netw[,Pi_ballast:=f_ballast(CargoToNode,parameters[nparset,"ball1"])]

      ## plot functional relationship of probability
      # if (plot_funct_rel==TRUE){
      #   prob_varx <- seq(min(Container_netw$numContainers),max(Container_netw$numContainers),length.out=1000)
      #   plot(prob_varx,f_container(prob_varx,parameters[nparset,"cont1"]),main="Container dispersal",xlab="Number of containers",ylab="Probability")
      # }
      # Container_netw[,numContainers:=NULL]
      #

      #Filling NAs
      water_netw[is.na(Pi_ballast),Pi_ballast:=0]

      ## ERROR check for Pi_ballast
      if (is.numeric(water_netw[,Pi_ballast])==FALSE |
          any(water_netw[,Pi_ballast<0]) | any(water_netw[,Pi_ballast>1])) {
        assign(x = "water_netw",value = water_netw,envir = .GlobalEnv)
        stop ("Problem in probability calculations: Pi_ballast either non-numeric or not in 0:1 range")
      }
    } else {
      water_netw[,Pi_ballast:=0]
    }


    cat("\n Calculating Probability of Introduction for each segment \n")

    ## Calculate natural dispersal #####################
    if (incl_natural_water==TRUE){

      water_netw[,p_natural:=f_natural_water(a = parameters[nparset,"nat_a"],b = parameters[nparset,"nat_b"],d=Length )]

    } else {

      water_netw[,p_natural:=0]

    }

    ## Calculate dispersal by hull fouling #####################
    if (incl_hullfouling==TRUE) {

      water_netw[,p_hull:=f_hullfouling(a = parameters[nparset,"alpha"],c1 = parameters[nparset,"c1"],
                                             g = parameters[nparset,"gamma"],c2 = parameters[nparset,"c2"],
                                             b = parameters[nparset,"beta"],c3 = parameters[nparset,"c3"],
                                             Dp = Port_time,Qp = Paint_time,VTp = Length)]
      water_netw[,p_hull:= 1-(1-p_hull)^(Traffic*parameters[nparset,"hull0"])]

    } else {

      water_netw[,p_hull:=0]

    }

    water_netw[is.na(p_hull),p_hull:=0]
    water_netw[is.na(p_natural),p_natural:=0]

    ## ERROR check for p_natural
    if (is.numeric(water_netw[,p_natural])==FALSE |
        any(water_netw[,p_natural<0]) | any(water_netw[,p_natural>1])) {
      assign(x = "water_netw",value = water_netw,envir = .GlobalEnv)
      stop ("Problem in probability calculations: p_natural either non-numeric or not in 0:1 range")
    }
    ## ERROR check for p_hull
    if (is.numeric(water_netw[,p_hull])==FALSE |
        any(water_netw[,p_hull<0]) | any(water_netw[,p_hull>1])) {
      assign(x = "water_netw",value = water_netw,envir = .GlobalEnv)
      stop ("Problem in probability calculations: p_hull either non-numeric or not in 0:1 range")
    }

    water_netw[, Pi_traffic:=1-Reduce("*", 1-.SD), .SDcols=grep("p_",colnames(water_netw))] # new solution


    ##################################################################
    # caculate probability of establishment for aquatic habitats

    #get PE for segment
    cat("\n Calculating Probability of Establishment for each segment \n")

    water_netw[,Pe:=parameters[nparset,"estW"]*Env_suit] # parameter for scaling down probability of establishment # new
    water_netw[is.na(Pe),Pe:=0]

    ## ERROR check for Pe
    if (is.numeric(water_netw[,Pe])==FALSE |
        any(water_netw[,Pe<0]) | any(water_netw[,Pe>1])) {
      assign(x = "water_netw",value = water_netw,envir = .GlobalEnv)
      stop ("Problem in probability calculations: Pe either non-numeric or not in 0:1 range")
    }

    ## set data.table key for road network (much faster)
    # And subset relevant information
    water_netw_details <- water_netw[,c("ID","Env_suit","Length","Traffic","p_natural","p_hull", "Order")]
    set( water_netw, j=which(colnames(water_netw) %in% c("Env_suit","Length","Traffic","p_natural","p_hull","Order")), value=NULL ) # new
    setkey(water_netw,FromNode)


    ##################################################################
    ##### start simulation ###########################################
    ##################################################################

    ## subsequent steps ######
    cat("\n Ongoing Simulation \n")

    #setup progress bar
    pb <- txtProgressBar(title = "Simulation state", label = "0% done",min=0, max=num_iter, initial=0,style = 3)

    modelList<- list()

    for (it in 1:num_iter){#num_iter

      ###### network part

      # ind <- which(water_netw$stateFromNode>0 & water_netw$stateToNode<1) # select links with non-empty start node and non-filled end node
      ## Calculate link probabilities (Hanno) ######
      water_netw[,newarrivals2:=newarrivals]
      ## Calculate Pintro for all links
      water_netw[,newarrivals2:=   1-(stateFromNode * Pi_traffic )] # calculate pintro for each link
      ## Combine all Pintros to each ToNode
      water_netw[,newarrivals2:=   1-prod(newarrivals2) ,by=ToNode] # calculate pintro for each link

      ## ERROR check for declining newarrivals
      if (any(water_netw[,newarrivals2<newarrivals]) &
          any((water_netw[newarrivals2<newarrivals,newarrivals-newarrivals2])>10^-15)) {
        assign(x = "water_netw",value = water_netw,envir = .GlobalEnv)
        stop ("Problem in spread probability calculations: Decline in newarrivals")
      } else { water_netw[,newarrivals:=newarrivals2] }
      water_netw[,newarrivals2:=NULL]

      ## update nodes #####################
      ## Select unique ToNodes
      newstate_aqua <- unique(water_netw[,c("ToNode","stateToNode","Pi_ballast","newarrivals")]) # select single node (remove duplicates)
      ## update stateToNodes
      newstate_aqua[,stateToNode2:=1-((1-stateToNode) * (1-Pi_ballast) * (1-newarrivals))] # update ToNodes with old and new state
      # water_netw[,stateToNode2:=stateToNode]
      # water_netw[ind,stateToNode2:=1-(prod((1-stateToNode) * (1-newarrivals) )),by=ToNode] # update ToNodes with old and new state
      #      water_netw[,stateToNode2:=1-(prod((1-stateToNode2))),by=ToNode] # update ToNodes with old and new state

      ## ERROR check for duplicated ToNodes (multiple probabilities assigned to the same node)
      if (any(duplicated(newstate_aqua$ToNode))) {
        assign(x = "newstate_aqua",value = newstate_aqua,envir = .GlobalEnv)
        stop ("Problem in node probability calculations: ToNode(s) with multiple probabilities")
      }

      ## ERROR check for declining stateToNode
      if (any(newstate_aqua[,stateToNode2<stateToNode]) &
          any((newstate_aqua[stateToNode2<stateToNode,stateToNode-stateToNode2])>10^-15)) {
        assign(x = "newstate_aqua",value = newstate_aqua,envir = .GlobalEnv)
        stop ("Problem in spread probability calculations: Decline in stateToNode")
      } else { newstate_aqua[,stateToNode:=stateToNode2] }
      newstate_aqua[,c("newarrivals","Pi_ballast","stateToNode2"):=NULL]

 #     assign(x = "newstate_aqua",value = newstate_aqua,envir = .GlobalEnv)

      ## Merge new state nodes and network ####################
      ## Update stateToNode
      setnames(newstate_aqua,c("ToNode","newstate"))# prepare file for merge (set names and key)
      ToNode_index <- match(water_netw$ToNode,newstate_aqua$ToNode)
      water_netw[,stateToNode:=newstate_aqua[ToNode_index,newstate]]

      ## ERROR check for wrong column merging
      if (!identical(water_netw[,ToNode],newstate_aqua[ToNode_index,ToNode])) {
        assign(x = "water_netw",value = water_netw,envir = .GlobalEnv)
        stop ("ERROR: Waterway network not sorted as ToNode file!")
      }

      ## Merge new node states with FromNodes of network file
      setnames(newstate_aqua,c("FromNode","newstate")) # prepare file for merge (set names and key)

      FromNode_index <- match(water_netw$FromNode,newstate_aqua$FromNode)
      noNA_index <- !is.na(FromNode_index) # identify FromNodes not in ToNodes
      water_netw[noNA_index,newstate:=newstate_aqua[FromNode_index[noNA_index],newstate]]

      ## ERROR check for wrong column merging
      if (!identical(water_netw[noNA_index,FromNode],newstate_aqua[FromNode_index[noNA_index],FromNode])) {
        assign(x = "newstate_aqua",value = newstate_aqua,envir = .GlobalEnv)
        stop ("ERROR: Waterway network not sorted as FromNode file!")
      }

      # newstate <-water_netw[,c("ToNode","stateToNode")] # extract new state of ToNodes to update FromNodes states
      # newstate<-as.data.table(aggregate(stateToNode ~ ToNode, newstate, pUnion))
      # setnames(newstate,c("FromNode","newstate")) # prepare file for merge (set names and key)
      # setkey(newstate,FromNode)
      # setkey(water_netw,FromNode)
      # water_netw <- merge(water_netw,newstate,by="FromNode",all.x=T)
      # water_netw[is.na(newstate),newstate:=0]

      ## ERROR check for declining stateFromNode
      if (any(water_netw[newstate>0,newstate<stateFromNode]) &
          any((water_netw[newstate>0 & newstate<stateFromNode,stateFromNode-newstate])>10^-15) ) {
        assign(x = "water_netw",value = water_netw,envir = .GlobalEnv)
        stop ("Problem in spread probability calculations: Decline in stateFromNode")
      }

      # water_netw <- newstate[water_netw] # merge water_netw and newstate to update FromNodes states
      water_netw[newstate>0 ,stateFromNode:=newstate] # assigne new states to FromNodes
      water_netw[,newstate:=NULL] # remove column to avoid columns with the same names


      # store results
      if (it%in%iter_save) {
        water_netw[,p_link:=stateToNode] #calculate probability of link invasion: to be involved in Pinv instead of Pi, or it stays constant in time
        water_netw[,Pinv:=Pe*p_link] # calculate total probability for links
        water_netw[ID%in%init_segm,Pinv:=1] # assigning Pinv=1 for initial links. No effect on Traffic spread dynamics (use nodes).
        setkey(water_netw,ID)
        setkey(water_netw_details,ID)
        water_netw_out <- water_netw_details[water_netw]
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

      #update progress bar
      info <- sprintf("%d%% done", round((it/num_iter)*100))
      setTxtProgressBar(pb, it/(100)*100, label=info)
    }
  }
  close(pb)

  water_shp@data<-water_netw_out

  return(modelList)
}

