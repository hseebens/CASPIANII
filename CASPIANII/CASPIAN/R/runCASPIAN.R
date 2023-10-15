#configFile: full path of the configuration file

runCASPIAN<-function(configFile,path2data=path2data){

  cat("\n Loading CASPIAN configuration file \n\n")

  source(configFile,local=TRUE)

  # build parameter matrix
  parameters <- ParMatrix(par_att0_Roads,par_att0_Railways,par_att1,par_att2,par_att3,
                       par_air0_Roads,par_air0_Railways,par_air1,par_air2,
                       par_nat1,par_nat2,par_nat_riverside1,par_nat_riverside2,
                       par_est_T,par_cont,par_pall,
                       par_nat_a,par_nat_b,par_ball,
                       par_hull0,par_a,par_c1,par_g,par_c2,par_b,par_c3,par_est_W
                       )

  modelResults<-list()

  if(runTerrestrialModel==TRUE){

    cat("\n Running Terrestrial Model \n")

    tmp <- proc.time()

    # build land cover species preference matrix
    species_preferences<- data.table(LC_cat_ID= 1:5,Species_preferences=c(Urban_areas,Arable_land,Pastures,Forests,Wetlands))

    # running model:
    dir.name_T <- file.path(getwd(),"CASPIAN",paste0("CASPIAN_Terrestrial_",format(Sys.time(), "%d-%b-%Y %H-%M-%S")))
    dir.create(dir.name_T)

    # Model initialization
    if (initialize==TRUE) {

      init_obj <- InitSpreadTerrestrial(Terrestrial_netw_data=Terrestrial_netw_data,
                                      Commodities_shape_data=Commodities_shape_data,
                                      Water_netw_data=Water_netw_data,
                                      Pallets_netw_data=Pallets_netw_data,
                                      Container_netw_data=Container_netw_data,
                                      env_terrestrial=env_terrestrial,
                                      init_coords=init_coords_T,
                                      max_dist=max_dist_T,
                                      netw_type=netw_type,
                                      save_init=save_init,
                                      save_dir=dir.name_T,
                                      file_init=file_init,
                                      species_preferences=species_preferences,
                                      traffic_type=traffic_type_T,
                                      incl_containers=incl_containers,
                                      incl_pallets=incl_pallets,
                                      # Cont_threshold=Cont_threshold,
                                      # Pall_threshold=Pall_threshold,
                                      incl_riverside=T)

    } else if (initialize==FALSE) {cat("\n Loading initialization data \n")
      load(file_init)
    }


    # Spread Calculations
    TerrestrialModelResults <- CASPIANSpreadTerrestrial(parameters=parameters,
                                                        init_obj=init_obj,
                                                        # Terrestrial_netw_data=Terrestrial_netw_data,
                                                        # Water_netw_data=Water_netw_data,
                                                        # Commodities_shape_data=Commodities_shape_data,
                                                        # Pallets_netw_data=Pallets_netw_data,
                                                        # Container_netw_data=Container_netw_data,
                                                        # netw_type=netw_type,
                                                        # traffic_type=traffic_type_T,
                                                        # init_coords=init_coords_T,
                                                        num_iter=num_iter_T,
                                                        # max_dist = max_dist_T,
                                                        incl_attachment=incl_attachment,
                                                        incl_airflow=incl_airflow,
                                                        incl_natural=incl_natural,
                                                        incl_containers=incl_containers,
                                                        incl_pallets=incl_pallets,
                                                        incl_riverside=incl_riverside,
                                                        # Cont_threshold=Cont_threshold,
                                                        # Pall_threshold=Pall_threshold,
                                                        iter_save = iter_save_T,
                                                        plot_funct_rel=plot_funct_rel
                                                        )

    cat("\n Terrestrial model calculation completed \n")

    print(proc.time() - tmp)

    # cat("\n Output files being created in ", dir.name_T, "\n")

    if ("csv"%in%export_results) {
      cat("\n Exporting Terrestrial results in .csv files \n")
      for (i in names(TerrestrialModelResults))
        fwrite(x = TerrestrialModelResults[[i]],file = file.path(dir.name_T, paste0("TerrestrialModelResults_",i,".csv")))
        # assign(x = "modelList",value = modelList,envir = .GlobalEnv)
    }

    if ("txt"%in%export_results) {
      cat("\n Exporting Terrestrial results in .txt files \n")
      for (i in names(TerrestrialModelResults))
        fwrite(x = TerrestrialModelResults[[i]],file = file.path(dir.name_T, paste0("TerrestrialModelResults_",i,".txt")))
        # assign(x = "modelList",value = modelList,envir = .GlobalEnv)
    }

    if ("shp"%in%export_results) {
      cat("\n Exporting Terrestrial results as ESRI shapefiles \n")
      for (i in names(TerrestrialModelResults)){
        shp <- init_obj$roads_shp
        shp@data<-TerrestrialModelResults[[i]]
        writeOGR(shp, dsn=file.path(dir.name_T, paste0("TerrestrialModelResults_",i)), layer="Terrestrial_network", driver="ESRI Shapefile")
      }
    }
  }

  if (runAquaticModel==TRUE){

    cat("\n Running Aquatic Model \n")
    tmp <- proc.time()

    #build aquatic species preference matrix
    species_preferences<- data.table(specTemp,specCond)

    #running model:
    dir.name_W<-file.path(getwd(),"CASPIAN",paste0("CASPIAN_Aquatic_",format(Sys.time(), "%d-%b-%Y %H-%M-%S")))
    dir.create(dir.name_W)

    #Model initialization
    if (initialize==TRUE) {
      init_water_data<-InitSpreadAquatic(Water_netw_data=Water_netw_data,env_aquatic=env_aquatic,
                                             init_coords=init_coords_W,max_dist=max_dist_W,
                                             save_init=save_init, save_dir=dir.name_W,file_init=file_init,
                                             species_preferences=species_preferences,
                                             traffic_type=traffic_type_W
      )

    } else if (initialize==FALSE) {cat("\n Loading initialization data \n")
      load(file_init)
    }

    #Spread calculations
    AquaticModelResults<-CASPIANSpreadAquatic(parameters=parameters,init_obj=init_water_data,
                                             Water_netw_data=Water_netw_data,
                                           traffic_type=traffic_type_W,
                                           init_coords=init_coords_W, num_iter=num_iter_W,max_dist = max_dist_W,
                                           incl_hullfouling=incl_hullfouling,incl_natural_water=incl_natural_water,incl_ballast=incl_ballast,
                                           Port_time=Port_time,Paint_time=Paint_time,
                                           iter_save = iter_save_W,plot_funct_rel=plot_funct_rel
                          )

    cat("\n Aquatic model calculation completed \n")
    print(proc.time() - tmp)
    cat("\n Output files being created in ", dir.name_W, "\n")

    if ("csv"%in%export_results) {
      cat("\n Exporting Aquatic results in .csv files \n")
      for (i in names(AquaticModelResults))
        write.csv(x = AquaticModelResults[[i]],file = file.path(dir.name_W, paste0("AquaticModelResults_",i,".csv")),quote = F,row.names = F)
      # assign(x = "modelList",value = modelList,envir = .GlobalEnv)
    }

    if ("txt"%in%export_results) {
      cat("\n Exporting Aquatic results in .txt files \n")
      for (i in names(AquaticModelResults))
        write.table(x = AquaticModelResults[[i]],file = file.path(dir.name_W, paste0("AquaticModelResults_",i,".txt")),quote = F,row.names = F,sep="\t")
      # assign(x = "modelList",value = modelList,envir = .GlobalEnv)
    }
    if ("shp"%in%export_results) {
      cat("\n Exporting Aquatic results as ESRI shapefiles \n")
      for (i in names(AquaticModelResults)){
        shp<-init_water_data$water_shp
        shp@data<-AquaticModelResults[[i]]
        writeOGR(shp, dsn=file.path(dir.name_W, paste0("AquaticModelResults_",i)), layer="Aquatic_network", driver="ESRI Shapefile")}
    }
  }

  if (makeplot) {
    if (runTerrestrialModel==TRUE){

      cat("\n Creating Terrestrial maps \n")

      if (incl_riverside==TRUE){
        plotCASPIAN(list_results=TerrestrialModelResults,shapeObj=init_obj$water_shp,variable="Pinv", save_plot=save_plot,save_dir=dir.name_T,lwd=linewidth)
      } else {
        plotCASPIAN(list_results=TerrestrialModelResults,shapeObj=init_obj$roads_shp,variable="Pinv", save_plot=save_plot,save_dir=dir.name_T,lwd=linewidth)
      }
    }
    if (runAquaticModel==TRUE){
      cat("\n Creating Aquatic maps \n")
      plotCASPIAN(list_results=AquaticModelResults,shapeObj=init_water_data$water_shp,variable="Pinv",save_plot=save_plot,save_dir=dir.name_W)
    }
  }

  if (runTerrestrialModel==TRUE){
    modelResults[["TerrestrialResults"]]<-TerrestrialModelResults
  }
  if (runAquaticModel==TRUE){
    modelResults[["AquaticResults"]]<-AquaticModelResults
  }

  cat("\n Simulation complete \n")
  print(proc.time() - tmp)

  return(modelResults)
}
