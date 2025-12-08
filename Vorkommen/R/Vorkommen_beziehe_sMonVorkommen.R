##################### Ermittlung von Vorkommensdaten fuer einzelne Arten #######
# 
# Skript ist Teil des Workflows "Vorkommen" zur Darstellung der Vorkommen von
# Arten in Deutschland. Es wird mit dem Skript erstelleListeNeobiota_MAIN.R 
# aufgerufen.
#
# Das Skript ermittelt Vorkommensdaten für eine ausgewählte Art in sMon. Die 
# Dateien von sMon müssen lokal gespeichert vorliegen.
# 
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 08.12.25
################################################################################



beziehe_sMonVorkommen <- function(TaxonName=TaxonName,
                                 sMon_Verzeichnis=sMon_Verzeichnis,
                                 sMon_Wahrscheinlichkeit){
  
  # sMon_Verzeichnis <- file.path("..","..","Storage_large","Species","sMon")
  # # TaxonName <- "Campanula cervicaria"
  
  ## set working directory temporally to sMon folder
  working_directory <- getwd()
  setwd(sMon_Verzeichnis)
  
  sMon_data <- list()
  for (i in 1:4){
    
    ind_records <- c()
    
    ## sMon data set #1
    sMon_data_species <- fread(file.path(sMon_Verzeichnis,paste0("1875_9_1875_2_Modelled_OPs_incl_sd_pt_",i,".csv")),select="TaxonName")
    ind_records <- which(sMon_data_species==TaxonName)
    
    if (length(ind_records)!=0){
      col_names <- colnames(fread(file.path(sMon_Verzeichnis,paste0("1875_9_1875_2_Modelled_OPs_incl_sd_pt_",i,".csv")),nrows=0))
      sMon_data_sub <- fread(file.path(sMon_Verzeichnis,paste0("1875_9_1875_2_Modelled_OPs_incl_sd_pt_",i,".csv")),
                             nrows=length(ind_records),skip=min(ind_records))
      colnames(sMon_data_sub) <- col_names
      
      sMon_data[[i]] <- subset(sMon_data_sub,OP>=sMon_Wahrscheinlichkeit)
    }
  }
  
  sMon_data <- rbindlist(sMon_data)
  
  setwd(working_directory) # reset working directory
  
  return(sMon_data)
}
