##################### get records of species occurrences from sMon ###########################################
# 
# The script returns the occurrence records of the requested species from the sMon database. 
# 
# Input variables: species names (canonical names) and folder, where sMon data are stored
# Output variables: extract from the sMon database for the requested species 
# 
# Project: CASPIAN II
# 
# Senckenberg Gesellschaft fuer Naturforschung, 04.11.22
###############################################################################################################


get_sMon_occurrences <- function(TaxonName=TaxonName,sMon_Verzeichnis=sMon_Verzeichnis){
  
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
      
      sMon_data[[i]] <- sMon_data_sub
    }
  }
  
  sMon_data <- rbindlist(sMon_data)
  
  setwd(working_directory) # reset working directory
  
  return(sMon_data)
}
