##################### get records of species occurrences from OBIS ###########################################
# 
# 
# Project: CASPIAN II
# 
# Hanno Seebens, 06.07.22
###############################################################################################################


get_OBIS_occurrences <- function(taxa_name=taxa_name){
  

  ## download OBIS records

  obis_down <- try(robis::occurrence(scientificname = taxon_name,
                                geometry = "POLYGON ((0 45, 25 45, 25 58, 0 58,0 45))", # restrict download roughly to German waters
                                verbose=F),silent=T)#
     
  #    map_leaflet(obis_down)
  #    
  #   if(nrow(obis_down)>0){
  #     print(i)
  #     x <- x + 1
  #     obis_records[[x]] <- obis_down
  #   }
}

