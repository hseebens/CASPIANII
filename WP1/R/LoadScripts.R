##################### Load all scripts ##############################
#
# Project: CASPIAN II
# 
# Hanno Seebens, 29.06.22
#####################################################################


## function to standardise taxonomic information across databases ###
## (applies StandardiseTaxonNames.R)
source(file.path("R","run_DataStandardisation.R"))

## function to check taxon names using GBIF #########################
source(file.path("R","StandardiseTaxonNames.R"))

## function to integrate standardised databases #####################
## (removes duplicates and adds common name of taxonomic groups)
source(file.path("R","run_IntegrateAlienSpeciesDataSets.R"))

## function to obtain the total number of records available on GBIF #
source(file.path("R","get_GBIFrecords.R"))

## function to extract pathway information from CBD (Saul et al. 2017)
source(file.path("R","get_pathways.R"))


