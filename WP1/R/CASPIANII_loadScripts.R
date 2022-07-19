##################### Load all scripts ##############################
#
# Project: CASPIAN II
# 
# Hanno Seebens, 18.07.22
#####################################################################


## function to standardise taxonomic information across databases ###
## (applies StandardiseTaxonNames.R)
source(file.path("WP1","R","ALG_step1_runDataStandardisation.R"))

## function to check taxon names using GBIF #########################
source(file.path("WP1","R","ALG_step1_runStandardiseTaxonNames.R"))

## function to integrate standardised databases #####################
## (removes duplicates and adds common name of taxonomic groups)
source(file.path("WP1","R","ALG_step2_runIntegrateAlienSpeciesDataSets.R"))

## function to obtain the total number of records available on GBIF #
source(file.path("WP1","R","ALG_step3_getGBIFrecords.R"))

## function to extract pathway information from CBD (Saul et al. 2017)
source(file.path("WP1","R","ALG_step4_getPathways.R"))

## function to extract pathway information from CBD (Saul et al. 2017)
source(file.path("WP1","R","ALG_step5_getsMonRecords.R"))

## function to extract occurrence records from various sources
source(file.path("WP1","R","ORC_get_occurrence_records.R"))

## function to extract occurrence recrods from sMon
source(file.path("WP1","R","get_sMon_occurrences.R"))

## function to map occurrence recrods 
source(file.path("WP1","R","OCR_map_records.R"))

