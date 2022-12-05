##################### Load all scripts ##############################
#
# Project: CASPIAN II
# 
# Senckenberg Gesellschaft fuer Naturforschung, 04.11.22
#####################################################################


## function to standardise taxonomic information across databases ###
## (applies StandardiseTaxonNames.R)
source(file.path("WP1","R","ListeNeobiota_Schritt1_DatenStandardisierung.R"))

## function to check taxon names using GBIF #########################
source(file.path("WP1","R","ListeNeobiota_Schritt1-1_standardisiereNamen.R"))

## function to integrate standardised databases #####################
## (removes duplicates and adds common name of taxonomic groups)
source(file.path("WP1","R","ListeNeobiota_Schritt2_integriereDatensaetze.R"))

## function to obtain the total number of records available on GBIF #
source(file.path("WP1","R","ListeNeobiota_Schritt3_bezieheGBIFDaten.R"))

## function to extract pathway information from CBD (Saul et al. 2017)
source(file.path("WP1","R","ListeNeobiota_Schritt4_beziehePfaddaten.R"))

## function to extract sMon records (Eichenberg et al. 2021)
source(file.path("WP1","R","ListeNeobiota_Schritt5_ermittle_sMonDaten.R"))

## function to extract occurrence records from various sources
source(file.path("WP1","R","Vorkommen_bezieheDaten.R"))

## function to obtain occurrence records from online repositories
source(file.path("WP1","R","Vorkommen_sammeleDatenOnline.R"))

## function to extract occurrence recrods from sMon
source(file.path("WP1","R","Vorkommen_beziehe_sMonVorkommen.R"))

## function to map occurrence recrods 
source(file.path("WP1","R","Vorkommen_erstelleKarte.R"))

