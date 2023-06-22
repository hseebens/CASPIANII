##################### Load all scripts ##############################
#
# Project: CASPIAN II
# 
# Senckenberg Gesellschaft fuer Naturforschung, 04.11.22
#####################################################################


## load all required packages #########################
source(file.path("LadePakete.R"))


### ListeNeobiota ###################################################################################

## function to standardise taxonomic information across databases ###
## (applies StandardiseTaxonNames.R)
source(file.path("ListeNeobiota","R","ListeNeobiota_Schritt1_DatenStandardisierung.R"))

## function to check taxon names using GBIF #########################
source(file.path("ListeNeobiota","R","ListeNeobiota_Schritt1-1_standardisiereNamen.R"))

## function to integrate standardised databases #####################
## (removes duplicates and adds common name of taxonomic groups)
source(file.path("ListeNeobiota","R","ListeNeobiota_Schritt2_integriereDatensaetze.R"))

## (removes duplicates and adds common name of taxonomic groups)
source(file.path("ListeNeobiota","R","ListeNeobiota_Schritt2a_bereinigeListe.R"))

## function to obtain the total number of records available on GBIF #
source(file.path("ListeNeobiota","R","ListeNeobiota_Schritt3_bezieheGBIFDaten.R"))

## function to extract pathway information from CBD (Saul et al. 2017)
source(file.path("ListeNeobiota","R","ListeNeobiota_Schritt4_beziehePfaddaten.R"))

## function to extract sMon records (Eichenberg et al. 2021)
source(file.path("ListeNeobiota","R","ListeNeobiota_Schritt5_ermittle_sMonDaten.R"))


### Vorkommen ###################################################################################

## function to extract occurrence records from various sources
source(file.path("Vorkommen","R","Vorkommen_bezieheDaten.R"))

## function to extract occurrence recrods from sMon
source(file.path("Vorkommen","R","Vorkommen_beziehe_sMonVorkommen.R"))

## function to map occurrence recrods 
source(file.path("Vorkommen","R","Vorkommen_erstelleKarte.R"))


### SDM ###################################################################################


source(file.path("SDM","R","SDM_UeberpruefeVerzeichnisse.R"))
source(file.path("SDM","R","SDM_ermittleVorkommen.R"))
source(file.path("SDM","R","SDM_sammleVorkommenOnline.R"))

source(file.path("SDM","R","SDM_ermittleUmweltdaten.R"))

source(file.path("SDM","R","SDM_generiereAbsenzDaten.R"))

# # model fitting and validation
source(file.path("SDM","R","SDM_Vorhersage_alleLaeufe.R"))
source(file.path("SDM","R","SDM_fitModel.R"))

# suitability prediction
source(file.path("SDM","R","SDM_erstelleKarte.R"))

# decompress large zip files efficiently
source(file.path("SDM","R","SDM_decompress_file.R"))

# get large data files from GBIF
# source(file.path("SDM","R","SDM_bezieheHoheDatenmengen.R"))

# integrate occurrence data of all species and map
source(file.path("SDM","R","SDM_erstelleKarte_istVorkommenAllerArten.R"))

# integrate habitat suitabilities of all species and map
source(file.path("SDM","R","SDM_erstelleKarte_potVorkommenAllerArten.R"))
