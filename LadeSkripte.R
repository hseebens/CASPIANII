##################### Load all scripts ##############################
#
# Project: CASPIAN II
# 
# Hanno Seebens, Senckenberg Gesellschaft fuer Naturforschung, 19.07.23
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

## function to check the required folder structure
source(file.path("ListeNeobiota","R","ListeNeobiota_ueberpruefeDatenVerzeichnisse.R"))

## function to extract German common names
source(file.path("ListeNeobiota","R","ListeNeobiota_Schritt6_ermittleDeutscheArtnamen.R"))



### Vorkommen ###################################################################################

## function to extract occurrence records from various sources
source(file.path("Vorkommen","R","Vorkommen_bezieheDaten.R"))

## function to extract occurrence recrods from sMon
source(file.path("Vorkommen","R","Vorkommen_beziehe_sMonVorkommen.R"))

## function to map occurrence recrods 
source(file.path("Vorkommen","R","Vorkommen_erstelleKarte.R"))

## function to map occurrence recrods 
source(file.path("Vorkommen","R","Vorkommen_ueberpruefeVerzeichnisse.R"))


### SDM ###################################################################################


source(file.path("SDM","R","SDM_ueberpruefeVerzeichnisse.R"))
source(file.path("SDM","R","SDM_ueberpruefeDatensaetze.R"))
source(file.path("SDM","R","SDM_importiereArtenliste.R"))
source(file.path("SDM","R","SDM_erstelleStatusFile.R"))

source(file.path("SDM","R","SDM_ermittleVorkommen.R"))
source(file.path("SDM","R","SDM_sammleVorkommenOnline.R"))

source(file.path("SDM","R","SDM_ermittleUmweltdaten.R"))

source(file.path("SDM","R","SDM_generiereAbsenzDaten.R"))

# # model fitting and validation
source(file.path("SDM","R","SDM_VorhersageGesamtgebiet.R"))
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


##### CASPIAN Modell #######################################################


source(file.path("CASPIAN","R","runCASPIAN.R"))
source(file.path("CASPIAN","R","InitializeSpread.R"))
source(file.path("CASPIAN","R","SpreadModel.R"))
source(file.path("CASPIAN","R","getNodesCoord.R"))
source(file.path("CASPIAN","R","getNeighborSegmCoord.R"))
source(file.path("CASPIAN","R","WaterSpreadModel.R"))
source(file.path("CASPIAN","R","pUnion.R"))
source(file.path("CASPIAN","R","plotCASPIAN.R"))
source(file.path("CASPIAN","R","ParMatrix.R"))
source(file.path("CASPIAN","R","InitializeWaterSpread.R"))
source(file.path("CASPIAN","R","getCoordExtent.R"))
source(file.path("CASPIAN","R","getConfigFile.R"))
source(file.path("CASPIAN","R","f_natural.R"))
source(file.path("CASPIAN","R","f_natural_riverside.R"))
source(file.path("CASPIAN","R","f_natural_water.R"))
source(file.path("CASPIAN","R","f_hullfouling.R"))
source(file.path("CASPIAN","R","f_container.R"))
source(file.path("CASPIAN","R","f_ballast.R"))
source(file.path("CASPIAN","R","combID.R"))
source(file.path("CASPIAN","R","Attachment_kernel.R"))
source(file.path("CASPIAN","R","Airflow_kernel.R"))
source(file.path("CASPIAN","R","combID.R"))
source(file.path("CASPIAN","R","createMixWithDefaults.R"))
source(file.path("CASPIAN","R","evaluateCASPIAN.R"))
source(file.path("CASPIAN","R","calibrateCASPIAN.R"))

source(file.path("CASPIAN","R","prepareOccurrencesCalibration.R"))



