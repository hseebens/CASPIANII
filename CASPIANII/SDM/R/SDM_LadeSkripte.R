###########################################################################################################
#
# Dieses Skript lädt alle notwendigen Skripte.
#
# Autor: Hanno Seebens, 01.12.22
##########################################################################################################


###########################################################################
### lade Skripte ##########################################################

# source(file.path("SDM","R","SDM_LadePakete.R"))

# data preparation
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
source(file.path("SDM","R","SDM_bezieheHoheDatenmengen.R"))
