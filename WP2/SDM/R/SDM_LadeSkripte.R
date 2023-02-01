###########################################################################################################
#
# Dieses Skript lädt alle notwendigen Skripte.
#
# Autor: Hanno Seebens, 01.12.22
##########################################################################################################


###########################################################################
### lade Skripte ##########################################################

source(file.path("R","SDM_LadePakete.R"))

# data preparation
source(file.path("R","SDM_ermittleVorkommen.R"))
source(file.path("R","SDM_sammleVorkommenOnline.R"))

source(file.path("R","SDM_ermittleUmweltdaten.R"))

source(file.path("R","SDM_generiereAbsenzDaten.R"))

# # model fitting and validation
source(file.path("R","SDM_Vorhersage_alleLäufe.R"))
source(file.path("R","SDM_fitModel.R"))
# 
# # suitability prediction
source(file.path("R","SDM_erstelleKarte.R"))
# source(file.path("R","loadBaseEnvLC2.R"))
# source(file.path("R","predictGAMParallel.R"))
# source(file.path("R","modelaverageParallel.R"))
# source(file.path("R","modelaverageParallelLC.R"))
# source(file.path("R","plotSuitabilities.R"))
# source(file.path("R","SuitabilityNet.R"))