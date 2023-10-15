# Calibration Script for CASPIAN


## Bereinigung der Arbeitsumgebung #######################################
rm(list=ls())
graphics.off()


##########################################################################
## Arbeitsverzeichnis des Modells
## Das Arbeitsverzeichnis wird automatisch beim Öffnen des R Projekts
## "CASPIANII.Rproj" festgelegt (Verzeichnis, in dem die Datei CASPIANII.Rproj
## vorliegt) und muss daher nicht extra spezifiziert werden.

# setwd("C:/Hanno/Bioinvasion/CASPIANII/CASPIANII")


##########################################################################
## Artname 
## Fuer diese Art werden Vorkommensdaten aus dem Verzeichnis "Vorkommen ->
## Daten" geladen. Diese Datei muss vorher durch die Anwendung des 
## Workflows "Vorkommen" generiert werden.
TaxonName <- "Impatiens glandulifera"


##########################################################################
## Datenbank mit Daten zum Vorkommen der Art
## Die Vorkommen aus den gewaehlten Datenbanken werden fuer die Kalibrierung
## verwendet. Moegliche Datenbanken sind "sMon", "iNat", "GBIF" und "OBIS".
Datenbank <- c("sMon")


##########################################################################
## Name zum Verzeichnis, in dem Netzwerkdaten für CASPIAN gespeichert sind 
# path2data <- file.path("..","..","EBAspread","Data","FinalDataFiles")
# path2data <- file.path("CASPIAN","Daten")


##########################################################################
################### Kalibrierung #########################################

##########################################################################
## Parameter, die zu kalibrieren sind (Parameternamen wie in configFile.R)
ParametersToCalibrate <- c("nat_riverside1","nat_riverside2") #names of the parameters to be calibrated


##########################################################################
## Laden weiterer R Skripte ##############################################
source(file.path("LadeSkripte.R")) 


##########################################################################
## Lade und installiere (sofern noch nicht geschehen) notwendige R Pakete 
LadePakete()

##########################################################################
## Ausfuehrung der Kalibrierung ##########################################
calibrateCASPIAN(path2data = path2data, 
                 # configFile = configFile, 
                 TaxonName = TaxonName,
                 database=Datenbank,
                 networkType="aquatic", # "aquatic" oder "terrestrial"
                 ParametersToCalibrate = ParametersToCalibrate,
                 yearToCalibrate=1996)

