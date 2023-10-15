
## Bereinigung der Arbeitsumgebung #######################################
graphics.off()
rm(list=ls())


# setwd("C:/Hanno/Bioinvasion/CASPIANII/CASPIANII")

##########################################################################
## Laden weiterer R Skripte ##############################################
source(file.path("LadeSkripte.R")) 


##########################################################################
## Lade und installiere (sofern noch nicht geschehen) notwendige R Pakete 
LadePakete()

##########################################################################
## Name zum Verzeichnis, in dem Netzwerkdaten für CASPIAN gespeichert sind 
path2data <- file.path("..","..","..","..","EBAspread","Data","FinalDataFiles")


##########################################################################
## Pfadname zu configFile.R
## configFile.R beinhaltet alle Angaben zur Konfiguration von CASPIAN
configFile <- file.path("CASPIAN","Configuration","configFile.R")
modelResults <- runCASPIAN(configFile=configFile,path2data=path2data)

