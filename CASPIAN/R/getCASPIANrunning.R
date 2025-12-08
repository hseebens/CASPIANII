##########################################################################
#
# Mit diesem Skript kann das Modell CASPIAN gestartet werden. Änderungen 
# der Voreinstellungen (z.B. Auswahl der Ausbreitungspfade, Startpunkte)
# können im Skript ConfigFile.R im Verzeichnis CASPIAN/Config/ vor-
# genommen werden.
#
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 08.12.25
##########################################################################


## Bereinigung der Arbeitsumgebung #######################################
graphics.off()
rm(list=ls())


##########################################################################
## Arbeitsverzeichnis des Modells
## Das Arbeitsverzeichnis wird automatisch beim Öffnen des R Projekts
## "CASPIANII.Rproj" festgelegt (Verzeichnis, in dem die Datei CASPIANII.Rproj
## vorliegt) und muss daher nicht extra spezifiziert werden.

# setwd(file.path("Path", "To", "folder"))


##########################################################################
## Laden weiterer R Skripte ##############################################
source(file.path("LadeSkripte.R")) 


##########################################################################
## Lade und installiere (sofern noch nicht geschehen) notwendige R Pakete 
LadePakete()

##########################################################################
## Name zum Verzeichnis, in dem Netzwerkdaten für CASPIAN gespeichert sind 
path2data <- file.path("CASPIAN","Daten", "Input")


##########################################################################
## Pfadname zu configFile.R
## configFile.R beinhaltet alle Angaben zur Konfiguration von CASPIAN
configFile <- file.path("CASPIAN","Configuration","configFile.R")

modelResults <- runCASPIAN(configFile=configFile, path2data=path2data)

