##################### Ermittlung von Vorkommensdaten fuer einzelne Arten ######################################
# 
# Das Skript buendelt Vorkommensdaten von einzelnen Arten aus verschiedenen Datenquellen (GBIF, iNaturalist,
# sMon und OBIS). Alternativ koennen eigene Datensaetze eingelesen werden.
# Die Vorkommen koennen als Karte nach Datenquellen unterschieden dargestellt werden.
#
# 
# Projekt: CASPIAN II
# 
# Senckenberg Gesellschaft fuer Naturforschung, 28.11.22
###############################################################################################################


## Bereinige Arbeitsumgebung #############################################
graphics.off()
rm(list=ls())


##########################################################################
## Lade Funktionen #######################################################

source(file.path("LadeSkripte.R"))


##########################################################################
## Lade und installiere (sofern noch nicht geschehen) notwendige R Pakete 

LadePakete()


##########################################################################
## Ueberpruefe und erstelle Verzeichnisstruktur (sofern noch nicht geschehen) 

ueberpruefe_Datenverzeichnis()


##########################################################################
## Laden der Vorkommensdaten #############################################
##########################################################################


##########################################################################
## Parameter zur Spezifizierung der Suche ################################

TaxonName <- "Crassostrea gigas"
# TaxonName <- "Impatiens glandulifera"

## Verzeichnis der sMon Datensaetze (optional)
sMon_Verzeichnis <- "C:/Hanno/Storage_large/Species/sMon"

## Berechnete Wahrscheinlichkeit des Auftretens der Art (0 (sehr unwahrscheinlich) - 1 (sehr wahrscheinlich))
sMon_Wahrscheinlichkeit <- 0.5

## Verzeichnis und Dateiname des eigenen Datensatzes (optional)
EigeneDaten_Verzeichnis <- "" # Name des Verzeichnis
EigeneDaten_Dateiname <- "" # Name der Datei

## Ausgewaehlte Datenbank zur Ermittlung von Vorkommensdaten 
## (moegliche Auswahl: OBIS, GBIF, iNat, sMon)
Datenbank <- c("OBIS","GBIF","iNat","sMon")#
Datenbank <- c("sMon","OBIS","iNat")#

## Auswahl des geographischen Bereichs, in dem nach Vorkommensdaten gesucht
## werden soll (Koordinaten (Laengengrad+Breitengrad) der linken unteren und rechten oberen Ecke eines
## Rechecks) (xmin,ymin,xmax,ymax)
Ausschnitt <- c(5,45,15,58) # lower left and upper right corner (long-lat)


##########################################################################
## Ermittle Vorkommen in den ausgewaehlten Datenquellen ##################

Vorkommen <- Vorkommen_bezieheDaten(TaxonName=TaxonName,
                                    Datenbank=Datenbank,
                                    sMon_Verzeichnis=sMon_Verzeichnis,
                                    sMon_Wahrscheinlichkeit=sMon_Wahrscheinlichkeit,
                                    Ausschnitt=Ausschnitt,
                                    max_limit=20000)



##########################################################################
## Darstellung der Vorkommen auf einer Karte #############################

Vorkommen_erstelleKarte(Vorkommen)
# Vorkommen_erstelleKarte(Vorkommen, Jahr=2010:2015)

