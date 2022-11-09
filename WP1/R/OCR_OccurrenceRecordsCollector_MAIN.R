##################### Ermittlung von Vorkommensdaten für einzelne Arten ######################################
# 
# Das Skript bündelt Vorkommensdaten von einzelnen Arten aus verschiedenen Datenquellen (GBIF, iNaturalist,
# sMon und OBIS). Alternativ können eigene Datensätze eingelesen werden.
# Die Vorkommen können als Karte nach Datenquellen unterschieden dargestellt werden.
#
# 
# Projekt: CASPIAN II
# 
# Senckenberg Gesellschaft für Naturforschung, 08.11.22
###############################################################################################################


## Bereinige Arbeitsumgebung ##########################
graphics.off()
rm(list=ls())

## Laden notwendiger Pakete #####################
library(rgbif)
library(robis)
library(spocc)
library(leaflet)
library(data.table)



## Laden notwendiger Funktionen ####################
source(file.path("WP1","R","CASPIANII_loadScripts.R"))



##########################################################################
## Laden der Vorkommensdaten #############################################
##########################################################################


## Parameter zur Spezifizierung der Suche ###############################

## Verzeichnis der sMon Datensätze (optional)
sMon_folder <- "/home/hanno/Storage_large/Species/sMon"

## Verzeichnis und Dateiname des eigenen Datensatzes (optional)
owndata_folder <- "" # Name des Verzeichnis
owndata_filename <- "" # Name der Datei

## Ausgewählte Datenbank zur Ermittlung von Vorkommensdaten 
## (mögliche Auswahl: OBIS, GBIF, iNat, sMon)
database <- c("OBIS","GBIF","iNat")# 

## Auswahl des geographischen Bereichs, in dem nach Vorkommensdaten gesucht
## werden soll (Koordinaten (Laengengrad+Breitengrad) der linken unteren und rechten oberen Ecke eines
## Rechecks) (xmin,ymin,xmax,ymax)
bounding_box <- c(5,45,15,58) # lower left and upper right corner (long-lat)


## Ermittle Vorkommen in den ausgewählten Datenquellen ###########

records <- get_occurrence_records(taxon_name="Rhaponticum repens",database=c("sMon","GBIF","iNat"),sMon_folder=sMon_folder)



## Darstellung der Vorkommen auf einer Karte ###################################

map_records(records)

