##################### Erstellung einer Liste von Neobiota für Deutschland #####################################
# 
# Dieses Skript führt den workflow "AlienListGenerator" aus, bei dem einzelne Listen von Neobiota 
# standardisiert und integriert werden. Hierzu wird eine Reihe von Skripten ausgeführt, die in fünf 
# Arbeitsschritte aufgeteilt sind:
#
# 1. Standardisierung der Rohdatensätze: Die Spaltennamen und Einträge der Rohdatensätze werden standardisiert.
# 2. Integration der standardisierten Datensätze: Die standardisierten Datensätze werden zu einem Datensatz
#    integriert und Duplikate werden entfernt.
# 3. GBIF Einträge ermitteln: Für jede Art wird die Anzahl an verfügbaren Einträgen in GBIF ermittelt.
# 4. Ergänzung der Ausbringungspfadd: Informationen zum Ausbringspfaden werden sofern vorhanden ergänzt.
# 5. sMon Einträge ermitteln: Für jede Art wird die Anzahl an verfügbaren Einträgen in sMon ermittelt.
# 
# Weitere Informationen befinden sich in der Veröffentlichung...
#
# Prokect: CASPIAN II
# 
# Senckenberg Gesellschaft für Naturforschung, 04.11.22
###############################################################################################################


## Bereinigung der Arbeitsumgebung ############
graphics.off()
rm(list=ls())

## Laden benötigter Pakete ####################
library(rgbif)
library(openxlsx)
library(data.table)
library(robis)
library(spocc)

setwd("/home/hanno/Bioinvasion/CASPIANII")

## Laden weiterer R Skripte ####################
source(file.path("WP1","R","CASPIANII_loadScripts.R"))


##########################################################################
## Generierung der Liste von Neobiota ####################################
##########################################################################


## Parameter zur Einstellung #######################################

## Namen von Datensätzen in "ListeGebietsfremderArten_einzelneDB_Rohdaten.xlsx" mit verfügbaren Informationen zu Ausbringspfaden
Pfad_Datensaetze <- c("Tackenberg_2017","BfN","EASIN_Germany") # pathway_datasets

## lokales Verzeichnis der sMon Daten (können hier bezogen werden: https://idata.idiv.de/ddm/Data/ShowData/1875?version=9)
sMon_folder <- "/home/hanno/Storage_large/Species/sMon"


## Arbeitsschritte #########################

## Step 0: Get all data into a spreadsheet (.xlsx) with each data set on one sheet and taxon names in columns 
# called either "WissenschaftlicherName", "Taxon" or "scientificName". 
# File has to be named "ListeGebietsfremderArten_Rohdaten.xlsx" or this should be changed in scripts

## Schritt 0 - Vorabeiten: 
# Vor Durchführung dieses Workflows müssen die zu integrierenden Listen der Neobiota
# in der Datei "ListeGebietsfremderArten_Rohdaten.xlsx" zusammengeführt werden. Jede Liste
# muss in ein Tabellenblatt kopiert werden, welches mit dem Kürzel der Quelle beschriftet
# sein sollte. Jedes Tabellenblatt sollte als Mindestanforderung eine Spalte mit Namen 
# der Organismen haben, die mit "WissenschaftlicherName", "Taxon" or "scientificName" 
# überschrieben sein muss.

## Schritt 1 - Vorbereiten der Datensätze: Datensätze werden eingelesen, Spaltenname und
# taxonomische Namen standardisiert.

cat("\nSchritt 1: Standardisierung der Datensätze\n")
run_DataStandardisation(Pfad_Datensaetze)


## Schritt 2 - Integration: Datensätze werden integriert und Duplikate entfernt

cat("\nSchritt 2: Integration der Datensätze\n")
run_IntegrateAlienSpeciesDataSets()


## Schritt 3 - GBIF Einträge: Ermittlung der verfügbaren Einträge auf GBIF für jede Art

cat("\nSchritt 3: Ermittlung der Anzahl an GBIF Einträgen\n")
get_GBIFrecords()


## 4. step: Add pathway information from Saul et al. 2017
## Schritt 4 - Pfade: Ergänzung von Informationen zu Pfade der Einbringung und Ausbreitung

cat("\nSchritt 4: Ergänzung von Pfadinformationen\n")
get_pathways()


## Schritt 5 - sMon Einträge: Ermittlung der Einträge von verfügbaren sMon Daten

cat("\nSchritt 5: Ermittlung der Anzahl an sMon Einträge\n")
get_nRecords_sMon(sMon_folder)



