##################### Erstellung einer Liste von Neobiota fuer Deutschland #####################################
# 
# Dieses Skript fuehrt den workflow "AlienListGenerator" aus, bei dem einzelne Listen von Neobiota 
# standardisiert und integriert werden. Hierzu wird eine Reihe von Skripten ausgefuehrt, die in fuenf 
# Arbeitsschritte aufgeteilt sind:
#
# 1. Standardisierung der Rohdatensaetze: Die Spaltennamen und Eintraege der Rohdatensaetze werden standardisiert.
# 2. Integration der standardisierten Datensaetze: Die standardisierten Datensaetze werden zu einem Datensatz
#    integriert und Duplikate werden entfernt.
# 3. GBIF Eintraege ermitteln: Fuer jede Art wird die Anzahl an verfuegbaren Eintraegen in GBIF ermittelt.
# 4. Ergaenzung der Ausbringungspfadd: Informationen zum Ausbringspfaden werden sofern vorhanden ergaenzt.
# 5. sMon Eintraege ermitteln: Fuer jede Art wird die Anzahl an verfuegbaren Eintraegen in sMon ermittelt.
# 
# Weitere Informationen befinden sich in der Veroeffentlichung...
#
# Projekt: CASPIAN II
# 
# Autor: Hanno Seebens, Senckenberg Gesellschaft fuer Naturforschung, 31.01.23
###############################################################################################################


## Bereinigung der Arbeitsumgebung #######################################
graphics.off()
rm(list=ls())


##########################################################################
## Laden weiterer R Skripte ##############################################

source(file.path("LadeSkripte.R"))


##########################################################################
## Lade und installiere (sofern noch nicht geschehen) notwendige R Pakete 

LadePakete()


##########################################################################
## Generierung der Liste von Neobiota ####################################
##########################################################################


## Parameter zur Einstellung #############################################

## Namen von Datensaetzen in "ListeGebietsfremderArten_einzelneDB_Rohdaten.xlsx" mit verfuegbaren Informationen zu Ausbringspfaden
Pfad_Datensaetze <- c("Tackenberg_2017","BfN","EASIN_Germany")

## lokales Verzeichnis der sMon Daten (koennen hier bezogen werden: https://idata.idiv.de/ddm/Data/ShowData/1875?version=9)
sMon_Verzeichnis <- "/home/hanno/Storage_large/Species/sMon"


##########################################################################
## Arbeitsschritte #######################################################

## Schritt 0 - Vorabeiten: 
# Vor Durchfuehrung dieses Workflows muessen die zu integrierenden Listen der Neobiota
# in der Datei "ListeGebietsfremderArten_Rohdaten.xlsx" zusammengefuehrt werden. Jede Liste
# muss in ein Tabellenblatt kopiert werden, welches mit dem Kuerzel der Quelle beschriftet
# sein sollte. Jedes Tabellenblatt sollte als Mindestanforderung eine Spalte mit Namen 
# der Organismen haben, die mit "WissenschaftlicherName", "Taxon" or "scientificName" 
# ueberschrieben sein muss.

##########################################################################
## Schritt 1 - Vorbereiten der Datensaetze: Datensaetze werden eingelesen, Spaltenname und
## taxonomische Namen standardisiert.

cat("\nSchritt 1: Standardisierung der Datensaetze\n")
standardisiereDaten(Pfad_Datensaetze)


##########################################################################
## Schritt 2 - Integration: Datensaetze werden integriert und Duplikate entfernt

cat("\nSchritt 2: Integration der Datensaetze\n")
integriereDatensaetze()


##########################################################################
## Schritt 2a - entferne einheimische Pflanzenarten basierend auf BfN Skripte 519

cat("\nSchritt 2a: entferne einheimische Pflanzenarten\n")
bereinigeListe()


##########################################################################
## Schritt 3 - GBIF Eintraege: Ermittlung der verfuegbaren Eintraege auf GBIF fuer jede Art

cat("\nSchritt 3: Ermittlung der Anzahl an GBIF Eintraegen\n")
bezieheGBIFDaten()


##########################################################################
## 4. step: Add pathway information from Saul et al. 2017
## Schritt 4 - Pfade: Ergaenzung von Informationen zu Pfade der Einbringung und Ausbreitung

cat("\nSchritt 4: Ergaenzung von Pfadinformationen\n")
beziehePfadDaten()


##########################################################################
## Schritt 5 - sMon Eintraege: Ermittlung der Eintraege von verfuegbaren sMon Daten

cat("\nSchritt 5: Ermittlung der Anzahl an sMon Eintraege\n")
ermittlesMonDaten(sMon_Verzeichnis)

##########################################################################
## Schritt 6: Identifiziere Arten mit starkem Anstieg an Eintraegen
######## ????????????????????????? 

cat("\nSchritt 6: Ermittlung der Anzahl an sMon Eintraege\n")
ermittlesMonDaten(sMon_Verzeichnis)


