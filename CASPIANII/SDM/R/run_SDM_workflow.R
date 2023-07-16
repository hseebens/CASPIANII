###########################################################################################################
#
# Dieses Skript beinhaltet den workflow "Habitatmodellierung" zur Vorhersage der Habitateignung von Arten
# mit Vorkommen in Deutschland. Der workflow besteht aus sieben Arbeitsschritte, die nacheinander ausge-
# führt werden. Eine Beschreibung der einzelnen Arbeitsschritte befindet sich im Abschlussbericht des 
# Projektvorhabens "Erweiterung des Modells CASPIAN zur Prognose der Einfuhr und Ausbreitung von invasiven 
# Arten durch verschiedene Verkehrsträger (CASPIAN II)". 
#
# Author: Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 05.05.23
##########################################################################################################

## Bereinigen der Arbeitsumgebung
graphics.off()
rm(list=ls())


##########################################################################################################
## Lade Funktionen #######################################################################################

source(file.path("LadeSkripte.R")) 


##########################################################################################################
## Lade und installiere (sofern noch nicht geschehen) notwendige R Pakete ################################

LadePakete()


##########################################################################################################
## Ueberpruefe und erstelle Verzeichnisstruktur (sofern noch nicht geschehen) ############################

ueberpruefe_Verzeichnisse()


##########################################################################################################
## Artenliste #######################################################################################

# Name des files, welches die Artenliste enthält. Dies muss eine .xlsx Datei sein mit den Spalten "Taxon"
# und "Eintraege_GBIF_DE" sein. Eine entsprechende Datei mit dem Workflow "ListeNeobiota" generiert.

Name_Artenliste <- "ListeGebietsfremderArten_gesamt_standardisiert.xlsx"

Min_Anzahl_GBIF_DE <- 50 # sollte 50 nicht unterschreiten
Max_Anzahl_GBIF_DE <- 10000 # sollte 10000 nicht überschreiten


##########################################################################################################
## Parameter zur Modellierung ############################################################################

## Name des jeweiligen Modelllaufs (frei vom Nutzer zu wählen)
identifier <- "191222_NoEASIN" # a unique identifier for every run of the SDM workflow, needs to be a character


## Variablen zur Vorhersage der Habitate ##########################################

## Klimatische Variablen (Gesamtliste und Übersetzung: https://www.worldclim.com/bioclim).
# Die Codes des Variablen findet man entweder auf der Homepage oder im Abschlussbericht des Projektvorhabens.
Klima_var <- c("bio1","bio12","bio4") 

## Variablen zu Landbedeckung
# LC2+LC3: urbane Regionen; LC12: Acker; LC18: Weideland/Grasland; LC23+LC24+LC25: Wälder; LC40+LC41: Binnengewässer
# Die Bedeckung mit den Variablen LC2, LC3, LC12, LC18, LC23, LC24, LC25 entsprechen 94% der Fläche von Deutschland.
# Die Codes der Variablen findet man im Abschlussbericht des Projekvorhabens.
Landbedeck_var <- c("LC2","LC3", "LC12","LC18","LC23","LC24","LC25","LC40","LC41")

## Geographischer Fokus ############################################################
# Geographischer Ausschnitt zum Fitten des Modells (Ausschnitt_ModellFit) und zur Vorhersage/Extrapolation der Ergebnisse (Ausschnitt_Extrapolation)
# Angaben beschreiben die Ausdehnung eines Rechtecks (long/lat für linke, untere und rechte, obere Ecke hintereinander)
Ausschnitt_ModellFit <- c(-30,25,40,78) # Grenzen von Europa (Modell wird für alle Vorkommen in Europa angefittet)
Ausschnitt_Extrapolation <- c(3,47,17,55) # Grenzen von Deutschland zur Extrapolation und Darstellung der Ergebnisse


## Anzahl der Läufe zur Generierung von Absenzdaten (pseudo absences) ##############
# Gesamtzahl der Modellläufe = n_AbsenzLaufe * n_Modelllaeufe
n_AbsenzDaten <- 5

## Anzahl der Modelläufe zur Evaluierung der Modellergebnisse (Validierung) ########
# Gesamtzahl der Modellläufe = n_AbsenzLaufe * n_Modelllaeufe
n_Modelllaeufe <- 5


##########################################################################################################
## Lade Artenliste #######################################################################################

Artenliste <- read.xlsx(file.path("SDM","Data","Input",Name_Artenliste),sheet=1)


## Filter nach Arten mit ausreichend Datenpunkten (>50) und nicht zu vielen Datenpunkten (<10000 in 
# Deutschland), da Simulationen ansonsten sehr lange dauern würden. Für Arten mit großen Datenmenge wurde 
# ein alternativer workflow entwickelt ("SDM_bezieheHoheDatenmengen.R"), mit dem die Daten bezogen und
# aufbereitet werden können. Dieser Schritt würde Schritt 1 unten für Arten mit großen Datenmengen
# ersetzen. Schritte 2-7 können dann für alle Arten gleich durchgeführt werden.
Artenliste <- subset(Artenliste,Eintraege_GBIF_DE<Max_Anzahl_GBIF_DE & Eintraege_GBIF_DE>Min_Anzahl_GBIF_DE)$Taxon


###########################################################################################################
##### Habitatmodellierung #################################################################################
###########################################################################################################

## generiere log-file um Status der Modellierung fuer jede Art aufzufuehren

status_species <- as.data.frame(read.xlsx(file.path("SDM","Data","Input",Name_Artenliste),sheet=1)[,c("Taxon","Eintraege_GBIF_DE")])

status_species$Status <- NA

status_species$Status[status_species$Eintraege_GBIF_DE<Min_Anzahl_GBIF_DE] <- "Keine Habitatmodellierung, da die Datenmenge der Vorkommensdaten nicht ausreicht."
status_species$Status[status_species$Eintraege_GBIF_DE>Max_Anzahl_GBIF_DE] <- "Datenmenge zu groß für diesen Workflow. Bitte alternativen Weg mit 'SDM_bezieheHoheDatenmengen.R' verwenden."

## speichere vorlaeufige Liste des Status aller Arten
write.xlsx(status_species,file=file.path("SDM","Data","Output","Status_Arten.xlsx"))


##########################################################################################################
## Schleife über alle Arten zur Berechnung der Habitateignung
for (i in 1:length(Artenliste)){

  ## Taxonname
  TaxonName <- Artenliste[i]
  # TaxonName <- "Teredo navalis"

  ##########################################################################################################
  ## Datenermittlung #######################################################################################
  ## Aufbereitung aller notwendiger Daten (Vorkommen der Art, Umweltdaten und Pseudo-Absenz Daten)

  ## Schritt 1: Ermittlung und Aufbereitung der Vorkommensdaten #############################################

  Vorkommen <- ermittleVorkommen(TaxonName=TaxonName,
                                 Name_Artenliste=Name_Artenliste,
                                 Min_Anzahl_GBIF_DE=Min_Anzahl_GBIF_DE,
                                 Max_Anzahl_GBIF_DE=Max_Anzahl_GBIF_DE,
                                 Datenbank=c("OBIS","GBIF","iNat"),
                                 Ausschnitt=Ausschnitt_ModellFit,
                                 identifier=identifier,
                                 max_limit=20000)
  ## Alternativ: Lade existierende Datei von Festplatte:
  # Vorkommen <- fread(file.path("SDM","Data","Input",paste0("Vorkommen_",TaxonName,"_",identifier,".csv"))) # stores the final occurrence file on the users computer

  
  ## Schritt 2: Kombiniere Vorkommensdaten und Umweltdaten ################################################
  
  VorkommenUmwelt <- ermittleUmweltdaten(TaxonName,
                                         Vorkommen=Vorkommen,
                                         identifier=identifier,
                                         Klima_var,
                                         Landbedeck_var,
                                         Ausschnitt=Ausschnitt_ModellFit,
                                         plot_predictors=T)
  
  ## Alternativ: Lade existierende Datei von Festplatte:
  # VorkommenUmwelt <- fread(file.path("SDM","Data","Input",paste0("VorkommenUmweltdaten_",TaxonName,"_",identifier,".csv"))) # stores the final occurrence file on the users computer

  
  ## Schritt 3: Generiere Pseudo-Absence Daten ##############################################################
  
  VorkommenUmweltPA <- generiereAbsenzDaten(TaxonName=TaxonName,
                                            VorkommenUmwelt=VorkommenUmwelt,
                                            n_AbsenzDaten=n_AbsenzDaten,
                                            speichern=T,
                                            identifier=identifier)
  
  ## Alternativ: Lade existierende Datei von Festplatte:
  # load(file=file.path("SDM","Data","Input", paste0("PAlist_",TaxonName,"_",identifier,".RData"))) # load file 'PAlist'
  # VorkommenUmweltPA <- PAlist


  ###########################################################################################################
  ### Eigentliche Habitatmodellierung #######################################################################
  
  ## Schritt 4: kalibriere (fit) und validiere Modell #######################################################
  
  Modelllaeufe <- fit_SDMs(TaxonName=TaxonName,
                           VorkommenUmweltPA=VorkommenUmweltPA,
                           n_Modelllaeufe=n_Modelllaeufe,
                           identifier=identifier)
  
  ## Alternativ: Lade existierende Datei von Festplatte:
  # load(file=file.path("SDM","Data","Output", paste0("ModelFit_",TaxonName,"_",identifier,".RData"))) # lade Datei 'modelruns'
  # Modelllaeufe <- modelruns

  
  ##########################################################################################################
  ### Bearbeitung der Ergebnisse ###########################################################################
  
  ## Schritt 5: generiere Vorhersage #######################################################################

  HabitatEignung <- Vorhersage_alleLaeufe(TaxonName=TaxonName,
                                          Modelllaeufe=Modelllaeufe,
                                          Ausschnitt=Ausschnitt_Extrapolation,
                                          speichern=T,
                                          identifier=identifier)

  ## Alternativ: Lade existierende Datei von Festplatte:
  # HabitatEignung <- fread(file=file.path("SDM","Data","Output", paste0("HabitatEignung_",TaxonName,"_",identifier,".gz")))

  ## Schritt 6: Erstelle Karte der Habitateignung ########################################################
  
  rasterHabitatEignung <- erstelleKarteHabitatEignung(HabitatEignung=HabitatEignung,
                                                      Vorkommen=Vorkommen) #

} # Ende der Habitatmodellierung für einzelne Arten



###########################################################################################################
## Schritt 7: Synthese von Vorkommen und Vorhersage #######################################################

## Verzeichnis mit Datensaetzen zum Vorkommen der Arten (Export von 'ermittle_vorkommen()' )

## Schritt 7a: integriere Vorkommen aller Arten; exportiere Daten und erstelle Karte
erstelleKarte_istVorkommenAlle(VorkommenVerzeichnis=file.path("..","..","..","Storage_large","CASPIANII","Vorkommen"),
                               identifier,
                               exportiereKarte=T,
                               rasterKarte=T)

## Schritt 7b: integriere Habitateignung aller Arten; exportiere Daten und erstelle Karte
erstelleKarte_potVorkommenAlle(VorhersageVerzeichnis=file.path("..","..","..","Storage_large","CASPIANII","Modelrun_191222")
                               ,identifier,
                               exportiereKarte=T)
