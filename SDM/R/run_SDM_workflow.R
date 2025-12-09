################################################################################
#
# Dieses Skript startet den Workflow "SDM" zur Vorhersage der Habitateignung 
# von Arten mit Vorkommen in Deutschland. Der Workflow besteht aus sieben 
# Arbeitsschritte, die nacheinander ausge# fuehrt werden. Eine Beschreibung der 
# einzelnen Arbeitsschritte befindet sich im Abschlussbericht des # Projekt-
# vorhabens "Erweiterung des Modells CASPIAN zur Prognose der Einfuhr und 
# Ausbreitung von invasiven Arten durch verschiedene Verkehrstraeger (CASPIAN II)". 
#
# Hinweis: Bevor dieses Skript gestartet wird, sollte das Skript 
# "SDM_bezieheHoheDatenmengen.R" ausgeführt werden. Diese Skript extrahiert 
# Vorkommensdaten für Arten mit besonders vielen Datenpunkte (>>10.000 Einträge), 
# die mit dem Skript run_SDM_workflow.R nur ineffizient verarbeitet werden können.
# Das Skript SDM_bezieheHoheDatenmengen.R extrahiert die Daten robuster und 
# effizienter. Nach Ausführen des Skripts SDM_bezieheHoheDatenmengen.R liegen 
# die Vorkommensdaten lokal vor und werden von run_SDM_workflow.R automatisch 
# erkannt und verwendet.
#
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 08.12.25
################################################################################

## Bereinigen der Arbeitsumgebung
graphics.off()
rm(list=ls())


##########################################################################################################
## Konfiguration der Habitatmodellierung #################################################################
##########################################################################################################

# setwd(file.path("C:","Users","3DScanner1","Desktop","Hanno","CASPIAN SDM"))

##########################################################################################################
## Artenliste ############################################################################################

# Name des Datensatzes, welches die Artenliste enthaelt. Dies muss eine .xlsx Datei sein mit den Spalten "Taxon"
# und "Eintraege_GBIF_DE" sein. Eine entsprechende Datei mit dem Workflow "ListeNeobiota" generiert.

Name_Artenliste <- "ListeGebietsfremderArten_gesamt_standardisiert.xlsx"

# Limits der Datenmenge fuer eine Habitatmodellierung (nur auf GBIF bezogen; weitere Datenpunkte 
# aus anderen Datenbanken zusaetzlich bezogen werden.)
Min_Anzahl_GBIF <- 50 # min Anzahl an Eintraegen fuer Europa; sollte 50 nicht unterschreiten
Max_Anzahl_GBIF <- 10000 # max Anzahl an Eintraegen fuer Europa; sollte 20000 nicht ueberschreiten


##########################################################################################################
## Parameter zur Modellierung ############################################################################

## Name des jeweiligen Modelllaufs (frei vom Nutzer zu waehlen)
identifier <- "GiveItAName" # eine eindeutige Kennzeichnung des Modelllaufs (z.B. Datum)

## Variablen zur Vorhersage der Habitate ##########################################

## Klimatische Variablen (Gesamtliste und uebersetzung: https://www.worldclim.com/bioclim).
# Die Codes des Variablen findet man entweder auf der Homepage oder im Abschlussbericht des Projektvorhabens.
Klima_var <- c("bio1","bio12","bio4","bio8", "bio15", "bio19") 

## Variablen zu Landbedeckung
# LC2+LC3: urbane Regionen; LC12: Acker; LC18: Weideland/Grasland; LC23+LC24+LC25: Waelder; LC40+LC41: Binnengewaesser
# Die Bedeckung mit den Variablen LC2, LC3, LC12, LC18, LC23, LC24, LC25 entsprechen 94% der Flaeche von Deutschland.
# Die Codes der Variablen findet man im Abschlussbericht des Projekvorhabens.
Landbedeck_var <- c("LC2","LC3", "LC12","LC18","LC23","LC24","LC25","LC40","LC41")

## Geographischer Fokus ############################################################
# Geographischer Ausschnitt zum Fitten des Modells (Ausschnitt_ModellFit) und zur Vorhersage/Extrapolation der Ergebnisse (Ausschnitt_Extrapolation)
# Angaben beschreiben die Ausdehnung eines Rechtecks (long/lat fuer linke, untere und rechte, obere Ecke hintereinander)
Ausschnitt_ModellFit <- c(-30,25,40,78) # Grenzen von Europa (Modell wird fuer alle Vorkommen in Europa angefittet)
Ausschnitt_Extrapolation <- c(5,47,16,55.5) # Grenzen von Deutschland zur Extrapolation und Darstellung der Ergebnisse

## Anzahl der Laeufe zur Generierung von Absenzdaten (pseudo absences) ##############
# Gesamtzahl der Modelllaeufe = n_AbsenzLaufe * n_Modelllaeufe
n_AbsenzDaten <- 5

## Anzahl der Modellaeufe zur Evaluierung der Modellergebnisse (Validierung) ########
# Gesamtzahl der Modelllaeufe = n_AbsenzLaufe * n_Modelllaeufe
n_Modelllaeufe <- 5

###########################################################################################################
## Ende: Konfiguration der Habitatmodellierung ############################################################
###########################################################################################################



###########################################################################################################
##### Beginn: Habitatmodellierung #########################################################################
###########################################################################################################
## In den folgenden Zeilen muessen keine Anpassungen mehr vorgenommen werden. Die obigen 
## Konfigurationen werden uebernommen.
###########################################################################################################

## Schritt 0: Vorbereitung der Habitatmodellierung ########################################################


###########################################################################################################
## Lade Funktionen ########################################################################################

source(file.path("LadeSkripte.R")) 

###########################################################################################################
## Lade und installiere (sofern noch nicht geschehen) notwendige R Pakete #################################

LadePakete()


###########################################################################################################
## Ueberpruefe und erstelle Verzeichnisstruktur (sofern noch nicht geschehen) #############################

ueberpruefeVerzeichnisse()

## Standard des identifiers
if (strtrim(identifier,1)!="_"){
  identifier <- paste0("_",identifier)
}

###########################################################################################################
## Erstelle log-file zum Speichern der Meldungen und Einstellungen ########################################

if (file.exists(file.path("SDM","Daten","Output",paste0("Logfile", identifier, ".txt")))){
  sink(file.path("SDM", "Daten", "Output", paste("Logfile", identifier, ".txt", sep="")), append=TRUE) # Oeffne log-file wenn bereits vorhanden,
} else {
  sink(file.path("SDM", "Daten", "Output", paste("Logfile", identifier, ".txt", sep="")), append=FALSE) # erstelle neues log-file
}

cat(paste0("********************************************** ", Sys.time(), " ********************************************** \n"))
cat(paste0("Name des Durchlaufs: ", identifier," \n"))
cat(paste0("Name der Artenliste: ", Name_Artenliste," \n"))
cat(paste0("\nParameter:"," \n"))
cat(paste0(" Min_Anzahl_GBIF: ", Min_Anzahl_GBIF," \n"))
cat(paste0(" Max_Anzahl_GBIF: ", Max_Anzahl_GBIF," \n"))
cat(paste0(c(" Bioklimatische Variablen: ", Klima_var," \n")))
cat(paste0(c(" Landbeckungs-Variablen:", Landbedeck_var," \n")))
cat(paste0(c(" Koordinaten fuer Ausschnitt zum Modellfit: ", Ausschnitt_ModellFit," \n")))
cat(paste0(c(" Koordinaten fuer Ausschnitt zur Extrapolation: ", Ausschnitt_Extrapolation," \n")))
cat(paste0(" Anzahl Absenzdatensaetze: ", n_AbsenzDaten," \n"))
cat(paste0(" Modellaeufe zur Evaluierung: ", n_Modelllaeufe," \n"))



###########################################################################################################
## Ueberpruefe Datensaetze (Artenliste und Daten der Vorhersagevariablen) #################################

ueberpruefeDatensaetze(Klima_var=Klima_var,
                        Landbedeck_var=Landbedeck_var,
                        Name_Artenliste=Name_Artenliste)

###########################################################################################################
## Lade Artenliste ########################################################################################

Artenliste <- importiereArtenliste(Name_Artenliste=Name_Artenliste,
                                   max_limit=Max_Anzahl_GBIF)

###########################################################################################################
## Generiere Tabelle zum Stand der Modellierung fuer jede Art #############################################


erstelleStatusFile(Name_Artenliste=Name_Artenliste,
                   identifier=identifier,
                   ueberschreiben=FALSE)

status_species <- read.xlsx(file.path("SDM","Daten","Output",paste0("StatusModellierung",identifier,".xlsx",sep="")),sheet=1)



##########################################################################################################
## Schleife ueber alle Arten zur Berechnung der Habitateignung
for (i in 1:length(Artenliste)){ #

  ## Auswahl einer Art ###################################################################################
  TaxonName <- Artenliste[i]  ## Taxonname
  # TaxonName <- "Adelges cooleyi"
  # TaxonName <- "Acorus calamus"
  
  cat(paste0("\n\n************* Bearbeitung von ",TaxonName," ************* \n") )
  
  ## Ueberpruefe ob Modellierung bereits durchgefuehrt wurde. Wenn ja, springe zur nächsten Art ##########
  ind_species <- which(status_species$Taxon==TaxonName)
  
  if (status_species[ind_species, "Status"]=="Habitatmodellierung ausgefuehrt."
      | status_species[ind_species, "Status"]=="Nach Bereinigung Datenmenge zu gering zur Habitatmodellierung."
      | status_species[ind_species, "Status"]=="Keine Habitatmodellierung, da es sich um eine marine Art handelt."
      | status_species[ind_species, "Status"]=="Keine Habitatmodellierung, da keine Modelle gefittet werden konnten."
      ){
  # if (status_species[ind_species, "Status"]!="Bisher keine Habitatmodellierung durchgefuehrt."){

    cat(paste0("\n************* Bearbeitung von ",TaxonName," bereits durchgeführt ***** ") )
    cat(paste0("\n************* Status: ",status_species[ind_species, "Status"], " ***** \n"))
    
    next
  }

  ##########################################################################################################
  ## Datenermittlung #######################################################################################
  ## Aufbereitung aller notwendiger Daten (Vorkommen der Art, Umweltdaten und Pseudo-Absenz Daten)

  ## Schritt 1: Ermittlung und Aufbereitung der Vorkommensdaten #############################################

  ## Entweder wird ein vorhandener Datensatz eingelesen oder ein Download vorgenommen.
  if (file.exists(file.path("SDM","Daten","Input",paste0("Vorkommen_",TaxonName,identifier,".csv")))){

    Vorkommen <- fread(file.path("SDM","Daten","Input",paste0("Vorkommen_",TaxonName,identifier,".csv")))

    cat(paste0("\n", nrow(Vorkommen)," Vorkommensdaten werden aus ",paste0("Vorkommen_",TaxonName,identifier,".csv")," im Verzeichnis 'Daten/Input' genommen.\n") ) # notification for the user

  } else {

    Vorkommen <- ermittleVorkommen(TaxonName=TaxonName,
                                   Datenbank=c("OBIS","GBIF","iNat"),
                                   Ausschnitt=Ausschnitt_ModellFit,
                                   identifier=identifier,
                                   min_limit=Min_Anzahl_GBIF,
                                   max_limit=Max_Anzahl_GBIF)
  }
  ## Alternativ: Lade existierende Datei von Festplatte:
  # Vorkommen <- fread(file.path("SDM","Daten","Input",paste0("Vorkommen_",TaxonName,identifier,".csv"))) # stores the final occurrence file on the users computer

  if (!is.data.frame(Vorkommen)){
    next # falls keine Vorkommensdaten bezogen werden konnten, ueberspringe diese Art
  }


  ## Schritt 2: Kombiniere Vorkommensdaten und Umweltdaten ################################################

  VorkommenUmwelt <- ermittleUmweltdaten(TaxonName,
                                         Vorkommen=Vorkommen,
                                         identifier=identifier,
                                         Klima_var,
                                         Landbedeck_var,
                                         Ausschnitt=Ausschnitt_ModellFit,
                                         min_limit=Min_Anzahl_GBIF,
                                         plot_predictors=FALSE,
                                         check_corr=FALSE) # Korrelation der Umweltvariablen kann lange dauern.

  ## Alternativ: Lade existierende Datei von Festplatte:
  # VorkommenUmwelt <- fread(file.path("SDM","Daten","Input",paste0("VorkommenUmweltdaten_",TaxonName,identifier,".csv"))) # stores the final occurrence file on the users computer


  if (!is.data.frame(VorkommenUmwelt)){

    cat(paste0("\n*** Keine Umweltdaten für ",TaxonName," gefunden. ***\n") ) # notification for the user

    cat(paste0("\n************* Bearbeitung von ",TaxonName," beendet ************* \n") ) # notification for the user

    next # falls keine VorkommenUmwelt Daten bezogen werden konnten, ueberspringe diese Art
  }

  ## Schritt 3: Generiere Pseudo-Absence Daten ##############################################################

  VorkommenUmweltPA <- generiereAbsenzDaten(TaxonName=TaxonName,
                                            VorkommenUmwelt=VorkommenUmwelt,
                                            n_AbsenzDaten=n_AbsenzDaten,
                                            speichern=TRUE,
                                            identifier=identifier)

  ## Alternativ: Lade existierende Datei von Festplatte:
  # load(file=file.path("SDM","Daten","Input", paste0("PAlist_",TaxonName,identifier,".RData"))) # load file 'PAlist'
  # VorkommenUmweltPA <- PAlist


  ###########################################################################################################
  ### Eigentliche Habitatmodellierung #######################################################################

  ## Schritt 4: kalibriere (fit) und validiere Modell #######################################################

  Modelllaeufe <- fitSDMs(TaxonName=TaxonName,
                           VorkommenUmweltPA=VorkommenUmweltPA,
                           n_Modelllaeufe=n_Modelllaeufe,
                           identifier=identifier)

  ## Alternativ: Lade existierende Datei von Festplatte:
  # load(file=file.path("SDM","Daten","Output", paste0("ModelFit_",TaxonName,identifier,".RData"))) # lade Datei 'modelruns'
  # Modelllaeufe <- modelruns


  ##########################################################################################################
  ### Bearbeitung der Ergebnisse ###########################################################################
  
  if (!is.list(Modelllaeufe)){  # falls keine Modelle gefittet werden konnten, ueberspringe diese Art
    cat(paste0("\n************* Bearbeitung von ",TaxonName," beendet ************* \n") ) 
    next
  }
  
  ## Schritt 5: generiere Vorhersage #######################################################################

  HabitatEignung <- VorhersageGesamtgebiet(TaxonName=TaxonName,
                                          Modelllaeufe=Modelllaeufe,
                                          Ausschnitt=Ausschnitt_Extrapolation,
                                          speichern=TRUE,
                                          identifier=identifier)

  ## Alternativ: Lade existierende Datei von Festplatte:
  # HabitatEignung <- fread(file=file.path("SDM","Daten","Output", paste0("HabitatEignung_",TaxonName,identifier,".gz")))

  ## Schritt 6: Erstelle Karte der Habitateignung ########################################################

  rasterHabitatEignung <- erstelleKarteHabitatEignung(HabitatEignung=HabitatEignung,
                                                      Vorkommen=Vorkommen,
                                                      identifier=identifier) #

  cat(paste0("\n************************** Habitatmodellierung fuer ",TaxonName," beendet ************************** \n") ) # notification for the user

} # Ende der Habitatmodellierung fuer einzelne Arten


## Schliessen des log files 
cat(paste0("\n************************************************************************************************************ \n"))
cat(paste0("Ende des Durchlaufs ", identifier," \n"))
cat(paste0("********************************************** ", Sys.time(), " ********************************************** \n"))

sink()




###########################################################################################################
## Schritt 7: Synthesekarten zum Vorkommen und Vorhersage #################################################
## Nur moeglich wenn mehrere Arten simuliert wurden (length(Artenliste)>1) !

VorkommenVerzeichnis <- file.path(getwd(), "SDM", "Daten", "Input")
# VorkommenVerzeichnis <- file.path("D:", "Bioinvasion", "CASPIAN II", "Files 230925", "Daten", "Input")

VorhersageVerzeichnis <- file.path(getwd(), "SDM", "Daten", "Output")
# VorhersageVerzeichnis <- file.path("D:", "Bioinvasion", "CASPIAN II", "Files 230925", "Daten", "Output")



if (length(Artenliste)==1){
  warning("\nWarnung: Es koennen keine Synthesekarten erstellt werden, da nur eine Art vorliegt.") 
}

## Verzeichnis mit Datensaetzen zum Vorkommen der Arten (Export von 'ermittle_vorkommen()' )

## Schritt 7a: integriere Vorkommen aller Arten; exportiere Daten und erstelle Karte
erstelleKarte_istVorkommenAlle(VorkommenVerzeichnis=VorkommenVerzeichnis,
                               # VorkommenVerzeichnis=file.path("SDM","Daten","Input"),
                               identifier,
                               Name_Artenliste=Name_Artenliste,
                               Ausschnitt=Ausschnitt_Extrapolation,
                               exportiereKarte=TRUE,
                               max_nTaxa_raster=100,
                               max_nTaxa_gemeinden=300,
                               max_nTaxa_kreise=700)

## Schritt 7b: integriere Habitateignung aller Arten; exportiere Daten und erstelle Karte
erstelleKarte_potVorkommenAlle(VorhersageVerzeichnis=VorhersageVerzeichnis,
                               # VorhersageVerzeichnis=file.path("SDM","Daten","Output"),
                               identifier,
                               Name_Artenliste=Name_Artenliste,
                               Ausschnitt=Ausschnitt_Extrapolation,
                               exportiereKarte=TRUE)
