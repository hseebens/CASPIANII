###########################################################################################################
#
# This script runs the SDM workflow for automated prediction of environmental suitabaility of a focal area
# for a focal species
# This workflow can be combined with CASPIAN
#
# Author: Hanno Seebens, Senckenberg Gesellschaft für Naturforschung
##########################################################################################################

graphics.off()
rm(list=ls())


##########################################################################################################
## Lade Funktionen #######################################################################################

source(file.path("R","SDM_LadeSkripte.R")) 


##########################################################################################################
## Lade und installiere (sofern noch nicht geschehen) notwendige R Pakete ################################

LadePakete()


##########################################################################################################
## Erstelle Unterordner ##################################################################################

# dir.create("output")
# dir.create("output/input")
# dir.create("output")


##########################################################################################################
## Lade Artenliste #######################################################################################

neobiota <- read.xlsx(file.path("Data","Input","ListeGebietsfremderArten_gesamt_standardisiert.xlsx"),sheet=1)

## Entfernt Einträge, die nur aus EASIN stammen (abweichende Definition)
neobiota <- subset(neobiota,Datenbank!="EASIN")

## Filter nach Arten mit ausreichend Datenpunkten (>100) und nicht zu vielen Datenpunkten (<5000), da 
## Simulationen sehr lange dauern würden
# artenliste <- subset(neobiota,Eintraege_GBIF_DE<10000 & Eintraege_GBIF_DE>50)$Taxon 
artenliste <- subset(neobiota,Eintraege_GBIF_DE>10000)$Taxon 


##########################################################################################################
## Parameter zur Modellierung ############################################################################

## Name des jeweiligen Modelllaufs (frei vom Nutzer zu wählen)
identifier <- "191222_NoEASIN" # a unique identifier for every run of the SDM workflow, needs to be a character

## Variablen zur Vorhersage der Habitate ##########################################

## Maximale Anzahl der Datenpunkte, die von jeder ausgewählten Datenbank höchstens bezogen werden soll.
## Hinweise: iNaturalist erlaubt keine downloads >10000 Datenpunkte. 
## Downloads großer Datenmengen (>10000 Einträge) können effizienter direkt über die jeweilige Webseite sein.
max_limit <- 30000

## Klimatische Variablen (Gesamtliste und Übersetzung: https://www.worldclim.com/bioclim):
Klima_var <- c("bio1","bio12","bio4") #  "tmin", "tmax", "tavg", "prec" and "bio" environmental variables of choice, user should insert the names of the desired variables as character as shown here

## Variablen zu Landbedeckung
# LC2+LC3: urbane Regionen; LC12: Acker; LC18: Weideland/Grasland; LC23+LC24+LC25: Wälder; LC40+LC41: Binnengewässer
# Die Bedeckung mit den Variablen LC2, LC3, LC12, LC18, LC23, LC24, LC25 entsprechen 94% der Fläche von Deutschland
Landnutz_var <- c("LC2","LC3", "LC12","LC18","LC23","LC24","LC25","LC40","LC41")

## Geographischer Ausschnitt zum Fitten des Modells (Ausschnitt_ModellFit) und zur Vorhersage/Extrapolation der Ergebnisse (Ausschnitt_Extrapolation)
## Angaben beschreiben die Ausdehnung eines Rechtecks (long/lat für linke, untere und rechte, obere Ecke hintereinander)
Ausschnitt_ModellFit <- (c(-30,25,40,78)) # Grenzen von Europa (Modell wird für alle Vorkommen in Europa angefittet)
Ausschnitt_Extrapolation <- (c(3,47,17,55)) # Grenzen von Deutschland zur Extrapolation und Darstellung der Ergebnisse

## Anzahl der Läufe zur Generierung von Absenzdaten (pseudo absences) 
## Gesamtzahl der Modellläufe = n_AbsenzLaufe * n_Modelllaeufe
n_AbsenzDaten <- 5

## Anzahl der Modelläufe zur Evaluierung der Modellergebnisse (Validierung)
n_Modelllaeufe <- 5 

still_some <- TRUE

# for (i in 1:length(artenliste)){ # Schleife über alle Arten zur Berechnung der Habitateignung
while (still_some){
  
  all_files <- list.files("Data/Input")
  Vorkommen_alle <- all_files[grep("Vorkommen_",all_files)]
  Vorkommen_alle <- Vorkommen_alle[grep(identifier,Vorkommen_alle)]
  Vorkommen_alle <- sort(Vorkommen_alle[grep(".csv",Vorkommen_alle)])
  available <- gsub("Vorkommen_|.csv","",Vorkommen_alle)
  available <- gsub(identifier,"",available)
  available <- gsub("_","",available)
  
  # available <- gsub(identifier,"",available)
  # available <- gsub("_","",available)
  # still_to_do <- artenliste[!artenliste%in%available]
  
  # # finished <- list.files(file.path("Data","Output"))
  # finished <- list.files(file.path("..","..","..","..","Storage_large","CASPIANII","Modelrun_191222"))
  # finished <- finished[grep("HabitatEignung_",finished)]
  # finished <- finished[grep(".gz",finished)]
  # finished <- gsub("HabitatEignung_|.gz","",finished)
  # finished <- gsub(identifier,"",finished)
  # finished <- gsub("_","",finished)

  req_files <- paste(artenliste,"_",identifier,sep="")
  still_to_do <- artenliste[!artenliste%in%available]
  
  # still_to_do <- available[!available%in%finished]
  
  if (length(still_to_do)>0){
      
  ## Taxonname
  # TaxonName <- artenliste[i]
  TaxonName <- still_to_do[1]
  # TaxonName <- "Astrantia major"
  
  ##########################################################################################################
  ## Datenermittlung #######################################################################################
  
  ## Aufbereitung aller notwendiger Daten (Vorkommen der Art, Umweltdaten und Pseudo-Absence Daten)
  
  ## Ermittlung und Aufbereitung der Vorkommensdaten #######################################################
  Datenbank <- c("OBIS","GBIF","iNat")# 
  
  Vorkommen <- ermittle_vorkommen(TaxonName=TaxonName,
                                  Datenbank=Datenbank,
                                  Ausschnitt=Ausschnitt_ModellFit,
                                  identifier=identifier,
                                  max_limit=max_limit)
  ## Lade existierende Datei von Festplatte:
  # Vorkommen <- fread(file.path("Data","Input",paste0("Vorkommen_",TaxonName,"_",identifier,".csv"))) # stores the final occurrence file on the users computer
  
  ## Karte....
  
  
  if (is.null(Vorkommen)){
    artenliste <- artenliste[artenliste!=TaxonName]
  }
  } else {
    still_some <- FALSE
  } 

}

  ## Kombiniere Vorkommensdaten und Umweltdaten ####################################################
  # loads desired environmental variables, checks for correlation of these variables accross the study region, extracts environmental variables for the occurrence records. If the correlation among environmental variables is too high, the user needs to remove them from envir and run this step again
  VorkommenUmwelt <- ermittleUmweltdaten(TaxonName,
                                         Vorkommen=Vorkommen,
                                         identifier=identifier,
                                         Klima_var,
                                         Landnutz_var,
                                         Ausschnitt=Ausschnitt_ModellFit,
                                         plot_predictors=T)
#   ## Lade existierende Datei von Festplatte:
#   # VorkommenUmwelt <- fread(file.path("Data","Input",paste0("Vorkommen+Umweltdaten_",TaxonName,"_",identifier,".csv"))) # stores the final occurrence file on the users computer
#   
#   ## Generiere Pseudo-Absence Daten #################################################################
#   # samples 10 sets of pseudoabsences, extracts environment info for the pseudoabsences, attaches everything to the occurence table, creates a list with the ten occurrence-pseudabsence datasets
#   VorkommenUmweltPA <- generiereAbsenzDaten(TaxonName=TaxonName,
#                                             VorkommenUmwelt=VorkommenUmwelt, 
#                                             n_AbsenzDaten=n_AbsenzDaten,
#                                             speichern=T,
#                                             identifier=identifier)
#   ## Lade existierende Datei von Festplatte:
#   # load(file=file.path("Data","Input", paste0("PAlist_",TaxonName,"_",identifier,".RData"))) # load file 'PAlist'
#   # VorkommenUmweltPA <- PAlist
# 
# 
#   ##########################################################################################################
#   ## fit und validiere Modell ###############################################################################
#   # splits each pseudoabsence dataset into 10 random 30-70 datasplits and fits and evaluates one GAM with
#   # each data split, returns an object called modelruns100
# 
#   Modelllaeufe <- fit_SDMs(TaxonName=TaxonName,
#                            VorkommenUmweltPA=VorkommenUmweltPA,
#                            n_Modelllaeufe=n_Modelllaeufe)
#   ## Lade existierende Datei von Festplatte:
#   # load(file=file.path("Data","Output", paste0("ModelFit_",TaxonName,"_",identifier,".RData"))) # lade Datei 'modelruns'
#   # Modelllaeufe <- modelruns
# 
#   ###########################################################################################################
#   ## generiere Vorhersage ###################################################################################
# 
#   # # predicts environmental suitability based on models with a sufficiently good quality (AUC > 0.7)
#   HabitatEignung <- Vorhersage_alleLaeufe(TaxonName=TaxonName,
#                                           Modelllaeufe=Modelllaeufe,
#                                           Ausschnitt=Ausschnitt_Extrapolation,
#                                           speichern=T,
#                                           identifier=identifier)
#   
#   ## Lade existierende Datei von Festplatte:
#   # HabitatEignung <- fread(file=file.path("Data","Output", paste0("HabitatEignung_",TaxonName,"_",identifier,".gz"))) 
# 
#   ## Erstelle Karte der Habitateignung
#   rasterHabitatEignung <- erstelleKarteHabitatEignung(HabitatEignung=HabitatEignung,
#                                                       Vorkommen=Vorkommen) #
# 
# }
