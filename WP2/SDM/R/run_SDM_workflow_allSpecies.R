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
## load functions ########################################################################################

source(file.path("R","SDM_LadeSkripte.R")) # this loads all functions that are required throughout the SDM workflow


##########################################################################################################
## Automatically install and load required packages, which are not yet in library ########################

LadePakete()


##########################################################################################################
## create outputfolder ###################################################################################

# dir.create("output")
# dir.create("output/input")
# dir.create("output")


##########################################################################################################
## load data ##################################################################

neobiota <- read.xlsx(file.path("Data","Input","ListeGebietsfremderArten_gesamt_standardisiert.xlsx"),sheet=1)
neobiota <- subset(neobiota,Datenbank!="EASIN") # avoid records only reported by EASIN
artenliste <- subset(neobiota,Eintraege_GBIF_DE<5000 & Eintraege_GBIF_DE>100)$Taxon

# occ <- read.csv("occtemp.csv")[-1] # alternatively, the user can insert a table with occurrence records here. This table should be in a .csv-format and be formatted exactly like the example in the manual; if the user does not run the entire analysis at once, she/he can insert the GBIF occurrence table created in  the first step here. 
# # when reading in the file created during this workflow, it is important to remove the first column by adding [-1] as shown here!


##########################################################################################################
## Parameters ##################################################################


##
identifier <- "191222_NoEASIAN" # a unique identifier for every run of the SDM workflow, needs to be a character
# identifier <- "test" # a unique identifier for every run of the SDM workflow, needs to be a character

## Predictors 
Klima_var <- c("bio1","bio12","bio4") #  "tmin", "tmax", "tavg", "prec" and "bio" environmental variables of choice, user should insert the names of the desired variables as character as shown here
Landnutz_var <- c("LC2","LC3", "LC12","LC18","LC23","LC24","LC25","LC40","LC41") #   land cover variables of choice, user should insert the names of the desired variables as character as shown here
# c("LC2","LC3", "LC12","LC18","LC23","LC24","LC25") entsprechen 94% der Fläche von Deutschland
# LC2+LC3: urban; LC12: croplands; LC18: pastures; LC23+LC24+LC25: forests; LC40+LC41: inland waters

# Ausschnitt <-c(NULL) # optional:the extent to which the area for model fitting (!) should be cropped; set to NULL for global/European extent
Ausschnitt_ModellFit <- (c(-30,25,40,78)) # crop to Europe
Ausschnitt_Extrapolation <- (c(3,47,17,55)) # crop to Germany  # lower left and upper right corner (long-lat)

## Anzahl der Läufe zur Generierung von Absenzdaten (pseudo absences) 
## Gesamtzahl der Modellläufe = n_AbsenzLaufe * n_Modelllaeufe
n_AbsenzDaten <- 5

## Anzahl der Modelläufe zur Evaluierung der Modellergebnisse (Validierung)
n_Modelllaeufe <- 5 

still_some <- TRUE

# for (i in 595:length(artenliste)){
while (still_some){

  all_files <- list.files("Data/Input")
  Vorkommen_alle <- all_files[grep("Vorkommen_",all_files)]
  Vorkommen_alle <- Vorkommen_alle[grep(identifier,Vorkommen_alle)]
  Vorkommen_alle <- sort(Vorkommen_alle[grep(".csv",Vorkommen_alle)])
  available <- gsub("Vorkommen_|.csv","",Vorkommen_alle)
  
  # available <- gsub(identifier,"",available)
  # available <- gsub("_","",available)
  # still_to_do <- artenliste[!artenliste%in%available]
  
  # finished <- list.files(file.path("Data","Output"))
  finished <- list.files(file.path("..","..","..","..","Storage_large","CASPIANII","Modelrun_021222"))
  finished <- finished[grep("HabitatEignung_",finished)]
  finished <- finished[grep(".gz",finished)]
  finished <- gsub("HabitatEignung_|.gz","",finished)

  # still_to_do <- available[!available%in%finished]

  if (length(still_to_do)>0){
    
    # print(i)
    # ptm <- proc.time()
    
    ## Taxon Name
    TaxonName <- "Cerastium arvense subsp. arvense x tomentosum" # focal species, user should insert the scientific name as character string in the format shown here
    # TaxonName <- artenliste[i]
    TaxonName <- still_to_do[1]
    
    
    ##########################################################################################################
    ## prepare data ##########################################################################################
    
    ## Aufbereitung aller notwendiger Daten (Vorkommen der Art, Umweltdaten und Pseudo-Absence Daten)
    
    ## Ermittlung und Aufbereitung der Vorkommensdaten 
    Datenbank <- c("OBIS","GBIF","iNat")# 
    
    Vorkommen <- ermittle_vorkommen(TaxonName=TaxonName,Datenbank=Datenbank,Ausschnitt=Ausschnitt_ModellFit,identifier=identifier)
    # fwrite(Vorkommen_alle, file.path("Data","Input",paste0("Vorkommen_",TaxonName,"_",identifier,".csv"))) # stores the final occurrence file on the users computer
    
    if (is.null(Vorkommen)){ # remove taxon from to-do list if no records available
      artenliste <- artenliste[!artenliste%in%TaxonName]
    }
    # print(proc.time()-ptm)
  # }
    # map_records(Vorkommen)
  
  
    # ## Kombiniere Vorkommensdaten und Umweltdaten
    # # loads desired environmental variables, checks for correlation of these variables accross the study region, extracts environmental variables for the occurrence records. If the correlation among environmental variables is too high, the user needs to remove them from envir and run this step again
    # VorkommenUmwelt <- ermittleUmweltdaten(TaxonName,identifier=identifier,Klima_var,Landnutz_var,Ausschnitt=Ausschnitt_ModellFit,plot_predictors=T)
    # 
    # ## Generiere Pseudo-Absence Daten
    # # samples 10 sets of pseudoabsences, extracts environment info for the pseudoabsences, attaches everything to the occurence table, creates a list with the ten occurrence-pseudabsence datasets
    # VorkommenUmweltPA <- generiereAbsenzDaten(TaxonName=TaxonName,VorkommenUmwelt=VorkommenUmwelt, n_AbsenzDaten=n_AbsenzDaten,
    #                                           speichern=T,identifier=identifier)
    # 
    # 
    # 
    # ##########################################################################################################
    # ## fit and validate models ###############################################################################
    # # splits each pseudoabsence dataset into 10 random 30-70 datasplits and fits and evaluates one GAM with
    # # each data split, returns an object called modelruns100
    # 
    # Modelllaeufe <- fit_SDMs(TaxonName=TaxonName,VorkommenUmweltPA=VorkommenUmweltPA,n_Modelllaeufe=n_Modelllaeufe)
    # 
    # 
    # ##########################################################################################################
    # ## predict suitability ###################################################################################
    # 
    TaxonName <- "Rhea americana"
    load(file=file.path("Data","Output", paste0("ModelFit_",TaxonName,"_",identifier,".RData")))
    Modelllaeufe <- modelruns
    # # predicts environmental suitability based on models with a sufficiently good quality (AUC > 0.7)
    HabitatEignung <- Vorhersage_alleLaeufe(TaxonName=TaxonName,Modelllaeufe=Modelllaeufe,Ausschnitt=Ausschnitt_Extrapolation,speichern=T,identifier=identifier)
    # 
    # # load(file=file.path("Data","Output", paste0("Suitability_",TaxonName,"_",identifier,".RData")))
    # # load(file=file.path("Data","Output", paste0("ModelFit_Abies concolor_211122_Europe.RData")))
    # #
    # # HabitatEignung <- all_preds_singlefile
    # 
    # # # averages the predicted environmental suitability across the different models and saves a csv-file with the average suitabilities over the model runs
    # # avgsuitability <- mittlereVorhersage(HabitatEignung)
    # 
    # # transforms the average suitabilities to a raster file, plots them, saves the plot as pdf and returns the raster file
    rasterHabitatEignung <- erstelleKarteHabitatEignung(HabitatEignung,Vorkommen) #
    # # NOTE: If the same plot should be plotted and stored again, make sure the pdf-file with the respective name is closed. Otherwise, R will be unable to overwrite the file and yield an error, when running this step.
    
  } else {
    still_some <- FALSE
  } 
}
