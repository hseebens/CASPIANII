###########################################################################################################
#
# Dieses Skript beinhaltet die Erweiterung für den Workflow "SDM" zur Vorhersage der Habitateignung von Arten
# mit Vorkommen in Deutschland. Mit dieser Erweiterung können Vorkommensdaten von Arten von GBIF herunter-
# geladen werden mit mehr als 20000 Datenpunkten. Diese Vorkommensdaten können anschließend mit Workflow
# SDM verwendet werden, um die Habitateignung berechnen und vorhersagen zu können.
#
# Author: Hanno Seebens, Senckenberg Gesellschaft fuer Naturforschung, 23.07.23
##########################################################################################################

## Bereinigen der Arbeitsumgebung
graphics.off()
rm(list=ls())


##########################################################################################################
## Konfiguration des Downloads ##########################################################################
##########################################################################################################

setwd("C:/Hanno/Bioinvasion/CASPIANII/CASPIANII")

## Artenliste ############################################################################################
# Name des Datensatzes, welches die Artenliste enthaelt. Dies muss eine .xlsx Datei sein mit den Spalten "Taxon"
# und "Eintraege_GBIF_DE" sein. Eine entsprechende Datei mit dem Workflow "ListeNeobiota" generiert.
Name_Artenliste <- "ListeGebietsfremderArten_gesamt_standardisiert.xlsx"

## Name des jeweiligen Modelllaufs (frei vom Nutzer zu waehlen)
identifier <- "170723" # eine eindeutige Kennzeichnung des Modelllaufs (z.B. Datum)

## GBIF Konto Details #############################################################
## Ein GBIF Konto muss auf https://www.gbif.org/ erstellt werden. Die Login Daten
## Name (user), Emailadresse und Passwort muessen in diesem Skript angegeben werden.
## Ein Beispiel Konto kann verwendet werden:
user <- "ekinhanno1" 
email <- "ekinhanno1@gmail.com"
pwd <- "seebenskaplan1234"

## Geographischer Fokus ############################################################
# Geographischer Ausschnitt zum Fitten des Modells (Ausschnitt_ModellFit) und zur Vorhersage/Extrapolation der Ergebnisse (Ausschnitt_Extrapolation)
# Angaben beschreiben die Ausdehnung eines Rechtecks (long/lat fuer linke, untere und rechte, obere Ecke hintereinander)
Ausschnitt <- c(-30,25,40,78) # Grenzen von Europa (Modell wird fuer alle Vorkommen in Europa angefittet)

##########################################################################################################
## Ende: Konfiguration der Habitatmodellierung ###########################################################
##########################################################################################################




##########################################################################################################
##### Beginn: Download ###################################################################################
##########################################################################################################


##########################################################################################################
## Lade Funktionen #######################################################################################
source(file.path("LadeSkripte.R")) 

##########################################################################################################
## Lade und installiere (sofern noch nicht geschehen) notwendige R Pakete ################################

LadePakete()


###########################################################################################################
## Lade Artenliste ########################################################################################

Artenliste <- read.xlsx(file.path("SDM","Data","Input",Name_Artenliste),sheet=1)

## Filter nach Arten mit >20000 Datenpunkten 
artenliste <- subset(Artenliste,Eintraege_GBIF_DE>20000)$Taxon 

## check identifier separator
if (strtrim(identifier,1)!="_"){
  identifier <- paste0("_",identifier)
}

#######################################################################################
### get GBIF keys for all species #####################################################

cat("\n Get GBIF keys for taxa \n")

GBIF_speclist <- list()
x <- 0
for (i in 1:length(artenliste)){# loop over all species
  
  specname <- name_backbone(artenliste[i], limit = 10, strict = TRUE)      # overrides the max limit to increase speed
  if (all(colnames(specname)!="species")) next
  
  x <- x + 1
  GBIF_speclist[[x]] <- c(specname$speciesKey,specname$scientificName,specname$canonicalName,specname$matchType,artenliste[i])
  
  if (x%%100==0) print(x)
}
GBIF_species <- as.data.frame(do.call("rbind",GBIF_speclist),stringsAsFactors = F)
colnames(GBIF_species) <- c("speciesKey","scientificName","canonicalName","matchType","Orig_name")

Ausschnitt <- paste("POLYGON ((",Ausschnitt[1],Ausschnitt[2],",",
                    Ausschnitt[3], Ausschnitt[2],",", 
                    Ausschnitt[3], Ausschnitt[4],",",
                    Ausschnitt[1], Ausschnitt[4],",", 
                    Ausschnitt[1], Ausschnitt[2],
                    "))",sep=" ")


## prepare requests for GBIF download (no execution!)
queries <- list()
queries[[1]] <- occ_download_prep(
  pred_in("taxonKey", GBIF_species$speciesKey),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  pred_within(Ausschnitt),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
)


## execute requests in sequence
out <- occ_download_queue(.list = queries, status_ping = 60)
 
## get data ################################
path_to_storage <- file.path("SDM","Data","Input")
occ_download_get(out,path=path_to_storage,overwrite=TRUE) # WARNING: function might not work properly! Beta-version.

 
## extract data ########################################
allfiles <- list.files(path_to_storage)
zippedfile <- allfiles[grepl(as.character(out[[1]]),allfiles)]
# zippedfile <- c("0116057-230530130749713.zip")
# zippedfile <- c("0270252-220831081235567.zip")
# extract_files <- zippedfile


## unzip files ###################################
if (length(zippedfile)>1) stop("More than one zip file found!")

if (!file.exists(paste0(path_to_storage,gsub("\\.zip","\\.csv",zippedfile)))){ # check if file has been unzipped already 
  # try(gbif_raw <- decompress_file(path_to_storage,zippedfile)) # try to unzip
  unzip(file.path(path_to_storage,zippedfile), overwrite=TRUE, exdir=path_to_storage)
} 

unzipped <- gsub("\\.zip","\\.csv",zippedfile)


## read csv file
Vorkommen <- fread(file=file.path(path_to_storage,unzipped),select=c("speciesKey","basisOfRecord","decimalLatitude","decimalLongitude","eventDate"),quote="")
# Vorkommen <- fread(file=file.path(path_to_storage,unzipped),quote="",nrows = 10)


#####################################################################
## clean data #######################################################
cat("\n Bereinigen der Vorkommensdaten...\n")

Vorkommen <- Vorkommen[basisOfRecord!="FOSSIL_SPECIMEN"]

# dat_sub <- dat[,c("scientificName","decimalLatitude","decimalLongitude")]
Vorkommen <- Vorkommen[,c("speciesKey","decimalLatitude","decimalLongitude","eventDate")]
colnames(Vorkommen) <- c("speciesKey","Breitengrad","Laengengrad","Zeitpunkt")

# remove wrong coordinates
ind <- (Vorkommen$Laengengrad>90 | Vorkommen$Laengengrad< -90) |  (Vorkommen$Breitengrad>180 | Vorkommen$Breitengrad< -180)
Vorkommen <- Vorkommen[!ind,]

# remove inprecise coordinates
ind <- nchar(sub('[0-9]+\\.', '', Vorkommen$Laengengrad))<3
Vorkommen <- Vorkommen[!ind,]
ind <- nchar(sub('[0-9]+\\.', '', Vorkommen$Breitengrad))<3
Vorkommen <- Vorkommen[!ind,]

Vorkommen$Taxon <- NA
Vorkommen_split <- split(Vorkommen[,c("Breitengrad","Laengengrad","Zeitpunkt")],f=Vorkommen$speciesKey)
country_borders <- readOGR(dsn=file.path("SDM","Data","Input","Shapefiles"),layer="ne_50m_land",stringsAsFactors=F,verbose=F)

Vorkommen_alle <- list()
for (i in 1:length(Vorkommen_split)){
  
  print(i)
  
  Vorkommen_sub <- Vorkommen_split[[i]]
  Vorkommen_sub$Taxon <- GBIF_species$Orig_name[GBIF_species$speciesKey==names(Vorkommen_split)[[i]] ]
  
  Vorkommen_cleaned <- suppressMessages(suppressWarnings(clean_coordinates(Vorkommen_sub, lon = "Laengengrad",
                                                                           lat = "Breitengrad",
                                                                           value ="clean",
                                                                           # countries = "countryCode",
                                                                           species = "Taxon",
                                                                           tests = c( "centroids", "equal", "gbif", "institutions", "outliers",
                                                                                      "zeros"), # remove "capitals" and "seas"
                                                                           country_ref=country_borders
  ))) 
  
  ## prepare output
  Vorkommen_cleaned$Datenbank <- "GBIF"
  Vorkommen_cleaned <- Vorkommen_cleaned[,c("Taxon","Laengengrad","Breitengrad","Zeitpunkt","Datenbank")]

  # print(Vorkommen_cleaned$Taxon[1])
  # 
  # print(str(Vorkommen_cleaned))
  
  fwrite(Vorkommen_cleaned,file.path("SDM","Data","Input",paste0("Vorkommen_",Vorkommen_cleaned$Taxon[1],identifier,".csv")))
  # Vorkommen_alle[[i]] <- Vorkommen_cleaned
  
}
# Vorkommen_alle <- rbindlist(Vorkommen_alle)

