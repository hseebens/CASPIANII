rm(list=ls())

library(rgbif)
library(openxlsx)
library(data.table)
library(rgdal)
library(CoordinateCleaner)

# source(file.path("R","decompress_file.R"))

identifier <- "191222_NoEASIN" # a unique identifier for every run of the SDM workflow, needs to be a character

user <- "ekinhanno1" 
email <- "ekinhanno1@gmail.com"
pwd <- "seebenskaplan1234"

Ausschnitt_ModellFit <- (c(-30,25,40,78))
Ausschnitt=Ausschnitt_ModellFit

neobiota <- read.xlsx(file.path("ListeNeobiota","Data","ListeGebietsfremderArten_gesamt_standardisiert.xlsx"),sheet=1)

## Entfernt Einträge, die nur aus EASIN stammen (abweichende Definition)
# neobiota <- subset(neobiota,Datenbank!="EASIN")

## Filter nach Arten mit ausreichend Datenpunkten (>100) und nicht zu vielen Datenpunkten (<5000), da 
## Simulationen sehr lange dauern würden
# artenliste <- subset(neobiota,Eintraege_GBIF_DE<10000 & Eintraege_GBIF_DE>50)$Taxon 
artenliste <- subset(neobiota,Eintraege_GBIF_DE>20000)$Taxon 
# artenliste <- artenliste[1:3]

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
# 
# ## get data ################################
path_to_storage <- file.path("SDM","Data","Input")
occ_download_get(out,path=path_to_storage)
# 
# 
# ## extract data ########################################
allfiles <- list.files(path_to_storage)
# zippedfile <- allfiles[grepl(as.character(out[[1]]),allfiles)]
zippedfile <- c("0270117-220831081235567.zip")
# zippedfile <- c("0270252-220831081235567.zip")
# extract_files <- zippedfile


## unzip files ###################################
if (length(zippedfile)>1) stop("More than one zip file found!")

if (!file.exists(paste0(path_to_storage,gsub("\\.zip","\\.csv",zippedfile)))){ # check if file has been unzipped already 
  try(gbif_raw <- decompress_file(path_to_storage,zippedfile)) # try to unzip
} 
if (class(gbif_raw)=="try-error") next

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
country_borders <- readOGR(dsn=file.path("Data","Input","Shapefiles"),layer="ne_50m_land",stringsAsFactors=F,verbose=F)

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

  print(Vorkommen_cleaned$Taxon[1])
  
  print(str(Vorkommen_cleaned))
  
  fwrite(Vorkommen_cleaned,file.path("Data","Input",paste0("Vorkommen_",Vorkommen_cleaned$Taxon[1],"_",identifier,".csv")))
  # Vorkommen_alle[[i]] <- Vorkommen_cleaned
  
}
# Vorkommen_alle <- rbindlist(Vorkommen_alle)

