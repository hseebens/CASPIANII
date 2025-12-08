################################################################################
#
# Skript ist Teil des Workflows "SDM" zur Habitatmodellierung von Neobiota in 
# Deutschland. 
#
# Dieses Skript beinhaltet die Erweiterung für den Workflow "SDM" (Habitateignung) 
# für Arten mit besonders vielen Vorkommensdatenpunkten (z.B. >10000 Einträge). 
# Diese Vorkommensdaten können anschließend mit Workflow SDM verwendet werden, 
# um die Habitateignung berechnen und vorhersagen zu können.
#
# Folgende Informationen werden benoetigt:
# - Name_Artenliste: Exceltabelle der Arten (Ausgabe aus Workflow ListeNeobiota)
# - identifier: Eindeutige Kennzeichnung fuer die Filenamen; derselbe identifier 
#    muss im Skript "run_SDM_workflow.R" verwendet werden
# - Logindaten fuer GBIF account (user name, Email, Passwort)
# - geographischer Ausschnitt (Europa ist als Standard gesetzt)
#
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 08.12.25
################################################################################

## Bereinigen der Arbeitsumgebung
graphics.off()
rm(list=ls())


##########################################################################################################
## Konfiguration des Downloads ##########################################################################
##########################################################################################################

# setwd("C:/Hanno/Bioinvasion/CASPIANII/CASPIANII")

## Artenliste ############################################################################################
# Name des Datensatzes, welches die Artenliste enthaelt. Dies muss eine .xlsx Datei sein mit den Spalten "Taxon"
# und "Eintraege_GBIF_DE" sein. Eine entsprechende Datei mit dem Workflow "ListeNeobiota" generiert.
Name_Artenliste <- "ListeGebietsfremderArten_gesamt_standardisiert.xlsx"

## Name des jeweiligen Modelllaufs (frei vom Nutzer zu waehlen)
identifier <- "230925" # eine eindeutige Kennzeichnung des Modelllaufs (z.B. Datum)

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
cat(paste0("\n****************************************************************************************\n"))
cat(paste0("Beginn der Datenermittlung \n"))
cat(paste0("************************************* ", Sys.time(), " **************************\n"))


## Lade Artenliste ########################################################################################

Artenliste <- read.xlsx(file.path("SDM","Daten","Input",Name_Artenliste), sheet=1)

## Filter nach Arten mit >10000 Datenpunkten 
artenliste <- subset(Artenliste, Eintraege_GBIF_Europa>9000) 


## Lange Listen sollten vermieden werden, da es zu sehr viele Records fuehren kann
## Eintraege werden auf max. 10^7 begrenzt
ind <- rep(1, nrow(artenliste))
records <- cumsum(artenliste$Eintraege_GBIF_Europa) 
for (i in 1:length(ind)){
  if (records[i]>10^7){
    ind[i:length(ind)] <- ind[i:length(ind)] + 1
    records <- records - records[i]
  }
}
artenliste_list <- split(artenliste$Taxon, ind)



## GBIF can handle three requests in parallel. If artenliste is less than 3 chunks, redistribute to get three
## This improves performance
if (length(artenliste_list)<3) {
  all_species <- unlist(artenliste_list)
  artenliste_list <- split(all_species, ceiling(seq_along(all_species)/ceiling(length(all_species)/3)))
}

## check identifier separator
if (strtrim(identifier,1)!="_"){
  identifier <- paste0("_",identifier)
}

#######################################################################################
### get GBIF keys for all species #####################################################


Ausschnitt_polygon <- paste("POLYGON ((",Ausschnitt[1],Ausschnitt[2],",",
                    Ausschnitt[3], Ausschnitt[2],",", 
                    Ausschnitt[3], Ausschnitt[4],",",
                    Ausschnitt[1], Ausschnitt[4],",", 
                    Ausschnitt[1], Ausschnitt[2],
                    "))",sep=" ")

cat("\n Get GBIF keys for taxa \n")

out <- list()
queries <- list()
GBIF_species_all <- list()
for (j in 1:length(artenliste_list)){
  
  teilliste <- artenliste_list[[j]]
  
  GBIF_speclist <- list()
  x <- 0
  for (i in 1:length(teilliste)){# loop over all species
    
    specname <- name_backbone(teilliste[i], limit = 10, strict = TRUE)      # overrides the max limit to increase speed
    if (all(colnames(specname)!="species")) next
    
    x <- x + 1
    GBIF_speclist[[x]] <- c(specname$speciesKey,specname$scientificName,specname$canonicalName,specname$matchType,teilliste[i])
    
  }
  if (j%%100==0) print(x)
  
  GBIF_species <- as.data.frame(do.call("rbind",GBIF_speclist),stringsAsFactors = F)
  colnames(GBIF_species) <- c("speciesKey","scientificName","canonicalName","matchType","Orig_name")

  GBIF_species_all[[j]] <- GBIF_species
  
  # prepare requests for GBIF download (no execution!)
  queries[[j]] <- occ_download_prep(
    pred_in("taxonKey", GBIF_species$speciesKey),
    pred("hasCoordinate", TRUE),
    pred("hasGeospatialIssue", FALSE),
    pred_within(Ausschnitt_polygon),
    format = "SIMPLE_CSV",
    user=user,pwd=pwd,email=email
  )
  # query <- occ_download(
  #   pred_in("taxonKey", GBIF_species$speciesKey),
  #   pred("hasCoordinate", TRUE),
  #   pred("hasGeospatialIssue", FALSE),
  #   pred_within(Ausschnitt_polygon),
  #   format = "SIMPLE_CSV",
  #   user=user,pwd=pwd,email=email
  # )
  # res <- occ_download_wait(query)
  # occ_download_meta(query)
}
GBIF_species_all <- do.call("rbind", GBIF_species_all)

## execute requests in sequence
out <- occ_download_queue(.list=queries, status_ping = 60)


## get data ################################
path_to_storage <- file.path("SDM","Daten","Input")
for (i in 1:length(artenliste_list)){
  occ_download_get(out[[i]], path=path_to_storage, overwrite=TRUE) # WARNING: function might not work properly! Beta-version.
}



## extract data ########################################

for (j in 1:length(artenliste_list)){
  
  # allfiles <- list.files(path_to_storage)
  # zippedfile <- allfiles[grepl(as.character(out[[1]]),allfiles)]
  # zippedfile <- c("0116057-230530130749713.zip")
  # zippedfile <- c("0270252-220831081235567.zip")
  # extract_files <- zippedfile
  zippedfile <- paste(occ_download_meta(out[[j]])$key, ".zip", sep="")
  
  
  ## unzip files ###################################
  if (length(zippedfile)>1) stop("More than one zip file found!")
  
  if (!file.exists(paste0(path_to_storage,gsub("\\.zip","\\.csv",zippedfile)))){ # check if file has been unzipped already 
    # try(gbif_raw <- decompress_file(path_to_storage,zippedfile)) # try to unzip
    unzip(file.path(path_to_storage,zippedfile), overwrite=TRUE, exdir=path_to_storage)
  } 
  
  unzipped <- gsub("\\.zip","\\.csv",zippedfile)

  ## read csv file
  Vorkommen <- fread(file=file.path(path_to_storage,unzipped),select=c("speciesKey","basisOfRecord","decimalLatitude","decimalLongitude","eventDate","datasetKey"),quote="")
  # Vorkommen <- fread(file=file.path(path_to_storage,unzipped),quote="",nrows = 10)

  #####################################################################
  ## clean data #######################################################
  
  cat("\n Bereinigen der Vorkommensdaten...\n")

  
  ## check for gridded data (with regular coordinates) and remove if detected ########
  ## gridded datasets are identified by calculating the euclidean distance 
  ## between consecutive coordinates and identifying regularities in distances
  ## (e.g., frequent similar distances between coordinates). The parameters
  ## were selected based on experience for a good match.
  ## it does not identify all but the majority of datasets and remove those
  ## records from the list of coordinates.
  
  # check only datasets with many records
  tab <- sort(table(Vorkommen$datasetKey), decreasing=T)  
  ds_names <- names(tab)[tab>20]
  
  if (length(ds_names)>1){ # is at least one dataset mentioned?
    for (i in 1:length(ds_names)){
      dat <- subset(Vorkommen, datasetKey==ds_names[i]) # single dataset
      
      ## order of lat/lon matters
      dat_sort <- dat[order(dat$decimalLatitude, dat$decimalLongitude), ] # order of coordinates matters
      dat_eucl <- sqrt( diff(dat_sort$decimalLatitude)^2 + diff(dat_sort$decimalLongitude)^2 ) # euclidean distance between consecutive records 
      dat_eucl <- dat_eucl[dat_eucl<1] # remove large distances
      dat_rnd <- round(dat_eucl, 4) # round values to avoid very small differences
      dat_rnd <- dat_rnd[dat_rnd!=0] # remove zeros (due to same coordinates)
      tab_diffs_latlon <- sort(table(dat_rnd), decreasing=T) # count similar coordinate differences
      if (length(dat_rnd)<10) next # move on if there are just a few remaining coordinates
      
      dat_sort <- dat[order(dat$decimalLongitude, dat$decimalLatitude), ] # order of coordinates matters
      dat_eucl <- sqrt( diff(dat_sort$decimalLatitude)^2 + diff(dat_sort$decimalLongitude)^2 ) # euclidean distance between consecutive records 
      dat_eucl <- dat_eucl[dat_eucl<1]
      dat_rnd <- round(dat_eucl, 4) # round values to avoid very small differences
      dat_rnd <- dat_rnd[dat_rnd!=0] # remove zeros (due to same coordinates)
      tab_diffs_lonlat <- sort(table(dat_rnd), decreasing=T) # count similar coordinate differences
      if (length(dat_rnd)<10) next # move on if there are just a few remaining coordinates
      
      # remove dataset with >40% records with regular distances (approximation through counting similar differences between coordinates)
      if (sum(tab_diffs_lonlat[1:3], na.rm=T)/sum(tab_diffs_lonlat) > 0.4 |
          sum(tab_diffs_latlon[1:3], na.rm=T)/sum(tab_diffs_latlon) > 0.4){
        # print(i)
        # x11()
        # plot(dat$decimalLongitude, dat$decimalLatitude, main=paste("Gridded", TaxonName, i))
        Vorkommen <- subset(Vorkommen, datasetKey!=ds_names[i])
      } 
    }
  }
  
    
  Vorkommen <- Vorkommen[basisOfRecord!="FOSSIL_SPECIMEN"]
  
  # dat_sub <- dat[,c("scientificName","decimalLatitude","decimalLongitude")]
  Vorkommen <- Vorkommen[, c("speciesKey","decimalLatitude","decimalLongitude","eventDate")]
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
  Vorkommen_split <- split(Vorkommen[, c("Breitengrad", "Laengengrad", "Zeitpunkt")], f=Vorkommen$speciesKey)
  # country_borders <- readOGR(dsn=file.path("SDM", "Daten", "Input", "Shapefiles"), layer="ne_50m_land", stringsAsFactors=F,verbose=F)
  
  Vorkommen_alle <- list()
  for (i in 1:length(Vorkommen_split)){
    
    print(round(i/length(Vorkommen_split), 2))
    
    Vorkommen_sub <- Vorkommen_split[[i]]
    Vorkommen_sub$Taxon <- GBIF_species_all$Orig_name[GBIF_species_all$speciesKey==names(Vorkommen_split)[[i]] ]
    
    Vorkommen_cleaned <- suppressMessages(suppressWarnings(clean_coordinates(Vorkommen_sub, lon = "Laengengrad",
                                                                             lat = "Breitengrad",
                                                                             value ="clean",
                                                                             # countries = "countryCode",
                                                                             species = "Taxon",
                                                                             tests = c( "centroids", "equal", "gbif", "institutions", "outliers",
                                                                                        "zeros"), # remove "capitals" and "seas"
                                                                             country_ref=NULL
    ))) 
    
    ## prepare output
    Vorkommen_cleaned$Datenbank <- "GBIF"
    Vorkommen_cleaned <- Vorkommen_cleaned[,c("Taxon","Laengengrad","Breitengrad","Zeitpunkt","Datenbank")]
  
    # print(Vorkommen_cleaned$Taxon[1])
    # 
    # print(str(Vorkommen_cleaned))
    
    fwrite(Vorkommen_cleaned, file.path("SDM","Daten","Input",paste0("Vorkommen_",Vorkommen_cleaned$Taxon[1],identifier,".csv")))
    # Vorkommen_alle[[i]] <- Vorkommen_cleaned
    
  }
  # Vorkommen_alle <- rbindlist(Vorkommen_alle)
}

cat(paste0("\n****************************************************************************************\n"))
cat(paste0("Ende der Datenermittlung \n"))
cat(paste0("********************************************** ", Sys.time(), " **************************\n"))
