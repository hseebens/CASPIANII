rm(list=ls())
graphics.off()

library(data.table)
library(sf)
library(terra)
library(dplyr)
library(tidyr)
library(rmapshaper)
library(units)
library(rgbif)
library(openxlsx)

setwd(file.path("C:","Hanno","Bioinvasion","CASPIANII","CASPIANII"))


## Verzeichnis der Vorkommensdaten (z.B. SDM/Data/Input)
path <- file.path("E:","CASPIANII","CASPIANII","SDM","Data","Input")


### generate Neobiota_AllOccurrences.gz !!!!!!!!
## and crop to Germany



## Name des Durchlaufs
identifier <- "281123"

## check identifier separator
if (strtrim(identifier,1) != "_"){
  identifier <- paste0("_",identifier)
}

## Arten der EU Liste und BfN Listen
list_aliens <- unique(read.xlsx(file.path("Shiny", "Daten", "ListeGebietsfremderArten_gesamt_standardisiert.xlsx")))

eu_concern_spec <- subset(list_aliens, EU_Anliegen=="x")$Taxon
ManListe_spec <- subset(list_aliens, BfNlisten=="Managementliste")$Taxon
AktListe_spec <- subset(list_aliens, BfNlisten=="Aktionsliste")$Taxon
BeoListe_spec <- subset(list_aliens, BfNlisten=="Beobachtungsliste")$Taxon
HanListe_spec <- subset(list_aliens, BfNlisten=="Handlungsliste")$Taxon

liste_artgruppen <- unique(list_aliens[, c("Taxon", "ArtGruppe")])
colnames(liste_artgruppen) <- c("Taxon", "Gruppe")


## Kurzformen der Pfade
paths_short <- read.xlsx(file.path("Shiny","Daten","Pfade_Kurzformen.xlsx"))

# sort(table(unlist(strsplit(list_aliens$Pfad, "; "))))
# all_paths <- sort(unique(unlist(strsplit(list_aliens$Pfad, "; "))))
# 
# for (i in 1:length(all_paths)){
#   ind_path <- grep(all_paths[i], list_aliens$Pfad)
#   all_paths[ind_path]
# }
# allpathdata <- data.frame(Pfad=all_paths)
# allpathdata$path_short <- NA
# allpathdata$path_short[allpathdata$Pfad=="absichtlich"] <- "abs."
# allpathdata$path_short[allpathdata$Pfad=="Gefangenschaftsflüchtling"] <- "GefFl"
# allpathdata$path_short[allpathdata$Pfad=="andere Gütertransporte"] <- "Gütertransp."
# allpathdata$path_short[allpathdata$Pfad=="andere Pfade"] <- "anderer"
# allpathdata$path_short[allpathdata$Pfad=="Erdaushub"] <- "Erdaushub"
# allpathdata$path_short[allpathdata$Pfad=="Anhaftung an Landfahrzeuge"] <- "Straße+Schiene"


##########################################################################################################
## Daten zum tatsächlichen Vorkommen #####################################################################

## Lade Daten der Kreise
all_sites_df <- fread(input=file.path("SDM","Data","Output", 
                                      paste0("istVorkommenAlleArten_Kreise",identifier,".gz")),
                      colClasses=c("character","character"))
all_sites_df <- all_sites_df[!is.na(all_sites_df$CC_2),]

## Lade Grenzen der Kreise
regions <- st_read(dsn=file.path("SDM","Data","Input","Shapefiles"),layer="gadm41_DEU_2")

regions <- unique(st_drop_geometry(regions)[,c("CC_2","NAME_2")])
regions <- regions[!is.na(regions$CC_2),]

# ind <- which(is.na(regions[,1]))
# regions[ind,]

## Export Liste der Neobiota fuer Kreise #########################
all_data_ist <- merge(all_sites_df,regions,by="CC_2",all=T)
# all_data_ist <- merge(all_data_ist, liste_artgruppen, by="Taxon", all.x=T)
# fwrite(all_data_ist,file.path("Shiny","Daten","Neobiota_ist_Liste_Kreise.gz"))

all_data_ist_eu <- subset(all_data_ist, Taxon%in%eu_concern_spec)
# fwrite(all_data_ist_eu,file.path("Shiny","Daten","Neobiota_ist_Liste_Kreise_EUIAS.gz"))

all_data_ist_man <- subset(all_data_ist, Taxon%in%ManListe_spec)
# fwrite(all_data_ist_man,file.path("Shiny","Daten","Neobiota_ist_Liste_Kreise_ManL.gz"))

all_data_ist_akt <- subset(all_data_ist, Taxon%in%AktListe_spec)
# fwrite(all_data_ist_akt,file.path("Shiny","Daten","Neobiota_ist_Liste_Kreise_AktL.gz"))

all_data_ist_beo <- subset(all_data_ist, Taxon%in%BeoListe_spec)
# fwrite(all_data_ist_beo,file.path("Shiny","Daten","Neobiota_ist_Liste_Kreise_BeoL.gz"))

all_data_ist_han <- subset(all_data_ist, Taxon%in%HanListe_spec)
# fwrite(all_data_ist_han,file.path("Shiny","Daten","Neobiota_ist_Liste_Kreise_HanL.gz"))





##########################################################################################################
## Export Anzahl Neobiota fuer Kreise ####################################################################

## Alle Neobiota
setkey(all_data_ist,NAME_2)
nSpec_obs <- all_data_ist[,list(nSpec_obs=length(Taxon)),by=c("NAME_2","CC_2")]
colnames(nSpec_obs) <- c("NAME_2","CC_2","nSpez")

fwrite(nSpec_obs,file.path("Shiny","Daten","Neobiota_ist_nSpez_Kreise.gz"))


## Export Anzahl IAS der Unionsliste fuer Kreise
setkey(all_data_ist_eu,NAME_2)
nSpec_obs_eu <- all_data_ist_eu[,list(nSpec_obs=length(Taxon)),by=c("NAME_2","CC_2")]
colnames(nSpec_obs_eu) <- c("NAME_2","CC_2","nSpez")

fwrite(nSpec_obs_eu,file.path("Shiny","Daten","Neobiota_ist_nSpez_Kreise_EUIAS.gz"))


## Export Anzahl IAS der Managementliste fuer Kreise
setkey(all_data_ist_man,NAME_2)
nSpec_obs_man <- all_data_ist_man[,list(nSpec_obs=length(Taxon)),by=c("NAME_2","CC_2")]
colnames(nSpec_obs_man) <- c("NAME_2","CC_2","nSpez")

fwrite(nSpec_obs_man,file.path("Shiny","Daten","Neobiota_ist_nSpez_Kreise_ManL.gz"))

## Export Anzahl IAS der Handlungsliste fuer Kreise
setkey(all_data_ist_han,NAME_2)
nSpec_obs_han <- all_data_ist_han[,list(nSpec_obs=length(Taxon)),by=c("NAME_2","CC_2")]
colnames(nSpec_obs_han) <- c("NAME_2","CC_2","nSpez")

fwrite(nSpec_obs_han,file.path("Shiny","Daten","Neobiota_ist_nSpez_Kreise_HanL.gz"))

## Export Anzahl IAS der Beobachtungsliste fuer Kreise
setkey(all_data_ist_beo,NAME_2)
nSpec_obs_beo <- all_data_ist_beo[,list(nSpec_obs=length(Taxon)),by=c("NAME_2","CC_2")]
colnames(nSpec_obs_beo) <- c("NAME_2","CC_2","nSpez")

fwrite(nSpec_obs_beo,file.path("Shiny","Daten","Neobiota_ist_nSpez_Kreise_BeoL.gz"))

## Export Anzahl IAS der Aktionsliste fuer Kreise
setkey(all_data_ist_akt,NAME_2)
nSpec_obs_akt <- all_data_ist_akt[,list(nSpec_obs=length(Taxon)),by=c("NAME_2","CC_2")]
colnames(nSpec_obs_akt) <- c("NAME_2","CC_2","nSpez")

fwrite(nSpec_obs_akt,file.path("Shiny","Daten","Neobiota_ist_nSpez_Kreise_AktL.gz"))




##########################################################################################################
## Daten zum potenziellen Vorkommen ######################################################################

## Lade Daten zum potenziellen Vorkommen
all_sites_df <- fread(file.path("SDM","Data","Output", paste0("potVorkommen_alleArten_Eignung07_Kreise",identifier,".gz")), colClasses = c("character","numeric","character"))
all_sites_df <- all_sites_df[!is.na(all_sites_df$HabitatEignung),]

all_data_pot <- merge(all_sites_df,regions,by="CC_2",all=T) # verbinde mit Kreisgrenzen

setkey(all_data_pot,CC_2,HabitatEignung) 
setorderv(all_data_pot,cols=c("NAME_2","HabitatEignung"),order=c(1,-1)) # sortiere Datensatz

## Export Liste der potenziellen Neobiota pro Kreis
# fwrite(all_data_pot,file.path("Shiny","Daten","Neobiota_pot_Liste_Kreise.gz"))


## Export Liste der potenziellen Arten der EU Liste pro Kreis
all_data_pot_eu <- subset(all_data_pot, Taxon%in%eu_concern_spec)
# fwrite(all_data_pot_eu,file.path("Shiny","Daten","Neobiota_pot_Liste_Kreise_EUIAS.gz"))

## Export Liste der potenziellen Arten der Handlungsliste pro Kreis
all_data_pot_han <- subset(all_data_pot, Taxon%in%HanListe_spec)
# fwrite(all_data_pot_han,file.path("Shiny","Daten","Neobiota_pot_Liste_Kreise_HanL.gz"))

## Export Liste der potenziellen Arten der Beobachtungsliste pro Kreis
all_data_pot_beo <- subset(all_data_pot, Taxon%in%BeoListe_spec)
# fwrite(all_data_pot_beo,file.path("Shiny","Daten","Neobiota_pot_Liste_Kreise_BeoL.gz"))

## Export Liste der potenziellen Arten der Managementliste pro Kreis
all_data_pot_man <- subset(all_data_pot, Taxon%in%ManListe_spec)
# fwrite(all_data_pot_man,file.path("Shiny","Daten","Neobiota_pot_Liste_Kreise_ManL.gz"))

## Export Liste der potenziellen Arten der Aktionsliste pro Kreis
all_data_pot_akt <- subset(all_data_pot, Taxon%in%AktListe_spec)
# fwrite(all_data_pot_akt,file.path("Shiny","Daten","Neobiota_pot_Liste_Kreise_AktL.gz"))






##########################################################################################################
## Kombiniere potenzielle und tatsächliche Vorkommen #####################################################

## Alle Arten ##################################
all_data_pot[,Pot:="x"]
all_data_ist[,Ist:="x"]

all_ist_pot <- merge(all_data_ist, all_data_pot, by=c("CC_2","NAME_2","Taxon"), all=T)
all_ist_pot <- merge(all_ist_pot, liste_artgruppen, by=c("Taxon"), all.x=T)
all_ist_pot <- all_ist_pot[, c("NAME_2", "CC_2", "Taxon", "Gruppe", "Ist", "Pot", "HabitatEignung")]

## Export der Liste Neobiota mit tatsächliche und potenziellen Vorkommen fuer Kreise
fwrite(all_ist_pot,file.path("Shiny","Daten","Neobiota_IstPot_Liste_Kreise.gz"))


## IAS der EU Liste ############################
all_data_pot_eu[,Pot:="x"]
all_data_ist_eu[,Ist:="x"]

all_ist_pot_eu <- merge(all_data_ist_eu,all_data_pot_eu,by=c("CC_2","NAME_2","Taxon"),all=T)
all_ist_pot_eu <- merge(all_ist_pot_eu, liste_artgruppen, by=c("Taxon"), all.x=T)
all_ist_pot_eu <- all_ist_pot_eu[,c("NAME_2", "CC_2", "Taxon", "Gruppe", "Ist", "Pot", "HabitatEignung")]

## Export der Liste Neobiota mit tatsächliche und potenziellen Vorkommen fuer Kreise
fwrite(all_ist_pot_eu,file.path("Shiny","Daten","Neobiota_IstPot_Liste_Kreise_EUIAS.gz"))


## IAS der Handlungsliste ############################
all_data_pot_han[,Pot:="x"]
all_data_ist_han[,Ist:="x"]

all_ist_pot_han <- merge(all_data_ist_han,all_data_pot_han,by=c("CC_2","NAME_2","Taxon"),all=T)
all_ist_pot_han <- merge(all_ist_pot_han, liste_artgruppen, by=c("Taxon"), all.x=T)
all_ist_pot_han <- all_ist_pot_han[,c("NAME_2", "CC_2", "Taxon", "Gruppe", "Ist", "Pot", "HabitatEignung")]

## Export der Liste Neobiota mit tatsächliche und potenziellen Vorkommen fuer Kreise
fwrite(all_ist_pot_han,file.path("Shiny","Daten","Neobiota_IstPot_Liste_Kreise_HanL.gz"))


## IAS der Aktionsliste ############################
all_data_pot_akt[,Pot:="x"]
all_data_ist_akt[,Ist:="x"]

all_ist_pot_akt <- merge(all_data_ist_akt,all_data_pot_akt,by=c("CC_2","NAME_2","Taxon"),all=T)
all_ist_pot_akt <- merge(all_ist_pot_akt, liste_artgruppen, by=c("Taxon"), all.x=T)
all_ist_pot_akt <- all_ist_pot_akt[,c("NAME_2", "CC_2", "Taxon", "Gruppe", "Ist", "Pot", "HabitatEignung")]

## Export der Liste Neobiota mit tatsächliche und potenziellen Vorkommen fuer Kreise
fwrite(all_ist_pot_akt,file.path("Shiny","Daten","Neobiota_IstPot_Liste_Kreise_AktL.gz"))


## IAS der Managementliste ############################
all_data_pot_man[,Pot:="x"]
all_data_ist_man[,Ist:="x"]

all_ist_pot_man <- merge(all_data_ist_man,all_data_pot_man,by=c("CC_2","NAME_2","Taxon"),all=T)
all_ist_pot_man <- merge(all_ist_pot_man, liste_artgruppen, by=c("Taxon"), all.x=T)
all_ist_pot_man <- all_ist_pot_man[,c("NAME_2","CC_2","Taxon", "Gruppe","Ist","Pot","HabitatEignung")]

## Export der Liste Neobiota mit tatsächliche und potenziellen Vorkommen fuer Kreise
fwrite(all_ist_pot_man,file.path("Shiny","Daten","Neobiota_IstPot_Liste_Kreise_ManL.gz"))


## IAS der Beobachtungsliste ############################
all_data_pot_beo[,Pot:="x"]
all_data_ist_beo[,Ist:="x"]

all_ist_pot_beo <- merge(all_data_ist_beo,all_data_pot_beo,by=c("CC_2","NAME_2","Taxon"),all=T)
all_ist_pot_beo <- merge(all_ist_pot_beo, liste_artgruppen, by=c("Taxon"), all.x=T)
all_ist_pot_beo <- all_ist_pot_beo[,c("NAME_2","CC_2","Taxon", "Gruppe","Ist","Pot","HabitatEignung")]

## Export der Liste Neobiota mit tatsächliche und potenziellen Vorkommen fuer Kreise
fwrite(all_ist_pot_beo,file.path("Shiny","Daten","Neobiota_IstPot_Liste_Kreise_BeoL.gz"))




##########################################################################################################
## Erstelle Karten der Anzahl Arten pro Region ###################################################################

# Lade Grenzen der Kreise
regions <- st_read(dsn=file.path("SDM","Data","Input","Shapefiles"),layer="gadm41_DEU_2")
regions$RegionName <- regions$NAME_2
regions <- regions[!is.na(regions$CC_2),]


## Alle Neobiota ###############################################################

# Lade Anzahl aller Arten mit tatsächlichem Vorkommen
nspez <- fread(file.path("Shiny","Daten","Neobiota_ist_nSpez_Kreise.gz"),colClasses = c("character","character","integer"))
nspez <- nspez[,c("CC_2","nSpez")]
# liste <- fread(file.path("Shiny","Daten","Neobiota_IstPot_Liste_Kreise.gz"), colClasses=c("character","character","character","character","character","character","numeric"))

# Kombiniere Grenzen und Anzahl Arten
regions_nspez = regions %>%
  inner_join(nspez, by = 'CC_2') %>%
  dplyr::select(NAME_2, CC_2, nSpez, geometry) 

## Export der Karte mit Anzahl Arten mit tatsächlichem Vorkommen
st_write(regions_nspez, file.path("Shiny","Daten","AnzahlNeobiota_ist_Kreise.shp"), delete_layer=T)

# Vereinfache Grenze fuer große Darstellung
regions_nspez_simp <- ms_simplify(regions_nspez)
st_write(regions_nspez_simp, file.path("Shiny","Daten","AnzahlNeobiota_ist_Kreise_simpl.shp"), delete_layer=T)



## Erstelle Karten der Anzahl EU-IAS ###################################################################

# Lade Anzahl Neobiota mit tatsächlichem Vorkommen
nspez_eu <- fread(file.path("Shiny","Daten","Neobiota_ist_nSpez_Kreise_EUIAS.gz"),
                  colClasses = c("character","character","integer"))
nspez_eu <- nspez_eu[,c("CC_2","nSpez")]
# liste_eu <- fread(file.path("Shiny","Daten","Neobiota_IstPot_Liste_Kreise_EUIAS.gz"), 
#                   colClasses=c("character","character","character","character","character","numeric"))


# Kombiniere Grenzen und Anzahl Neobiota
regions_nspez_eu <- merge(regions[, c("CC_2", "NAME_2", "geometry")], nspez_eu, by="CC_2", all.x=T)
regions_nspez_eu$nSpez[is.na(regions_nspez_eu$nSpez)] <- 0

## Export der Karte mit Anzahl Neobiota mit tatsächlichem Vorkommen
# st_write(regions_nspez_eu, 
#          file.path("Shiny","Daten","AnzahlNeobiota_ist_Kreise_EUIAS.shp"), 
#          delete_layer=T)

# Vereinfache Grenze fuer große Darstellung
regions_nspez_simp_eu <- ms_simplify(regions_nspez_eu)
st_write(regions_nspez_simp_eu, 
         file.path("Shiny","Daten","AnzahlNeobiota_ist_Kreise_simpl_EUIAS.shp"), 
         delete_layer=T)



## Alle Arten der Handlungsliste ###############################################

# Lade Anzahl aller Arten mit tatsächlichem Vorkommen
nspez <- fread(file.path("Shiny","Daten","Neobiota_ist_nSpez_Kreise_HanL.gz"),
               colClasses = c("character","character","integer"))
nspez <- nspez[,c("CC_2","nSpez")]
# liste <- fread(file.path("Shiny","Daten","Neobiota_IstPot_Liste_Kreise_HanL.gz"), 
#                colClasses=c("character","character","character","character","character","numeric"))

# Kombiniere Grenzen und Anzahl Arten
regions_nspez_han <- merge(regions[, c("CC_2", "NAME_2", "geometry")], nspez, by="CC_2", all.x=T)
regions_nspez_han$nSpez[is.na(regions_nspez_han$nSpez)] <- 0

## Export der Karte mit Anzahl Arten mit tatsächlichem Vorkommen
# st_write(regions_nspez_han, 
#          file.path("Shiny","Daten","AnzahlNeobiota_ist_Kreise_HanL.shp"), delete_layer=T)

# Vereinfache Grenze fuer große Darstellung
regions_nspez_simp_han <- ms_simplify(regions_nspez_han)
st_write(regions_nspez_simp_han, file.path("Shiny","Daten","AnzahlNeobiota_ist_Kreise_simpl_HanL.shp"), delete_layer=T)



## Alle Arten der Aktionsliste ###############################################

# Lade Anzahl aller Arten mit tatsächlichem Vorkommen
nspez <- fread(file.path("Shiny","Daten","Neobiota_ist_nSpez_Kreise_AktL.gz"),
               colClasses = c("character","character","integer"))
nspez <- nspez[,c("CC_2","nSpez")]
# liste <- fread(file.path("Shiny","Daten","Neobiota_IstPot_Liste_Kreise_AktL.gz"), 
#                colClasses=c("character","character","character","character","character","numeric"))

# Kombiniere Grenzen und Anzahl Arten
regions_nspez_akt <- merge(regions[, c("CC_2", "NAME_2", "geometry")], nspez, by="CC_2", all.x=T)
regions_nspez_akt$nSpez[is.na(regions_nspez_akt$nSpez)] <- 0

## Export der Karte mit Anzahl Arten mit tatsächlichem Vorkommen
# st_write(regions_nspez_akt, 
#          file.path("Shiny","Daten","AnzahlNeobiota_ist_Kreise_AktL.shp"), delete_layer=T)

# Vereinfache Grenze fuer große Darstellung
regions_nspez_simp_akt <- ms_simplify(regions_nspez_akt)
st_write(regions_nspez_simp_akt, 
         file.path("Shiny","Daten","AnzahlNeobiota_ist_Kreise_simpl_AktL.shp"), delete_layer=T)


## Alle Arten der Managemetliste ###############################################

# Lade Anzahl aller Arten mit tatsächlichem Vorkommen
nspez <- fread(file.path("Shiny","Daten","Neobiota_ist_nSpez_Kreise_ManL.gz"),
               colClasses = c("character","character","integer"))
nspez <- nspez[,c("CC_2","nSpez")]
# liste <- fread(file.path("Shiny","Daten","Neobiota_IstPot_Liste_Kreise_ManL.gz"), 
#                colClasses=c("character","character","character","character","character","numeric"))

# Kombiniere Grenzen und Anzahl Arten
regions_nspez_man <- merge(regions[, c("CC_2", "NAME_2", "geometry")], nspez, by="CC_2", all.x=T)
regions_nspez_man$nSpez[is.na(regions_nspez_man$nSpez)] <- 0

## Export der Karte mit Anzahl Arten mit tatsächlichem Vorkommen
# st_write(regions_nspez_man, 
#          file.path("Shiny","Daten","AnzahlNeobiota_ist_Kreise_ManL.shp"), delete_layer=T)

# Vereinfache Grenze fuer große Darstellung
regions_nspez_simp_man <- ms_simplify(regions_nspez_man)
st_write(regions_nspez_simp_man, 
         file.path("Shiny","Daten","AnzahlNeobiota_ist_Kreise_simpl_ManL.shp"), 
         delete_layer=T)



## Alle Arten der Beobachtungsliste ###############################################

# Lade Anzahl aller Arten mit tatsächlichem Vorkommen
nspez <- fread(file.path("Shiny","Daten","Neobiota_ist_nSpez_Kreise_BeoL.gz"),
               colClasses = c("character","character","integer"))
nspez <- nspez[,c("CC_2","nSpez")]
# liste <- fread(file.path("Shiny","Daten","Neobiota_IstPot_Liste_Kreise_BeoL.gz"), 
#                colClasses=c("character","character","character","character","character","numeric"))

# Kombiniere Grenzen und Anzahl Arten
regions_nspez_beo <- merge(regions[, c("CC_2", "NAME_2", "geometry")], nspez, by="CC_2", all.x=T)
regions_nspez_beo$nSpez[is.na(regions_nspez_beo$nSpez)] <- 0

## Export der Karte mit Anzahl Arten mit tatsächlichem Vorkommen
# st_write(regions_nspez_beo, 
#          file.path("Shiny","Daten","AnzahlNeobiota_ist_Kreise_BeoL.shp"), delete_layer=T)

# Vereinfache Grenze fuer große Darstellung
regions_nspez_simp_beo <- ms_simplify(regions_nspez_beo)
st_write(regions_nspez_simp_beo, file.path("Shiny","Daten","AnzahlNeobiota_ist_Kreise_simpl_BeoL.shp"), delete_layer=T)



##########################################################################################################
## Ermittle potenziell neue Arten ###################################################################

## Namen der Regionen
# regions <- st_read(dsn=file.path("SDM","Data","Input","Shapefiles"),layer="gadm41_DEU_2")
# regions$RegionName <- regions$NAME_2
uni_Kreise <- unique(regions$RegionName)

all_regions <- cbind.data.frame(regions$RegionName, regions$CC_2)
colnames(all_regions) <- c("RegionName","CC_2")


## Pufferzone um Regionen
dist_buff <- 100
units(dist_buff) <- as_units("km")

## Lade Koordinaten
coords <- fread(file.path("Shiny","Daten","Neobiota_AllOccurrences.gz"))

potist <- fread(file.path("Shiny","Daten","Neobiota_IstPot_Liste_Kreise.gz"), colClasses = c("character","character","character","character","character","character","numeric"))
potist <- merge(potist, all_regions, by="CC_2", all=T)



## Ermittle deutsche Namen von GBIF ##########################################

cat("\n Ermittle deutsche Namen von GBIF.\n\n")

uni_spec <- unique(coords$Taxon)
all_common_names <- list()
for (i in 1:length(uni_spec)){
  spec_dat <- name_backbone(uni_spec[i])
  try({
    all_names <- name_usage(spec_dat$usageKey, data="vernacularNames")
    all_germans <- all_names$data[all_names$data$language=="deu",]
    common_name <- names(sort(table(all_germans$vernacularName), decreasing=T)[1])
    all_common_names[[i]] <- common_name
  }, silent=TRUE)
  if (i%%100==0) print(paste(round(i/length(uni_spec)*100),"%"))
}
ind_miss <- unlist(lapply(all_common_names,length))==0
all_common_names[ind_miss] <- NA
scientific_common <- cbind.data.frame(unlist(all_common_names),uni_spec)
colnames(scientific_common) <- c("Taxon_deutsch","Taxon_wissensch")

## Export
fwrite(scientific_common,file.path("..","WorkingDocs","WP4","Tabelle_DeutscheName.csv"))
# scientific_common <- fread(file.path("..","WorkingDocs","WP4","Tabelle_DeutscheName.csv"))


cat("\n Ermittle potenzielle Arten.\n\n") #########################################

all_pot_spec <- list()
for (i in 1:length(uni_Kreise)){
  
  ## generate single polygon with buffer
  poly <- regions[regions$RegionName==uni_Kreise[i],]
  poly_buff <- st_buffer(poly,dist=dist_buff)
  ext <- st_bbox(poly_buff)
  
  ## Identifiziere potenzielle Arten
  abs_spec <- subset(potist,RegionName==uni_Kreise[i] & !is.na(HabitatEignung) & Ist!="x")
  abs_spec <- subset(abs_spec,HabitatEignung>0.7)
  
  ## subset of coordinates
  coords_sub <- subset(coords,Laengengrad>ext$xmin & Laengengrad<ext$xmax)
  coords_sub <- subset(coords_sub,Breitengrad>ext$ymin & Breitengrad<ext$ymax)
  coords_sub <- subset(coords_sub,Taxon%in%abs_spec$Taxon)
  
  coords_sub <- st_as_sf(coords_sub, coords=c("Laengengrad","Breitengrad"))
  st_crs(coords_sub) <- st_crs(poly_buff)
  
  ## identify closely located species
  pts_inner <- st_intersection(coords_sub,poly_buff)
  pot_spec <- unique(pts_inner$Taxon)
  
  all_pot_spec[[i]] <- pot_spec
  
  # print(length(pot_spec))
  
  if (i%%50==0){
    print(paste(round(i/length(uni_Kreise)*100),"%"))
    names(all_pot_spec) <- uni_Kreise[1:i]
    # fwrite(all_pot_spec,file.path("..","WorkingDocs","WP4","List_all_pot_spec.gz"))
    saveRDS(all_pot_spec,file.path("Shiny","Daten","Liste_PotSpez_Kreise.rds"))
  }
}
names(all_pot_spec) <- uni_Kreise
saveRDS(all_pot_spec,file.path("Shiny","Daten","Liste_PotSpez_Kreise.rds"))
# fwrite(all_pot_spec,file.path("Shiny","Daten","List_all_pot_spec.gz"))
# all_pot_spec <- readRDS(file.path("Shiny","Daten","Liste_PotSpez_Kreise.rds"))


## EU concern ################
all_pot_spec_eu <- lapply(all_pot_spec, function(s) subset(s, s%in%eu_concern_spec))
saveRDS(all_pot_spec_eu,file.path("Shiny","Daten","Liste_PotSpez_Kreise_EUIAS.rds"))

## Handlungsliste ################
all_pot_spec_han <- lapply(all_pot_spec, function(s) subset(s, s%in%HanListe_spec))
saveRDS(all_pot_spec_han,file.path("Shiny","Daten","Liste_PotSpez_Kreise_HanL.rds"))

## Aktionsliste ################
all_pot_spec_akt <- lapply(all_pot_spec, function(s) subset(s, s%in%AktListe_spec))
saveRDS(all_pot_spec_akt, file.path("Shiny","Daten","Liste_PotSpez_Kreise_HanL.rds"))

## Beobachtungsliste ################
all_pot_spec_beo <- lapply(all_pot_spec, function(s) subset(s, s%in%BeoListe_spec))
saveRDS(all_pot_spec_beo, file.path("Shiny","Daten","Liste_PotSpez_Kreise_BeoL.rds"))

## Managementliste ################
all_pot_spec_man <- lapply(all_pot_spec, function(s) subset(s, s%in%ManListe_spec))
saveRDS(all_pot_spec_man, file.path("Shiny","Daten","Liste_PotSpez_Kreise_ManL.rds"))


# plot(st_geometry(poly_buff),axes=T,col="red")
# plot(st_geometry(poly),add=T)
# # points(coords_sub$Laengengrad,coords_sub$Breitengrad,cex=0.1)
# plot(st_geometry(coords_sub),add=T,pch=1,cex=0.2)
# 


# ##########################################################################################################
# ## Sammle alle Vorkommen #################################################################################
# 
# all_Vorkommen <- list.files(path)
# all_Vorkommen <- all_Vorkommen[grep(identifier, all_Vorkommen)]
# all_Vorkommen <- all_Vorkommen[grep(".csv", all_Vorkommen)]
# 
# ## Pufferzone um Regionen
# dist_buff <- 100
# units(dist_buff) <- as_units("km")
# 
# germany <- st_read(dsn=file.path("Shiny","Daten"),layer="gadm41_DEU_0")
# germany_buff <- ms_simplify(st_buffer(germany, dist_buff))
# 
# ext_reg <- st_bbox(germany_buff)
# 
# dat_all <- list()
# for (i in 1:length(all_Vorkommen)){
#   
#   dat <- fread(file.path(path,all_Vorkommen[i]))
#   dat[,Zeitpunkt:=NULL]
#   # dat[,Datenbank:=NULL]
#   
#   dat <- subset(dat, Laengengrad<ext_reg$xmax & Laengengrad>ext_reg$xmin & Breitengrad<ext_reg$ymax & Breitengrad>ext_reg$ymin)
#   
#   dat_sf <- st_as_sf(dat, coords = c("Laengengrad","Breitengrad"))
#   st_crs(dat_sf) <- st_crs(germany_buff)
#   
#   pts_germ <- st_intersection(dat_sf,germany_buff)
#   dat_germ <- as.data.table(do.call(rbind, st_geometry(pts_germ)))
#   if (nrow(dat_germ)==0) next
#   
#   colnames(dat_germ) <- c("Laengengrad","Breitengrad")
#   dat_germ[, Taxon:=pts_germ$Taxon]
#   dat_germ[, Datenbank:=pts_germ$Datenbank]
#   
#   dat_all[[i]] <- dat_germ
#   
#   if (i%%10==0) print(i)
# }
# dat_out <- rbindlist(dat_all)
# 
# fwrite(dat_out,file.path("Shiny","Daten","Vorkommen_alleArten.gz"))
# # dat_out <- fread(file.path("Shiny","Daten","Vorkommen_alleArten.gz"))
# 
# # dat_out_eu <- dat_out[dat_out$Taxon%in%eu_concern_spec,] # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# # fwrite(dat_out_eu,file.path("Shiny","Daten","Vorkommen_alleArten_EUIAS.gz"))
