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

setwd(file.path("C:","Hanno","Bioinvasion","CASPIANII","CASPIANII","NaVI_app","Datenaufbereitung"))



## Name des Durchlaufs
identifier <- "281123"

## check identifier separator
if (strtrim(identifier,1) != "_"){
  identifier <- paste0("_",identifier)
}

## Arten der EU Liste und BfN Listen
list_aliens <- unique(read.xlsx(file.path("Daten", "ListeGebietsfremderArten_gesamt_standardisiert.xlsx")))

eu_concern_spec <- subset(list_aliens, EU_Anliegen=="x")$Taxon
ManListe_spec <- subset(list_aliens, BfNlisten=="Managementliste")$Taxon
AktListe_spec <- subset(list_aliens, BfNlisten=="Aktionsliste")$Taxon
BeoListe_spec <- subset(list_aliens, BfNlisten=="Beobachtungsliste")$Taxon
HanListe_spec <- subset(list_aliens, BfNlisten=="Handlungsliste")$Taxon

liste_artgruppen <- unique(list_aliens[, c("Taxon", "ArtGruppe")])
colnames(liste_artgruppen) <- c("Taxon", "Gruppe")


## Kurzformen der Pfade
paths_short <- read.xlsx(file.path("Daten","Pfade_Kurzformen.xlsx"))



##########################################################################################################
## Daten zum tatsächlichen Vorkommen #####################################################################

## Lade Daten der Kreise
all_sites_df <- fread(input=file.path("Daten", paste0("istVorkommenAlleArten_Kreise",identifier,".gz")),
                      colClasses=c("character","character"))
all_sites_df <- all_sites_df[!is.na(all_sites_df$CC_2),]

## Lade Grenzen der Kreise
regions <- st_read(dsn=file.path("Daten"),layer="gadm41_DEU_2")

regions <- unique(st_drop_geometry(regions)[,c("CC_2","NAME_2")])
regions <- regions[!is.na(regions$CC_2),]

# ind <- which(is.na(regions[,1]))
# regions[ind,]

## Export Liste der Neobiota fuer Kreise #########################
all_data_ist <- merge(all_sites_df,regions,by="CC_2",all=T)
all_data_ist_eu <- subset(all_data_ist, Taxon%in%eu_concern_spec)
all_data_ist_man <- subset(all_data_ist, Taxon%in%ManListe_spec)
all_data_ist_akt <- subset(all_data_ist, Taxon%in%AktListe_spec)
all_data_ist_beo <- subset(all_data_ist, Taxon%in%BeoListe_spec)
all_data_ist_han <- subset(all_data_ist, Taxon%in%HanListe_spec)





##########################################################################################################
## Beobachtete Anzahl Neobiota fuer Kreise ####################################################################

## Alle Neobiota
setkey(all_data_ist,NAME_2)
nSpec_obs <- all_data_ist[,list(nSpec_obs=length(Taxon)),by=c("NAME_2","CC_2")]
colnames(nSpec_obs) <- c("NAME_2","CC_2","nSpez")

## Export Anzahl IAS der Unionsliste fuer Kreise
setkey(all_data_ist_eu,NAME_2)
nSpec_obs_eu <- all_data_ist_eu[,list(nSpec_obs=length(Taxon)),by=c("NAME_2","CC_2")]
colnames(nSpec_obs_eu) <- c("NAME_2","CC_2","nSpez")

## Export Anzahl IAS der Managementliste fuer Kreise
setkey(all_data_ist_man,NAME_2)
nSpec_obs_man <- all_data_ist_man[,list(nSpec_obs=length(Taxon)),by=c("NAME_2","CC_2")]
colnames(nSpec_obs_man) <- c("NAME_2","CC_2","nSpez")

## Export Anzahl IAS der Handlungsliste fuer Kreise
setkey(all_data_ist_han,NAME_2)
nSpec_obs_han <- all_data_ist_han[,list(nSpec_obs=length(Taxon)),by=c("NAME_2","CC_2")]
colnames(nSpec_obs_han) <- c("NAME_2","CC_2","nSpez")

## Export Anzahl IAS der Beobachtungsliste fuer Kreise
setkey(all_data_ist_beo,NAME_2)
nSpec_obs_beo <- all_data_ist_beo[,list(nSpec_obs=length(Taxon)),by=c("NAME_2","CC_2")]
colnames(nSpec_obs_beo) <- c("NAME_2","CC_2","nSpez")

## Export Anzahl IAS der Aktionsliste fuer Kreise
setkey(all_data_ist_akt,NAME_2)
nSpec_obs_akt <- all_data_ist_akt[,list(nSpec_obs=length(Taxon)),by=c("NAME_2","CC_2")]
colnames(nSpec_obs_akt) <- c("NAME_2","CC_2","nSpez")




##########################################################################################################
## Daten zum potenziellen Vorkommen ######################################################################

## Lade Daten zum potenziellen Vorkommen
all_sites_df <- fread(file.path("Daten", paste0("potVorkommen_alleArten_Eignung07_Kreise",identifier,".gz")), colClasses = c("character","numeric","character"))
all_sites_df <- all_sites_df[!is.na(all_sites_df$HabitatEignung),]

all_data_pot <- merge(all_sites_df,regions,by="CC_2",all=T) # verbinde mit Kreisgrenzen

setkey(all_data_pot,CC_2,HabitatEignung) 
setorderv(all_data_pot,cols=c("NAME_2","HabitatEignung"),order=c(1,-1)) # sortiere Datensatz


## Export Liste der potenziellen Arten der EU Liste pro Kreis
all_data_pot_eu <- subset(all_data_pot, Taxon%in%eu_concern_spec)

## Export Liste der potenziellen Arten der Handlungsliste pro Kreis
all_data_pot_han <- subset(all_data_pot, Taxon%in%HanListe_spec)

## Export Liste der potenziellen Arten der Beobachtungsliste pro Kreis
all_data_pot_beo <- subset(all_data_pot, Taxon%in%BeoListe_spec)

## Export Liste der potenziellen Arten der Managementliste pro Kreis
all_data_pot_man <- subset(all_data_pot, Taxon%in%ManListe_spec)

## Export Liste der potenziellen Arten der Aktionsliste pro Kreis
all_data_pot_akt <- subset(all_data_pot, Taxon%in%AktListe_spec)





##########################################################################################################
## Kombiniere potenzielle und tatsächliche Vorkommen #####################################################

## Alle Arten ##################################
all_data_pot[,Pot:="x"]
all_data_ist[,Ist:="x"]

all_ist_pot <- merge(all_data_ist, all_data_pot, by=c("CC_2","NAME_2","Taxon"), all=T)
all_ist_pot <- merge(all_ist_pot, liste_artgruppen, by=c("Taxon"), all.x=T)
all_ist_pot <- all_ist_pot[, c("NAME_2", "CC_2", "Taxon", "Gruppe", "Ist", "Pot", "HabitatEignung")]



## IAS der EU Liste ############################
all_data_pot_eu[,Pot:="x"]
all_data_ist_eu[,Ist:="x"]

all_ist_pot_eu <- merge(all_data_ist_eu,all_data_pot_eu,by=c("CC_2","NAME_2","Taxon"),all=T)
all_ist_pot_eu <- merge(all_ist_pot_eu, liste_artgruppen, by=c("Taxon"), all.x=T)
all_ist_pot_eu <- all_ist_pot_eu[,c("NAME_2", "CC_2", "Taxon", "Gruppe", "Ist", "Pot", "HabitatEignung")]


## IAS der Handlungsliste ############################
all_data_pot_han[,Pot:="x"]
all_data_ist_han[,Ist:="x"]

all_ist_pot_han <- merge(all_data_ist_han,all_data_pot_han,by=c("CC_2","NAME_2","Taxon"),all=T)
all_ist_pot_han <- merge(all_ist_pot_han, liste_artgruppen, by=c("Taxon"), all.x=T)
all_ist_pot_han <- all_ist_pot_han[,c("NAME_2", "CC_2", "Taxon", "Gruppe", "Ist", "Pot", "HabitatEignung")]


## IAS der Aktionsliste ############################
all_data_pot_akt[,Pot:="x"]
all_data_ist_akt[,Ist:="x"]

all_ist_pot_akt <- merge(all_data_ist_akt,all_data_pot_akt,by=c("CC_2","NAME_2","Taxon"),all=T)
all_ist_pot_akt <- merge(all_ist_pot_akt, liste_artgruppen, by=c("Taxon"), all.x=T)
all_ist_pot_akt <- all_ist_pot_akt[,c("NAME_2", "CC_2", "Taxon", "Gruppe", "Ist", "Pot", "HabitatEignung")]


## IAS der Managementliste ############################
all_data_pot_man[,Pot:="x"]
all_data_ist_man[,Ist:="x"]

all_ist_pot_man <- merge(all_data_ist_man,all_data_pot_man,by=c("CC_2","NAME_2","Taxon"),all=T)
all_ist_pot_man <- merge(all_ist_pot_man, liste_artgruppen, by=c("Taxon"), all.x=T)
all_ist_pot_man <- all_ist_pot_man[,c("NAME_2","CC_2","Taxon", "Gruppe","Ist","Pot","HabitatEignung")]


## IAS der Beobachtungsliste ############################
all_data_pot_beo[,Pot:="x"]
all_data_ist_beo[,Ist:="x"]

all_ist_pot_beo <- merge(all_data_ist_beo,all_data_pot_beo,by=c("CC_2","NAME_2","Taxon"),all=T)
all_ist_pot_beo <- merge(all_ist_pot_beo, liste_artgruppen, by=c("Taxon"), all.x=T)
all_ist_pot_beo <- all_ist_pot_beo[,c("NAME_2","CC_2","Taxon", "Gruppe","Ist","Pot","HabitatEignung")]




##########################################################################################################
## Erstelle Karten der Anzahl Arten pro Region ###################################################################

# Lade Grenzen der Kreise
regions <- st_read(dsn=file.path("Daten"),layer="gadm41_DEU_2")
regions$RegionName <- regions$NAME_2
regions <- regions[!is.na(regions$CC_2),]


## Alle Neobiota ###############################################################

# Lade Anzahl aller Arten mit tatsächlichem Vorkommen
nSpec_obs <- nSpec_obs[,c("CC_2","nSpez")]

# Kombiniere Grenzen und Anzahl Arten
map_fine = regions %>%
  inner_join(nSpec_obs, by = 'CC_2') %>%
  dplyr::select(NAME_2, CC_2, nSpez, geometry) 

# Vereinfache Grenze fuer große Darstellung
map_simp <- ms_simplify(map_fine)



## Erstelle Karten der Anzahl EU-IAS ###################################################################

# Lade Anzahl Neobiota mit tatsächlichem Vorkommen
nSpec_obs_eu <- nSpec_obs_eu[,c("CC_2","nSpez")]

# Kombiniere Grenzen und Anzahl Neobiota
regions_nspez_eu <- merge(regions[, c("CC_2", "NAME_2", "geometry")], nSpec_obs_eu, by="CC_2", all.x=T)
regions_nspez_eu$nSpez[is.na(regions_nspez_eu$nSpez)] <- 0

# Vereinfache Grenze fuer große Darstellung
map_simp_eu <- ms_simplify(regions_nspez_eu)



## Alle Arten der Handlungsliste ###############################################

nSpec_obs_han <- nSpec_obs_han[,c("CC_2","nSpez")]

# Kombiniere Grenzen und Anzahl Arten
regions_nspez_han <- merge(regions[, c("CC_2", "NAME_2", "geometry")], nSpec_obs_han, by="CC_2", all.x=T)
regions_nspez_han$nSpez[is.na(regions_nspez_han$nSpez)] <- 0

# Vereinfache Grenze fuer große Darstellung
map_simp_han <- ms_simplify(regions_nspez_han)



## Alle Arten der Aktionsliste ###############################################

# Lade Anzahl aller Arten mit tatsächlichem Vorkommen
nSpec_obs_akt <- nSpec_obs_akt[,c("CC_2","nSpez")]

# Kombiniere Grenzen und Anzahl Arten
regions_nspez_akt <- merge(regions[, c("CC_2", "NAME_2", "geometry")], nSpec_obs_akt, by="CC_2", all.x=T)
regions_nspez_akt$nSpez[is.na(regions_nspez_akt$nSpez)] <- 0

# Vereinfache Grenze fuer große Darstellung
map_simp_akt <- ms_simplify(regions_nspez_akt)


## Alle Arten der Managemetliste ###############################################

# Lade Anzahl aller Arten mit tatsächlichem Vorkommen
nSpec_obs_man <- nSpec_obs_man[,c("CC_2","nSpez")]

# Kombiniere Grenzen und Anzahl Arten
regions_nspez_man <- merge(regions[, c("CC_2", "NAME_2", "geometry")], nSpec_obs_man, by="CC_2", all.x=T)
regions_nspez_man$nSpez[is.na(regions_nspez_man$nSpez)] <- 0

# Vereinfache Grenze fuer große Darstellung
map_simp_man <- ms_simplify(regions_nspez_man)



## Alle Arten der Beobachtungsliste ###############################################

# Lade Anzahl aller Arten mit tatsächlichem Vorkommen
nSpec_obs_beo <- nSpec_obs_beo[,c("CC_2","nSpez")]

# Kombiniere Grenzen und Anzahl Arten
regions_nspez_beo <- merge(regions[, c("CC_2", "NAME_2", "geometry")], nSpec_obs_beo, by="CC_2", all.x=T)
regions_nspez_beo$nSpez[is.na(regions_nspez_beo$nSpez)] <- 0

# Vereinfache Grenze fuer große Darstellung
map_simp_beo <- ms_simplify(regions_nspez_beo)



##########################################################################################################
## Ermittle potenziell neue Arten ###################################################################

## Namen der Regionen
uni_Kreise <- unique(regions$RegionName)

all_regions <- cbind.data.frame(regions$RegionName, regions$CC_2)
colnames(all_regions) <- c("RegionName","CC_2")


## Pufferzone um Regionen
dist_buff <- 100
units(dist_buff) <- as_units("km")

## Lade Koordinaten
coords <- fread(file.path("Daten","Neobiota_AllOccurrences.gz"))
coords <- subset(coords, Taxon %in% unique(list_aliens$Taxon)) # remove some entries listed in the ListeNeobiota...xlsx

potist <- all_ist_pot # fread(file.path("Daten","Neobiota_IstPot_Liste_Kreise.gz"), colClasses = c("character","character","character","character","character","character","numeric"))
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
fwrite(scientific_common,file.path("Daten","Tabelle_DeutscheName.csv"))



cat("\n Ermittle potenzielle Arten.\n\n") #########################################

all_pot_spec <- list()
for (i in 1:length(uni_Kreise)){
  
  ## generate single polygon with buffer
  poly <- regions[regions$RegionName==uni_Kreise[i],]
  poly_buff <- st_buffer(poly,dist=dist_buff)
  ext <- st_bbox(poly_buff)
  
  ## Identifiziere potenzielle Arten
  abs_spec <- subset(potist,NAME_2==uni_Kreise[i] & !is.na(HabitatEignung) & is.na(Ist))
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

  if (i%%50==0){
    print(paste(round(i/length(uni_Kreise)*100),"%"))
    names(all_pot_spec) <- uni_Kreise[1:i]
  }
}
names(all_pot_spec) <- uni_Kreise












dist_buff <- 100
units(dist_buff) <- as_units("km")

# polygon data sets
uni_regs <- sort(unique(map_fine$NAME_2))
all_regs <- unique(cbind.data.frame(map_fine$NAME_2,map_fine$CC_2))
colnames(all_regs) <- c("RegionName","CC_2")


## Species lists by region #####################################################

## All neobiota ###############
spec_istpot <- all_ist_pot
setorderv(spec_istpot,cols=c("NAME_2","HabitatEignung"),order=-1)
colnames(spec_istpot) <- c("NAME_2","CC_2","Art","Gruppe","Ist","Pot","Habitateignung")
spec_istpot_regs <- merge(spec_istpot,all_regs, by="CC_2", all=T)


## EU IAS list ################
spec_istpot_eu <- all_ist_pot_eu
setorderv(spec_istpot_eu,cols=c("NAME_2","HabitatEignung"),order=-1)
colnames(spec_istpot_eu) <- c("NAME_2","CC_2","Art","Gruppe","Ist","Pot","Habitateignung")
spec_istpot_regs_eu <- merge(spec_istpot_eu,all_regs, by="CC_2", all=T)

## Handlungsliste ################
spec_istpot_han <- all_ist_pot_han
setorderv(spec_istpot_han,cols=c("NAME_2","HabitatEignung"),order=-1)
colnames(spec_istpot_han) <- c("NAME_2","CC_2","Art","Gruppe","Ist","Pot","Habitateignung")
spec_istpot_regs_han <- merge(spec_istpot_han,all_regs, by="CC_2", all=T)

## Beobachtungsliste ################
spec_istpot_beo <- all_ist_pot_beo
setorderv(spec_istpot_beo,cols=c("NAME_2","HabitatEignung"),order=-1)
colnames(spec_istpot_beo) <- c("NAME_2","CC_2","Art","Gruppe","Ist","Pot","Habitateignung")
spec_istpot_regs_beo <- merge(spec_istpot_beo,all_regs, by="CC_2", all=T)

## Aktionsliste ################
spec_istpot_akt <- all_ist_pot_akt
setorderv(spec_istpot_akt,cols=c("NAME_2","HabitatEignung"),order=-1)
colnames(spec_istpot_akt) <- c("NAME_2","CC_2","Art","Gruppe","Ist","Pot","Habitateignung")
spec_istpot_regs_akt <- merge(spec_istpot_akt,all_regs, by="CC_2", all=T)

## Managementliste ################
spec_istpot_man <- all_ist_pot_man
setorderv(spec_istpot_man,cols=c("NAME_2","HabitatEignung"),order=-1)
colnames(spec_istpot_man) <- c("NAME_2", "CC_2", "Art", "Gruppe", "Ist", "Pot", "Habitateignung")
spec_istpot_regs_man <- merge(spec_istpot_man,all_regs, by="CC_2", all=T)



# species occurrences
point_data <- fread(file.path("Daten","Vorkommen_alleArten.gz"))
point_data <- subset(point_data, Taxon %in% unique(list_aliens$Taxon)) # remove some entries listed in the ListeNeobiota...xlsx

uni_spec <- unique(point_data$Taxon)

point_data$DBcols <- as.numeric(as.factor(point_data$Datenbank))
all_cols <- c('royalblue3','darkgreen','skyblue1')
all_DBs <- sort(unique(point_data$Datenbank))



## common names
common_names <- fread(file.path("Daten","Tabelle_DeutscheName.csv"))
colnames(common_names) <- c("Deutscher Artname", "Taxon_wissensch")

region_lists <- merge(spec_istpot_regs,common_names,by.x="Art", by.y="Taxon_wissensch")
region_lists_eu <- merge(spec_istpot_regs_eu,common_names,by.x="Art", by.y="Taxon_wissensch")
region_lists_han <- merge(spec_istpot_regs_han,common_names,by.x="Art", by.y="Taxon_wissensch")
region_lists_beo <- merge(spec_istpot_regs_beo,common_names,by.x="Art", by.y="Taxon_wissensch")
region_lists_akt <- merge(spec_istpot_regs_akt,common_names,by.x="Art", by.y="Taxon_wissensch")
region_lists_man <- merge(spec_istpot_regs_man,common_names,by.x="Art", by.y="Taxon_wissensch")

## export all required files into one file

save(region_lists, region_lists_eu, region_lists_han, region_lists_beo, region_lists_akt, region_lists_man,
     map_simp, map_simp_eu, map_simp_han, map_simp_beo, map_simp_man, map_simp_akt,
     map_fine,
     uni_regs, uni_spec,
     all_pot_spec,
     point_data,
     all_cols,
     all_DBs,
     dist_buff,
     file=file.path("..","All_data_for_NaVI.RData"))
