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

### generate Neobiota_AllOccurrences.gz !!!!!!!!
## and crop to Germany


# library(geodata)
# 
# ## get GADM layers to aggregate to level 2 (Landkreise)
# 
# gadm("Germany",path="Shiny/Daten/",level=2)
# gadm("Germany",path="Shiny/Daten/",level=3)

# DEU_level2 <- readRDS(file.path("Shiny","Daten","gadm41_DEU_2_pk.rds"))
# # s <- sf::st_as_sf(DEU_level2)
# # st_write(s, file.path("Shiny","Daten","gadm41_DEU_2.shp"))
# 
# DEU_level3 <- readRDS(file.path("Shiny","Daten","gadm41_DEU_3_pk.rds"))
# names_level2 <- as.data.frame(DEU_level2[,c("CC_2","NAME_2")])
# names_level3 <- as.data.frame(DEU_level3[,c("CC_3","NAME_3","NAME_2")])
# names_trans <- merge(names_level2,names_level3,by="NAME_2",all=T)
# names_trans_CC <- names_trans[,c("CC_3","CC_2","NAME_2")]


## Name des Durchlaufs
identifier <- "160823"

## check identifier separator
if (strtrim(identifier,1)!="_"){
  identifier <- paste0("_",identifier)
}


eu_concern <- read.xlsx(file.path("Shiny","Daten","ListeGebietsfremderArten_gesamt_standardisiert.xlsx"))
eu_concern_spec <- subset(eu_concern,EU_Anliegen=="x")$Taxon



##########################################################################################################
## Daten zum tatsächlichen Vorkommen #####################################################################

## Lade Daten der Kreise
all_sites_df <- fread(input=file.path("SDM","Data","Output", paste0("istVorkommenAlleArten_Kreise",identifier,".gz")),colClasses=c("character","character"))
all_sites_df <- all_sites_df[!is.na(all_sites_df$CC_2),]

## Lade Grenzen der Kreise
regions <- st_read(dsn=file.path("SDM","Data","Input","Shapefiles"),layer="gadm41_DEU_2")

regions <- unique(st_drop_geometry(regions)[,c("CC_2","NAME_2")])
regions <- regions[!is.na(regions$CC_2),]

# ind <- which(is.na(regions[,1]))
# regions[ind,]

## Export Liste der Neobiota fuer Kreise
all_data_ist <- merge(all_sites_df,regions,by="CC_2",all=T)

fwrite(all_data_ist,file.path("Shiny","Daten","Neobiota_ist_Liste_Kreise.gz"))

all_data_ist_eu <- subset(all_data_ist, Taxon%in%eu_concern_spec)
fwrite(all_data_ist_eu,file.path("Shiny","Daten","Neobiota_ist_Liste_Kreise_EUIAS.gz"))


## Export Anzahl Neobiota fuer Kreise
setkey(all_data_ist,NAME_2)
nSpec_obs <- all_data_ist[,list(nSpec_obs=length(Taxon)),by=c("NAME_2","CC_2")]
colnames(nSpec_obs) <- c("NAME_2","CC_2","nSpez")

fwrite(nSpec_obs,file.path("Shiny","Daten","Neobiota_ist_nSpez_Kreise.gz"))


## Export Anzahl IAS der Unionsliste fuer Kreise
setkey(all_data_ist_eu,NAME_2)
nSpec_obs_eu <- all_data_ist_eu[,list(nSpec_obs=length(Taxon)),by=c("NAME_2","CC_2")]
colnames(nSpec_obs_eu) <- c("NAME_2","CC_2","nSpez")

fwrite(nSpec_obs_eu,file.path("Shiny","Daten","Neobiota_ist_nSpez_Kreise_EUIAS.gz"))





##########################################################################################################
## Daten zum potenziellen Vorkommen ######################################################################

## Lade Daten zum potenziellen Vorkommen
all_sites_df <- fread(file.path("SDM","Data","Output", paste0("potVorkommen_alleArten_Eignung07_Kreise",identifier,".gz")), colClasses = c("character","numeric","character"))
all_sites_df <- all_sites_df[!is.na(all_sites_df$HabitatEignung),]

all_data_pot <- merge(all_sites_df,regions,by="CC_2",all=T) # verbinde mit Kreisgrenzen

setkey(all_data_pot,CC_2,HabitatEignung) 
setorderv(all_data_pot,cols=c("NAME_2","HabitatEignung"),order=c(1,-1)) # sortiere Datensatz

## Export Liste der potenziellen Neobiota pro Kreis
fwrite(all_data_pot,file.path("Shiny","Daten","Neobiota_pot_Liste_Kreise.gz"))

## Export Liste der potenziellen EU IAS pro Kreis
all_data_pot_eu <- subset(all_data_pot, Taxon%in%eu_concern_spec)

fwrite(all_data_pot_eu,file.path("Shiny","Daten","Neobiota_pot_Liste_Kreise_EUIAS.gz"))


## Kombiniere potenzielle und tatsächliche Vorkommen
all_data_pot[,Pot:="x"]
all_data_ist[,Ist:="x"]

all_ist_pot <- merge(all_data_ist,all_data_pot,by=c("CC_2","NAME_2","Taxon"),all=T)
all_ist_pot <- all_ist_pot[,c("NAME_2","CC_2","Taxon","Ist","Pot","HabitatEignung")]

## Export der Liste Neobiota mit tatsächliche und potenziellen Vorkommen fuer Kreise
fwrite(all_ist_pot,file.path("Shiny","Daten","Neobiota_IstPot_Liste_Kreise.gz"))


## Kombiniere potenzielle und tatsächliche Vorkommen für IAS der EU
all_data_pot_eu[,Pot:="x"]
all_data_ist_eu[,Ist:="x"]

all_ist_pot_eu <- merge(all_data_ist_eu,all_data_pot_eu,by=c("CC_2","NAME_2","Taxon"),all=T)
all_ist_pot_eu <- all_ist_pot_eu[,c("NAME_2","CC_2","Taxon","Ist","Pot","HabitatEignung")]

## Export der Liste Neobiota mit tatsächliche und potenziellen Vorkommen fuer Kreise
fwrite(all_ist_pot_eu,file.path("Shiny","Daten","Neobiota_IstPot_Liste_Kreise_EUIAS.gz"))




##########################################################################################################
## Erstelle Karten der Anzahl Neobiota ###################################################################

# Lade Anzahl Neobiota mit tatsächlichem Vorkommen
nspez <- fread(file.path("Shiny","Daten","Neobiota_ist_nSpez_Kreise.gz"),colClasses = c("character","character","integer"))
nspez <- nspez[,c("CC_2","nSpez")]
liste <- fread(file.path("Shiny","Daten","Neobiota_IstPot_Liste_Kreise.gz"), colClasses=c("character","character","character","character","character","numeric"))

# Lade Grenzen der Kreise
regions <- st_read(dsn=file.path("SDM","Data","Input","Shapefiles"),layer="gadm41_DEU_2")
# regions$RegionName <- paste0(regions$NAME_3," (",regions$NAME_2,")")
regions <- st_read(dsn=file.path("SDM","Data","Input","Shapefiles"),layer="gadm41_DEU_2")
regions$RegionName <- regions$NAME_2
regions <- regions[!is.na(regions$CC_2),]

# Kombiniere Grenzen und Anzahl Neobiota
regions_nspez = regions %>%
  inner_join(nspez, by = 'CC_2') %>%
  dplyr::select(NAME_2, CC_2, nSpez, geometry) %>%
  # cleaning step that should be removed
  distinct(CC_2, .keep_all = TRUE)

## Export der Karte mit Anzahl Neobiota mit tatsächlichem Vorkommen
st_write(regions_nspez, file.path("Shiny","Daten","AnzahlNeobiota_ist_Kreise.shp"), delete_layer=T)

# Vereinfache Grenze fuer große Darstellung
regions_nspez_simp <- ms_simplify(regions_nspez)
st_write(regions_nspez_simp, file.path("Shiny","Daten","AnzahlNeobiota_ist_Kreise_simpl.shp"), delete_layer=T)



## Erstelle Karten der Anzahl EU-IAS ###################################################################

# Lade Anzahl Neobiota mit tatsächlichem Vorkommen
nspez_eu <- fread(file.path("Shiny","Daten","Neobiota_ist_nSpez_Kreise_EUIAS.gz"),colClasses = c("character","character","integer"))
nspez_eu <- nspez_eu[,c("CC_2","nSpez")]
liste_eu <- fread(file.path("Shiny","Daten","Neobiota_IstPot_Liste_Kreise_EUIAS.gz"), colClasses=c("character","character","character","character","character","numeric"))


# Kombiniere Grenzen und Anzahl Neobiota
regions_nspez_eu = regions %>%
  inner_join(nspez_eu, by = 'CC_2') %>%
  dplyr::select(NAME_2, CC_2, nSpez, geometry) %>%
  # cleaning step that should be removed
  distinct(CC_2, .keep_all = TRUE)

## Export der Karte mit Anzahl Neobiota mit tatsächlichem Vorkommen
st_write(regions_nspez_eu, file.path("Shiny","Daten","AnzahlNeobiota_ist_Kreise_EUIAS.shp"), delete_layer=T)

# Vereinfache Grenze fuer große Darstellung
regions_nspez_simp_eu <- ms_simplify(regions_nspez_eu)
st_write(regions_nspez_simp_eu, file.path("Shiny","Daten","AnzahlNeobiota_ist_Kreise_simpl_EUIAS.shp"), delete_layer=T)






##########################################################################################################
## Ermittle potenziell neue Arten ###################################################################

## Namen der Regionen
# regions <- st_read(dsn=file.path("SDM","Data","Input","Shapefiles"),layer="gadm41_DEU_2")
# regions$RegionName <- regions$NAME_2
uni_Kreise <- unique(regions$RegionName)

all_regions <- cbind.data.frame(regions$RegionName,regions$CC_2)
colnames(all_regions) <- c("RegionName","CC_2")


## Pufferzone um Regionen
dist_buff <- 100
units(dist_buff) <- as_units("km")

## Lade Koordinaten
coords <- fread(file.path("Shiny","Daten","Neobiota_AllOccurrences.gz"))

potist <- fread(file.path("Shiny","Daten","Neobiota_IstPot_Liste_Kreise.gz"), colClasses = c("character","character","character","character","character","numeric"))
potist <- merge(potist,all_regions, by="CC_2", all=T)



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

all_pot_spec_eu <- lapply(all_pot_spec, function(s) subset(s, s%in%eu_concern_spec))
saveRDS(all_pot_spec_eu,file.path("Shiny","Daten","Liste_PotSpez_Kreise_EUIAS.rds"))


# plot(st_geometry(poly_buff),axes=T,col="red")
# plot(st_geometry(poly),add=T)
# # points(coords_sub$Laengengrad,coords_sub$Breitengrad,cex=0.1)
# plot(st_geometry(coords_sub),add=T,pch=1,cex=0.2)
# 


##########################################################################################################
## Sammle alle Vorkommen #################################################################################

ident <- "_281123"
path <- file.path("E:","CASPIANII","CASPIANII","SDM","Data","Input")

all_Vorkommen <- list.files(path)
all_Vorkommen <- all_Vorkommen[grep(ident, all_Vorkommen)]
all_Vorkommen <- all_Vorkommen[grep(".csv", all_Vorkommen)]

## Pufferzone um Regionen
dist_buff <- 100
units(dist_buff) <- as_units("km")

germany <- st_read(dsn=file.path("Shiny","Daten"),layer="gadm41_DEU_0")
germany_buff <- ms_simplify(st_buffer(germany, dist_buff))

ext_reg <- st_bbox(germany_buff)

dat_all <- list()
for (i in 1:length(all_Vorkommen)){
  
  dat <- fread(file.path(path,all_Vorkommen[i]))
  dat[,Zeitpunkt:=NULL]
  dat[,Datenbank:=NULL]
  
  dat <- subset(dat, Laengengrad<ext_reg$xmax & Laengengrad>ext_reg$xmin & Breitengrad<ext_reg$ymax & Breitengrad>ext_reg$ymin)
  
  dat_sf <- st_as_sf(dat, coords = c("Laengengrad","Breitengrad"))
  st_crs(dat_sf) <- st_crs(germany_buff)
  
  pts_germ <- st_intersection(dat_sf,germany_buff)
  dat_germ <- as.data.table(do.call(rbind, st_geometry(pts_germ)))
  if (nrow(dat_germ)==0) next
  
  colnames(dat_germ) <- c("Laengengrad","Breitengrad")
  dat_germ[, Taxon:=pts_germ$Taxon]
  
  dat_all[[i]] <- dat_germ
  
  if (i%%10==0) print(i)
}
dat_out <- rbindlist(dat_all)

fwrite(dat_out,file.path("Shiny","Daten","Vorkommen_alleArten.gz"))
# dat_out <- fread(file.path("Shiny","Daten","Vorkommen_alleArten.gz"))

# dat_out_eu <- dat_out[dat_out$Taxon%in%eu_concern_spec,]
# fwrite(dat_out_eu,file.path("Shiny","Daten","Vorkommen_alleArten_EUIAS.gz"))
