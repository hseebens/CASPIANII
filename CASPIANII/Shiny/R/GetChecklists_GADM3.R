rm(list=ls())
graphics.off()

library(data.table)
library(sf)
library(terra)


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

##########################################################################################################
## Daten zum tatsächlichen Vorkommen #####################################################################

## Lade Daten der Kreise
all_sites_df <- fread(input=file.path("SDM","Data","Output", paste0("istVorkommenAlleArten_Kreise",identifier,".gz")),colClasses=c("character","character"))
all_sites_df <- all_sites_df[!is.na(all_sites_df$CC_2),]

## Lade Grenzen der Kreise
germany_Kreise <- st_read(dsn=file.path("SDM","Data","Input","Shapefiles"),layer="gadm41_DEU_2")

germany_Kreise <- unique(st_drop_geometry(germany_Kreise)[,c("CC_2","NAME_2")])
germany_Kreise <- germany_Kreise[!is.na(germany_Kreise$CC_2),]

# ind <- which(is.na(germany_Kreise[,1]))
# germany_Kreise[ind,]

## Export Liste der Neobiota fuer Kreise
all_data_ist <- merge(all_sites_df,germany_Kreise,by="CC_2",all=T)

fwrite(all_data_ist,file.path("Shiny","Daten","Neobiota_ist_Liste_Kreise.gz"))

## Export Anzahl Neobiota fuer Kreise
setkey(all_data_ist,NAME_2)
nSpec_obs <- all_data_ist[,list(nSpec_obs=length(Taxon)),by=c("NAME_2")]
colnames(nSpec_obs) <- c("NAME_2","nSpez")

fwrite(nSpec_obs,file.path("Shiny","Daten","Neobiota_ist_nSpez_Kreise.gz"))






##########################################################################################################
## Daten zum potenziellen Vorkommen ######################################################################

## Lade Daten zum potenziellen Vorkommen
all_sites_df <- fread(file.path("SDM","Data","Output", paste0("potVorkommen_alleArten_Eignung07_Kreise",identifier,".gz")), colClasses = c("character","numeric","character"))
all_sites_df <- all_sites_df[!is.na(all_sites_df$HabitatEignung),]

all_data_pot <- merge(all_sites_df,germany_Kreise,by="CC_2",all=T) # verbinde mit Kreisgrenzen

setkey(all_data_pot,CC_2,HabitatEignung) 
setorderv(all_data_pot,cols=c("NAME_2","HabitatEignung"),order=c(1,-1)) # sortiere Datensatz

## Export Liste der potenziellen Neobiota pro Kreis
fwrite(all_data_pot,file.path("Shiny","Daten","Neobiota_pot_Liste_Kreise.gz"))

## Kombiniere potenzielle und tatsächliche Vorkommen
all_data_pot[,Pot:="x"]
all_data_ist[,Ist:="x"]

all_ist_pot <- merge(all_data_ist,all_data_pot,by=c("CC_2","NAME_2","Taxon"),all=T)
all_ist_pot <- all_ist_pot[,c("NAME_2","CC_2","Taxon","Ist","Pot","HabitatEignung")]

## Export der Liste Neobiota mit tatsächliche und potenziellen Vorkommen fuer Kreise
fwrite(all_ist_pot,file.path("Shiny","Daten","Neobiota_IstPot_Liste_Kreise.gz"))
