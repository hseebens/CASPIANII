graphics.off()
rm(list=ls())

library(sf)
library(data.table)
library(raster)
library(RColorBrewer)

setwd("/home/hanno/Bioinvasion/CASPIANII/WP1")

## load data ############################


## alien population coordinates ####
alien_coords_OBIS <- fread(file=file.path("Data","DASCO_OBISCoords_SInAS_2.4.1_Germany.gz"))
alien_coords_GBIF <- fread(file=file.path("Data","DASCO_GBIFCoords_SInAS_2.4.1_Germany.gz"))
GBIFkeys <- fread(file=file.path("Data","GBIF_SpeciesKeys_SInAS_2.4.1_Germany.csv"))
ind <- duplicated(GBIFkeys$speciesKey)
GBIFkeys <- GBIFkeys[!ind,] # remove duplicated species keys (note: mostly applies to taxa, which were not found in GBIF taxonomy; selection of taxon names is somewhat arbitrary (first in row))
columns <- c("speciesKey","canonicalName")
GBIFkeys <- unique(GBIFkeys[,..columns])
alien_coords_GBIFkeys <- merge(alien_coords_GBIF,GBIFkeys,by="speciesKey")
setnames(alien_coords_GBIFkeys,c("speciesKey","location","Realm","Longitude","Latitude","taxon"))

# GBIFkeys[speciesKey==4309237]
alien_coords_all <- unique(rbind(alien_coords_GBIFkeys[,c("taxon","location","Realm","Longitude","Latitude")],alien_coords_OBIS))
alien_coords <- alien_coords_all

# #### Subset to invasive alien species ################
# ## identify invasive species according to GRIIS
# SInAS <- fread(file=file.path("..","IndicatorAliens","Workflow","SInAS","Output","SInAS_AlienSpeciesDB_2.4.1.csv"))
# SInAS_inv <- subset(SInAS,grepl("INVASIVE",SInAS$isInvasive))
# 
# alien_coords <- subset(alien_coords_all,taxon%in%unique(SInAS_inv$Taxon))
# ######################################################

# #### Subset to taxonomic group ################
# SInAS_tax <- fread(file=file.path("..","IndicatorAliens","Workflow","SInAS","Output","SInAS_AlienSpeciesDB_2.4.1_FullTaxaList.csv"))
# SInAS_tax$TaxGroup <- NA
# 
# SInAS_tax$TaxGroup[SInAS_tax$class=="Mammalia"] <- "Mammals"
# SInAS_tax$TaxGroup[SInAS_tax$class=="Aves"] <- "Birds"
# SInAS_tax$TaxGroup[SInAS_tax$class%in%c("Cephalaspidomorphi","Actinopterygii","Elasmobranchii","Sarcopterygii")] <- "Fishes"
# SInAS_tax$TaxGroup[SInAS_tax$class=="Reptilia"] <- "Reptiles"
# SInAS_tax$TaxGroup[SInAS_tax$class=="Amphibia"] <- "Amphibians"
# SInAS_tax$TaxGroup[SInAS_tax$class=="Insecta"] <- "Insects"
# SInAS_tax$TaxGroup[SInAS_tax$phylum=="Arthropoda"] <- "Arthropods"
# # arthropods <- subset(sTwist_tax,phylum=="Arthropoda")
# # arthropods_nocrustaceans <- subset(arthropods,!class%in%c("Branchiopoda","Hexanauplia","Maxillopoda","Ostracoda","Malacostraca"))
# SInAS_tax$TaxGroup[SInAS_tax$scientificName%in%arthropods_nocrustaceans$scientificName] <- "Arthropods (excl. crustaceans)"
# SInAS_tax$TaxGroup[SInAS_tax$class%in%c("Branchiopoda","Hexanauplia","Maxillopoda","Ostracoda","Malacostraca")] <- "Crustaceans"
# SInAS_tax$TaxGroup[SInAS_tax$phylum=="Mollusca"] <- "Molluscs"
# SInAS_tax$TaxGroup[SInAS_tax$phylum=="Tracheophyta"] <- "Vascular plants"
# SInAS_tax$TaxGroup[SInAS_tax$phylum%in%c("Bryophyta","Anthocerotophyta")] <- "Bryophytes"
# SInAS_tax$TaxGroup[SInAS_tax$phylum%in%c("Rhodophyta","Chlorophyta","Charophyta","Cryptophyta","Euglenozoa","Haptophyta","Foraminifera","Ciliophora","Ochrophyta","Myzozoa","Cercozoa")] <- "Algae"
# SInAS_tax$TaxGroup[SInAS_tax$phylum%in%c("Ascomycota","Chytridiomycota","Basidiomycota","Microsporidia","Oomycota","Zygomycota")] <- "Fungi"
# SInAS_tax$TaxGroup[SInAS_tax$phylum%in%c("Actinobacteria","Chlamydiae","Cyanobacteria","Firmicutes","Proteobacteria")] <- "Bacteria and protozoans"
# 
# # # SInAS_sub <- subset(SInAS_tax,TaxGroup=="Birds")$Taxon
# # # SInAS_sub <- subset(SInAS_tax,TaxGroup=="Vascular plants")$Taxon
# # # SInAS_sub <- subset(SInAS_tax,TaxGroup=="Mammals")$Taxon
# # # SInAS_sub <- subset(SInAS_tax,TaxGroup=="Insects")$Taxon
# SInAS_sub <- subset(SInAS_tax,TaxGroup=="Arthropods")$Taxon
# 
# alien_coords <- subset(alien_coords_all,taxon%in%SInAS_sub)
# ######################################################


# example_raster <- raster(file.path("Data","HS_IAS_Biomes.tif"))
# example_raster <- raster(ncols=1296,nrows=648)
example_raster <- raster(ncols=500,nrows=425) # factor 0.85
# countries <- raster(file.path("Data","HS_IAS_Regions.tif"))
all_countries <- st_read(dsn=file.path("Data"),layer="RegionsTerrMarine_160621")
all_countries <- st_transform(all_countries,crs=4326)
countries <- st_read(dsn=file.path("Data"),layer="RegionsTerrMarine_160621_Germany")
countries <- st_transform(countries,crs=4326)
# mainlands <- st_read(dsn=file.path("..","DASCO","PrepareData"),layer="One_regs_poly")
# mainlands <- st_transform(mainlands,crs=4326)

germany <- countries[countries$Location=="Germany",]
extent(example_raster) <- extent(germany)


## check coordinate precision ###################
ind_long <- ((alien_coords$Longitude*1000)%%1==0)
ind_lat <- ((alien_coords$Latitude*1000)%%1==0)
table((alien_coords$Longitude*10)%%1==0)
alien_coords[ind_long,]

ind_imprecise <- nchar(sub('[0-9]+\\.', '', alien_coords$Latitude)) < 5
# alien_coords <- alien_coords[!ind_imprecise,]


## loop over all species to calculate species richness per cell
uni_spec <- unique(alien_coords$taxon)
all_rasters <- example_raster
values(all_rasters) <- 0
for (i in 1:length(uni_spec)){
  raster_aliens <- rasterize(subset(alien_coords,taxon==uni_spec[i])[,c("Longitude","Latitude")],example_raster)
  values(raster_aliens)[!is.na(values(raster_aliens))] <- 1
  values(raster_aliens)[is.na(values(raster_aliens))] <- 0
  all_rasters <- all_rasters + raster_aliens
  if (i%%100==0) print(i)
}
writeRaster(all_rasters,file.path("Data","GermanAlienSpecNumbers_SInAS2.4.1.tif"),overwrite=T)

values(all_rasters)[values(all_rasters)==0] <- NA
values(all_rasters)[values(all_rasters)>100] <- 100

plot(all_rasters)
plot(st_geometry(countries),add=T)

points(alien_coords[1:500000,c("Longitude","Latitude")],pch=16,cex=0.1)


# all_rasters <- raster(file.path("Data","InvasiveAlienSpecNumbersGlobal_2.4.1.tif"))
# values(all_rasters)[values(all_rasters)==0] <- NA


## plot number of alien species raster ###############################
getcols <- colorRampPalette((brewer.pal(11,"Spectral")))
# getcols <- colorRampPalette((brewer.pal(9,"YlOrRd")))
cols <- (getcols(100))

x11(width=4,height=4)
# png(file.path("Figures","AlienSpeciesNumbers_DASCO_Raster.png"),unit="in",width=4,height=4,res=300)
op <- par(mar = c(0,0,0,0.5))
plot(all_rasters,axes=FALSE, box=FALSE)
plot(st_geometry(all_countries),add=T,lwd=0.5)
text(16.2,53.12,">",xpd=T)
par(op)
# dev.off()


## Number of records

x11(width=4,height=4)
# png(file.path("Figures","AlienSpeciesRecords_DASCO_Raster.png"),unit="in",width=4,height=4,res=300)
op <- par(mar = c(0,0,0,0.5))
plot(all_rasters,axes=FALSE, box=FALSE,legend=FALSE)
plot(st_geometry(all_countries),add=T,lwd=0.5)
# points(alien_coords[,c("Longitude","Latitude")],pch=16,cex=0.1)
# text(16.2,53.12,">",xpd=T)
par(op)
# dev.off()
