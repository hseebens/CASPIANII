###################################################################################
# extract species occurrence records from iNaturalist
# 
# Hanno Seebens, 13.06.2022
###################################################################################

rm(list=ls())
graphics.off()

library(spocc) # accessing databases
library(data.table) # read data efficiently
# library(censable) # adding access API key for eBird
library(sf) # reading shapefile


### Generate an eBird kay beforehand on https://ebird.org/api/keygen !

add_r_environ(value="kk1nvvf18rea",name="EBIRD_KEY")
# Sys.getenv() # check if including in .Renviron


### load species list #########################################

all_species <- fread(file.path("WP1","Data","GAVIA_Germany.csv"))



### identify extent of area of interest for downloading records (here Germany) #######################
regions <- st_read(dsn=file.path("WP1","Data","Shapefiles"),layer="RegionsTerrMarine_160621_Germany",stringsAsFactors = F)

# ## get spatial extent
# bounding_box <- st_bbox(regions)
# 
# ## enlarge bounding box to also cover buffer zones
# bounding_box[1] <- bounding_box[1] -1
# bounding_box[2] <- bounding_box[2] -1
# bounding_box[3] <- bounding_box[3] +1
# bounding_box[4] <- bounding_box[4] +1
# 
# ## define WKT string to define area for GBIF request
# WKT_string <- paste('POLYGON((',
#                     bounding_box[1],bounding_box[2],",",
#                     bounding_box[3],bounding_box[2],",",
#                     bounding_box[3],bounding_box[4],",",
#                     bounding_box[1],bounding_box[4],",",
#                     bounding_box[1],bounding_box[2],
#                     "))",sep=" ")

### bulk download from eBird ####################

# note: bulk download from iNaturalist does not work for large requests -> done via GBIF

# occ_options("ebird")

ebirdopts <- list(loc = 'DE') # search in Germany only

out <- occ(query = all_species$Taxon, from = c('ebird')
           ,ebirdopts = ebirdopts
           # ,geometry=regions # does not work
           ,limit = 50000
           ,has_coords=T
           )

dat <- occ2df(out)

## plot records ######################################
plot(st_geometry(regions),axes=T)
points(dat$longitude,dat$latitude,cex=0.5,col="red")

head(dat); tail(dat)
