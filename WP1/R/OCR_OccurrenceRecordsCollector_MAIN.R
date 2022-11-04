##################### Extract occurrence records for individual species ######################################
# 
# This script sends requests to individual data sources and collates available occurrence records for 
# individual species. 
# 
# databases: GBIF, iNat, sMon, OBIS, NA (if NA, no records are extracted and only own_records used)
#
# Minimum required input: taxon_name (name of taxon); database or own_records
# Function output: List of coordinates and date of record
# 
# 
# Project: CASPIAN II
# 
# Hanno Seebens, 04.11.22
###############################################################################################################


## clear environment ##########################
graphics.off()
rm(list=ls())

## load required packages #####################
library(rgbif)
library(robis)
library(spocc)
library(leaflet)
library(data.table)

# setwd("/home/hanno/Bioinvasion/CASPIANII")

## load required functions ####################
source(file.path("WP1","R","CASPIANII_loadScripts.R"))





##########################################################################
## Load occurrence records ###############################################
##########################################################################


## Parameters to specify search ###############################

## folder where sMon data are stored
sMon_folder <- "/home/hanno/Storage_large/Species/sMon"

## folder where own data are stored
owndata_folder <- ""
owndata_filename <- ""

## selected databases for occurrence records (possible: OBIS, GBIF, iNat, sMon)
database <- c("OBIS","GBIF","iNat")# 

## bounding box of area to search for occurrence records giving in geographic coordinate (long-lat)
bounding_box <- c(5,45,15,58) # lower left and upper right corner (long-lat)


## get occurrence records from suggested databases ###########

# records <- get_occurrence_records(taxon_name="Corbicula fluminea",database=c("OBIS","GBIF","iNat"))
records <- get_occurrence_records(taxon_name="Crassostrea gigas", database=c("OBIS","GBIF","iNat"), bounding_box = bounding_box)
# records <- get_occurrence_records(taxon_name="Acer negundo",database=c("sMon","GBIF","iNat"),sMon_folder=sMon_folder)
# records <- get_occurrence_records(taxon_name="Rhaponticum repens",database=c("sMon","GBIF","iNat"),sMon_folder=sMon_folder)



## Plot occurrence records ###################################

map_records(records)

