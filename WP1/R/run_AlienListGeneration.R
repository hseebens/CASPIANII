graphics.off()
rm(list=ls())

setwd("/home/hanno/Bioinvasion/CASPIANII/WP1")

library(rgbif)
library(openxlsx)

# path_to_scripts <- c("WP1","R")
# path_to_data <- c("WP1","Data")

source(file.path("R","LoadScripts.R"))

## Step 0: Get all data into a spreadsheet (.xlsx) with each data set on one sheet and taxon names in columns 
# called either "Wissenschaftlicher_Name", "Taxon" or "scientificName". 
# File has to be named "ListeGebietsfremderArten_Rohdaten.xlsx" or this should be changed in scripts

# 1. step: load data, standardise column names and taxonomic names

cat("Step 1: Data standardisation running\n")

## names of data set sheets in "ListeGebietsfremderArten_einzelneDB_Rohdaten.xlsx" with available pathways information 
pathway_datasets <- c("Tackenberg_2017","BfN","EASIN_Germany")

run_DataStandardisation(pathway_datasets)

## 2. step: Integrate databases and remove duplicates

cat("Step 2: Data integration running\n")
run_IntegrateAlienSpeciesDataSets()


## 3. step: Get number of available GBIF records

cat("Step 3: obtain GBIF records\n")
get_GBIFrecords()


## 4. step: Add pathway information from Saul et al. 2017

cat("Step 4: add CBD pathway information\n")
get_pathways()

