##################### Generate a list alien species for Germany ##############################################
# 
# This scripts execute a series of scripts to prepare, standardise and integrate lists of alien species
# provided in various sources. Standardisation covers the 1. taxonomic information (i.e., taxon names are
# cross-checked with the GBIF backbone taxonomy to identify and correct spelling errors, synonyms, and
# to obtain the taxonomic tree for each taxon), 2. year of first record and 3. information about the
# introduction pathway. Subsequently, the numbers of occurrence records available in GBIF and sMon are added.
# 
# Project: CASPIAN II
# 
# Hanno Seebens, 18.07.22
###############################################################################################################


## clear environment ##########################
graphics.off()
rm(list=ls())

## load required packages #####################
library(rgbif)
library(openxlsx)
library(data.table)
library(robis)
library(spocc)

setwd("/home/hanno/Bioinvasion/CASPIANII")

## load required functions ####################
source(file.path("WP1","R","CASPIANII_loadScripts.R"))


##########################################################################
## Generate alien species lists ##########################################
##########################################################################


## Parameters for data extraction #######################################

## names of data set sheets in "ListeGebietsfremderArten_einzelneDB_Rohdaten.xlsx" with available pathways information 
pathway_datasets <- c("Tackenberg_2017","BfN","EASIN_Germany")

## folder where sMon data are stored
sMon_folder <- "/home/hanno/Storage_large/Species/sMon"


## Generation of alien species list #########################

## Step 0: Get all data into a spreadsheet (.xlsx) with each data set on one sheet and taxon names in columns 
# called either "Wissenschaftlicher_Name", "Taxon" or "scientificName". 
# File has to be named "ListeGebietsfremderArten_Rohdaten.xlsx" or this should be changed in scripts


# 1. step: load data, standardise column names and taxonomic names

cat("\nStep 1: Data standardisation running\n")
run_DataStandardisation(pathway_datasets)


## 2. step: Integrate databases and remove duplicates

cat("\nStep 2: Data integration running\n")
run_IntegrateAlienSpeciesDataSets()


## 3. step: Get number of available GBIF records

cat("\nStep 3: obtain GBIF records\n")
get_GBIFrecords()


## 4. step: Add pathway information from Saul et al. 2017

cat("\nStep 4: add CBD pathway information\n")
get_pathways()


## 5. step: Add number of available records from sMon

cat("\nStep 5: add sMon records\n")
get_nRecords_sMon(sMon_folder)



