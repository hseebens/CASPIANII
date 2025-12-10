################################################################################
#
# Shiny App NaVI - Funktion 'global'
#
# Die globale Funktion lädt R Pakete und Datensätze, die zur Darstellung der 
# Shiny App NaVI notwendig sind.
#
# Author: Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 11.11.25
################################################################################


packages <- c("shiny", "shinybusy", "leaflet", "markdown", "maps", "sf","DT",
              "dplyr","tidyr", "data.table", "units") 

new.packages <- packages[!(packages %in% installed.packages()[,"Package"])] # check which of them is not yet installed
if(length(new.packages)) install.packages(new.packages); rm(new.packages) # install them

l <- sapply(packages, function(s) suppressMessages(suppressWarnings(require(s, quietly=T, character.only = TRUE)))); rm(packages, l) # load all required packages


## R Pakete der shiny app
library(shiny)
library(shinybusy)
library(leaflet)
library(markdown)

## R Pakete zur Datenbearbeitung
library(sf) 
library(DT) 
library(dplyr)
library(tidyr)
library(data.table)
library(units)


load(file="All_data_for_NaVI.RData")

# all_reg_data <- fread("RegList_AllData.csv")
# regions <- st_read(dsn=file.path("DatenAufbereitung","Daten"),layer="gadm41_DEU_2")
