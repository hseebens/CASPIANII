################################################################################
#
# Shiny App NaVI - Funktion 'global'
#
# Die global Funktion lädt R Pakete und Datensätze, die zur Darstellung der 
# Shiny App NaVI notwendig sind.
#
# Die Shiny App muss über die Funktion ... aufgerufen werden, die wiederum 
# diese Funktion aufruft.
#
# Author: Hanno Seebens, Senckenberg Gesellschaft für Naturforschung
################################################################################



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


