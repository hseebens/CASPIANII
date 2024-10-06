################################################################################
#
# Shiny App "NaVI - Neobiota an Verkehrswegen Informationssystem"
#
# Das Skript laedt und startet die Shiny App NaVI. 
# Das Skript die folgenden Skripte auf:
#
# 1. NAvI_global_function.R: Laden der notwendigen R Pakete und Datensaetze
# 2. NAvI_server_function.R: Laden des 'backends' der Shiny App
# 3. NAvI_userInterface_function.R: Laden des 'frontends' der Shiny App
#
# Die Shiny App NaVI dient der Darstellung und Bereitstellung von Datensaetzen 
# und Karten von gebietsfremden und invasiven Arten. Die zugrundliegenden 
# Datensaetze können mit der Funktion 'DataPreparationShiny.R' erzeugt werden.
#
# Author: Hanno Seebens, Senckenberg Gesellschaft für Naturforschung
################################################################################




## Bereinigen der R Umgebung
rm(list=ls())

# setwd(file.path("NaVI_app"))


################################################################################
## Laden der notwendigen R Pakete und Datensaetze ##############################

## Lade Funktion 'global' zum Laden von R Paketen und Datensaetzen
source("NAvI_global_function.R")

## Lade R Pakete und Datensaetze
global()


################################################################################
## Laden der Shiny App #########################################################

## Lade Server Funktion ('backend') der Shiny App
source("NAvI_server_function.R")

## Lade user interface Funktion ('frontend') der Shiny App
source("NAvI_userInterface_function.R")

## Lade Shiny App
shinyApp(ui = ui, server = server)


# shiny::runGitHub(repo="CASPIANII", subdir="CASPIANII/NaVI_app",username= "hseebens")
