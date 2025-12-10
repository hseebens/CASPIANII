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
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 09.12.25
################################################################################




## Bereinigen der R Umgebung
rm(list=ls())

setwd(file.path("C:","Hanno","Bioinvasion","CASPIANII","NaVI_app"))


################################################################################
## Laden der notwendigen R Pakete und Datensaetze ##############################

## Lade Funktion 'global' zum Laden von R Paketen und Datensaetzen
source("global.R")


################################################################################
## Laden der Shiny App #########################################################

## Lade Server Funktion ('backend') der Shiny App
source("server.R")

## Lade user interface Funktion ('frontend') der Shiny App
source("ui.R")

## Lade Shiny App
shinyApp(ui = ui, server = server)


# shiny::runGitHub(repo="CASPIANII", subdir="CASPIANII/NaVI_app",username= "hseebens")
