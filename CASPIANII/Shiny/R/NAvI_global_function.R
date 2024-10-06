###########################################################################################################
#
# Shiny App NaVI - Funktion 'global'
#
# Die global Funktion lädt R Pakete und Datensätze, die zur Darstellung der Shiny App NaVI notwendig sind.
#
# Die Shiny App muss über die Funktion ... aufgerufen werden, die wiederum diese Funktion aufruft.
#
# Author: Hanno Seebens, Senckenberg Gesellschaft für Naturforschung
##########################################################################################################


global <- function(){
  
  ## R Pakete der shiny app
  library(shiny)
  library(shinybusy)
  library(leaflet)
  
  ## R Pakete zur Datenbearbeitung
  library(sf) 
  library(DT) 
  library(dplyr)
  library(tidyr)
  library(data.table)
  library(units)
  
  
  # setwd(file.path("Shiny","R"))
  
  
  
  ## load data----
  
  dist_buff <<- 100
  units(dist_buff) <- as_units("km")
  dist_buff <<- dist_buff # make it a global variable
  
  # polygon data sets
  map_fine <<- st_read(dsn="Daten", layer= "AnzahlNeobiota_ist_Kreise")
  uni_regs <<- sort(unique(map_fine$NAME_2))
  all_regs <- unique(cbind.data.frame(map_fine$NAME_2,map_fine$CC_2))
  colnames(all_regs) <- c("RegionName","CC_2")
  all_regs <<- all_regs # make it a global variable
  
  map_simp <<- st_read(dsn="Daten",layer= "AnzahlNeobiota_ist_Kreise_simpl")
  map_simp_eu <<- st_read(dsn="Daten",layer= "AnzahlNeobiota_ist_Kreise_simpl_EUIAS")
  map_simp_han <<- st_read(dsn="Daten",layer= "AnzahlNeobiota_ist_Kreise_simpl_HanL")
  map_simp_beo <<- st_read(dsn="Daten",layer= "AnzahlNeobiota_ist_Kreise_simpl_BeoL")
  map_simp_man <<- st_read(dsn="Daten",layer= "AnzahlNeobiota_ist_Kreise_simpl_ManL")
  map_simp_akt <<- st_read(dsn="Daten",layer= "AnzahlNeobiota_ist_Kreise_simpl_AktL")
  
  
  ## Species lists by region #####################################################
  
  ## All neobiota ###############
  spec_istpot <- fread(file.path("Daten","Neobiota_IstPot_Liste_Kreise.gz"), 
                       colClasses=c("character","character","character","character","character","character","numeric"))
  setorderv(spec_istpot,cols=c("NAME_2","HabitatEignung"),order=-1)
  colnames(spec_istpot) <- c("NAME_2","CC_2","Art","Gruppe","Ist","Pot","Habitateignung (0-1)")
  spec_istpot_regs <- merge(spec_istpot,all_regs, by="CC_2", all=T)
  
  
  ## EU IAS list ################
  spec_istpot_eu <- fread(file.path("Daten","Neobiota_IstPot_Liste_Kreise_EUIAS.gz"), 
                          colClasses=c("character","character","character","character","character","character","numeric"))
  setorderv(spec_istpot_eu,cols=c("NAME_2","HabitatEignung"),order=-1)
  colnames(spec_istpot_eu) <- c("NAME_2","CC_2","Art","Gruppe","Ist","Pot","Habitateignung (0-1)")
  spec_istpot_regs_eu <- merge(spec_istpot_eu,all_regs, by="CC_2", all=T)
  
  ## Handlungsliste ################
  spec_istpot_han <- fread(file.path("Daten","Neobiota_IstPot_Liste_Kreise_HanL.gz"), 
                           colClasses=c("character","character","character","character","character","character","numeric"))
  setorderv(spec_istpot_han,cols=c("NAME_2","HabitatEignung"),order=-1)
  colnames(spec_istpot_han) <- c("NAME_2","CC_2","Art","Gruppe","Ist","Pot","Habitateignung (0-1)")
  spec_istpot_regs_han <- merge(spec_istpot_han,all_regs, by="CC_2", all=T)
  
  ## Beobachtungsliste ################
  spec_istpot_beo <- fread(file.path("Daten","Neobiota_IstPot_Liste_Kreise_BeoL.gz"), 
                           colClasses=c("character","character","character","character","character","character","numeric"))
  setorderv(spec_istpot_beo,cols=c("NAME_2","HabitatEignung"),order=-1)
  colnames(spec_istpot_beo) <- c("NAME_2","CC_2","Art","Gruppe","Ist","Pot","Habitateignung (0-1)")
  spec_istpot_regs_beo <- merge(spec_istpot_beo,all_regs, by="CC_2", all=T)
  
  ## Aktionsliste ################
  spec_istpot_akt <- fread(file.path("Daten","Neobiota_IstPot_Liste_Kreise_AktL.gz"), 
                           colClasses=c("character", "character","character","character","character","character","numeric"))
  setorderv(spec_istpot_akt,cols=c("NAME_2","HabitatEignung"),order=-1)
  colnames(spec_istpot_akt) <- c("NAME_2","CC_2","Art","Gruppe","Ist","Pot","Habitateignung (0-1)")
  spec_istpot_regs_akt <- merge(spec_istpot_akt,all_regs, by="CC_2", all=T)
  
  ## Managementliste ################
  spec_istpot_man <- fread(file.path("Daten","Neobiota_IstPot_Liste_Kreise_ManL.gz"), 
                           colClasses=c("character", "character", "character", "character", "character", "character", "numeric"))
  setorderv(spec_istpot_man,cols=c("NAME_2","HabitatEignung"),order=-1)
  colnames(spec_istpot_man) <- c("NAME_2", "CC_2", "Art", "Gruppe", "Ist", "Pot", "Habitateignung (0-1)")
  spec_istpot_regs_man <- merge(spec_istpot_man,all_regs, by="CC_2", all=T)
  
  
  
  # species occurrences
  point_data <- fread(file.path("Daten","Vorkommen_alleArten.gz"))
  point_data$DBcols <- as.numeric(as.factor(point_data$Datenbank))
  point_data <<- point_data # make it a global variable
  
  uni_spec <<- unique(point_data$Taxon)
  all_cols <<- c('royalblue3','darkgreen','skyblue1')
  all_DBs  <<- sort(unique(point_data$Datenbank))

  
  ## potential new arrivals
  all_pot_spec <<- readRDS(file.path("Daten","Liste_PotSpez_Kreise.rds"))

  ## common names
  common_names <- fread(file.path("Daten","Tabelle_DeutscheName.csv"))
  colnames(common_names) <- c("Deutscher Artname", "Taxon_wissensch")
  
  region_lists <<- merge(spec_istpot_regs,common_names,by.x="Art", by.y="Taxon_wissensch")
  region_lists_eu <<- merge(spec_istpot_regs_eu,common_names,by.x="Art", by.y="Taxon_wissensch")
  region_lists_han <<- merge(spec_istpot_regs_han,common_names,by.x="Art", by.y="Taxon_wissensch")
  region_lists_beo <<- merge(spec_istpot_regs_beo,common_names,by.x="Art", by.y="Taxon_wissensch")
  region_lists_akt <<- merge(spec_istpot_regs_akt,common_names,by.x="Art", by.y="Taxon_wissensch")
  region_lists_man <<- merge(spec_istpot_regs_man,common_names,by.x="Art", by.y="Taxon_wissensch")
  
}
