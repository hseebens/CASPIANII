################################################################################
#
# Dieses Skript ist Teil der Shiny App "NaVI" und bereitet die notwendigen 
# Datensätze auf, so dass sie von NaVI eingelesen werden können. Die notwendigen
# Datensätze werden von den Workflows "ListeNeobiota", "SDM" und "Ausbreitung" 
# erzeugt. Folgende Datensätze müssen in den jeweiligen Verzeichnissen vorhanden 
# sein (alternativ können die Datensätze ins Verzeichnis NaVI_app/Datenaufbereitung/Daten/
# kopiert werden, wobei dann die Pfade in diesem Skript angepasst werden müssen):
#
# - gadm41_DEU_2 (*.shp, *.dbf, *.shx, *.prj): Polygone der Landkreise von 
#    Deutschland. Zu beziehen von www.gadm.org
#    Verzeichnis: NaVI_app/Datenaufbereitung/Daten/
# - ListeGebietsfremderArten_gesamt_standardisiert.xlsx: Erstellt mit dem 
#    Workflow "ListeNeobiota" aus dem Projekt CASPIAN II
#    Verzeichnis: ListeNeobiota/Daten/Output/
# - istVorkommenAlleArten_Kreise_[identifier].gz: Liste der tatsächlichen 
#    Vorkommen der Neobiota pro Landkreis. Der "identifier" ist eine vom Nutzer
#    gewählte Kennzeichnung des zu verwendenden Durchlaufs des Workflows "SDM" 
#    (z.B. "100925")
#    Verzeichnis: SDM/Daten/Output/
# - InvasionsRisiken_Landkreise_[identifier]_all.csv: Liste von Neobiota mit 
#    dem berechneten Invasionspotenzial für den jeweiligen Landkreis. Der 
#    "identifier" ist eine vom Nutzer gewählte Kennzeichnung des zu verwendenden 
#    Durchlaufs des Workflows SDM (z.B. "100925")
#    Verzeichnis: Ausbreitung/Daten/
#
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 09.12.25
################################################################################


#### Vorbereitung der Arbeitsschritte ##########################################

## Bereinigen der Arbeitsumgebung
rm(list=ls())
graphics.off()

## Laden der R Pakete
library(data.table)
library(sf)
library(units)
library(openxlsx)

## Setzen des Arbeitsverzeichnis ########
## (geschieht automatisch beim Öffnen des Rstudio Projekts mit "CASPIANII.Rproj")
# setwd(file.path("Path","To","Main","Folder"))
setwd(file.path("C:","Hanno","Bioinvasion","CASPIANII"))


## Name des jeweiligen Modelllaufs (wie im Workflow "SDM" gewählt)
identifier <- "GiveItAName"
identifier <- "230925"

## Standard des identifiers
if (strtrim(identifier,1) != "_"){
  identifier <- paste0("_",identifier)
}


##### Import der notwendigen Datensätze ########################################

## Lade Grenzen der Kreise von Deutschland
regions <- st_read(dsn=file.path("SDM","Daten","Input","Shapefiles"), layer="gadm41_DEU_2")

## Datensatz mit Namen der Kreise
regions_df <- unique(st_drop_geometry(regions)[,c("CC_2","NAME_2")])
regions_df <- regions_df[!is.na(regions_df$CC_2),]

## Definiere Pufferzone um Regionen
dist_buff <- 100
units(dist_buff) <- as_units("km")

## Vereinheitlichte Liste der Arten der EU Liste und BfN Listen
list_aliens <- unique(read.xlsx(file.path("ListeNeobiota", "Daten", "Output",
                                          "ListeGebietsfremderArten_gesamt_standardisiert.xlsx")))


## Laden der beobachteten Neobiota in Landkreisen #####################
all_sites_ist <- fread(input=file.path("SDM", "Daten", "Output",
                                       paste0("istVorkommenAlleArten_Kreise", identifier, ".gz")),
                       colClasses=c("character","character"))
all_sites_ist <- all_sites_ist[!is.na(all_sites_ist$CC_2),]
all_sites_ist[,Status:="Ist"]
all_sites_ist[,Invasionspotenzial:=0]


## Laden der Ausbreitungspotenziale alle Arten und Landkreise ##########
invasion_risks <- fread(file.path("Ausbreitung", "Daten", paste0("InvasionsRisiken_Landkreise", identifier, "_all.csv")))
invasion_risks <- invasion_risks[!is.na(invasion_risks$prob_invasion),]

# invasion_risks[which.max(subset(invasion_risks, focal_region=="Ahrweiler")$prob_invasion),]
# subset(invasion_risks, focal_region=="Ahrweiler" & Taxon=="Brunnera macrophylla")

invasion_risks[, Status:="Pot"]
colnames(invasion_risks)[colnames(invasion_risks)=="focal_region"] <- "RegionName"
colnames(invasion_risks)[colnames(invasion_risks)=="prob_invasion"] <- "Invasionspotenzial"
invasion_risks <- merge(invasion_risks, regions_df, by.x="RegionName", by.y="NAME_2")
invasion_risks <- invasion_risks[, c("Taxon", "CC_2", "Status", "Invasionspotenzial")]
invasion_risks$Invasionspotenzial <- round(invasion_risks$Invasionspotenzial, 4)

## Kombination der beobachteten Vorkommen und des Ausbreitungspotenzials
all_ist_pot <- rbind(all_sites_ist, invasion_risks)

## entferne potenzielle Einträge, wenn Art beobachtet ist
ind <- (duplicated(all_ist_pot[, c("Taxon","CC_2")]))
all_ist_pot <- all_ist_pot[!(ind & Status=="Pot"), ]

## ergänze Informationen zu Managementlisten
all_data <- merge(all_ist_pot, list_aliens[, c("Taxon", "Artname", "EU_Anliegen", "BfNlisten", "Artengruppe")], by="Taxon")
all_reg_data <- merge(all_data, regions_df, by="CC_2")

## sortiere Datensatz alphabetisch
oo <- order(all_reg_data$NAME_2, all_reg_data$Invasionspotenzial, decreasing=TRUE)
all_reg_data[oo,]

## Anpassung der Variablennamen
colnames(all_reg_data)[colnames(all_reg_data)=="NAME_2"] <- "RegionName"
# colnames(all_reg_data)[colnames(all_reg_data)=="Artengruppe"] <- "TaxonGruppe"
colnames(all_reg_data)[colnames(all_reg_data)=="Artname"] <- "Deutscher Artname"


## Aufbereitung der Grenzen der Kreise
regions$RegionName <- regions$NAME_2
regions <- regions[!is.na(regions$CC_2),]
regions <- regions[, c("CC_2", "RegionName", "geometry")]
regions$CC_2 <- as.integer(regions$CC_2)

## Laden aller Vorkommen der Arten 
all_coords <- fread(file.path("SDM","Daten", "Output", paste0("istVorkommenAlleArten_Koordinaten", identifier, ".gz")), fill=TRUE)
all_coords <- subset(all_coords, Laengengrad>5  & Laengengrad<16 )
all_coords <- subset(all_coords, Breitengrad>47 & Breitengrad<56 )

point_data <- subset(all_coords, Taxon %in% unique(list_aliens$Taxon)) # remove some entries listed in the ListeNeobiota...xlsx

# point_data$DBcols <- as.numeric(as.factor(point_data$Datenbank))
# all_cols <- c('royalblue3','darkgreen','skyblue1')


# Isodontia mexicana
# 
# x11()
# dat <- subset(point_data, Taxon=="Acacia saligna")
# plot(dat$Laengengrad, dat$Breitengrad)


## Exportiere alle Datensätze in einem file ####################################

save(regions, 
     all_reg_data,
     point_data,
     # all_cols,
     dist_buff,
     file=file.path("NaVI_app", "All_data_for_NaVI.RData"))
