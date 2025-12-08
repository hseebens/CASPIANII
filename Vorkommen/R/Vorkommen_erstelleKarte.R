##################### Ermittlung von Vorkommensdaten fuer einzelne Arten #######
# 
# Skript ist Teil des Workflows "Vorkommen" zur Darstellung der Vorkommen von
# Arten in Deutschland. Es wird mit dem Skript erstelleListeNeobiota_MAIN.R 
# aufgerufen.
#
# Das Skript stellt Vorkommensdaten einer Art in einer interaktiven Karte dar.
# 
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 08.12.25
################################################################################



Vorkommen_erstelleKarte <- function(Daten_Vorkommen=NULL, Jahr=NA){

  records <- Daten_Vorkommen
  
  if (!any(is.na(Jahr))){
    records <- records[year(records$Zeitpunkt) %in% Jahr, ]
  }
  records$col <- as.numeric(as.factor(records$Datenbank))
  all_cols <- c('red', 'orange','black', 'blue')
  all_DBs <- sort(unique(records$Datenbank))
  
  pal <- colorFactor(
    palette = all_cols,
    domain = records$col
  )
  
  uni_col_DB <- unique(cbind.data.frame(records$Datenbank,pal(records$col)))
  
  map_records <- leaflet(records) %>% addTiles() %>% 
    clearBounds() %>%
    addCircleMarkers(lng=~Laengengrad, lat=~Breitengrad,radius = 2, weight = 1,color=~pal(col),fillOpacity = 0.5) %>%
    addLegend("bottomright", colors=uni_col_DB[,2] ,labels=uni_col_DB[,1],
            title = "Datenbank",
            opacity = 1
  )
  
  return(map_records)
}
