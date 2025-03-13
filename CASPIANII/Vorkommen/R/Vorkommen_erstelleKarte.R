##################### Plot occurrence records on a map ######################################################
# 
# This function plots occurrence records of species (lat-long) on an interactive map using leaflet.
#
# Project: CASPIAN II
# 
# Senckenberg Gesellschaft fuer Naturforschung, 28.11.22
###############################################################################################################



Vorkommen_erstelleKarte <- function(records, Jahr=NA){

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
