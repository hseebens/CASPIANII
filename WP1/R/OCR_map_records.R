##################### Plot occurrence records on a map ######################################################
# 
# This function plots occurrence records of species (lat-long) on an interactive map using leaflet.
#
# Project: CASPIAN II
# 
# Hanno Seebens, 18.07.22
###############################################################################################################



map_records <- function(records){

  records$col <- as.numeric(as.factor(records$Database))
  all_cols <- c('red', 'orange','black', 'blue')
  all_DBs <- sort(unique(records$Database))
  
  pal <- colorFactor(
    palette = all_cols,
    domain = records$col
  )
  
  uni_col_DB <- unique(cbind.data.frame(records$Database,pal(records$col)))
  
  map_records <- leaflet(records) %>% addTiles() %>% 
    clearBounds() %>%
    addCircleMarkers(lng=~Longitude, lat=~Latitude,radius = 3, weight = 1,color=~pal(col),fillOpacity = 0.5) %>%
    addLegend("bottomright", colors=uni_col_DB[,2] ,labels=uni_col_DB[,1],
            title = "Databases",
            opacity = 1
  )
  
  return(map_records)
}
