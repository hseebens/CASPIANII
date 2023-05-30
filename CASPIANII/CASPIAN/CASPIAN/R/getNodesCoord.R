getNodesCoord <- function(shapeObj){


  ## extract coordinates from shapefile
  # res <- lapply(slot(shapeObj, "lines"), function(x) lapply(slot(x, "Lines"),function(y) slot(y, "coords")))
  coords <- st_coordinates(shapeObj)

  ## extract first/last row of coordinates
  from_node <- as.data.frame(coords[!duplicated(coords[,3]),],stringsAsFactors=F)
  to_node <- as.data.frame(rbind(coords[which(!duplicated(coords[,3]))-1,],coords[nrow(coords),]),stringsAsFactors=F)
  # colnames(from_node) <- c("lon_from","lat_from","L1")
  # from_node <- from_node[,c("lon_from","lat_from")]
  # colnames(to_node) <- c("lon_to","lat_to","L1")
  # to_node <- to_node[,c("lon_to","lat_to")]

  # from_node <- as.data.frame(do.call("rbind",lapply(lapply(res,"[[",1),function(s) s[dim(s)[1]-1,])),stringsAsFactors=F)
  # to_node   <- as.data.frame(do.call("rbind",lapply(lapply(res,"[[",1),function(s) s[dim(s)[1],])),stringsAsFactors=F)

  from_node$nodeID <- shapeObj$FromNode
  to_node$nodeID <- shapeObj$ToNode

  allnodes <- merge(from_node,to_node,by="nodeID",all=T)
  allnodes <- allnodes[!duplicated(allnodes[,c('nodeID')]),]
  ## node coordinates
  nodeIDs<-allnodes[,1:3]

  ## fill NAs (some nodes are only to_nodes or only from_nodes)
  nodeIDs[is.na(nodeIDs[,2]),"X.x"] <- allnodes[is.na(nodeIDs[,2]),"X.y"]
  nodeIDs[is.na(nodeIDs[,3]),"Y.x"] <- allnodes[is.na(nodeIDs[,3]),"Y.y"]
  #
  colnames(nodeIDs)[2:3] <- c("Long","Lat")

  return(nodeIDs)
}

