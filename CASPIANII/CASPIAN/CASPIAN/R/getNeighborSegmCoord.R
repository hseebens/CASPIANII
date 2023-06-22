# returns segments ID close to initial coordinates provided by the user.
# the maximum distance for a segment to be considered invaded is set by default at 1000 m in the main model function
# The function calculates a subset of 100 closest segment portions, then calculates their distance from the given coordinates
# and returns the unique IDs of the segments with at least one portion at a distance lower than max_dist.
# It is possible that no segments are close enough to the given coordinates. An Error message or warning should be returned if so!
# init_coords MUST be a data.table object

getNeighbourSegmCoord <- function(shapeObj,init_coords,max_dist){

  ## extract coordinates from shapefile
  # res <- lapply(slot(shapeObj, "lines"), function(x) lapply(slot(x, "Lines"),function(y) slot(y, "coords")))
  # names(res)<-shapeObj@data$ID
  # segsID<-sapply(lapply(res,"[[",1),nrow)
  #
  # coords<-as.data.frame(do.call("rbind",lapply(res,"[[",1)),stringsAsFactors=F)
  # colnames(coords)<- c("lon","lat")
  # coords$ID<-rep(names(segsID),segsID)

  coords <- st_coordinates(shapeObj)
  coords <- cbind.data.frame(coords,shapeObj$ID[coords[,3]])
  colnames(coords) <- c("lon","lat","L1","ID")
  coords <- coords[,c("lon","lat","ID")]

  ## create a 'search tree' from coordinates
  tree <- SearchTrees::createTree(coords[,1:2]) # required to calculate the nearest neighbour for each set of coordinates

  ## get ID of nearest segment, the ID is the same as in road_shp (at least, it should be)
  # idsList<-vector("list", length(as.numeric(unlist(unique(init_coords[,3]))))) ## not necessary
  # names(idsList)<-as.numeric(unlist(unique(init_coords[,3]))) ## not necessary

  idsList <- vector() # new
  for (i in 1:nrow(init_coords)){
    neighbor <- SearchTrees::knnLookup(tree,newdat=init_coords[i,1:2],k=1000) # lookup nearest neighbour, 'k' determines the number of nearest neighbors

    D<-coords[neighbor,]
    D<-as.data.frame(D)
    D$dist<-geosphere::distm(D[,1:2], init_coords[i,1:2],fun=distVincentyEllipsoid)

    neigh_segm <- D$ID[D$dist<max_dist] # segments with parts below max_dist, new
    if (length(neigh_segm) == 0) warning(paste("No segments found within the specified maximum distance for ",i,"th coordinate.",sep=""))

    idsList <- unique(c(idsList,neigh_segm)) # new
    # idsList[[as.character(init_coords[i,3])]]<-c(idsList[[as.character(init_coords[i,3])]],as.character(unique(D$ID[D$dist<max_dist])))
  }
  return(idsList)
}

