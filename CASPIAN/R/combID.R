#Combines link probabilities in both directions

combID<-function(shapeObj_combID,variable=variable){

  dir1 <- paste(shapeObj_combID$FromNode,shapeObj_combID$ToNode,sep="_") # direction 1 of link
  dir2 <- paste(shapeObj_combID$ToNode,shapeObj_combID$FromNode,sep="_") # direction 2 of link (opposite of 1)

  ind <- match(dir1,dir2) # match links in both directions

  bi_dir_var <- shapeObj_combID[ind,..variable] # extract variable for direction 2 for each row of direction 1
  bi_dir_var[is.na(bi_dir_var)] <- 0 # set NAs to 0 (for one-directional links)
  colnames(bi_dir_var) <- paste0(variable,"_bi") # rename variable for direction 2

  shapeObj_combID[,bi_dir_var:=bi_dir_var] # merge variables for direction 1 and 2 into same row

  only_variables <- shapeObj_combID[,c(variable,"bi_dir_var"),with=FALSE] # get only variables (for easier application of pUnion)
  merged_vars <- apply(only_variables,1,pUnion) # merge probabilities into one (mean or pUnion)

  shapeObj_combID[,Pinv2:=merged_vars] # overwrite new values to variable
  shapeObj_combID[,bi_dir_var:=NULL] # remove column used only for calculation

  # shapeObj_combID[24164,]


  # #  cat ("\n", which(shp$ID==x), " links out of ", length(shp$ID),"\n")
  # cat('\r',which(shp$ID==x), " out of ", length(shp$ID))
  # flush.console()
  # if (x%in%already==FALSE) {
  #   b<-which(shp$ID==x) #identify 1st direction
  #   y<-which(shp$FromNode==shp$ToNode[b] & shp$ToNode==shp$FromNode[b])#identify 2nd direction
  #
  #   comb<-st_drop_geometry(shp[c(b,y),eval(var)])
  #   shp[c(b,y),which(colnames(shp)==eval(var))] <- pUnion(comb) #combine Pinv
  #   assign(x = "already",value=c(alr,shp[c(b,y),"ID"]),envir = .GlobalEnv) #add to already done
  # }
  return(shapeObj_combID)
}

