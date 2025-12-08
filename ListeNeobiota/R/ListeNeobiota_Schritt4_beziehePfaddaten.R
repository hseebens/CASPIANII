##################################################################################
#
# Skript ist Teil des Workflows "ListeNeobiota" zur Erstellung einer 
# einheitlichen Liste von Neobiota in Deutschland. Es wird mit dem Skript 
# erstelleListeNeobiota_MAIN.R aufgerufen.
#
# Dieses Skript extrahiert Informationen zum Pfad der Einfuhr der Neobiota. Die
# Pfadinformation wird aus den Datenbanken GISD und DAISIE entnommen, die in der 
# folgenden Publikation zusammengefasst ist:. 
# Saul et al. (2017). Assessing patterns in introduction pathways of alien species 
# by linking major invasion data bases. Journal of Applied Ecology, 54(2), 
# 657–669. https://doi.org/10.1111/1365-2664.12819
# 
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 08.12.25
##################################################################################



beziehePfadDaten <- function(){

  dat <- read.xlsx(file.path("ListeNeobiota","Daten","Output","ListeGebietsfremderArten_gesamt_standardisiert.xlsx"),sheet=1)

  ## get translation table
  path_translate <- read.xlsx(file.path("ListeNeobiota","Daten","Input","PfadUebersetzung.xlsx"),sheet=1)

  ## Prepare pathway data ###################################################################
  
  ## Pathway data from Saul et al. (2017) JAE 54, 657–669, doi: 10.1111/1365-2664.12819
  pathways <- read.table(file.path("ListeNeobiota","Daten","Input","Einfuhrpfade.csv"),sep=";",stringsAsFactors = F,header=T)
  colnames(pathways)[4] <- c("PathwayMain")
  colnames(pathways)[5] <- c("PathwaySub")
  colnames(pathways)[6] <- c("PathwayIntential")
  pathways$PathwayMain[pathways$PathwayMain=="Release in Nature"] <- "Release in nature"
  pathways$PathwaySub[pathways$PathwaySub=="Horticulture "] <- "Horticulture"
  pathways <- pathways[,1:6]
  pathways$PathwaySub <- gsub("^\\s+|\\s+$", "",pathways$PathwaySub) # trim leading and trailing whitespace
  pathways$PathwayMain <- gsub("^\\s+|\\s+$", "",pathways$PathwayMain) # trim leading and trailing whitespace
  
  # pathways$PathwaySub[pathways$PathwaySub==""] <- pathways$PathwayMain[pathways$PathwaySub==""]
  
  # table(pathways$PathwaySub)
  
  ## collapse pathways information into a single row per taxon ##############
  dupl_entries <- unique(pathways$Species.name[duplicated(pathways$Species.name)]) # identify duplicates
  new_rows <- list()
  ind_rm_all <- c()
  for (i in 1:length(dupl_entries)){
    
    path_sub <- subset(pathways,Species.name==dupl_entries[i]) # identify all rows for the same taxon
    ind_rm <- pathways$Species.name==dupl_entries[i] # duplicated rows to remove

    ## collapse database entries
    all_paths <- path_sub$PathwaySub
    all_paths <- unique(all_paths[all_paths!=""])
    all_paths <- paste(all_paths,collapse = "; ")
    path_sub[1,]$PathwaySub <- all_paths
    
    ## output
    new_rows[[i]] <- path_sub[1,]
    ind_rm_all <- c(ind_rm_all,which(ind_rm))
  }
  pathways_noDupl <- pathways[-ind_rm_all,]
  new_rows <- do.call("rbind",new_rows)
  
  pathway_dataset <- rbind(pathways_noDupl,new_rows)

  
  ## merge pathway information with species table ##########################################
  dat_path <- merge(dat,pathway_dataset,by.x="Taxon",by.y="Species.name",all.x=T)
  
  
  ## EASIN paths #############################################################
  pathsEASIN <- read.xlsx(file.path("ListeNeobiota","Daten","Input","Pfadangaben_EASIN.xlsx"),sheet=1)
  pathsEASIN <- pathsEASIN[!duplicated(pathsEASIN$Taxon),]
  
  ## translate paths to standard
  pathsEASIN$path_EASIN <- NA
  
  all_paths <- colnames(pathsEASIN)[-(1:2)]
  all_paths <- (unique(all_paths[!is.na(all_paths)]))
  all_paths <- all_paths[grep("RELEASE|TRANSPORT|ESCAPE|CORRIDOR|UNAIDED|UNKNOWN",all_paths)]
  # all_paths_spaces <- gsub("\\."," ",all_paths)
  for (j in 1:length(all_paths)){ #
    
    spec_row_ind <- which(!is.na(pathsEASIN[,all_paths[j]])) # relevant entries of species with the respective pathway in the species table
    trans_row_ind <- grep(all_paths[j],path_translate[,"EASIN_Germany"],fixed=T) # respective row in translation table 
    
    pathsEASIN$path_EASIN[spec_row_ind] <- paste(pathsEASIN$path_EASIN[spec_row_ind],path_translate[trans_row_ind,1],sep="; ")
    pathsEASIN$path_EASIN <- gsub("; $","",pathsEASIN$path_EASIN)
  }
  pathsEASIN$path_EASIN <- gsub("NA; ","",pathsEASIN$path_EASIN)
  pathsEASIN$path_EASIN[is.na(pathsEASIN$path_EASIN)] <- ""
  
  pathsEASIN <- pathsEASIN[,c("Taxon","path_EASIN")]
  
  
  ## merge pathway information with species table ##########################################
  dat_path <- merge(dat_path,pathsEASIN,by="Taxon",all.x=T)
  
  
  ### merge all pathway information ###########################

  ## all pathways to species table
  dat_path$pathway <- paste(dat_path$Pfad, dat_path$PathwaySub, dat_path$path_EASIN , sep = "; ")
  # dat_path$pathway[is.na(dat_path$pathway) | dat_path$pathway==""] <- dat_path$PathwaySub[is.na(dat_path$pathway) | dat_path$pathway==""]
  # unique(unlist(strsplit(dat_path$PathwaySub,"; ")))
  dat_path$pathway <- gsub("; NA","",dat_path$pathway)
  dat_path$pathway <- gsub("NA; ","",dat_path$pathway)
  

  ## replace CBD pathway names, sub-category
  all_paths_CBD <- unique(unlist(strsplit(dat_path$PathwaySub,"; ")))
  all_paths_CBD <- all_paths_CBD[!is.na(all_paths_CBD)]
  for (i in 1:length(all_paths_CBD)){
    
    ## find CBD pathway in species table
    spec_ind <- grep(all_paths_CBD[i],dat_path$PathwaySub,fixed=T)
    # dat_path[spec_ind,]$pathway
    
    ind_new_name <- grep(all_paths_CBD[i],path_translate[,which(colnames(path_translate)=="CBD_Sub")],fixed=T) # position of translated pathway name
    if (length(ind_new_name)>1) print(i)
    if (length(ind_new_name)>0){
      dat_path$pathway[spec_ind] <- gsub(all_paths_CBD[i],path_translate[,1][ind_new_name],dat_path$pathway[spec_ind],fixed=T)
    }
  }
  # table(unlist(strsplit(dat_path$pathway,"; ")))
  
  ## replace CBD pathway names, main category (only in BfN documents used)
  all_paths_CBD <- unique(unlist(strsplit(dat_path$PathwayMain,"; ")))
  all_paths_CBD <- all_paths_CBD[!is.na(all_paths_CBD)]
  for (i in 1:length(all_paths_CBD)){
    
    ## find CBD pathway in species table
    spec_ind <- grep(all_paths_CBD[i],dat_path$pathway,fixed=T)
    # dat_path[spec_ind,]$pathway
    
    ind_new_name <- grep(all_paths_CBD[i],path_translate[,which(colnames(path_translate)=="CBD_Main")],fixed=T) # position of translated pathway name
    if (length(ind_new_name)>1) print(i)
    if (length(ind_new_name)>0){
      dat_path$pathway[spec_ind] <- gsub(all_paths_CBD[i],path_translate[,1][ind_new_name],dat_path$pathway[spec_ind],fixed=T)
    }
  }
  # table(unlist(strsplit(dat_path$pathway,"; ")))
  
  ## remove duplicates
  dat_path$pathway <- unlist(lapply(strsplit(dat_path$pathway,"; "),function(s) paste(unique(s),collapse = "; ")))
  dat_path$pathway <- gsub("; NA","",dat_path$pathway)
  dat_path$pathway <- gsub("NA; ","",dat_path$pathway)
  dat_path$pathway <- gsub("^; ","",dat_path$pathway)
  dat_path$pathway <- gsub("; $","",dat_path$pathway)
  dat_path$pathway <- gsub("NA","",dat_path$pathway)
  dat_path$pathway <- gsub("; Unbekannt","",dat_path$pathway)
  dat_path$pathway <- gsub("Unbekannt; ","",dat_path$pathway)
  dat_path$pathway <- gsub("; ;",";",dat_path$pathway)
  
  
  ## generate output ######################################################################
  dat_path <- dat_path[order(dat_path$Artengruppe,dat_path$wissenschaftlicherName),] # sort output
  
  dat_path <- dat_path[, - which(colnames(dat_path)=="Pfad")]
  colnames(dat_path)[colnames(dat_path)=="pathway"] <- "Pfad"
  
  dat_path <- dat_path[,c("Taxon","wissenschaftlicherName","Artengruppe","EU_Anliegen","Status","Erstnachweis","Pfad","Gattung","Familie","Ordnung","Klasse","Phylum","Reich","Eintraege_GBIF_DE","Eintraege_GBIF_Europa","Datenbank","BfNlisten")]

  # table(dat_path$pathway=="" |dat_path$pathway=="Unbekannt")
  # ind <- grep("Unbekannt",dat_path$pathway)
  # dat_path$pathway[ind]
  
  ## Create Workbook object and add worksheets for output
  wb <- createWorkbook()
  hs2 <- createStyle(halign = "center", valign = "center", textDecoration = "bold",border = "Bottom")
  
  addWorksheet(wb, "GesamtListeGebietsfremdeArten")
  writeData(wb,"GesamtListeGebietsfremdeArten",dat_path, headerStyle = hs2)
  
  ## export file (overrides existing file!) ##########################
  saveWorkbook(wb, file.path("ListeNeobiota","Daten","Output","ListeGebietsfremderArten_gesamt_standardisiert.xlsx"), overwrite = T, returnValue = FALSE)
}
