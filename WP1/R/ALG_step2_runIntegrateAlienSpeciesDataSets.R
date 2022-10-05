#####################################################################################
# Integrate standardised taxon names into one file
# 
# Hanno Seebens, 18.07.22
#####################################################################################

run_IntegrateAlienSpeciesDataSets <- function(){

  
  ## Integrate all data into one file ##########################################################
  
  sheet_names <- getSheetNames(file.path("WP1","Data","ListeGebietsfremderArten_einzelneDB_standardisiert.xlsx"))
  
  all_dat_sub <- list()
  for (i in 1:length(sheet_names)){
    
    dat <- read.xlsx(file.path("WP1","Data","ListeGebietsfremderArten_einzelneDB_standardisiert.xlsx"),sheet=i)

    ## add column of invasion status
    if (!"status"%in%colnames(dat)){
      dat$status <- NA
    }      
    if (!"pathway"%in%colnames(dat)){
      dat$pathway <- NA
    }
    if (!"firstRecord"%in%colnames(dat)){
      dat$firstRecord <- NA
    }      
    dat_sub <- cbind.data.frame(dat[,c("Taxon","scientificName","genus","family","class","order","phylum","kingdom","status","firstRecord","pathway")],sheet_names[i])
    
    all_dat_sub[[i]] <- dat_sub
  }  
  all_data <- do.call("rbind",all_dat_sub)
  colnames(all_data)[length(colnames(all_data))] <- "database"
  
  ## remove empty rows
  ind_NAs <- (!apply(all_data[,1:8],1,function(s) all(is.na(s)))) # remove NAs
  all_data <- all_data[ind_NAs,]
  
  ## remove duplicates ######################################################################
  all_data <- unique(all_data)
  all_data$database <- gsub("_Germany","",all_data$database)
  
  dupl_entries <- sort(unique(all_data$scientificName[duplicated(all_data[,which(colnames(all_data)%in%c("Taxon","scientificName","genus","family"))])]))
  new_rows <- list()
  ind_rm_all <- c()
  for (i in 1:length(dupl_entries)){
    
    ## identify rows with identical entries
    single <- subset(all_data,scientificName==dupl_entries[i])
    ind_rm <- all_data$scientificName==dupl_entries[i]
    
    ## collapse database entries
    all_sources <- paste(unique(single$database),collapse = "; ")
    single[1,]$database <- all_sources
    
    all_paths <- single$pathway[!is.na(single$pathway)]
    all_paths <- unique(all_paths)
    all_paths <- paste(all_paths,collapse = "; ")
    single[1,]$pathway <- all_paths
    
    all_status <- paste(unique(single$status[!is.na(single$status)]),collapse = "; ")
    single[1,]$status <- all_status
    
    if (is.numeric(single$firstRecord) & !all(is.na(single$firstRecord))){
      single[1,]$firstRecord <- min(single$firstRecord,na.rm=T)
    }
    
    ## output
    new_rows[[i]] <- single[1,]
    ind_rm_all <- c(ind_rm_all,which(ind_rm))
  }

  ## generate output file ############################################################
  
  all_data_noDupl <- all_data[-ind_rm_all,]
  new_rows <- do.call("rbind",new_rows)
  
  final_dataset <- rbind(all_data_noDupl,new_rows)

  ## remove duplicates among pathways
  final_dataset$pathway <- unlist(lapply(strsplit(final_dataset$pathway,"; "),function(s) paste(unique(s),collapse = "; ")))
  
  
  ## mark/add species of union concern #####################################

  eu_concern <- read.xlsx(file.path("WP1","Data","List_IAS_union_concern.xlsx"))
  eu_concern$scientificName <- gsub("\\s*\\([^\\)]+\\)","",eu_concern$scientificName) # remove synonyms provided in brackets
  
  ## standardise taxon names 
  eu_concern_stand <- CheckGBIFTax(eu_concern)
  eu_taxa <- cbind.data.frame(eu_concern_stand[[1]][,c("Taxon")],"x")
  colnames(eu_taxa) <- c("Taxon","EU_concern")
  
  final_dataset <- merge(final_dataset,eu_taxa,by="Taxon",all=T)

      
  ## Add common names of groups #############################################
  
  final_dataset$taxonGroup <- NA
  final_dataset$taxonGroup[final_dataset$class=="Mammalia"] <- "Säugetiere"
  final_dataset$taxonGroup[final_dataset$class=="Aves"] <- "Vögel"
  final_dataset$taxonGroup[final_dataset$class%in%c("Cephalaspidomorphi","Actinopterygii","Elasmobranchii","Sarcopterygii")] <- "Fische"
  final_dataset$taxonGroup[final_dataset$class=="Reptilia"] <- "Reptilien"
  final_dataset$taxonGroup[final_dataset$class=="Amphibia"] <- "Amphibien"
  # arthropods <- subset(sTwist_tax,phylum=="Arthropoda")
  # arthropods_nocrustaceans <- subset(arthropods,!class%in%c("Branchiopoda","Hexanauplia","Maxillopoda","Ostracoda","Malacostraca"))
  # alienrecords_full$taxonGroup[alienrecords_full$scientificName%in%arthropods_nocrustaceans$scientificName] <- "Arthropods (excl. crustaceans)"
  final_dataset$taxonGroup[final_dataset$class%in%c("Branchiopoda","Hexanauplia","Maxillopoda","Ostracoda","Malacostraca")] <- "Crustaceen"
  final_dataset$taxonGroup[final_dataset$class%in%c("Insecta")] <- "Insekten"
  final_dataset$taxonGroup[final_dataset$class%in%c("Arachnida")] <- "Spinnentiere"
  final_dataset$taxonGroup[final_dataset$class%in%c("Diplopoda","Diplura","Collembola","Chilopoda","Symphyla","Merostomata")] <- "Andere Arthropoden"
  final_dataset$taxonGroup[final_dataset$phylum=="Mollusca"] <- "Weichtiere"
  final_dataset$taxonGroup[final_dataset$phylum=="Tracheophyta"] <- "Höhere Pflanzen"
  final_dataset$taxonGroup[final_dataset$phylum%in%c("Bryophyta","Anthocerotophyta","Marchantiophyta")] <- "Niedere Pflanzen"
  final_dataset$taxonGroup[final_dataset$phylum%in%c("Rhodophyta","Chlorophyta","Charophyta","Cryptophyta","Euglenozoa","Haptophyta","Foraminifera","Ciliophora","Ochrophyta","Myzozoa","Cercozoa")] <- "Algen"
  final_dataset$taxonGroup[final_dataset$phylum%in%c("Ascomycota","Chytridiomycota","Basidiomycota","Microsporidia","Zygomycota")] <- "Pilze"
  # final_dataset$taxonGroup[final_dataset$phylum%in%c("Oomycota")] <- "Oomyceten"
  final_dataset$taxonGroup[final_dataset$kingdom%in%c("Viruses")] <- "Viren"
  # final_dataset$taxonGroup[final_dataset$phylum%in%c("Actinobacteria","Chlamydiae","Cyanobacteria","Firmicutes","Proteobacteria")] <- "Bakterien und Protozoen"
  final_dataset$taxonGroup[final_dataset$kingdom%in%c("Chromista","Protozoa","Bacteria")] <- "Oomyceten, Bakterien und Protozoen"
  final_dataset$taxonGroup[final_dataset$class%in%c("Ascidiacea")] <- "Manteltiere"
  
  final_dataset$taxonGroup[final_dataset$phylum%in%c("Acanthocephala","Ascidiacea",
           "Annelida","Brachiopoda","Bryozoa","Chaetognatha","Cnidaria","Ctenophora","Echinodermata","Kamptozoa",
           "Nematoda","Nemertea","Onychophora","Phoronida","Platyhelminthes","Porifera","Rotifera",
           "Sipuncula","Xenacoelomorpha")] <- "Andere Tiere"

  final_dataset[is.na(final_dataset$taxonGroup),]
    
  final_dataset <- final_dataset[order(final_dataset$taxonGroup,final_dataset$scientificName),]
  final_dataset <- final_dataset[,c("Taxon","scientificName","taxonGroup","EU_concern","status","firstRecord","pathway","genus","family","order","class","phylum","kingdom","database")]
  
  ## Create Workbook object and add worksheets #######################################################
  wb <- createWorkbook()
  hs2 <- createStyle(halign = "center", valign = "center", textDecoration = "bold",border = "Bottom")
  
  addWorksheet(wb, "GesamtListeGebietsfremdeArten")
  writeData(wb,"GesamtListeGebietsfremdeArten",final_dataset, headerStyle = hs2)
  
  ## export file ##########################
  saveWorkbook(wb, file.path("WP1","Data","ListeGebietsfremderArten_gesamt_standardisiert.xlsx"), overwrite = T, returnValue = FALSE)
}
