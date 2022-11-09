#####################################################################################
# Integrate standardised taxon names into one file
# 
# Senckenberg Gesellschaft für Naturforschung, 04.11.22
#####################################################################################

run_IntegrateAlienSpeciesDataSets <- function(){

  
  ## Integrate all data into one file ##########################################################
  
  sheet_names <- getSheetNames(file.path("WP1","Data","ListeGebietsfremderArten_einzelneDB_standardisiert.xlsx"))
  
  all_dat_sub <- list()
  for (i in 1:length(sheet_names)){
    
    dat <- read.xlsx(file.path("WP1","Data","ListeGebietsfremderArten_einzelneDB_standardisiert.xlsx"),sheet=i)

    ## add missing columns
    if (!"Status"%in%colnames(dat)){
      dat$Status <- NA
    }      
    if (!"Pfad"%in%colnames(dat)){
      dat$Pfad <- NA
    }
    if (!"Erstnachweis"%in%colnames(dat)){
      dat$Erstnachweis <- NA
    }      
    dat_sub <- cbind.data.frame(dat[,c("Taxon","wissenschaftlicherName","Gattung","Familie","Klasse","Ordnung","Phylum","Reich","Status","Erstnachweis","Pfad")],sheet_names[i])
    
    all_dat_sub[[i]] <- dat_sub
  }  
  all_data <- do.call("rbind",all_dat_sub)
  colnames(all_data)[length(colnames(all_data))] <- "Datenbank"
  
  ## remove empty rows
  ind_NAs <- (!apply(all_data[,1:8],1,function(s) all(is.na(s)))) # remove NAs
  all_data <- all_data[ind_NAs,]
  
  ## remove duplicates ######################################################################
  all_data <- unique(all_data)
  all_data$Datenbank <- gsub("_Germany","",all_data$Datenbank)
  
  dupl_entries <- sort(unique(all_data$wissenschaftlicherName[duplicated(all_data[,which(colnames(all_data)%in%c("Taxon","wissenschaftlicherName","Gattung","Familie"))])]))
  
  new_rows <- list()
  ind_rm_all <- c()
  all_data_noDupl <- all_data
  if (length(dupl_entries)>0){
    for (i in 1:length(dupl_entries)){
      
      ## identify rows with identical entries
      single <- subset(all_data,wissenschaftlicherName==dupl_entries[i])
      ind_rm <- all_data$wissenschaftlicherName==dupl_entries[i]
      
      ## collapse database entries
      all_sources <- paste(unique(single$Datenbank),collapse = "; ")
      single[1,]$Datenbank <- all_sources
      
      all_paths <- single$Pfad[!is.na(single$Pfad)]
      all_paths <- unique(all_paths)
      all_paths <- paste(all_paths,collapse = "; ")
      single[1,]$Pfad <- all_paths
      
      all_status <- paste(unique(single$Status[!is.na(single$Status)]),collapse = "; ")
      single[1,]$Status <- all_status
      
      if (is.numeric(single$Erstnachweis) & !all(is.na(single$Erstnachweis))){
        single[1,]$Erstnachweis <- min(single$Erstnachweis,na.rm=T)
      }
      
      ## output
      new_rows[[i]] <- single[1,]
      ind_rm_all <- c(ind_rm_all,which(ind_rm))
    }
    
    all_data_noDupl <- all_data[-ind_rm_all,]
    new_rows <- do.call("rbind",new_rows)
  }
  
  
  ## generate output file ############################################################

  final_dataset <- rbind(all_data_noDupl,new_rows)

  ## remove duplicates among pathways
  final_dataset$Pfad <- unlist(lapply(strsplit(final_dataset$Pfad,"; "),function(s) paste(unique(s),collapse = "; ")))
  
  
  ## mark/add species of union concern #####################################

  eu_concern <- read.xlsx(file.path("WP1","Data","List_IAS_union_concern.xlsx"))
  eu_concern$scientificName <- gsub("\\s*\\([^\\)]+\\)","",eu_concern$scientificName) # remove synonyms provided in brackets
  
  ## standardise taxon names 
  eu_concern_stand <- CheckGBIFTax(eu_concern)
  
  eu_concern_DB <- eu_concern_stand[[1]]
  colnames(eu_concern_DB)[colnames(eu_concern_DB)=="scientificName"] <- "wissenschaftlicherName"
  colnames(eu_concern_DB)[colnames(eu_concern_DB)=="species"] <- "Art"
  colnames(eu_concern_DB)[colnames(eu_concern_DB)=="genus"] <- "Gattung"
  colnames(eu_concern_DB)[colnames(eu_concern_DB)=="family"] <- "Familie"
  colnames(eu_concern_DB)[colnames(eu_concern_DB)=="order"] <- "Ordnung"
  colnames(eu_concern_DB)[colnames(eu_concern_DB)=="class"] <- "Klasse"
  colnames(eu_concern_DB)[colnames(eu_concern_DB)=="phylum"] <- "Phylum"
  colnames(eu_concern_DB)[colnames(eu_concern_DB)=="kingdom"] <- "Reich"
  
  ## integrate EU taxa into total list of species
  ind <- eu_concern_DB$Taxon%in%final_dataset$Taxon
  existing <- eu_concern_DB[ind,]
  nonexisting <- eu_concern_DB[!ind,]
  
  eu_taxa <- cbind.data.frame(existing[,c("Taxon")],"x")
  colnames(eu_taxa) <- c("Taxon","EU_Anliegen")
  
  final_dataset <- merge(final_dataset,eu_taxa,by="Taxon",all=T)

  # colnames(final_dataset)[colnames(final_dataset)%in%colnames(nonexisting)]
  nonexisting <- nonexisting[,c("Taxon","wissenschaftlicherName","Gattung","Familie","Klasse","Ordnung","Phylum","Reich")]
  nonexisting$Status <- ""
  nonexisting$Erstnachweis <- ""
  nonexisting$Pfad <- ""
  nonexisting$Datenbank <- ""
  nonexisting$EU_Anliegen <- "x"

  final_dataset <- rbind(final_dataset,nonexisting)  

  # eu_concern_stand[[1]]$Taxon %in% final_dataset$Taxon
  
  ## Add common names of groups #############################################
  
  final_dataset$ArtGruppe <- NA
  final_dataset$ArtGruppe[final_dataset$Klasse=="Mammalia"] <- "Säugetiere"
  final_dataset$ArtGruppe[final_dataset$Klasse=="Aves"] <- "Vögel"
  final_dataset$ArtGruppe[final_dataset$Klasse%in%c("Cephalaspidomorphi","Actinopterygii","Elasmobranchii","Sarcopterygii")] <- "Fische"
  final_dataset$ArtGruppe[final_dataset$Klasse=="Reptilia"] <- "Reptilien"
  final_dataset$ArtGruppe[final_dataset$Klasse=="Amphibia"] <- "Amphibien"
  # arthropods <- subset(sTwist_tax,phylum=="Arthropoda")
  # arthropods_nocrustaceans <- subset(arthropods,!Klasse%in%c("Branchiopoda","Hexanauplia","Maxillopoda","Ostracoda","Malacostraca"))
  # alienrecords_full$ArtGruppe[alienrecords_full$scientificName%in%arthropods_nocrustaceans$scientificName] <- "Arthropods (excl. crustaceans)"
  final_dataset$ArtGruppe[final_dataset$Klasse%in%c("Branchiopoda","Hexanauplia","Maxillopoda","Ostracoda","Malacostraca")] <- "Crustaceen"
  final_dataset$ArtGruppe[final_dataset$Klasse%in%c("Insecta")] <- "Insekten"
  final_dataset$ArtGruppe[final_dataset$Klasse%in%c("Arachnida")] <- "Spinnentiere"
  final_dataset$ArtGruppe[final_dataset$Klasse%in%c("Diplopoda","Diplura","Collembola","Chilopoda","Symphyla","Merostomata")] <- "Andere Arthropoden"
  final_dataset$ArtGruppe[final_dataset$Phylum=="Mollusca"] <- "Weichtiere"
  final_dataset$ArtGruppe[final_dataset$Phylum=="Tracheophyta"] <- "Höhere Pflanzen"
  final_dataset$ArtGruppe[final_dataset$Phylum%in%c("Bryophyta","Anthocerotophyta","Marchantiophyta")] <- "Niedere Pflanzen"
  final_dataset$ArtGruppe[final_dataset$Phylum%in%c("Rhodophyta","Chlorophyta","Charophyta","Cryptophyta","Euglenozoa","Haptophyta","Foraminifera","Ciliophora","Ochrophyta","Myzozoa","Cercozoa")] <- "Algen"
  final_dataset$ArtGruppe[final_dataset$Phylum%in%c("Ascomycota","Chytridiomycota","Basidiomycota","Microsporidia","Zygomycota")] <- "Pilze"
  # final_dataset$ArtGruppe[final_dataset$Phylum%in%c("Oomycota")] <- "Oomyceten"
  final_dataset$ArtGruppe[final_dataset$Reich%in%c("Viruses")] <- "Viren"
  # final_dataset$ArtGruppe[final_dataset$Phylum%in%c("Actinobacteria","Chlamydiae","Cyanobacteria","Firmicutes","Proteobacteria")] <- "Bakterien und Protozoen"
  final_dataset$ArtGruppe[final_dataset$Reich%in%c("Chromista","Protozoa","Bacteria")] <- "Oomyceten, Bakterien und Protozoen"
  final_dataset$ArtGruppe[final_dataset$Klasse%in%c("Ascidiacea")] <- "Manteltiere"
  
  final_dataset$ArtGruppe[final_dataset$Phylum%in%c("Acanthocephala","Ascidiacea",
           "Annelida","Brachiopoda","Bryozoa","Chaetognatha","Cnidaria","Ctenophora","Echinodermata","Kamptozoa",
           "Nematoda","Nemertea","Onychophora","Phoronida","Platyhelminthes","Porifera","Rotifera",
           "Sipuncula","Xenacoelomorpha")] <- "Andere Tiere"

  # final_dataset[is.na(final_dataset$ArtGruppe),]
    
  final_dataset <- final_dataset[order(final_dataset$ArtGruppe,final_dataset$wissenschaftlicherName),]
  final_dataset <- final_dataset[,c("Taxon","wissenschaftlicherName","ArtGruppe","EU_Anliegen","Status","Erstnachweis","Pfad","Gattung","Familie","Ordnung","Klasse","Phylum","Reich","Datenbank")]
  
  ## Create Workbook object and add worksheets #######################################################
  wb <- createWorkbook()
  hs2 <- createStyle(halign = "center", valign = "center", textDecoration = "bold",border = "Bottom")
  
  addWorksheet(wb, "GesamtListeGebietsfremdeArten")
  writeData(wb,"GesamtListeGebietsfremdeArten",final_dataset, headerStyle = hs2)
  
  ## export file ##########################
  saveWorkbook(wb, file.path("WP1","Data","ListeGebietsfremderArten_gesamt_standardisiert.xlsx"), overwrite = T, returnValue = FALSE)
}
