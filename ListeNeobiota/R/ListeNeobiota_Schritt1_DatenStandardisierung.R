##################################################################################
#
# Skript ist Teil des Workflows "ListeNeobiota" zur Erstellung einer 
# einheitlichen Liste von Neobiota in Deutschland. Es wird mit dem Skript 
# erstelleListeNeobiota_MAIN.R aufgerufen.
#
# Dieses Skript lädt alle Datensaetze aus 'ListeGebietsfremderArten_Rohdaten.xlsx',
# extrahiert Informationen zu taxonomischne Namen und standardisiert diese
# mittels der GBIF backbone taxonomy. Synonyme werden mit akzeptierten Namen
# ersetzt.
# 
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 08.12.25
##################################################################################


standardisiereDaten <- function(Pfad_Datensaetze=Pfad_Datensaetze){

  
  ## load data sets and standardise taxon names ##########
  
  ## get names of individual sheets
  sheet_names <- getSheetNames(file.path("ListeNeobiota","Daten","Input","ListeGebietsfremderArten_einzelneDB_Rohdaten.xlsx"))

  ## Create Workbook object and add worksheets
  wb <- createWorkbook()
  hs2 <- createStyle(halign = "center", valign = "center", textDecoration = "bold",border = "Bottom")
  
  for (i in 1:length(sheet_names)){#
    
    ## load data from single sheet
    # cat(paste0("\nWorking on data set '",sheet_names[i],"'\n"))
    cat(paste0("\n  Bearbeiten von Datensatz '",sheet_names[i],"'\n"))
    
    dat <- read.xlsx(file.path("ListeNeobiota","Daten","Input","ListeGebietsfremderArten_einzelneDB_Rohdaten.xlsx"),sheet=i)
    
    ## standardise column names ########################################################
    
    ## remove entries with unclear or casual status #########
    if ("status"%in%tolower(colnames(dat))){
      colnames(dat)[grep("status",tolower(colnames(dat)))] <- "status"
      dat$status <- tolower(dat$status)
      ind <- grepl("kryptog|cryptogen|unklar|nicht etabliert|fehlend|cryptogenic|questionable|unknown|unbestaendig|unbeständig",dat$status) # remove species with unclear, not established or cryptogenic status
      dat <- dat[!ind,]
    }
    if ("degreeOfEstablishment"%in%colnames(dat)){
      ind <- grepl("casual",dat$degreeOfEstablishment) # remove species with unclear, not established or cryptogenic status
      dat <- dat[!ind,]
      colnames(dat)[grep("degreeOfEstablishment",colnames(dat))] <- "status"
    }
    if (any(c("Erstnachweis","Erstnachweis.in.Deutschland","Erstnachweis.(Nordsee/Ostsee)","firstrecords","FirstRecord","eventDate")%in%colnames(dat))){
      colnames(dat)[which(colnames(dat)%in%c("Erstnachweis","Erstnachweis.in.Deutschland","Erstnachweis.(Nordsee/Ostsee)","firstrecords","FirstRecord","eventDate"))] <- "Erstnachweis"
    }
    
    ## standardise first records and remove very early records ##################################
    if ("Erstnachweis"%in%colnames(dat)){
      
      dat$Erstnachweis <- gsub("Um |vor |um ","",dat$Erstnachweis)
      
      ## calculate mean years for ranges
      ind <- grep("-",dat$Erstnachweis)
      ind_twoYears <- nchar(gsub("\\D+"," ",dat$Erstnachweis[ind]))>=8
      if (length(ind[ind_twoYears])>0) {
        list_ranges <- strsplit(dat$Erstnachweis[ind[ind_twoYears]],"-")
        dat$Erstnachweis[ind[ind_twoYears]] <- unlist(lapply(list_ranges,function(s) floor(mean(as.numeric(s)))))
      }
      
      ## for others, take the earliest year
      numeric_vals <- gsub("\\D+"," ",dat$Erstnachweis) # remove all non-numerics and separate by " "
      split_numbers <- strsplit(numeric_vals," ") # split sequences into pieces
      fourdigits <- lapply(split_numbers,function(s) s[grep('.*(\\d{4}).*', s)]) # keep only four digit numbers
      
      ## remove non-numeric entries
      oldw <- getOption("warn")
      options(warn = -1)
      dat$Erstnachweis <- as.numeric(unlist(lapply(fourdigits,min)))
      options(warn = oldw)
      
      dat <- subset(dat,Erstnachweis>1500 | is.na(Erstnachweis))
    }
    
    ## standardise pathway names #################################################################
    
    if (any(sheet_names[i]%in%Pfad_Datensaetze)){
      
      ## get translation table
      path_translate <- read.xlsx(file.path("ListeNeobiota","Daten","Input","PfadUebersetzung.xlsx"),sheet=1)
      
      if (sheet_names[i]=="EASIN_Germany"){
        
        dat$Pfad <- NA
        # ind_trans <- which(colnames(path_translate)%in%sheet_names[i]) # column number in translation table
        
        all_paths <- colnames(dat)[-(1:2)]
        all_paths <- (unique(all_paths[!is.na(all_paths)]))
        all_paths <- all_paths[grep("RELEASE|TRANSPORT|ESCAPE|CORRIDOR|UNAIDED|UNKNOWN",all_paths)]
        # all_paths_spaces <- gsub("\\."," ",all_paths)
        for (j in 1:length(all_paths)){ #
          
          spec_row_ind <- which(!is.na(dat[,all_paths[j]])) # relevant entries of species with the respective pathway in the species table
          trans_row_ind <- grep(all_paths[j],path_translate[,"EASIN_Germany"],fixed=T) # respective row in translation table 
          
          dat$Pfad[spec_row_ind] <- paste(dat$Pfad[spec_row_ind],path_translate[trans_row_ind,1],sep="; ")
          dat$Pfad <- gsub("; $","",dat$Pfad)
        }
        dat$Pfad <- gsub("NA; ","",dat$Pfad)
        dat$Pfad[is.na(dat$Pfad)] <- ""
      }
      
      if (sheet_names[i]=="Tackenberg_2017"){
        
        dat$Pfad <- NA
        ind <- which(colnames(path_translate)%in%sheet_names[i])
        
        all_paths <- colnames(dat)[-(1:2)]
        all_paths <- sort(unique(all_paths[!is.na(all_paths)]))
        all_paths_spaces <- gsub("\\."," ",all_paths)
        for (j in 1:length(all_paths)){ #
          
          ind <- which(!is.na(dat[,all_paths[j]]))
          row_ind <- grep(all_paths_spaces[j],gsub("\\."," ",path_translate[,"Tackenberg_2017"]),fixed=T)
          
          dat$Pfad[ind] <- paste(dat$Pfad[ind],path_translate[row_ind,1],sep="; ")
          dat$Pfad <- gsub("; $","",dat$Pfad)
        }
        dat$Pfad <- gsub("NA; ","",dat$Pfad)
        dat$Pfad <- gsub("NA$","",dat$Pfad)
        dat$Pfad[is.na(dat$Pfad)] <- ""
      }
      
      if (sheet_names[i]=="BfN"){
        
        dat$Pfad <- dat$Pfad
        dat$Pfad <- gsub(" \\(.*?\\)", "", dat$Pfad) # remove brackets
        
        ind <- which(colnames(path_translate)%in%sheet_names[i])
        
        all_paths <- unique(unlist(strsplit(path_translate[,ind],"; ")))
        all_paths <- sort(unique(all_paths[!is.na(all_paths)]))
        for (j in 1:length(all_paths)){ #
          
          row_ind <- grep(all_paths[j],path_translate[,sheet_names[i]],fixed=T)
          new_name <- path_translate[row_ind,1]
          # print(new_name)
          dat$Pfad <- gsub(all_paths[j],new_name,dat$Pfad)
          dat$Pfad <- gsub("; $","",dat$Pfad)
        }
      }
      dat$Pfad <- gsub("^ ;","",dat$Pfad)
      
      ## remove duplicates
      dat$Pfad <- unlist(lapply(strsplit(dat$Pfad,"; "),function(s) paste(unique(s),collapse = "; ")))
      
      ## replace missing values by NA
      dat$Pfad[dat$Pfad==""] <- NA
      
    }
    if (!"Pfad"%in%colnames(dat) & !"pathway"%in%colnames(dat)){
      dat$Pfad <- NA
    }

    # ## export species action lists by BfN
    # if (sheet_names[i]=="BfN"){
    #   if (any("Liste"==colnames(dat))){
    #     lists_data <- dat[,c("Wissenschaftlicher_Name","Artengruppen","Liste")]
    #     lists_data <- lists_data[!is.na(lists_data$Liste),]
    #     lists_data <- lists_data[order(lists_data$Liste,lists_data$Artengruppen),]
    #     
    #     write.table(lists_data,file.path("ListeNeobiota","Daten","BfN_InvasiveArtenListen.csv"), row.names = F)
    #   }
    # }
    
    ## get taxonomic information from GBIF and add to data set #######################################################
    new_names <- CheckGBIFTax(dat)
    
    ## output
    DB <- new_names[[1]]
    mismatches <- new_names[[2]]
    
    colnames(DB)[colnames(DB)=="scientificName"] <- "wissenschaftlicherName"
    colnames(DB)[colnames(DB)=="scientificName_orig"] <- "wissenschaftlicherName_orig"
    colnames(DB)[colnames(DB)=="status"] <- "Status"
    colnames(DB)[colnames(DB)=="species"] <- "Art"
    colnames(DB)[colnames(DB)=="genus"] <- "Gattung"
    colnames(DB)[colnames(DB)=="family"] <- "Familie"
    colnames(DB)[colnames(DB)=="order"] <- "Ordnung"
    colnames(DB)[colnames(DB)=="class"] <- "Klasse"
    colnames(DB)[colnames(DB)=="phylum"] <- "Phylum"
    colnames(DB)[colnames(DB)=="kingdom"] <- "Reich"
    colnames(DB)[colnames(DB)=="Liste"] <- "BfNliste"
    
    ## store data on single sheets in workbook
    addWorksheet(wb, sheet_names[i])
    writeData(wb,sheet_names[i],DB, headerStyle = hs2)
  }  
  
  ## Final output as xlsx
  saveWorkbook(wb, file.path("ListeNeobiota","Daten","Output","ListeGebietsfremderArten_einzelneDB_standardisiert.xlsx"), overwrite = T, returnValue = FALSE)
}
