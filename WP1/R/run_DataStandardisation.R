##################### run standardisation of data sets ########################################################
# 
# This scripts loads data sets stored on sheets in the spreadsheet 'ListeGebietsfremderArten_Rohdaten.xlsx',
# extracts information of taxonomic names, obtains taxonomic information from the GBIF backbone taxonomy such
# as accepted taxonomic name, synonym and the taxonomic tree and exports the standardised files.
# 
# Project: CASPIAN II
# 
# Hanno Seebens, 29.06.22
###############################################################################################################


run_DataStandardisation <- function(pathway_datasets=pathway_datasets){

  
  ## load data sets and standardise taxon names #################################################
  
  ## get names of individual sheets
  sheet_names <- getSheetNames(file.path("Data","ListeGebietsfremderArten_einzelneDB_Rohdaten.xlsx"))

  ## Create Workbook object and add worksheets
  wb <- createWorkbook()
  hs2 <- createStyle(halign = "center", valign = "center", textDecoration = "bold",border = "Bottom")
  
  for (i in 1:length(sheet_names)){#
    
    ## load data from single sheet
    # cat("\n")
    print(paste0("Working on data set '",sheet_names[i],"'"))
    
    dat <- read.xlsx(file.path("Data","ListeGebietsfremderArten_einzelneDB_Rohdaten.xlsx"),sheet=i)
    
    ## standardise column names ########################################################
    
    ## remove entries with unclear or casual status #########
    if ("Status"%in%colnames(dat)){
      ind <- grepl("kryptog|cryptogen|unklar|nicht etabliert|Cryptogenic|Questionable|Unknown",dat$Status) # remove species with unclear, not established or cryptogenic status
      dat <- dat[!ind,]
      colnames(dat)[grep("Status|status",colnames(dat))] <- "status"
    }
    if ("status"%in%colnames(dat)){
      ind <- grepl("kryptog|cryptogen|unklar|nicht etabliert|Cryptogenic|Questionable|Unknown",dat$status) # remove species with unclear, not established or cryptogenic status
      dat <- dat[!ind,]
      colnames(dat)[grep("Status|status",colnames(dat))] <- "status"
    }
    if ("degreeOfEstablishment"%in%colnames(dat)){
      ind <- grepl("casual",dat$degreeOfEstablishment) # remove species with unclear, not established or cryptogenic status
      dat <- dat[!ind,]
      colnames(dat)[grep("degreeOfEstablishment",colnames(dat))] <- "status"
    }
    
    if (any(c("Erstnachweis","Erstnachweis.in.Deutschland","Erstnachweis.(Nordsee/Ostsee)","firstrecords","FirstRecord","eventDate")%in%colnames(dat))){
      colnames(dat)[which(colnames(dat)%in%c("Erstnachweis","Erstnachweis.in.Deutschland","Erstnachweis.(Nordsee/Ostsee)","firstrecords","FirstRecord","eventDate"))] <- "firstRecord"
    }
    
    ## standardise first records and remove very early records ##################################
    if ("firstRecord"%in%colnames(dat)){
      
      dat$firstRecord <- gsub("Um |vor |um ","",dat$firstRecord)

      ## calculate mean years for ranges
      ind <- grep("-",dat$firstRecord)
      ind_twoYears <- nchar(gsub("\\D+"," ",dat$firstRecord[ind]))>=8
      if (length(ind[ind_twoYears])>0) {
        list_ranges <- strsplit(dat$firstRecord[ind[ind_twoYears]],"-")
        dat$firstRecord[ind[ind_twoYears]] <- unlist(lapply(list_ranges,function(s) floor(mean(as.numeric(s)))))
      }

      ## for others, take the earliest year
      numeric_vals <- gsub("\\D+"," ",dat$firstRecord) # remove all non-numerics and separate by " "
      split_numbers <- strsplit(numeric_vals," ") # split sequences into pieces
      fourdigits <- lapply(split_numbers,function(s) s[grep('.*(\\d{4}).*', s)]) # keep only four digit numbers

      ## remove non-numeric entries
      oldw <- getOption("warn")
      options(warn = -1)
      dat$firstRecord <- as.numeric(unlist(lapply(fourdigits,min)))
      options(warn = oldw)

      dat <- subset(dat,firstRecord>1500 | is.na(firstRecord))
    }
    
    ## standardise pathway names #################################################################

    
    if (any(sheet_names[i]%in%pathway_datasets)){
      
      ## get translation table
      path_translate <- read.xlsx(file.path("Data","VektorenÜbersetzung.xlsx"),sheet=1)

      if (sheet_names[i]=="EASIN_Germany"){
        
        dat$Einbringungsvektoren <- NA
        ind_trans <- which(colnames(path_translate)%in%sheet_names[i]) # column number in translation table
        
        all_paths <- colnames(dat)[-(1:2)]
        all_paths <- (unique(all_paths[!is.na(all_paths)]))
        all_paths <- all_paths[grep("RELEASE|TRANSPORT|ESCAPE|CORRIDOR|UNAIDED|UNKNOWN",all_paths)]
        # all_paths_spaces <- gsub("\\."," ",all_paths)
        for (j in 1:length(all_paths)){ #
          
          spec_row_ind <- which(!is.na(dat[,all_paths[j]])) # relevant entries of species with the respective pathway in the species table
          trans_row_ind <- grep(all_paths[j],path_translate[,"EASIN_Germany"],fixed=T) # respective row in translation table 
          
          dat$Einbringungsvektoren[spec_row_ind] <- paste(dat$Einbringungsvektoren[spec_row_ind],path_translate[trans_row_ind,1],sep="; ")
          dat$Einbringungsvektoren <- gsub("; $","",dat$Einbringungsvektoren)
        }
        dat$Einbringungsvektoren <- gsub("NA; ","",dat$Einbringungsvektoren)
        dat$Einbringungsvektoren[is.na(dat$Einbringungsvektoren)] <- ""
      }
      
      if (sheet_names[i]=="Tackenberg_2017"){
        
        dat$Einbringungsvektoren <- NA
        ind <- which(colnames(path_translate)%in%sheet_names[i])
        
        all_paths <- colnames(dat)[-(1:2)]
        all_paths <- sort(unique(all_paths[!is.na(all_paths)]))
        all_paths_spaces <- gsub("\\."," ",all_paths)
        for (j in 1:length(all_paths)){ #
          
          ind <- which(!is.na(dat[,all_paths[j]]))
          row_ind <- grep(all_paths_spaces[j],gsub("\\."," ",path_translate[,"Tackenberg_2017"]),fixed=T)
          
          dat$Einbringungsvektoren[ind] <- paste(dat$Einbringungsvektoren[ind],path_translate[row_ind,1],sep="; ")
          dat$Einbringungsvektoren <- gsub("; $","",dat$Einbringungsvektoren)
        }
        dat$Einbringungsvektoren <- gsub("NA; ","",dat$Einbringungsvektoren)
        dat$Einbringungsvektoren <- gsub("NA$","",dat$Einbringungsvektoren)
        dat$Einbringungsvektoren[is.na(dat$Einbringungsvektoren)] <- ""
      }
      
      if (sheet_names[i]=="BfN"){
        
        dat$Einbringungsvektoren <- dat$Vektoren
        dat$Einbringungsvektoren <- gsub(" \\(.*?\\)", "", dat$Einbringungsvektoren) # remove brackets
        
        ind <- which(colnames(path_translate)%in%sheet_names[i])
        
        all_paths <- unique(unlist(strsplit(path_translate[,ind],"; ")))
        all_paths <- sort(unique(all_paths[!is.na(all_paths)]))
        for (j in 1:length(all_paths)){ #
          
          row_ind <- grep(all_paths[j],path_translate[,sheet_names[i]],fixed=T)
          new_name <- path_translate[row_ind,1]
          # print(new_name)
          dat$Einbringungsvektoren <- gsub(all_paths[j],new_name,dat$Einbringungsvektoren)
          dat$Einbringungsvektoren <- gsub("; $","",dat$Einbringungsvektoren)
        }
      }
      dat$Einbringungsvektoren <- gsub("^ ;","",dat$Einbringungsvektoren)
      
      ## remove duplicates
      dat$Einbringungsvektoren <- unlist(lapply(strsplit(dat$Einbringungsvektoren,"; "),function(s) paste(unique(s),collapse = "; ")))
      
      ## replace missing values by NA
      dat$Einbringungsvektoren[dat$Einbringungsvektoren==""] <- NA
      
      colnames(dat)[colnames(dat)=="Einbringungsvektoren"] <- "pathway"
    }
    if (!"Einbringungsvektoren"%in%colnames(dat) & !"pathway"%in%colnames(dat)){
      dat$pathway <- NA
    }
    
    ## get taxonomic information from GBIF and add to data set #######################################################
    new_names <- CheckGBIFTax(dat)
    
    ## output
    DB <- new_names[[1]]
    mismatches <- new_names[[2]]
    
    ## store data on single sheets in workbook
    addWorksheet(wb, sheet_names[i])
    writeData(wb,sheet_names[i],DB, headerStyle = hs2)
  }  
  
  
  ## Final output as xlsx
  saveWorkbook(wb, file.path("Data","ListeGebietsfremderArten_einzelneDB_standardisiert.xlsx"), overwrite = T, returnValue = FALSE)

}
