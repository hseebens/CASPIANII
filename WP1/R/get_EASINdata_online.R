rm(list=ls())
graphics.off()

library(httr)
library(jsonlite)
library(data.table)

## API of EASIN database ######################
base_url <- "https://easin.jrc.ec.europa.eu/apixg/catxg/countrycode/DE/skip/"
# https://easin.jrc.ec.europa.eu/apixg/catxg/countrycode/DE/skip/0/take/30

## loop of steps of 50 species (restriction by the database)
entries <- 50
max_entry <- 4000
steps <- seq(0,max_entry,by=entries)

out_all <- list()
for (i in 2:length(steps)){
  
  cat(round(i/length(steps)*100),"% \n")
  
  ## access database #############
  raw_data <- GET(paste0(base_url,steps[i-1],"/take/",entries))
  
  ## process retrieved records ############
  raw_data_list <- fromJSON(rawToChar(raw_data$content), flatten = TRUE)
  
  spec_names <- raw_data_list$results$SpeciesName
  spec_names_authors <- raw_data_list$results$SpeciesNameAuthor
  status <- raw_data_list$results$Status
  kingdom <- raw_data_list$results$Kingdom
  phylum <- raw_data_list$results$Phylum
  class <- raw_data_list$results$Class
  
  firstrecords <- rep(NA,length(spec_names))
  firstrecords_raw <- raw_data_list$results$FirstIntroductions
  ind <- lapply(firstrecords_raw,nrow)!=0
  firstrecords[ind] <- unlist(lapply(firstrecords_raw,function(s) s[1,1]))
  
  environments <- rep(NA,length(spec_names))
  environments_raw <- raw_data_list$results$SpeciesEnvironments
  ind <- lapply(environments_raw,nrow)!=0
  environments[ind] <- unlist(lapply(environments_raw[ind],function(s) paste(s[,1],collapse="; ")))
  
  SpeciesPathways <- raw_data_list$results$SpeciesPathways
  SpeciesPathways <- unlist(lapply(SpeciesPathways,function(s) paste(s$Name,collapse="; ")))

  ## output ######
  out_step <- cbind.data.frame(spec_names,
                               spec_names_authors,
                               status,
                               kingdom,
                               phylum,
                               class,
                               firstrecords,
                               environments,
                               SpeciesPathways)
  
  out_all[[i-1]] <- out_step
}

out_all_df <- do.call("rbind",out_all)

# fwrite(out_all_df,file.path("WP1","Data","EASIN_records.csv"))

table(out_all_df$status)
