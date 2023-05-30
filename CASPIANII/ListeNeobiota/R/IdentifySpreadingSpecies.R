graphics.off()

library(openxlsx)
library(rgbif)
library(data.table)

# taxon_name <- "Salvia verticillata"
# 
# all_taxa <- c("Geum montanum","Papaver orientale","Sinapis alba","Buddleja davidii","Salvia verticillata","Netta rufina","Chenopodium desiccatum","Cartodere bifasciata")

# Ausschnitt <- c(5,45,15,58) # lower left and upper right corner (long-lat)
# 
# bounding_box <- paste("POLYGON ((",Ausschnitt[1],Ausschnitt[2],",",
#                     Ausschnitt[3], Ausschnitt[2],",", 
#                     Ausschnitt[3], Ausschnitt[4],",",
#                     Ausschnitt[1], Ausschnitt[4],",", 
#                     Ausschnitt[1], Ausschnitt[2],
#                     "))")


dat <- read.xlsx(file.path("ListeNeobiota","Data","ListeGebietsfremderArten_gesamt_standardisiert.xlsx"),sheet=1)

# dat_minor <- subset(dat,Eintraege_GBIF_DE<5000 & Eintraege_GBIF_DE>100)
dat_minor <- dat
all_taxa <- dat_minor$Taxon

# ## generate baseline increase for the country (taking all records)
# # time_period <- 2000:2022
# lastfullyear <- year(Sys.Date())-1
# time_period <- (lastfullyear-9):lastfullyear
# 
# nrecords_year <- data.frame(year=time_period,nrecords=NA,relchange=NA)
# for (i in 1:length(time_period)){
#   nrecords_year$nrecords[i] <- occ_count(country="DE",georeferenced = T, year=time_period[i]) 
#   if (i>1){
#     nrecords_year$relchange[i] <- nrecords_year$nrecords[i] / nrecords_year$nrecords[i-1]
#   }
# }
# nrecords_year$percent <- round((nrecords_year$relchange - 1) * 100 )
# 
# percent_increase_baseline <- round(sum(nrecords_year$nrecords[(nrow(nrecords_year)-2):nrow(nrecords_year)]) / sum(nrecords_year$nrecords) *100)
# # percent_increase_baseline <- percent_increase_baseline * 1.20 # increase by 20% to find extreme developments
# 
# ## loop over all species
# 
# min_recent <- min(time_period[(length(time_period)-2):length(time_period)])
# 
# max_limit <- 10000 # max number of records for download

# out_all <- data.frame(Taxon=all_taxa,nEintraege_10Jahre=NA,nEintraege_3Jahre=NA,RelAnstieg=NA)
out_all <- fread(file.path("ListeNeobiota","Data","relAnstieg.csv"))
for (i in 1:length(all_taxa)){
  
  # if (i%%10==0){
  #   print(i)
  #   fwrite(out_all,file.path("ListeNeobiota","Data","relAnstieg.csv"))
  # }
  
  taxon_name <- all_taxa[i]
  # taxon_name <- "Bunias orientalis"
  
  # xx <- name_backbone(name=taxon_name)$usageKey
  # nrecords <- occ_count(xx,georeferenced=T,country="DE",year=2010) # number of available records
  
  time_period <- 2000:2023
  occ_dat_all <- as.data.frame(time_period)
  occ_dat_all$spec_records <- 0
  occ_dat_all$higher_records <- 0
  
  for (t in 1:nrow(occ_dat_all)){
    
    try(occ_dat_all$spec_records[t] <-  occ_count(scientificName = taxon_name,country="DE",hasCoordinate=T, year=occ_dat_all$time_period[t]))
    try(occ_dat_all$higher_records[t] <- occ_count(familyKey = name_backbone(taxon_name)$familyKey,country="DE",hasCoordinate=T, year=occ_dat_all$time_period[t]))
    
  }  


  if (nrow(occ_dat_spec)<100) next
  
  occ_dat_all$anteil_art <- occ_dat_all$spec_records / occ_dat_all$higher_records
  
  plot(occ_dat_all$time_period,occ_dat_all$anteil_art)
  
  regress_res <- lm(occ_dat_all$anteil_art ~ occ_dat_all$time_period)

  overall_p <- function(my_model) {
    f <- summary(my_model)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
  }
  overall_p(regress_res)
  
  if (overall_p<0.05) 
    
    
    
  
  if (is.null(nrow(occ_dat))) next
  if (nrow(occ_dat)==max_limit) next # unreliable for species reaching max limit

  if ("eventDate"%in%colnames(occ_dat)){
    
    occ_dat <- subset(occ_dat,!is.na(eventDate))
    
    out_all$nEintraege_10Jahre[i] <- nrow(occ_dat)
    
    if (nrow(occ_dat)==0) next
    
    occ_dat$eventDate <- as.Date(occ_dat$eventDate)
    
    ## determine number of records during last 10 years    
    ind_recent <- year(occ_dat$eventDate) >= min(time_period) # at least 10 years of records required
    occ_dat <- occ_dat[ind_recent,]
    
    if (nrow(occ_dat)<50) next
    # print(i)
    # if (length(table(year(occ_dat$eventDate)))>9){ 
      
    ## determine number of records during last three years
    dat_recent <- subset(occ_dat,year(eventDate) >= min_recent)
    # dat_since20y <- subset(occ_dat,eventDate > Sys.Date()-(20*365))
    
    perc_increase <- nrow(dat_recent) / nrow(occ_dat) * 100
    
    compare2reference <- round(perc_increase/percent_increase_baseline,2)
    
    if (compare2reference > 1.5){
      cat(paste0("Vermehrte Eintraege fuer ",taxon_name,":\n",round(perc_increase),"% der Eintraege seit 10 Jahren in den letzten drei Jahren.\n"))
    }
    
    out_all$nEintraege_3Jahre[i] <- nrow(dat_recent)
    # out_all$RelAnstieg[i] <- round(perc_increase,2)
    out_all$RelAnstieg[i] <- compare2reference
    
  }
  # }
}

output <- merge(dat,out_all,by="Taxon",all=T)

write.xlsx(output,file.path("ListeNeobiota","Data","ListeGebietsfremderArten_gesamt_standardisiert_relAnstieg.xlsx"),sheet=1)

# x11()
# plot(as.data.frame(table(year(as.Date(occ_dat$eventDate)))))
# # hist(out_all$RelAnstieg,100)
