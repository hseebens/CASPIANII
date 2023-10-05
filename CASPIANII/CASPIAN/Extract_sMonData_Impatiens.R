## load sMon data for Impatiens glandulifera

library(data.table)

i <- 4

sMon_Verzeichnis <- "C:/Hanno/Storage_large/Species/sMon"


sMon_data_species <- fread(file.path(sMon_Verzeichnis,paste0("1875_9_1875_2_Modelled_OPs_incl_sd_pt_",i,".csv")),select="TaxonName")
ind_records <- which(sMon_data_species==TaxonName)

if (length(ind_records)!=0){
  col_names <- colnames(fread(file.path(sMon_Verzeichnis,paste0("1875_9_1875_2_Modelled_OPs_incl_sd_pt_",i,".csv")),nrows=0))
  sMon_data_sub <- fread(file.path(sMon_Verzeichnis,paste0("1875_9_1875_2_Modelled_OPs_incl_sd_pt_",i,".csv")),
                         nrows=length(ind_records),skip=min(ind_records))
  colnames(sMon_data_sub) <- col_names
  
  sMon_data[[i]] <- subset(sMon_data_sub,OP>=sMon_Wahrscheinlichkeit)
  
  plot(subset(sMon_data[[i]],Period=="1997 - 2017")$Longitude,
       subset(sMon_data[[i]],Period=="1997 - 2017")$Latitude)
  
  fwrite(sMon_data_sub,file=file.path("Vorkommen","Data","Vorkommen_Impatiens_glandulifera_sMon.csv"))
}
