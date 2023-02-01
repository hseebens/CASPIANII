rm(list=ls())

library(data.table)

neobiota <- read.xlsx(file.path("Data","Input","ListeGebietsfremderArten_gesamt_standardisiert.xlsx"),sheet=1)
neobiota <- subset(neobiota,Datenbank!="EASIN") # avoid records only reported by EASIN
artenliste <- subset(neobiota,Eintraege_GBIF_DE<5000 & Eintraege_GBIF_DE>100)$Taxon

identifier <- "021222_Europe"
identifier_new <- "191222_NoEASIN"

all_files <- list.files(file.path("Data","Input"))
Vorkommen_alle <- all_files[grep("Vorkommen_",all_files)]
Vorkommen_alle <- Vorkommen_alle[grep(identifier,Vorkommen_alle)]
Vorkommen_alle <- sort(Vorkommen_alle[grep(".csv",Vorkommen_alle)])
available <- gsub("Vorkommen_|.csv","",Vorkommen_alle)
species <- gsub(identifier,"",available)
species <- gsub("_","",species)

artenliste[!artenliste%in%species]

for (i in 1:length(artenliste)){
  ind <- grep(artenliste[i],Vorkommen_alle,perl=F)
  if (length(ind)==0) next
  dat <- fread(file.path("Data","Input",Vorkommen_alle[ind]))
  new_file <- gsub(identifier,identifier_new,Vorkommen_alle[ind])
  fwrite(dat,file.path("Data","Input",new_file))
}