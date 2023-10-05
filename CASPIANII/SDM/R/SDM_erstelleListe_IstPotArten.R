
library(data.table)

identifier <- "191222_NoEASIN" # a unique identifier for every run of the SDM workflow, needs to be a character

rec_presence <- fread(file.path("SDM","Data","Output", paste0("istVorkommenAlleArten_GADM4_",identifier,".gz")))
# table(duplicated(rec_presence))
setkey(rec_presence,"CC_4")
rec_presence <- na.omit(rec_presence)

# pot_presence_max <- fread(file.path("SDM","Data","Output",  paste0("potVorkommen_alleArten_maxEignung_GADM4_",identifier,".gz")))
pot_presence_mean <- fread(file.path("SDM","Data","Output", paste0("potVorkommen_alleArten_meanEignung_GADM4_",identifier,".gz")))
setkey(pot_presence_mean,"CC_4")

pot_presence_mean <- na.omit(pot_presence_mean)

rec_pot <- merge(rec_presence,pot_presence_mean,all=T,by=c("CC_4","Taxon"))
rec_pot <- na.omit(rec_pot)

setorder(rec_pot,cols="CC_4", -"HabitatEignung")
