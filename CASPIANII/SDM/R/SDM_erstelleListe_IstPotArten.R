
library(data.table)

identifier <- "191222_NoEASIN" # a unique identifier for every run of the SDM workflow, needs to be a character

rec_presence <- fread(file.path("SDM","Data","Output", paste0("istVorkommenAlleArten_GADM4_",identifier,".gz")))
# pot_presence_max <- fread(file.path("SDM","Data","Output",  paste0("potVorkommen_alleArten_maxEignung_GADM4_",identifier,".gz")))
pot_presence_mean <- fread(file.path("SDM","Data","Output", paste0("potVorkommen_alleArten_meanEignung_GADM4_",identifier,".gz")))

rec_pot <- merge(rec_presence,pot_presence_mean,by="CC_4")

