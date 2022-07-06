graphics.off()
rm(list=ls())

library(data.table)
library(openxlsx)

sTwist_full <- fread(file.path("..","IndicatorAliens","Workflow","SInAS","Output","SInAS_AlienSpeciesDB_2.4.1.csv"),stringsAsFactors = F,header=T)

sTwist_Germany <- subset(sTwist_full,Location=="Germany")
# write.table(sTwist_Germany,file.path("WP1","Data","SInAS_Extract_Germany.csv"))

## stats over databases ############################
table(unlist(strsplit(sTwist_Germany$origDB,"; ")))

sort(table(sTwist_Germany$origDB))


## export #######################

# fwrite(subset(sTwist_Germany,grepl("FirstRecords",sTwist_Germany$origDB)),file.path("WP1","Data","FirstRecords_Germany.csv"))
# fwrite(subset(sTwist_Germany,grepl("GloNAF",sTwist_Germany$origDB)),file.path("WP1","Data","GloNAF_Germany.csv"))
# fwrite(subset(sTwist_Germany,grepl("GRIIS",sTwist_Germany$origDB)),file.path("WP1","Data","GRIIS_Germany.csv"))
# fwrite(subset(sTwist_Germany,grepl("GAVIA",sTwist_Germany$origDB)),file.path("WP1","Data","GAVIA_Germany.csv"))
# fwrite(subset(sTwist_Germany,grepl("AmphRep",sTwist_Germany$origDB)),file.path("WP1","Data","AmphRep_Germany.csv"))
# fwrite(subset(sTwist_Germany,grepl("DAMA",sTwist_Germany$origDB)),file.path("WP1","Data","DAMA_Germany.csv"))
# fwrite(subset(sTwist_Germany,grepl("MacroFungi",sTwist_Germany$origDB)),file.path("WP1","Data","MacroFungi_Germany.csv"))
