##################### prepare EASIN data ########################################################
# 
# 
# Project: CASPIAN II
# 
# Hanno Seebens, 29.06.22
###############################################################################################################


graphics.off()
rm(list=ls())

library(openxlsx)

easin_raw <- read.xlsx(file.path("WP1","Data","SingleDataSets","EASIN data extraction_040722_IvanDeriu.xlsx"),sheet=2)

## combine habitat types ############
easin_raw$Lebensraum <- NA

easin_raw$Lebensraum[easin_raw$TER==1] <- paste(easin_raw$Lebensraum[easin_raw$TER==1],"terrestrisch",sep="; ")
easin_raw$Lebensraum[easin_raw$FRW==1] <- paste(easin_raw$Lebensraum[easin_raw$FRW==1],"limnisch",sep="; ")
easin_raw$Lebensraum[easin_raw$MAR==1] <- paste(easin_raw$Lebensraum[easin_raw$MAR==1],"marin",sep="; ")
easin_raw$Lebensraum[easin_raw$OLI==1] <- paste(easin_raw$Lebensraum[easin_raw$OLI==1],"brackisch",sep="; ")

easin_raw$Lebensraum <- gsub("NA; ","",easin_raw$Lebensraum)


## remove unsure/early records mostly from DAISIE

easin_raw <- subset(easin_raw,Year.of.First.Introducton.in.EU>1500)


