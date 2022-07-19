graphics.off()
rm(list=ls())

setwd("/home/hanno/Bioinvasion/CASPIANII")

library(rgbif)
library(openxlsx)
library(data.table)


fulllist <- read.xlsx(file.path("WP1","Data","ListeGebietsfremderArten_gesamt_standardisiert.xlsx"))


tab_pathways <- sort(table(unlist(strsplit(fulllist$pathway,"; "))),decreasing=T)[1:10]
names(tab_pathways)[names(tab_pathways)=="Anhaftung an Landfahrzeuge (Bahn, Straßenverkehr)"] <- "Anhaftung an Landfahrzeuge"

x11(width=6,height=5)
# png(file.path("WP1","Figures","Hist_Pathways.png"),width=6,height=5,units = "in",res=300)
op <- par(mar=c(4,12,1,1),las=1,mgp=c(1.5,0.5,0))
barplot(rev(tab_pathways),horiz = T,xlab="Artenzahl")
text(1200,seq(11.5,0.7,length.out=10),paste("n =",(tab_pathways)))
par(op)
# dev.off()

tab_data <- sort(table(unlist(strsplit(fulllist$comment,"; "))),decreasing=T)

x11(width=6,height=5)
# png(file.path("WP1","Figures","Hist_DataAvailability.png"),width=6,height=5,units = "in",res=300)
op <- par(mar=c(4,18,1,1),las=1,mgp=c(1.5,0.5,0))
barplot(rev(tab_data),horiz = T,xlab="Artenzahl")
text(2000,seq(8,0.7,length.out=7),paste("n =",(tab_data)))
par(op)
# dev.off()


## 2D table of number of species per pathway and data availability
uni_paths <- sort(unique(unlist(strsplit(fulllist$pathway,"; "))))
uni_data <- sort(unique(unlist(strsplit(fulllist$comment,"; "))))
unique(fulllist$pathway)
path_data <- cbind.data.frame(uni_paths,0)
colnames(path_data) <- c("path","dummy")
for (i in 1:length(uni_data)){
  
  ## identify rows of respective pathway
  # if (uni_paths[i]=="Anhaftung an Landfahrzeuge (Bahn, Straßenverkehr)") uni_paths[i] <- "Anhaftung an Landfahrzeuge"
  ind_data <- grep(uni_data[i],fulllist$comment,fixed = T)
  
  ## get data availability of respective pathway 
  tab_path <- as.data.frame(table(unlist(strsplit(fulllist$pathway[ind_data],"; "))))
  
  path_data <- merge(path_data,tab_path,all.x=T,by.x="path",by.y="Var1")
  
  colnames(path_data)[i+2] <- uni_data[i]
}
# write.table(path_data,file.path("WP1","Data","Tab_Pathway_Data.csv"))


### top species with high data availability and right pathway

## Landfahrzeuge
landfahr <- subset(fulllist,grepl("sehr gute ",fulllist$comment) & grepl("Landfahrzeuge",fulllist$pathway))[,c("Taxon","taxonGroup","nRecords_GBIF_DE","nRecords_GBIF_All","nRecords_sMon")]

# write.table(landfahr,file.path("WP1","Data","Kandidaten_sehrGuteDaten_Landfahrzeuge.csv"))

## Ballast
ballast <- subset(fulllist,grepl("",fulllist$comment) & grepl("Ballast",fulllist$pathway))[,c("Taxon","taxonGroup","nRecords_GBIF_DE","nRecords_GBIF_All")]
# write.table(ballast,file.path("WP1","Data","Kandidaten_sehrGuteDaten_Ballastwasser.csv"))
