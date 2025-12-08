################################################################################
#
# Skript ist Teil des Workflows "SDM" zur Habitatmodellierung von Neobiota in 
# Deutschland. Es wird mit dem Skript run_SDM_workflow.R aufgerufen.
#
# Dieses Skript liest die zu bearbeitende Artenliste ein.
#
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 08.12.25
################################################################################


importiereArtenliste <- function(Name_Artenliste=NULL,
                                 max_limit=10000
                                 ){
 
  Artenliste <- read.xlsx(file.path("SDM","Daten","Input",Name_Artenliste),sheet=1)
  
  ListeVieleDaten <- Artenliste[Artenliste$Eintraege_GBIF_DE >= max_limit,]
  
  if (nrow(ListeVieleDaten)>0){
    
    cat(c("\n Einige Arten haben sehr viele Eintraege. Die Vorkommen dieser Arten 
        sollten mit SDM_bezieheHoheDatenmengen.R herunter geladen und anschließend 
        run_SDM_workflow.R ausgeführt werden. Diese Arten werden unter 
        Arten_mit_vielen_Eintraegen.csv gespeichert.\n"))
    
    ListeVieleDaten <- ListeVieleDaten[,c("Taxon","wissenschaftlicherName", "Artengruppe")]
    
    fwrite(ListeVieleDaten,file.path("SDM","Daten","Input","Liste_Arten_mit_vielen_Eintraegen.csv"))
  } 
  
  return(sort(Artenliste$Taxon))
  
} ## end of main function
