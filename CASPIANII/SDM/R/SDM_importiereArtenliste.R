##################################################################################
#
# Dieses Skript liest die Artenliste ein.

# Filter nach Arten mit ausreichend Datenpunkten (>50) und nicht zu vielen Datenpunkten (<10000 in 
# Deutschland), da Simulationen ansonsten sehr lange dauern würden. Für Arten mit großen Datenmenge wurde 
# ein alternativer workflow entwickelt ("SDM_bezieheHoheDatenmengen.R"), mit dem die Daten bezogen und
# aufbereitet werden können. Dieser Schritt würde Schritt 1 unten für Arten mit großen Datenmengen
# ersetzen. Schritte 2-7 können dann für alle Arten gleich durchgeführt werden.
#
# Hanno Seebens, 17.07.2023
##################################################################################


importiereArtenliste <- function(Name_Artenliste=NULL,
                                 Min_Anzahl_GBIF_DE=50,
                                 Max_Anzahl_GBIF_DE=20000){
 
  Artenliste <- read.xlsx(file.path("SDM","Data","Input",Name_Artenliste),sheet=1)
  
  Artenliste <- subset(Artenliste,Eintraege_GBIF_DE<Max_Anzahl_GBIF_DE & Eintraege_GBIF_DE>Min_Anzahl_GBIF_DE)$Taxon
  
  return(Artenliste)
  
}
