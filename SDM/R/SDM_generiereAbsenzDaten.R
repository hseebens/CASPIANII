################################################################################
#
# Skript ist Teil des Workflows "SDM" zur Habitatmodellierung von Neobiota in 
# Deutschland. Es wird mit dem Skript run_SDM_workflow.R aufgerufen.
#
# Die Funktion generiert Pseudo-Absenz Datensätze, die zur Bestimmung der 
# Modellparameter des SDMs verwendet werden. Für jeden Datenpunkt werden 
# Umweltdaten hinzugefügt.
#
# Hanno Seebens, Senckenberg Gesellschaft für Naturforschung, 08.12.25
################################################################################


generiereAbsenzDaten <- function(TaxonName=NULL,
                                 VorkommenUmwelt=NULL, 
                                 n_AbsenzDaten=5,
                                 speichern=TRUE,
                                 identifier=NULL) { ## start of main function

  cat(paste0("\n*** Generiere ",n_AbsenzDaten," Absenzdatensaetze für ",TaxonName," ***\n") ) # notification for the user

  ## load predictor variables
  predictor_stack <- rast(file.path("SDM","Daten","Input",paste0("UmweltdatenRaster",identifier,".tif")))

  col_names_pred <- names(predictor_stack)
  
  ## extract and prepare occurrence records
  occ_data <- as.data.frame(VorkommenUmwelt,stringsAsFactors=F)
  occ_data$Zeitpunkt <- 1
  colnames(occ_data)[colnames(occ_data)=="Zeitpunkt"] <- "Praesenz"
  occ_data$Taxon <- NULL
  occ_data$Datenbank <- NULL
  
  ## prepare a template with all terretrial cells to sample from
  n_cells <- global(predictor_stack, fun="notNA")
  template <- predictor_stack[[which.min(n_cells$notNA)]] 

  ## set cells with available occurrence records to NA (not selected for random points)
  cell_ind <- terra::cellFromXY(template, as.matrix(occ_data[,c("Laengengrad","Breitengrad")]))
  values(template)[cell_ind] <- NA

  PAlist <- list()
  for (i in 1:n_AbsenzDaten){  
    
    ## generate n random pseoudo absences
    all_cell_IDs <- terra::cells(template)
    sample_cell_IDs <- all_cell_IDs[sample(1:length(all_cell_IDs), size=max(c(3*nrow(VorkommenUmwelt), 10000)))] # select either 3 times the number of presences (advice from biomod2 team) or a max of 10000
    coords_cell_IDS <- xyFromCell(template, sample_cell_IDs)
    
    PA <- as.data.frame(coords_cell_IDS)
    colnames(PA) <- c("Laengengrad","Breitengrad")
    PA$Praesenz <- 0

    ## extract and add predictor variables to pseudo absence location
    PAenv <- cbind(PA, pred_var = terra::extract(x = predictor_stack, y = data.frame(PA[,c('Laengengrad','Breitengrad')]), ID=FALSE))
    colnames(PAenv) <- c("Laengengrad", "Breitengrad", "Praesenz", col_names_pred)

    ## select complete cases 
    PAenv <- PAenv[complete.cases(PAenv),]
    # PAenv <- PAenv[sample(1:nrow(PAenv),10000),]

    ## output
    OccenvPA <- rbind(occ_data, PAenv) # combining PAs with occurrences

    PAlist[[i]] <- OccenvPA
    # return(PA)

    if (i==n_AbsenzDaten){
      cat("\n Absenzdatensatz ",i," generiert.\n")
    } else {
      cat("\n Absenzdatensatz ",i," generiert.")
    }
  }
  
  ## save results to disk
  if (speichern){
    if (is.null(TaxonName) | is.null(identifier)){
      warning("Angabe 'TaxonName' oder 'identifier' fehlt. Bitte ergänzen.")
    }
    save(PAlist, file=file.path("SDM","Daten","Input", paste0("PAlist_",TaxonName,identifier,".RData")))
    # load(file=file.path("Daten","Input", "PAlist_Acer saccharinum_run.071122.test.RData"))
  }
  
  return(PAlist)
  
} 
