
VorhersageVerzeichnis=file.path("SDM","Data","Output")
identifier <- "160823" # eine eindeutige Kennzeichnung des Modelllaufs (z.B. Datum)


## read all files of model fits
all_files <- list.files(VorhersageVerzeichnis)
all_files <- all_files[grep(identifier,all_files)]
all_files <- all_files[grep("ModelFit_",all_files)]
all_files <- all_files[grep("\\.RData",all_files)]

auc_values <- rsq_values <- dev_values <- list()
for (i in 1:length(all_files)){
  load(file=file.path(VorhersageVerzeichnis, all_files[i])) # load data set 'modelruns'
  auc_values[[i]] <- mean(unlist(lapply(modelruns,"[[",4)))
  rsq_values[[i]] <- summary(modelruns[[1]]$mod)$r.sq
  dev_values[[i]] <- summary(modelruns[[1]]$mod)$dev.expl
  
  print(i)
}

auc_mean <- mean(unlist(auc_values))
auc_sd <- sd(unlist(auc_values))
rsq_mean <- mean(unlist(rsq_values))
rsq_sd <- sd(unlist(rsq_values))
dev_mean <- mean(unlist(dev_values))
dev_sd <- sd(unlist(dev_values))
