####################################################################
#### CASPIAN Model settings and parameters #########################
####################################################################

####################################################################
# Input Files: #####################################################

Terrestrial_netw_data <- st_read(dsn=file.path(path2data),layer="RailRoadNetw_Intersection_100519") # A shapefile containing a terrestrial traffic network; will be read in as a SpatialLinesDataFrame object

# ## Future increase for road traffic (2030) #######################
# Terrestrial_netw_data$passengers[Terrestrial_netw_data$Type!="Rail"] <- Terrestrial_netw_data$passengers[Terrestrial_netw_data$Type!="Rail"] * 1.055532 # future increases until 2030
# Terrestrial_netw_data$cargo[Terrestrial_netw_data$Type!="Rail"] <- Terrestrial_netw_data$cargo[Terrestrial_netw_data$Type!="Rail"] * 1.096781 # future increases until 2030

# ## Past level of traffic for 1994 ##################################
# ## (data avaibility less comparable before 1994)
# Terrestrial_netw_data$passengers[Terrestrial_netw_data$Type!="Rail"] <- Terrestrial_netw_data$passengers[Terrestrial_netw_data$Type!="Rail"] / 1.151327 # past increases since 1991
# Terrestrial_netw_data$cargo[Terrestrial_netw_data$Type!="Rail"] <- Terrestrial_netw_data$cargo[Terrestrial_netw_data$Type!="Rail"] / 1.8288 # past increases since 1994
# # Terrestrial_netw_data$passengers[Terrestrial_netw_data$Type=="Rail"] <- Terrestrial_netw_data$passengers[Terrestrial_netw_data$Type=="Rail"] /  # past increases since 1994
# Terrestrial_netw_data$cargo[Terrestrial_netw_data$Type=="Rail"] <- Terrestrial_netw_data$cargo[Terrestrial_netw_data$Type=="Rail"] / 1.79444 # past increases since 1994


Water_netw_data <-       st_read(dsn=file.path(path2data),layer="Waterways_Netw") # A shapefile containing a aquatic traffic network; will be read in as a SpatialLinesDataFrame object
colnames(Water_netw_data)[colnames(Water_netw_data)=="FromNod"] <- "FromNode"

# shapeObj <- readOGR(dsn=file.path("..","..","..","..","EBAspread","Data","FinalDataFiles"),layer="RailRoadNetw_Intersection_100519")

env_terrestrial <- fread(file.path(path2data,"EnvironData_Terrestrial.csv")) # file with environmental information for terrestrial network
env_aquatic <-     fread(file.path(path2data,"EnvironData_Waterways.csv")) # file with environmental information for aquatic network

Commodities_shape_data<- st_read(dsn=file.path(path2data),layer="Cargo_shp") # A shapefile containing the cargo areas IDs and locations
Pallets_netw_data<-   fread(file.path(path2data,"PalletsFlow.csv")) # file containing iformation about cargo pallets flow
Container_netw_data<- fread(file.path(path2data,"ContainerFlow.csv")) # file containing iformation about container flow

Germany_borders <- st_read(dsn=file.path(path2data), layer="gadm41_DEU_0") # shapefile of German borders

####################################################################
## General model settings ##########################################

makeplot<-TRUE # should model results be plotted as maps at steps of iter_save_?
save_plot<-TRUE # If TRUE, plots are created in the newly created folder as .png files. If FALSE, an x11() device is opened. Only considered if makeplot=TRUE.
linewidth=5 # line width for drawing lines on the map; sparse networks lwd=3; dense networks lwd=1

export_results=c("csv") #"txt",,"shp" #Should results be exported in the newly created folder? Supported values are "csv","txt",and "shp for ESRI Shapefiles.
#This will create one file per each iteration saved.
#For "csv" and "txt", the spatial information (e.g. links coordinates) will NOT be retained.

initialize<-TRUE  # Whether the model should be initialized.
save_init<-FALSE # if initialize=TRUE, should the initialization file be saved?
file_init<- "init_data.Rdata" # if initialize=TRUE, the name of the file to be created by InitializeSpread().
#   in the newly created folder (default  "init_data.rData" if save_init=TRUE). If initialize=FALSE, the FULL path
#   of the file to be read in (created by InitializeSpread() or ModelSpread() ). MUST BE an .Rdata file.

plot_funct_rel <- FALSE # plot functional relationships of dispersal kernels



######################################################################
### Settings for terrestrial model ###################################

runTerrestrialModel<-TRUE # consider terrestrial spread?

## model structure: select pathways of dispersal #####################
incl_attachment   <-FALSE # if attachment to vehicles should be considered.
incl_airflow      <-FALSE # if vehicle airstream should be considered.
incl_natural      <-FALSE #if natural dispersal should be considered.
incl_containers   <-FALSE #if container flow should be considered.
incl_pallets      <-FALSE #if pallets flow should be considered.
incl_riverside    <-TRUE #if spread along riverside of water ways should be considered (Note that this can only run in isolation. Other pathways will be ignored.)

## simulation: set simulation setting ################################
num_iter_T<- 10000 # simulation steps. Terrestrial model only.
iter_save_T <- c(1,seq(1000,9000,1000),num_iter_T) # time steps at which results should be stored. Terrestrial model only.

netw_type <- c("A","B","L") # types of network considered : "Rail" "A"    "B"    "L"    "S"    "K"    "F"    "G"    "X"    "R"    "k"
traffic_type_T <- c("all") # types of traffic considered : "cargo" (trucks and cargo trains), "passengers" (cars and passenger trains),   "all"

## initial conditions ################################################
## load data set of initial distribution of sprecies
init_coords_T <-data.frame(Long=c(9.9938531,13.2862487),Lat=c(53.5396466,52.5588327),Iter=c(0,20))  # Hamburg Hafen & Berlin airport. Terrestrial Model

max_dist_T<-10^4 # Maximum distance (m) from initial coordinates for a segment to be considered infected. Terrestrial model only.



####################################################################
## Terrestrial parameters ##########################################

############################################################
## WARNING: The parameter values were obtained from       ##
## extensive calibration analyses. Change only with care. ##
############################################################

## Dispersal parameters ####################################
## attachment kernel parameters
par_att0_Roads <- 8.371457e-07 ## pick-up probability on roads
par_att0_Railways<- 9.398072e-06 ## pick-up probability on railways

par_att1 <- 6.627903e-01  # parameter c in Taylor et al. 2012, Model 2
par_att2 <-  -3.681763e-02   # parameter b in Taylor et al. 2012, Model 2.
par_att3 <- 3.569937e-01  # parameter g in Taylor et al. 2012, Model 2

## airflow kernel parameters
par_air0_Roads <- 0.001 ## pick-up probability on Roads
par_air0_Railways <- 0.005 ## pick-up probability on Railways

par_air1 <- 2.307 # parameter b in Von Der Lippe et al. 2013, Lognormal.
par_air2 <- 0.724 # parameter a in Von Der Lippe et al. 2013, Lognormal.

## natural dispersal kernel parameter
par_nat1 <- 1.06 # González-Martínez et al. 2006. Always >0
par_nat2 <- 4.813674e-01 # González-Martínez et al . 2006.   >1: thin-tailed ; <1: fat-tailed. Values for b generally found from 0.3 to 0.6 (Nathan et al. 2012)
par_nat_riverside1 <- 1.06 # González-Martínez et al. 2006. Always >0
par_nat_riverside2 <- 4.813674e-01 # González-Martínez et al . 2006.   >1: thin-tailed ; <1: fat-tailed. Values for b generally found from 0.3 to 0.6 (Nathan et al. 2012)


## parameter for introduction by container
par_cont<-10^3 #increase for lower container probability (not fitted)
## parameter for introduction by pallet
par_pall<-10^3 #increase for lower pallet probability (not fitted)

## Treshold for container volume: all areas with number of containers arriving per year lower than Cont_treshold will not be considered
Cont_threshold<-5
## Treshold for pallets volume: all links with number of pallets exchanged per year lower than Pall_threshold will not be considered
Pall_threshold<-5

## Terrestrial establishment scale parameter ################
par_est_T<- 9.409051e-01 # <=1. Pioneer species should have high values (more likely to establish if the habitat is suitable), late succession species lower values.


## Set landcover IDs suitability for establishment ##########

Urban_areas	<- 1    #	LC_cat_ID=1
Arable_land	<-	0.5 #LC_cat_ID=2
Pastures	<-	0.1 #LC_cat_ID=3
Forests	<-	0   #LC_cat_ID=4
Wetlands	<-	0   #LC_cat_ID=5



######################################################################
### Settings for aquatic model #######################################

runAquaticModel <- FALSE # consider aquatic spread?

## model structure: select pathways of dispersal #####################
incl_natural_water<-TRUE #if natural dispersal along rivers should be considered.
incl_hullfouling <-TRUE #if hull-fouling dispersal along rivers should be considered.
incl_ballast<-TRUE #if ballast water should be considered.

## simulation: set simulation setting ################################
num_iter_W<- 10 # simulation steps. Acquatic model only.
iter_save_W <- c(1,num_iter_W) # time steps at which results should be stored. Aquatic model only.

traffic_type_W <- c("all") # types of traffic considered : "Motorized", "Non-motorized",   "all"

## initial conditions ################################################
## load data set of initial distribution of species
init_coords_W <-data.frame(Long=c(9.9938531,13.2862487),Lat=c(53.5396466,52.5588327),Iter=c(0,20))  # Hamburg Hafen & Berlin airport. Acquatic Model
max_dist_W<-10^3 # Maximum distance (m) from initial coordinates for a segment to be considered infected. Acquatic model only.

######################################################################
# Aquatic Parameters: ################################################

############################################################
## WARNING: The parameter values were obtained from       ##
## extensive calibration analyses. Change only with care. ##
############################################################

## Dispersal parameters ####################################
## Natural dispersal: see Elliot 2003, eq. 2a
par_nat_a<- 6.830226e+00 # scale parameter for Gammarus spp
par_nat_b<- 3.499327e+00 # shape parameter for Gammarus spp


## Introduction through hull-fouling: see Sylvester 2011, eq. 9

par_hull0 <- 1e-06 ## pick-up probability

par_a <-5.85e-20
par_c1 <-20.9e+00
par_g <-1.03e-10
par_c2 <-3.63e+00
par_b <-2.727334e+00  # Sylvester 2011 used 3.15 * 10^-7
par_c3 <-2.305912e+00 # ideally <3 and >2 (arbitrary)

## The following factors related to hull fouling are implemented but not
## tested due to the lack of data, and therefore not considered in the
## current version. If data are available, the data has to be provided in
## correct order. Depending on the kind of data, this could be changed by
## incorporating the data directly into the shapefile.
Port_time<-NA # Time spent in port. Set to strictly >0 if data are provided, otherwise NA
Paint_time<-NA # Time since last antifouling painting. Set to strictly >=0 if data are provided, otherwise NA

## parameter for introduction by ballast water
par_ball<-9.819106e+04 # shape parameter

## Aquatic establishment scale parameter
par_est_W<- 7.004247e-01 #arbitrary,<=1. Pioneer species should have high values (more likely to establish if the habitat is suitable), late succession species lower values.

# Optimal Temperature and Conductivity for establishment
specTemp <- 13 # optimal Temperature (degrees C)
specCond <- 100 # optimal Conductivity (mS/m)

## END OF CASPIAN CONFIGURATION FILE ###########################################################################################################################################################
