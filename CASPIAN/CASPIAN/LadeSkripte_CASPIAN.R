

all_scripts <- list.files(file.path("R"))

source(file.path("R","runCASPIAN.R"))
source(file.path("R","InitializeSpread.R"))
source(file.path("R","SpreadModel.R"))
source(file.path("R","getNodesCoord.R"))
source(file.path("R","getNeighborSegmCoord.R"))
source(file.path("R","WaterSpreadModel.R"))
source(file.path("R","pUnion.R"))
source(file.path("R","plotCASPIAN.R"))
source(file.path("R","ParMatrix.R"))
source(file.path("R","InitializeWaterSpread.R"))
source(file.path("R","getCoordExtent.R"))
source(file.path("R","getConfigFile.R"))
source(file.path("R","f_natural.R"))
source(file.path("R","f_natural_riverside.R"))
source(file.path("R","f_natural_water.R"))
source(file.path("R","f_hullfouling.R"))
source(file.path("R","f_container.R"))
source(file.path("R","f_ballast.R"))
source(file.path("R","combID.R"))
source(file.path("R","Attachment_kernel.R"))
source(file.path("R","Airflow_kernel.R"))
source(file.path("R","combID.R"))


