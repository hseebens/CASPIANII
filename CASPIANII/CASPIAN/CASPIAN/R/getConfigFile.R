#' @title Function to obtain the Configuration File for CASPIAN.
#' @description This function allows the user to obtain the Configuration File needed to run CASPIAN. An existing file with the same name will be overwritten.
#' It is STRONGLY RECOMMENDED to change only the values of parameters and variables, not their names, or the model will not recognize them.
#' @param outputDir character, the path where the Configuration File will be saved
#'
#' @keywords CASPIAN
#' @author Maurizio Bagnara, Hanno Seebens
#' @examples
#' #saving the configuration file in "C:"
#' getConfigFile("C:/")
#'
#' #saving the configuration file in the current working directory, overwriting existing file:
#' getConfigFile(getwd(),overwrite=TRUE)
#'

getConfigFile <- function(outputDir,overwrite=FALSE){
  if (file.exists(file.path(outputDir,"ConfigFile.R"))) {
    if (overwrite==TRUE) {
      file.copy(from=file.path(system.file("extdata", package="CASPIAN"),"ConfigFile.R"),
                                     to=file.path(outputDir,"ConfigFile.R"),overwrite=overwrite)
      warning("Existing configuration file has been overwritten")
      cat("\n Configuration file saved in ",outputDir, " \n")
      } else if (overwrite==FALSE) {
        stop("Configuration file already exists. To overwrite, set overwrite=TRUE")
      }
  } else {
    file.copy(from=file.path(system.file("extdata", package="CASPIAN"),"ConfigFile.R"),
                     to=file.path(outputDir,"ConfigFile.R"),overwrite=F)
    cat("\n Configuration file saved in ",outputDir, " \n")
  }
}


