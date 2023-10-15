# Utility function for calibration: 
# replaces the parameters to be calibrated in the default parameter matrix

createMixWithDefaults<-function (pars, defaults, locations) {  
  out = defaults
  out[locations] = pars
  return(out)
}
