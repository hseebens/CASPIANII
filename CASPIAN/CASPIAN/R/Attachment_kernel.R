#' @title Function for the terrestrial attachment kernel.
#' @description This function computes the probability of a species to reach a point in the network
#' starting from a different point by attachment to vehicles. Mathematically described as exponential function (see Taylor et al. 2012, model 2).
#' @param D numeric, a vector of distances between the points (in Km)
#' @param a numeric, shape parameter.
#' @param b numeric, scale parameter.
#' @param g numeric, scale parameter.
#' @return numeric vector, including the probabilities of a species travelling the input distances by attachment to vehicles.
#'
#' @keywords CASPIAN
#' @author Maurizio Bagnara, Hanno Seebens
#' @examples
#' #generating vector of distances, in Km:
#' dist<-seq(from=0.1,to=10,length.out=100)
#'
#' #assigning parameter values:
#' par_a <- 0.6556  # parameter c in Taylor et al. 2012, Model 2
#' par_b <-  -0.03675   # parameter b in Taylor et al. 2012, Model 2.
#' par_g <- 0.3311  # parameter g in Taylor et al. 2012, Model 2

#'
#' #Calculating and plotting probabilities
#' AttachmentDisp<-f_airflow(D = dist, a = par_a, b = par_b,g = par_g)
#' plot(AttachmentDisp~D,xlab="Distance (Km)", ylab="Probability", pch=16)
#'
#'
#' @references
#' Taylor, K., Brummer, T., Taper, M. L., Wing, A., & Rew, L. J. (2012). Human‐mediated long‐distance dispersal: an empirical evaluation of seed dispersal by vehicles. Diversity and Distributions, 18(9), 942-951.
#'
#'

## attachment kernel. Models dispersal due to distance between nodes
# currently represented as model 2 in Taylor et al 2012, requires distance in k
f_attach <- function(D,a,b,g) return(exp(b*exp(a*(D^g))))
