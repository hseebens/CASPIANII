#' @title Function for the terrestrial natural dispersal kernel.
#' @description This function computes the probability of a species to reach a point in the network
#' starting from a different point by natural processes. Mathematically described as an exponential power law (see Nathan et al.2012).
#' @param D numeric, a vector of distances between the points (in Km).
#' @param a numeric, shape parameter. Should be always >0
#' @param b numeric, shape parameter. Should be always >0. if b>1: thin-tailed kernel; if b<1: fat-tailed kernel;
#' Exponential if b=1, Gaussian if b=2
#' @return numeric vector, including the probabilities of a species travelling the input distances by natural processes.
#'
#' @keywords CASPIAN
#' @author Maurizio Bagnara, Hanno Seebens
#' @examples
#' #generating vector of distances, in Km:
#' dist<-seq(from=0.1,to=10,length.out=100)
#'
#' #assigning parameter values:
#' par_a <- 0.0001 # González-Martínez et al . 2006. Values for P. pinaster.
#' par_b <- 0.5 # González-Martínez et al . 2006. Values for P. pinaster.
#'
#' #Calculating and plotting probabilities
#' natDisp<-f_natural(D = dist,a = par_a, b = par_b)
#' plot(natDisp~D,xlab="Distance (Km)", ylab="Probability", pch=16)
#'
#'
#' @references
#' Gonzàlez-Martìnez, S. C., Burczyk, J., Nathan, R. A. N., Nanos, N., Gil, L., & Alia, R. (2006). Effective gene dispersal and female reproductive success in Mediterranean maritime pine (Pinus pinaster Aiton). Molecular Ecology, 15(14), 4577-4588.
#' Nathan, R., Klein, E. K., Robledo-Arnuncio, J. J., & Revilla, E. (2012). Dispersal kernels. In Dispersal ecology and evolution (pp. 187-210). Oxford: Oxford University Press.
#'
#'

## natural dispersal kernel. Models dispersal due to natural movement between nodes.
# currently represented as negative exponential, Nathan et al.2012, originally requires D in m, this function requires Km
# Time step?


f_natural_riverside <- function(D,a,b) {
  # D<-D*1000  #converting km to m
  #negative logistic:
  #return(1/(2*pi*(a^2))*exp(-D/a))

  #exponential power:
  # return((b/(2*pi*(a^2)*gamma(2/b)))*exp(-(D^b)/(a^b)))
  
  # D <- 10
  # a <- 1.06 # González-Martínez et al. 2006. Always >0
  # b <- 4.813674e-01 # González-Martínez et al . 2006.   >1: thin-tailed ; <1: fat-tailed. Values for b generally found from 0.3 to 0.6 (Nathan et al. 2012)
  # 
  res <- (b/(2*pi*(a^2)*gamma(2/b)))*exp(-(D^b)/(a^b))
  
  return(res)
}

