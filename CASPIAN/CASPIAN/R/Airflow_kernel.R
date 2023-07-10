#' @title Function for the terrestrial airstream kernel.
#' @description This function computes the probability of a species to reach a point in the network
#' starting from a different point by vehicle airstream. Mathematically described as lognormal function (see Von der Lippe et al. 2013).
#' @param D numeric, a vector of distances between the points (in Km)
#' @param b numeric, shape parameter. Should be always >0.
#' @param a numeric, scale parameter.
#' @return numeric vector, including the probabilities of a species travelling the input distances by vehicle airstream.
#'
#' @keywords CASPIAN
#' @author Maurizio Bagnara, Hanno Seebens
#' @examples
#' #generating vector of distances, in Km:
#' dist<-seq(from=0.1,to=10,length.out=100)
#'
#' #assigning parameter values:
#' par_a <- -0.724 # Von Der Lippe et al. 2013. Values for A. altissima
#' par_b <- 2.307 # Von Der Lippe et al. 2013. Values for A. altissima
#'
#' #Calculating and plotting probabilities
#' AirstreamDisp<-f_airflow(D = dist, b = par_b, a = par_a)
#' plot(AirstreamDisp~D,xlab="Distance (Km)", ylab="Probability", pch=16)
#'
#' @references
#' von der Lippe, M., Bullock, J. M., Kowarik, I., Knopp, T., & Wichmann, M. (2013). Human-mediated dispersal of seeds by the airflow of vehicles. PloS one, 8(1), e52733.
#'
#'

## airflow kernel. Models dispersal due to vehicle airflow between nodes.
# currently represented as lognormal, Von der Lippe et al. 2013, originally requires D in m, this function requires Km

#IMPORTANT NOTE: in Von der Lippe et al. 2013, the parameter values they publish for "a" are actually "b" and viceversa.
# It is so, because they provide values of b<0, and it cannot be in a lognormal function.


f_airflow<-function (D,b,a) {
  D<-D*1000 #converting km to m
 return(1/(sqrt(2*pi)*b*D)*exp(-((abs(log(D) - a))^2 / (2*b^2))))
  }
