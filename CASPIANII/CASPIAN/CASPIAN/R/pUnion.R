#' @title The function calculates the combined probabilities of non mutually exclusive events: p(E1) OR p(E2) OR p(E3)... OR p(En)
#' @description This function computes the overall combined probability of a series of events, non mutually exclusive,
#' starting from the probability of each single event.
#' @param p numeric, a vector of probabilities
#' @return a single numeric value, the overall combined probability of the events

#' @keywords CASPIAN
#' @author Maurizio Bagnara, Hanno Seebens
#' @examples
#' # define probability for 3 events:
#' p1<- 0.3
#' p2<- 0.6
#' p3<- 0.5
#' prob<-c(p1,p2,p3)
#'
#' # calculate the combined probability of event 1 happening, OR event 2, OR event 3, OR Events 1 AND 2 etc.
#' CombProb<-pUnion(prob)
#' print(CombProb)
#'
#'

pUnion<-function(p)  1 - prod(1-p)
