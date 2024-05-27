#' Home Range Movement Function
#'
#' This function calculates home range movement values based on input parameters.
#'
#' @param X A numeric vector representing the coastline.
#' @param Mu The mean value for the movement distribution.
#' @param Sig The standard deviation of the movement distribution.
#' @return A matrix of home range movement values.
#' @export
#'
#' @author Margaret Campbell
#' @keywords home range movement values distribution matrix
#Home Range Yellowtail Rockfish
home_range <- function(X,Mu){
  HR_X <- as.matrix(dist(1:length(X)))
  HR <- HR_X * 0
  Mu <- 0
  SigHR <- 0.4625
  x = length(X)
  weight_func <- pnorm(2, mean = Mu, sd = SigHR) - pnorm(-2, mean = Mu, sd = SigHR) # Weight for 2km movement
  
  for (i in -10:10) {
    HR <- HR + weight_func * (pnorm(HR_X + 0.5 + x * i, Mu, SigHR) - pnorm(HR_X - 0.5 + x * i, Mu, SigHR))
  }
  
  HR = HR/max(eigen(HR)$values) # make sure it has an eigenvalue of one
  
  return(HR)
}