#' Dispersal Function
#'
#' This function calculates the dispersal values based on the input parameters.
#'
#' @param X A numeric vector representing the coastline.
#' @param Mu The mean value for the dispersal distribution.
#' @param Sig The standard deviation of the dispersal distribution.
#' @return A matrix of dispersal values.
#' @export
#'
#' @details
#' The function computes the dispersal values based on the input parameters.
#' It takes a numeric vector \code{X} representing the coastline for the dispersal calculation,
#' a mean value \code{Mu} for the dispersal distribution, and a standard deviation \code{Sig} for the dispersal distribution.
#' The function then generates a distance matrix \code{Dist_X} based on the input vector \code{X}.
#' The dispersal values \code{D} are initialized as a matrix of zeros with the same dimensions as \code{Dist_X}.
#' It then calculates the dispersal values for each distance in the range [-10, 10] from the mean value \code{Mu}
#' using the normal distribution with standard deviation \code{Sig}.
#' The final dispersal values \code{D} are obtained by summing up the differences between the two normal distributions
#' for each distance in the range [-10, 10].
#' The function returns a matrix of dispersal values \code{D}.
#'
#'
#' @author Margaret Campbell
#' @keywords dispersal values distribution matrix
dispersal_function <- function(X, Mu, Sig) {
  x <- length(X)
  x_value <- 1:x
  

  Dist_X <- as.matrix(dist(x_value))
  D <- Dist_X * 0
  
  for (i in -10:10) {
    D <- D + pnorm(Dist_X + 0.5 + x * i, Mu, Sig) - pnorm(Dist_X - 0.5 + x * i, Mu, Sig)
  }
  
  # constrain to be positive
  D[D<0] = 0
  return(D)
}