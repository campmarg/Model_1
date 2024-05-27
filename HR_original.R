#' Home Range Movement Function
#'
#' This function calculates home range movement values based on input parameters.
#'
#' @param X A numeric vector representing the input values for home range movement calculation.
#' @param Mu The mean value for the movement distribution.
#' @param Sig The standard deviation of the movement distribution.
#' @return A matrix of home range movement values.
#' @export
#'
#' @examples
#' x_values <- seq(0, 100, length.out = 100)
#' hr_matrix <- home_range(x_values, Mu = 0, Sig = 1)
#' image(hr_matrix, main = "Home Range Movement", xlab = "X", ylab = "Distance")
#'
#' @details
#' The function calculates home range movement values based on the input parameters.
#' It takes a numeric vector \code{X} representing the input values for the movement calculation,
#' a mean value \code{Mu} for the movement distribution, and a standard deviation \code{Sig} for the movement distribution.
#' The function computes a distance matrix \code{HR_X} based on the input vector \code{X}.
#' The home range movement values \code{HR} are initialized as a matrix of zeros with the same dimensions as \code{HR_X}.
#' It then calculates the weight function (\code{weight_func}) for 2 km movement using the normal distribution
#' with mean \code{Mu} and standard deviation \code{Sig}.
#' For each distance in the range [-10, 10] from the mean value \code{Mu}, the function calculates the movement values
#' based on the weight function and the differences between two normal distributions for each distance in the range [-10, 10].
#' The final home range movement values \code{HR} are obtained by summing up the movement values for each distance.
#' The function returns a matrix of home range movement values \code{HR}.
#'
#'
#' @author Margaret Campbell
#' @keywords home range movement values distribution matrix
home_range <- function(X,Mu,Sig){
  HR_X <- as.matrix(dist(1:length(X)))
  HR <- HR_X * 0
  Mu <- 0
  Sig <- 1
  x = length(X)
  weight_func <- pnorm(2, mean = Mu, sd = Sig) - pnorm(-2, mean = Mu, sd = Sig) # Weight for 2km movement
  
  for (i in -10:10) {
    HR <- HR + weight_func * (pnorm(HR_X + 0.5 + x * i, Mu, Sig) - pnorm(HR_X - 0.5 + x * i, Mu, Sig))
  }
  
  HR = HR/max(eigen(HR)$values) # make sure it has an eigenvalue of one
  
  return(HR)
}




home_range <- function(X,Mu,Sig){
  HR_X <- as.matrix(dist(1:length(X)))
  HR <- HR_X * 0
  Mu <- 0
  Sig <- 1
  x = length(X)
  weight_func <- pnorm(2, mean = Mu, sd = Sig) - pnorm(-2, mean = Mu, sd = Sig) # Weight for 2km movement
  
  for (i in -10:10) {
    HR <- HR + weight_func * (pnorm(HR_X + 0.5 + x * i, Mu, Sig) - pnorm(HR_X - 0.5 + x * i, Mu, Sig))
  }
  
  HR = HR/max(eigen(HR)$values) # make sure it has an eigenvalue of one
  
  return(HR)
}