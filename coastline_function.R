#' Coastline Function
#'
#' This function generates a coastline based on input parameters.
#'
#' @param Mu The mean value. 
#' @param Sig The standard deviation of the dispersal distance.
#' @return A numeric vector representing the coastline with 1s and 0s.
#' @export
#'
#' @details
#' The function creates a coastline with 100 patches, 10 of which become an OWF. 
#' The function returns a numeric vector representing the coastline pattern with 1s
#' for cells in the OWF and 0s for cells with no OWF.
#'
#'
#'
#' @author Margaret Campbell
#' @keywords coastline pattern reserve wind-farm
coastline_function <- function(Mu = 0, Sig = 10, Rw_R1 = 1, Fr_R1 = 0.1) {

  
  # Making the coastline
  Res <- Sig * Rw_R1 # how wide reserve is equal to std of dispersal distance
  Frac <- 1 / Fr_R1 # if 10% how many cells
  Facts <- 1:20
  Rem = (Frac * Facts) %% 1 # numbers with  decimal
  Fact <- Facts[which(Rem == min(Rem))[1]] # factor multiplying by to get number of patchces
  
  # reserve width times factor - for 1/0s - how many in/not
  NumberofOnes <- round(Res * Fact) # This is how many will be in OWF
  NumberofZeros <- round(Res * Fact / Fr_R1 - Res * Fact) # This is how many will not be in OWF
  Ones <- rep(1, NumberofOnes)  # Create a vector of 1s with length NumberofOnes
  Zeros <- rep(0, NumberofZeros) # Create a vector of 0s with length NumberofZeros
  X <- c(Ones, Zeros) # Combine the vectors
  
  return(X)
}
