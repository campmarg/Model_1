#Home Range Dover Sole
home_range <- function(X,Mu){
  HR_X <- as.matrix(dist(1:length(X)))
  HR <- HR_X * 0
  Mu <- 0
  SigHR <- 0.0175 
  x = length(X)
  weight_func <- pnorm(2, mean = Mu, sd = SigHR) - pnorm(-2, mean = Mu, sd = SigHR) # Weight for 2km movement
  
  for (i in -10:10) {
    HR <- HR + weight_func * (pnorm(HR_X + 0.5 + x * i, Mu, SigHR) - pnorm(HR_X - 0.5 + x * i, Mu, SigHR))
  }
  
  HR = HR/max(eigen(HR)$values) # make sure it has an eigenvalue of one
  
  return(HR)
}