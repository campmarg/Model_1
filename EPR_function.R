#' EPR Function
#'
#' This function calculates the egg per recruit (EPR) based on the input parameters.
#'
#' @param Fs A numeric vector representing fishing mortality rates for different scenarios.
#' @param params A list containing model parameters including M, Amax, Ages, Linf, k, t0, c, d, A_c_t, Amat, eggsaa, and Af_t.
#' @return A numeric vector representing the egg per recruit (EPR) for each fishing scenario in \code{Fs}.
#' @export
#'
#' @author Margaret Campbell
#' @keywords egg per recruit (EPR) fishing mortality biomass
EPR_function <- function(Fs,params){

EPR <- rep(0,length=length(Fs)) # variable to store the results

M = params$M
Amax = params$Amax
Ages = params$Ages
Linf = params$L_inf
k = params$k
t0 = params$t0
c = params$c
d = params$d
A_c_t = params$A_c_t
Amat = params$Amat
eggsaa = params$eggsaa
Af_t = params$Af_t

# length-at-age
L = Linf * (1 - exp(-k*(Ages-t0))) # the von Bertalanffy formula for length at age

# biomass-age-age
B = c*L^d


for (f in 1:length(Fs)){

  
  # annual survival:
  Surv = exp(-(M + Af_t*Fs[f]))
  CumSurv = c(1,cumprod(Surv[1:(Amax-1)])) # cumulative survival to age, so we drop the last entry

  Eggs = CumSurv * eggsaa
  EPR[f] = sum(Eggs)
  
}
return(EPR)
}