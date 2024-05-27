#' Parameter Function
#'
#' This function sets and calculates the model parameters for a species.
#'
#' @return A list containing the gear model parameters.
#' @export
#'
#' @details
#' The function sets and calculates the  gear model parameters for a species.
#' It initializes and calculates various model parameters including \code{M} (natural mortality rate),
#' \code{Amax} (maximum age), \code{Ages} (age vector from 1 to \code{Amax}), \code{A_m} (age of maturity),
#' \code{Amat} (age at first maturity), \code{L_inf} (asymptotic maximum length), \code{k} (von Bertalanffy growth constant),
#' \code{c} (length-weight coefficient), \code{d} (length-weight exponent), \code{t0} (von Bertalanffy growth parameter),
#' \code{lengthaa} (length-at-age calculated using von Bertalanffy equation), \code{biomass} (biomass-at-age),
#' \code{eggsaa} (eggs-at-age assuming fecundity is proportional to biomass), \code{e} (eggs at age - fecundity proportional to biomass),
#' \code{size} (selectivity as a function of age), \code{b1} (selectivity function part 1),
#' \code{selectivity_t} (vector representing proportion of the population vulnerable at any given age to gear)
#'
#' @author Margaret Campbell
#' @keywords species parameters
#Yellowtail Rockfish Trawl
tparam <- function() {
  M <- 0.084375  # natural mortality rate
  Amax <- 64  # max age
  Ages <- 1:Amax
  A_m <- 9.5
  Amat <- 9.5
  L_inf <- 52.2  # asymptotic maximum length
  k <- 0.17  # von Bertalanffy growth  # Growth rate constant
  c <- 0.0287 # page62  # Biomass coefficient -> length-weight coefficient -> biomass is a function of length
  d <- 2.822  # page62  # Biomass exponent -> length-weight exponent
  Ages_mat = pnorm(Ages,Amat,sd=1) 
  L_0 <- 0.14
  t0 = log(1-L_0/L_inf)
  
  von_bertalanffy <- function(Ages, L_inf, k, t0) {
    L_t <- L_inf * (1 - exp(-k * (Ages - t0)))
    return(L_t)
  }
  
  lengthaa <- von_bertalanffy(Ages, L_inf, k, t0)  # Length-at-age (von Bert)
  biomass <- c * lengthaa^d  # Biomass-at-age - c*length at age to the d power B=c*L^d
  eggsaa <- biomass  # Eggs-at-age (assuming fecundity is proportional to biomass)
  e <- biomass  # Eggs at age - fecundity proportional to biomass # biomass-eggs coefficient
  eggsaa[Ages<=Amat] = 0
  e[Ages<=Amat] = 0
  
  # survival-to-age
  surv <- cumprod(c(1,rep(exp(-M),Amax-1)))
  LEP <- sum(surv*eggsaa)
  
  biomass_distro <- sum(biomass * surv)
  
  size <- lengthaa
  b1 <- pnorm(size, mean=49.319-2*4.293,sd=4.293)
  selectivity_t <- b1
  
  Af_t <- selectivity_t
  # Plotting examples
  # plot(Ages, Af_t)
  #  plot(Ages, lengthaa)
  #  plot(size, selectivity_t)
  
  # plot(lengthaa, selectivity_t, type = "l",
  #      xlab = "Length (cm)", ylab = "Selectivity",
  #      main = "Yellowtail rockfish selectivity comm. trawl")
  
  # Return a list of assigned variables
  return(list(
    M = M,
    Amax = Amax,
    Ages = Ages,
    A_m = A_m,
    Amat = Amat,
    L_inf = L_inf,
    k = k,
    c = c,
    d = d,
    t0 = t0,
    lengthaa = lengthaa,
    biomass = biomass,
    biomass_distro = biomass_distro,
    eggsaa = eggsaa,
    e = e,
    LEP = LEP,
    surv = surv,
    size = size,
    b1 = b1,
    selectivity_t = selectivity_t,
    Af_t = Af_t,
    LEP = LEP
  ))
}
