#Doversole Trawl
tparam <- function() {
  M <- 0.117 # natural mortality rate
  Amax <- 50  # max age
  Ages <- 1:Amax
  Amat <- 7.5
  L_inf <- 48.5  # asymptotic maximum length
  k <- 0.117  # von Bertalanffy growth  # Growth rate constant
  c <- 0.00000297  # Biomass coefficient -> length-weight coefficient -> biomass is a function of length
  d <- 3.33  # Biomass exponent -> length-weight exponent
  
  Ages_mat = pnorm(Ages,Amat,sd=1) 
  L_0 <- 2.33 
  t0 = log(1-L_0/L_inf)
  
  von_bertalanffy <- function(Ages, L_inf, k, t0) {
    L_t <- L_inf * (1 - exp(-k * (Ages - t0)))
    return(L_t)
  }
  
  lengthaa <- von_bertalanffy(Ages, L_inf, k, t0)  # Length-at-age (von Bert)
  biomass <- c * lengthaa^d  # Biomass-at-age - c*length at age to the d power B=c*L^d
  eggsaa <- biomass * Ages_mat  # Eggs-at-age (assuming fecundity is proportional to biomass)
  e <- biomass  # Eggs at age - fecundity proportional to biomass # biomass-eggs coefficient
  
  # survival-to-age
  surv <- cumprod(c(1,rep(exp(-M),Amax-1)))
  LEP <- sum(surv*eggsaa)
  
  biomass_distro <- sum(biomass * surv)
  
  size <- lengthaa
  
  b1 <- pnorm(size, 36.462 - 2 * 9.977, 9.977)  # descend 37, 3, 6
  b2 <- pnorm(size, 36.462 + 2 * 3.183, 3.183)  # ascend
  
  selectivity_t <- b1 * b2
  
  Af_t <- selectivity_t
  
  # Plotting examples
  # plot(Ages, Af_t)
  #  plot(Ages, lengthaa)
  #  plot(size, selectivity_t)
  # plot(lengthaa, selectivity_t, type = "l",
  #      xlab = "Length (cm)", ylab = "Selectivity",
  #     main = "Dover sole selectivity comm. trawl")
  
  # Return a list of assigned variables
  return(list(
    M = M,
    Amax = Amax,
    Ages = Ages,
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
    b2 = b2,
    selectivity_t = selectivity_t,
    Af_t = Af_t,
    LEP = LEP
  ))
}
