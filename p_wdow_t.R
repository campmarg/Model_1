#Widow Rockfish Trawl
tparam <- function() {
  M <- 0.14 # natural mortality rate
  Amax <- 54  # max age
  Ages <- 1:Amax
  #A_m <- 5.47
  Amat <- 5.47
  L_inf <- 50.34  # asymptotic maximum length
  k <- 0.15  # von Bertalanffy growth  # Growth rate constant
  c <- 0.000017355  # Biomass coefficient -> length-weight coefficient -> biomass is a function of length
  d <- 2.9617   # Biomass exponent -> length-weight exponent
  t0 <- -2.22
  
  
  Ages_mat = pnorm(Ages,Amat,sd=1) 
  
  
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
  b1 <- pnorm(size, mean=45.8061-2*4.63698,sd=4.63698) #correct selectivity 
  selectivity_t <- b1
  Af_t <- selectivity_t
  
  # Plotting examples
  # plot(Ages, Af_t)
  #  plot(Ages, lengthaa)
  #  plot(size, selectivity_t)
  
  # plot(lengthaa, selectivity_t, type = "l",
  #      xlab = "Length (cm)", ylab = "Selectivity",
  #      main = "Widow rockfish selectivity comm. trawl")
  
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
    selectivity_t = selectivity_t,
    Af_t = Af_t,
    LEP = LEP
  ))
}
