#lingcod Fixed Gear
tparam <- function() {
  M <- 0.418  # natural mortality rate (from stock assessment)
  Amax <- 25  # max age
  Ages <- 1:Amax
  A_m <- 3.23
  Amat <- 3.23
  
  Ages_mat = pnorm(Ages,Amat,sd=1) # will added this
  
  L_inf <- 103.645  # asymptotic maximum length
  k <- 0.193  # von Bertalanffy growth  # Growth rate constant
  c <- 0.000002802  # page62  # Biomass coefficient -> length-weight coefficient -> biomass is a function of length
  d <- 3.2766  # page62  # Biomass exponent -> length-weight exponent
  #t0 <- 0
  
  L_0 <- 11.99 #page34
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
  # b1 <- pnorm(size, 69-2*15,15)
  # b2 <- pnorm(size, 69+2*5.6,5.6)
  # selectivity_t <- b1*b2
  # Af_t <- selectivity_t
  
  b1 <- pnorm(size, mean=69.43-3*5.698,sd=5.698) #correct selectivity for fixed gear
  selectivity_t <- b1
  Af_t <- selectivity_t
  
  # Plotting examples
  # plot(Ages, Af_t)
  #  plot(Ages, lengthaa)
  # plot(size, selectivity_t)
  # plot(lengthaa, selectivity_t, type = "l", 
  #            xlab = "Length (cm)", ylab = "Selectivity", 
  #            main = "Length based selectivity for comm. fixed")
  
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
    # b2 = b2,
    selectivity_t = selectivity_t,
    Af_t = Af_t,
    LEP = LEP
  ))
}
