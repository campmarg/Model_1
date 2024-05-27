#Sablefish Fixed Gear
tparam <- function() {
  M <- 0.07 # natural mortality rate
  Amax <- 70  # max age
  Ages <- 1:Amax
  #A_m <- 6
  Amat <- 6
  L_inf <- 57  # asymptotic maximum length
  k <- 0.41  # von Bertalanffy growth  # Growth rate constant
  c <- 0.000003315  # Biomass coefficient -> length-weight coefficient -> biomass is a function of length
  d <- 3.27264   # Biomass exponent -> length-weight exponent
  L_0 <- 10.7
  t0 = log(1-L_0/L_inf)
  
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
  
  #FG
  b1 <- pnorm(Ages, mean=5-2*0.19,sd=0.19) 
  b2 <- pnorm(Ages, mean=5+2*2.84,sd=2.84) 
  
  
  selectivity_t <- b1*b2
  Af_t <- selectivity_t
  
  # Plotting examples
  # plot(Ages, Af_t)
  #  plot(Ages, lengthaa)
  #  plot(size, selectivity_t)
  # 
  # plot(Ages, selectivity_t, type = "l",
  #      xlab = "Ages", ylab = "Selectivity",
  #      main = "Sablfish selectivity comm. fg")
  
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

