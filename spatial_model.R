#' Spatial Model Function
#'
#' This function implements the spatial model using input the parameters and habitat means created in the other functions.
#'
#' @param params A list containing model parameters created by fgparam or tparam.
#' @param Tmax An integer representing the maximum time (number of time steps) for the simulation.
#' @param X A numeric vector representing the spatial locations - the coastline.
#' @param hab_means A numeric vector representing habitat means for 3 different spatial locations.
#' @param MLing An array representing the Leslie matrix for each fishing scenario and spatial location.
#' @param HR A matrix representing home range movement values.
#' @param f A numeric value representing fishing mortality.
#' @param a A numeric value representing the density-dependent recruitment parameter.
#' @param CV a numeric representing the sampling CV of biomass
#' @return A list containing the results of the spatial model simulation including N_values (settlement), Y_values (yield), and CPUE (catch per unit effort).
#' @export
#'
#' @author Margaret Campbell
#' @keywords spatial model population dynamics fishing mortality recruitment
spatial_model <- function(Tmax, X, hab_means, HR, f, a, params) {
  
  z<-0
  Amax <- params$Amax
  M <- params$M 
  CV <- params$CV 
  LEP <- params$LEP 
  percent <- 0
  
  # Matrixes/arrays for the model - initial conditions: stable age distribution (Still need to do this)
  
  N <- array(0, dim = c(Amax, length(X), Tmax)) # Amax rows, one T, one space
  Y <- matrix(1, nrow = length(X), ncol = Tmax) 
  B <- matrix(1, nrow = length(X), ncol = Tmax) 
  R <- matrix(0, nrow = length(X), ncol = Tmax)
  S <- array(0, dim = c(length(X), Tmax))
  N[,,1] <- 1 # time = 1 to start
  
  Bact <- array(0, dim = c(length(X), Tmax,length(hab_means)))
  b_values <- vector("list", length(hab_means))
  N_values <- array(0, dim = c(Amax, length(X), Tmax, length(hab_means)))
  Y_values <- array(0, dim = c(length(X), Tmax, length(hab_means)))  
  H_values <- array(0, dim = c(length(X), Tmax, length(hab_means)))
  Bobsmatrix <- array(0, dim = c(Tmax, length(hab_means)))
  
  CPUE <- matrix(1, nrow = length(X), ncol = Tmax)  
  CPUE_sampled <- matrix(0, nrow = length(X), ncol = Tmax) # Added for sampled CPUE
  
  Fvec <- matrix(0, nrow = length(X), ncol = Tmax) 
  Y_eff = rep(0,length(X))
  
  
  # now run the actual model
  
  x <- 1:length(X)
  for (i in 1:length(hab_means)) {
    
    hab_i <- dnorm(1:length(X), mean = as.numeric(hab_means[i]), sd = 0.25 * length(X)) # may not want to hard-code the 0.25 here
    hab_i <- hab_i / sum(hab_i)
    
    b <- numeric(length(X))
    b <- hab_i 
    
    b_values[[i]] <- b
    
    Rt = rep(1, length(X))

    # calculate unfished biomass 
    for (t in 1:Tmax){ #changed this to TMax?
      Et = Rt * LEP
      St = D %*% as.matrix(Et)
      Rt = a * St / (1 + (a / b) * St)
    }
    B0 = mean(Rt * sum(params$biomass_distro))
    # end calculating unfished biomass

    Xtmp = rep(0,length.out=length(X)) # get a temporary habitat vector for pre-OWF part of the simulation
    #Bact[,i,] = B
    
    for (t in 2:Tmax) {
      #for (t in 2:20) {
      if (t > (Tmax-20)) {
 
        # Include the OWF once Tmax exceeds 100
        Xtmp = X
      } else {
        # Exclude the OWF before Tmax reaches 100 
        # do nothing, Xtmp remains as set prior to the time loop
      }
      # Calculate observed biomass (Bobs) relative to unfished biomass (B), sampling at particular locations
      Locs = round(c(0.05, 0.09, 0.25, 0.4, 0.5, 0.6, 0.7, 0.85, 0.95, 0.99) * length(X)) # sampling locations. Could be something you specify elsewhere
      #Locs = round(c(0.05, 0.5, 0.95) * length(X)) # sampling locations. Could be something you specify elsewhere
      LocsOK = Xtmp[Locs] # value of X for each sampling location
      
      #Bact[,i,] = B
      Bsamp = B[Locs,t-1] 
      
      Bsamp = rnorm(mean = Bsamp, sd = Bsamp * CV, n = length(Locs))
      Bobs = mean(Bsamp[!LocsOK]) # take the mean, excluding samples in the wind farm
      Bobsmatrix[t,i] <- Bobs
      Bobs_ratio = min(1,Bobs / B0)
      
      # Define control update interval
      control_update_interval <- 4
      # Define a variable to keep track of the control rule update iteration
      control_update_iter <- 0
      
      # Simulate sampling
      if (t %% control_update_interval == 0 | t == 2) {  # Control rule every year
        percent = Bobs_ratio
      }
      
      if (t %% 1 == 0) {
        if (percent > 0.4) {
          SPR <- 0.5
          harvest_rate <- f #* SPR * mean(Bobs) # Incorporate mean Bobs/B into the control rule
          CanIFish <- "Of Course"
          # Print the calculated harvest rate and other relevant variables
          #print(paste("t =", t, ", percent =", percent, ", control_update_iter =", control_update_iter, ", harvest_rate =", harvest_rate))
          # Update the control rule iteration counter
          control_update_iter <- control_update_iter + 1
        } else if (percent >= 0.1 && percent <= 0.4) {  # Ramp down when percent is between 0 and 0.4
          harvest_rate <- f/0.3 * percent - f/3
          CanIFish <- "Ramp Down"
          # Print the calculated harvest rate and other relevant variables
          #print(paste("t =", t, ", percent =", percent, ", control_update_iter =", control_update_iter, ", harvest_rate =", harvest_rate))
        } 
        else {  # Survey sampling every year
          harvest_rate <- 0
          CanIFish <- "No Fishing"
        }
      }
      
      # Calculate scaled CPUE
      scaled_CPUE <- CPUE[, t-1] / sum(CPUE[, t-1],na.rm=TRUE)
      scaled_CPUE[is.nan(scaled_CPUE)] <- 0.0000000000
      # Calculate total effort
      total_effort <- sum(Fvec[,t-1])
      # Distribute effort proportionally based on scaled CPUE
      #### Updated to ensure no fishing in the OWF, once that is imposed:
      scaled_CPUE = scaled_CPUE*(1-Xtmp)
      sampled_effort <- total_effort * scaled_CPUE
      # Update CPUE_sampled with sampled effort
      CPUE_sampled[, t] <- sampled_effort
      f0 <- is.na(CPUE_sampled[,t])
      CPUE_sampled[f0,t] = 0
      
      
      if (all(CPUE_sampled[,t]==0)){
        CPUE_sampled[,t] <- 1-Xtmp} # if there had been no fishing and thus no yield then just give a default distribution of CPUE for calculating ftmp
      
      f_undist <- CPUE_sampled[,t]  * harvest_rate
      
      ftmp <- as.matrix(HR) %*% as.matrix(CPUE_sampled[,t])
      ftmp = ftmp/max(ftmp) # scale so it ranges from zero to 1
      ftmp = ftmp*harvest_rate # from the control rule
      
      
      Fvec[,t] = ftmp
      
      for (x in 1:length(X)) {
        
        # Recalculate the Leslie matrix for each spatial cell here
        MLing <- leslie_matrix(params = tparam(), f = ftmp[x])  # Pass appropriate arguments
        
        N[, x, t] <- as.matrix(MLing) %*% as.matrix(N[, x, t - 1])
        
      }
      
      Dead <- N[1:(Amax-1) , , t-1 ] - N[2:Amax, , t]
      Dead[Dead<0] = 0
      #print(sum(Dead))
      for (x in 1:length(X)) {
        Y[x, t] <- sum(Dead[, x] * (ftmp[x] * (params$Af_t[1:(Amax-1)])) / (M+(ftmp[x] * (params$Af_t[1:(Amax-1)])))) # use ftmp, the distributed fishing effort
        B[x, t] <- sum(N[,x,t] * params$biomass)
      }   
      
      for (x in 1:length(X)) {
        if (harvest_rate == 0) {
          Y_eff[x] = 0
          z = z + 1
        } else {
          Y_eff[x] <- sum(Y[, t] * HR[, x] * f_undist[x] / sum(f_undist * HR[, x]))
        }
      }
      for (x in 1:length(X)) {
        if (is.nan(Y_eff[x])) {
          Y_eff[x] = 0
        }
      }
      
      
      Y[,t] = Y_eff
      
      for (x in 1:length(X)) {
        Y_values[x,t,i] <- Y[x,t]# Save the current Y values for the i-th hab_mean iteration
        H_values[x,t,i] <- harvest_rate
      }

      CPUE[, t] <- Y[,t] / Fvec[,t] # CPUE Y/F in each location
      CPUE[is.infinite(CPUE[,t]),t] = NA
      
      S[,t] <- D %*% N[1, , t]
      # print(N[1,1:5,t])
      # print(S[1:5,t])
      # density-dependent recruitment
      N[1,,t] <- a * S[, t] / (1 + (a / b) * S[, t])  #changed ,t to 1
      
      # Save the current N values for the i-th hab_mean iteration
      N_values[, , t, i] <- N[, , t]
    }
    
    
    for (timee in 1:Tmax) { #loop over time instead
      Bact[,timee,i] = B[,timee]
    }
    
  }
  
  
  
  Result <- list(N_values = N_values, Y_values = Y_values, CPUE = CPUE, CPUE_sampled = CPUE_sampled, H_values = H_values,CV = CV, Bobsmatrix = Bobsmatrix, Bact = Bact) # Added CPUE_sampled to the result
  return(Result)
}
