###For rockfish + sablefish
#Model 1
library(ggplot2)
library(devtools)
install_local("mypackage",force=TRUE) 
library(mypackage)
library(matlib)
library(dplyr)
library(gridExtra)
library(RColorBrewer)

Mu = 0
Sig = 10
CRT = 0.2
Rw_R1 = 1
Fr_R1 = 0.1

LEP_target = 0.5 # desired level of LEP in fished area (50% would be the usual groundfish target) yes 0.5 should be the SPR target (for our purposes LEP = SPR)

X = coastline_function(Mu = Mu, Sig = Sig, Rw_R1 = Rw_R1, Fr_R1 = Fr_R1)

D = dispersal_function(X=X, Mu=Mu, Sig=Sig) # create gaussian dispersal matrix
HR = home_range(X=X,Mu=0) # 2km homerange movement

#Parameters to simulate recruit distribution inside an OWF
T <- 400;
Tmax = T+20
R <- matrix(1, nrow = length(X), ncol = T)
params <- tparam()

Fs = seq(from=0,to=0.5,length.out=T)

EPR <- EPR_function(Fs,params)
EPR_frac <- EPR / EPR[1] 

# Translate YPR into F for simulations 
indices <- which(EPR_frac >= LEP_target) 
chosenIndex <- max(indices)  # Choose the first index (can be adjusted as per your requirements)
f <- Fs[chosenIndex]

a <- 1 / (0.15*max(EPR))

hab_means <- quantile(1:length(X),probs = c(0.05,0.3,0.55))

#Leslie Matrix
MLing <- leslie_matrix(params,f)

Amax=getElement(params, 'Amax')
A_c_t=getElement(params, 'A_c_t')
M=getElement(params, 'M')
LEP=getElement(params, 'LEP')

# error rate for survey sampling
params$CV = 0.02
percent = 0
SPR = 0.5

loops<- 100

#Run the model once
Result = spatial_model(Tmax,X,hab_means,HR,f,a,params)
RN_values <- array(0, dim = c(dim(Result$N_values),loops))
RH_values <- array(0, dim = c(dim(Result$H_values),loops))
RY_values <- array(0, dim = c(dim(Result$Y_values),loops))
RY_valuesAVG <- array(0, dim = c(dim(Result$Y_values)))
Robs_values <- array(0, dim = c(dim(Result$Bobsmatrix),loops))
RBact_values <- array(0, dim = c(dim(Result$Bact),loops))


#Run the model 100 times for each time step 100
j<-1
RY_values[ , , ,j] <- Result$Y_values
RH_values[ , , , j] <- Result$H_values
RN_values[, , , ,j] <- Result$N_values
Robs_values[ , ,j] <- Result$Bobsmatrix
RBact_values[ , , ,j] <- Result$Bact
for (j in 2:loops) {
  Result = spatial_model(Tmax,X,hab_means,HR,f,a,params)
  
  RY_values[, , ,j]<-Result$Y_values
  RH_values[, , , j]<-Result$H_values
  RN_values[, , , ,j] <- Result$N_values
  Robs_values[ , ,j] <- Result$Bobsmatrix
  RBact_values[ , , ,j]<-Result$Bact
}

######Plotting#####
#Plot Y Values - 380-420 as is
par(mfrow = c(1, 3))  # Set up a 1x3 grid for the plots
y_min <- 0
y_max <- 0.5

RY_valuesAVG <- array(0, dim = (c(100,Tmax,3)))
#make loop of averaging 100 loops 
for (col in 1:3) {
  for (Space in 1:100) {
    for (Row in 1:Tmax) {
      RY_valuesAVG[Space,Row,col] <- mean(RY_values[Space,Row,col,])
    }
  }
}

#Average all values in space into vector RY_valuesAVG2
RY_valuesAVG2 <- array(0,dim = c(Tmax,3))
for (col in 1:3) {
  for (time in (Tmax-40):Tmax) {
    RY_valuesAVG2[time,col] <- mean(RY_valuesAVG[,time,col]) 
  }
}

x_seq <- seq((Tmax-40), Tmax, by = 1)


#biomass overtime
#Biomass distribution - this is 64 for ytrk
RN_valuesAVG <- array(0, dim = c(64,100,Tmax,3))
RN_valuesAVG1 <- array(0, dim = c(64,100,3))
RN_valuesAVG1before <- array(0, dim = c(64,100,3))
RN_valuesAVG1after <- array(0, dim = c(64,100,3))
BiomassRun<- array(0, dim = c(64,41,3))

for (col in 1:3) {
  for (Space in 1:100) {
    for (Row in (Tmax-40):Tmax) {
      for (Age in 1:64) {
        RN_valuesAVG[Age,Space,Row,col] <- mean(RN_values[Age,Space,Row,col,])
      }
    }
  }
}
#after
for (col in 1:3) {
  for (Space in 1:100) {
    for (Age in 1:64) {
      RN_valuesAVG1after[Age,Space,col] <- mean(RN_valuesAVG[Age,Space,(Tmax-19):Tmax,col])*params$biomass[Age]
    }
  }
}
#before
for (col in 1:3) {
  for (Space in 1:100) {
    for (Age in 1:64) {
      RN_valuesAVG1before[Age,Space,col] <- mean(RN_valuesAVG[Age,Space,(Tmax-40):(Tmax-20),col])*params$biomass[Age]
    }
  }
}
#Run
for (col in 1:3) {
  for (year in (Tmax-40):(Tmax)) {
    for (Age in 1:64) {
      BiomassRun[Age,year-(Tmax-41),col] <- mean(RN_valuesAVG[Age,,year,col])*params$biomass[Age]
    }
  }
}

#Biomass Observed from Sampled
#mean of 100 iterations over tmax iterations
RBobsmean <- array(dim = c(41, 3))
for (col in 1:3) {
  for (year in (Tmax-40):(Tmax)) {
    RBobsmean[year-(Tmax-41),col] <- mean(Robs_values[year,col,])
  }
}


RBactmean <- array(dim = c(100, 41, 3))
RBupper_quantile <- array(dim = c(100, 41, 3))
RBlower_quantile <- array(dim = c(100, 41, 3))

for (col in 1:3) {
  for (year in (Tmax-40):Tmax) {
    for (space in 1:length(X)) {
      RBactmean[space, year-Tmax+41, col] <- mean(RBact_values[space, year, col, ])
      RBupper_quantile[space,year-Tmax+41,col] <- quantile(RBact_values[space, year, col, ],0.75)
      RBlower_quantile[space,year-Tmax+41,col] <- quantile(RBact_values[space, year, col, ],0.25)
    }
  }
}

RBactmeannnn <- array(dim = c(41, 3))
RBupper_quantileeee <- array(dim = c(41, 3))
RBlower_quantileeee <- array(dim = c(41, 3))


for (col in 1:3) {
  for (year in 1:41) {
    RBactmeannnn[year, col] <- mean(RBactmean[ , year, col])
    RBupper_quantileeee[year,col] <- mean(RBupper_quantile[, year, col])
    RBlower_quantileeee[year,col] <- mean(RBlower_quantile[, year, col])
    
  }
}


color_blind_friendly <- brewer.pal(9, "Set1")[4]
par(mfrow = c(3, 1))

for (col in 1:3) {

  # Create a data frame for the current scenario
  
  df <- data.frame(
    Time = 1:41,  # Assuming time is from 1 to 41
    Data = RBactmeannnn[,col]/RBactmeannnn[20,col],
    UpperQuantile = RBupper_quantileeee[,col]/RBactmeannnn[20,col],
    LowerQuantile = RBlower_quantileeee[,col]/RBactmeannnn[20,col],
    RBobsmean = RBobsmean[, col] / RBobsmean[20, col]
  )
  # Open an EPS file for each scenario
  postscript(paste("avgbiomassytrkCV0.2S_", col, ".eps"), width = 6, height = 6)
  
  # Plot the normalized data for the current scenario
  plot(
    df$Time, df$Data, type = 'l',
    main = paste("Scenario", col),
    xlab = "Time", ylab = "Normalized Average Biomass", ylim = c(0, 3.5), col = "black"
  )
  
  # Plot the normalized upper quantile line in red
  lines(df$Time, df$UpperQuantile, col = "red", lty = 2)
  
  # Plot the normalized lower quantile line in blue
  lines(df$Time, df$LowerQuantile, col = "blue", lty = 2)
  
  # Plot Biomass Observed data in a color-blind friendly color and different shape
  lines(df$Time, df$RBobsmean, col = color_blind_friendly, lty = 4)
  
  # Add a straight line going up at time = 100
  abline(v = 20, col = "black", lty = 4)  # Assuming time 100 corresponds to the 20th year
  # Close the EPS file
  dev.off()
}



#Normalized Yield
RYmean <- array(dim = c(100, 41, 3))
RYupper_quantile <- array(dim = c(100, 41, 3))
RYlower_quantile <- array(dim = c(100, 41, 3))

for (col in 1:3) {
  for (year in (Tmax-40):Tmax) {
    for (space in 1:length(X)) {
      RYmean[space, year-Tmax+41, col] <- mean(RY_values[space, year, col, ])
      RYupper_quantile[space,year-Tmax+41,col] <- quantile(RY_values[space, year, col, ],0.75)
      RYlower_quantile[space,year-Tmax+41,col] <- quantile(RY_values[space, year, col, ],0.25)
    }
  }
}

RYmeannn <- array(dim = c(41, 3))
RYupper_quantileee <- array(dim = c(41, 3))
RYlower_quantileee <- array(dim = c(41, 3))


for (col in 1:3) {
  for (year in 1:41) {
    RYmeannn[year, col] <- mean(RYmean[ , year, col])
    RYupper_quantileee[year,col] <- mean(RYupper_quantile[, year, col])
    RYlower_quantileee[year,col] <- mean(RYlower_quantile[, year, col])
    
  }
}
par(mfrow = c(3, 1))
for (col in 1:3) {
  
  # Create a data frame for the current scenario
  
  df <- data.frame(
    Time = 1:41,  # Assuming time is from 1 to 41
    Data = RYmeannn[,col]/RYmeannn[20,col],
    UpperQuantile = RYupper_quantileee[,col]/RYmeannn[20,col],
    LowerQuantile = RYlower_quantileee[,col]/RYmeannn[20,col]
  )

  # Open an EPS file for each scenario
  postscript(paste("normyieldytrkCV0.2S_", col, ".eps"), width = 6, height = 6)

  # Plot the normalized data for the current scenario
  plot(df$Time, df$Data, type = 'l', main = paste("Scenario", col), xlab = "Time", ylab = "Normalized Average Yield", ylim = c(0, 2.5), col = "black")

  # Plot the normalized upper quantile line in red
  lines(df$Time, df$UpperQuantile, col = "red", lty = 2)

  # Plot the normalized lower quantile line in blue
  lines(df$Time, df$LowerQuantile, col = "blue", lty = 2)

  # Add a straight line going up at time = 400
  abline(v = 20, col = "black", lty = 2)

  # Close the EPS file
  dev.off()
}

# Harvest Plotting total
RH_valuesAVG <- array(0, dim = (c(100, 420, 3, 3)))
RH_BaselineAVG_tmp <- array(0, dim = (c(100, 20, 3)))
RH_BaselineAVG <- array(0, dim = (c(100, 3)))

RH_BaselineLQ_tmp <- array(0, dim = (c(100, 20, 3)))
RH_BaselineLQ <- array(0, dim = (c(100, 3)))

RH_BaselineUQ_tmp <- array(0, dim = (c(100, 20, 3)))
RH_BaselineUQ <- array(0, dim = (c(100, 3)))

for (col in 1:3) {
  for (Space in 1:100) {
    for (Row in 380:399) {
      RH_BaselineAVG_tmp[Space, Row-379, col]<- mean(RH_values[Space,Row,col,])
      RH_BaselineLQ_tmp [Space, Row-379, col]<- quantile(RH_values[Space,Row,col,], 0.25)
      RH_BaselineUQ_tmp [Space, Row-379, col]<- quantile(RH_values[Space,Row,col,], 0.75)
    }
  }
}

for (col in 1:3) {
  for (Space in 1:100) {
    RH_BaselineAVG[Space, col]<- mean(RH_BaselineAVG_tmp[Space, ,col])
    RH_BaselineLQ [Space, col]<- mean(RH_BaselineLQ_tmp[Space, ,col])
    RH_BaselineUQ [Space,  col]<- mean(RH_BaselineUQ_tmp[Space, ,col])
  }
}


# Make a loop to calculate mean, upper quartile, and lower quartile for each run
for (col in 1:3) {
  for (Space in 1:100) {
    for (Row in 1:420) {
      # Calculate mean, upper quartile, and lower quartile
      data <- RH_values[Space, Row, col, ]
      RH_valuesAVG[Space, Row, col, 1] <- mean(data)/RH_BaselineAVG[Space, col]
      RH_valuesAVG[Space, Row, col, 2] <- quantile(data, 0.25)/RH_BaselineLQ [Space, col]  # Lower quartile
      RH_valuesAVG[Space, Row, col, 3] <- quantile(data, 0.75)/RH_BaselineUQ [Space,col]  # Upper quartile
    }
  }
}

par(mfrow = c(3, 1))

for (col in 1:3) {
  # Extract the data for the current column and rows 380 to 420
  mean_data <- RH_valuesAVG[1, 380:420, col, 1]
  lower_quartile_data <- RH_valuesAVG[1, 380:420, col, 2]
  upper_quartile_data <- RH_valuesAVG[1, 380:420, col, 3]
  
  # Save the plot as an EPS file with the name "HS_col.eps" (e.g., "HS_1.eps", "HS_2.eps", "HS_3.eps")
  postscript(paste("HarvestytrkCV0.2S_", col, ".eps"), width = 6, height = 6)
  
  # Plot the mean data as lines for all 400 runs with custom x-axis labels and common y-axis range
  matplot(380:420, mean_data, type = 'l', xlab = "Time", ylab = "Harvest rate (relative to F50%)", ylim = c(0.6, 1.1), col = "black")
  
  # Add lines for the upper and lower quartiles
  lines(380:420, lower_quartile_data, col = "blue")
  lines(380:420, upper_quartile_data, col = "red")
  
  abline(v = 400, col = "black", lty = 2)
  # Close the EPS file
  dev.off()
}


############ New Section 


RBactmean2 <- array(0,dim = c(41,3))
RBact_before <- array(0,dim = c(length(X),41,3))

for (col in 1:3) {
for (year in 1:41) {
  RBactmean2[year,col] <- mean(RBactmean[,year,col])
}
}

for (col in 1:3) {
  for (year in 1:41) {
    for (space in 1:length(X)){
    RBact_before[space,year,col] <- RBactmean[space,year,col]/RBactmean2[year,col]
  }
  }
}


plot_data <- data.frame()

for (scenario_num in 1:3) {
  # Extract data for the current scenario
  scenario_data1 <- t(RBact_before[,1:20 , scenario_num])
  
  # Calculate the mean age over space
  mean_age1 <- colMeans(scenario_data1)
  
  # Calculate the lower quartile of age over space
  lower_quantile <- 0.25
  lower_quartile <- apply(scenario_data1, 2, quantile, probs = lower_quantile)
  
  # Calculate the upper quartile of age over space
  upper_quantile <- 0.75
  upper_quartile <- apply(scenario_data1, 2, quantile, probs = upper_quantile)
  
  # Create a data frame for the current scenario
  scenario_df1 <- data.frame(
    Space = 1:100,
    Scenario = factor(scenario_num),
    MeanAge = mean_age1,
    LowerQuantile = lower_quartile,
    UpperQuantile = upper_quartile
  )
  
  # Append data for the current scenario to the plot_data data frame
  plot_data <- rbind(plot_data, scenario_df1)
}

# Create the graph with white background and no grid lines
#plot_filenames <- c()
postscript(paste("BOWFdvertCV0.02_all", ".eps"), width = 6, height = 6)
ggplot(plot_data, aes(x = Space, y = MeanAge, color = Scenario, linetype = Scenario)) +
  geom_line(size = 1, aes(group = Scenario)) +
  geom_ribbon(aes(ymin = LowerQuantile, ymax = UpperQuantile, fill = Scenario), alpha = 0.3) +
  labs(main = "Before OWF", x = "Coastline", y = "Biomass Distribution", color = "Scenario", linetype = "Scenario") +
  scale_color_manual(values = c("orange", "black", "blue")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_fill_manual(values = c("orange", "black", "blue")) +
  scale_y_continuous(limits = c(0, 4)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove grid lines
        panel.background = element_rect(fill = "white"))  # Set background color to white
dev.off()


#after - normalized
# Loop through each scenario (1 to 3)
plot_data1 <- data.frame()
mean_age <- numeric(100) 
for (scenario_num in 1:3) {
  # Extract data for the current scenario
  scenario_data <- t(RBact_before[, 21:41, scenario_num])
  
  # Calculate the mean age over space
  for (space in 1:100) {
    mean_age[space] <- mean(RBact_before[space,21:41,scenario_num])
  }
  
  
  # Calculate the lower quartile of age over space
  lower_quantile <- 0.25  # Define the lower quantile here (e.g., 0.25 for the lower quartile)
  lower_quartile <- apply(scenario_data, 2, quantile, probs = lower_quantile)
  
  # Calculate the upper quartile of age over space
  upper_quantile <- 0.75  # Define the upper quantile here (e.g., 0.75 for the upper quartile)
  upper_quartile <- apply(scenario_data, 2, quantile, probs = upper_quantile)
  
  # Create a data frame for the current scenario
  scenario_df <- data.frame(
    Space = 1:100,
    Scenario = factor(scenario_num),
    MeanAge = mean_age,
    LowerQuantile = lower_quartile,
    UpperQuantile = upper_quartile
  )
  
  # Append data for the current scenario to the plot_data data frame
  plot_data1 <- rbind(plot_data1, scenario_df)
}

# Create the graph with white background and no grid lines
#plot_filenames <- c()
postscript(paste("AOWFdvertCV0.02_all", ".eps"), width = 6, height = 6)
ggplot(plot_data1, aes(x = Space, y = MeanAge, color = Scenario, linetype = Scenario)) +
  geom_line(size = 1, aes(group = Scenario)) +
  geom_ribbon(aes(ymin = LowerQuantile, ymax = UpperQuantile, fill = Scenario), alpha = 0.3) +
  labs(x = "Coastline", y = "", color = "Scenario", linetype = "Scenario") +
  scale_color_manual(values = c("orange", "black", "blue")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_fill_manual(values = c("orange", "black", "blue")) +
  scale_y_continuous(limits = c(0, 4)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove grid lines
        panel.background = element_rect(fill = "white"))
dev.off()




#######old b/a owf ###########
#before and after graphs
#####Before OWF #####
# Initialize an empty data frame to store the data
plot_data <- data.frame()

for (scenario_num in 1:3) {
  # Extract data for the current scenario
  scenario_data1 <- RN_valuesAVG1before[, , scenario_num]
  
  # Calculate the mean age over space
  mean_age1 <- colMeans(scenario_data1)
  
  # Calculate the lower quartile of age over space
  lower_quantile <- 0.25
  lower_quartile <- apply(scenario_data1, 2, quantile, probs = lower_quantile)
  
  # Calculate the upper quartile of age over space
  upper_quantile <- 0.75
  upper_quartile <- apply(scenario_data1, 2, quantile, probs = upper_quantile)
  
  # Create a data frame for the current scenario
  scenario_df1 <- data.frame(
    Space = 1:100,
    Scenario = factor(scenario_num),
    MeanAge = mean_age1,
    LowerQuantile = lower_quartile,
    UpperQuantile = upper_quartile
  )
  
  # Append data for the current scenario to the plot_data data frame
  plot_data <- rbind(plot_data, scenario_df1)
}

# Create the graph with white background and no grid lines
#plot_filenames <- c()
postscript(paste("BOWFytrkCV0.2_all", ".eps"), width = 6, height = 6)
ggplot(plot_data, aes(x = Space, y = MeanAge, color = Scenario, linetype = Scenario)) +
  geom_line(size = 1, aes(group = Scenario)) +
  geom_ribbon(aes(ymin = LowerQuantile, ymax = UpperQuantile, fill = Scenario), alpha = 0.3) +
  labs(main = "Before OWF", x = "Coastline", y = "Biomass Distribution", color = "Scenario", linetype = "Scenario") +
  scale_color_manual(values = c("orange", "black", "blue")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_fill_manual(values = c("orange", "black", "blue")) +
  scale_y_continuous(limits = c(0, 5)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove grid lines
        panel.background = element_rect(fill = "white"))  # Set background color to white
dev.off()


#after - normalized
# Loop through each scenario (1 to 3)
plot_data1 <- data.frame()
mean_age <- numeric(100) 
for (scenario_num in 1:3) {
  # Extract data for the current scenario
  scenario_data <- RN_valuesAVG1after[, , scenario_num]
  
  # Calculate the mean age over space
  for (space in 1:100) {
    mean_age[space] <- mean(RN_valuesAVG1after[,space,scenario_num])
  }
  
  
  # Calculate the lower quartile of age over space
  lower_quantile <- 0.25  # Define the lower quantile here (e.g., 0.25 for the lower quartile)
  lower_quartile <- apply(scenario_data, 2, quantile, probs = lower_quantile)
  
  # Calculate the upper quartile of age over space
  upper_quantile <- 0.75  # Define the upper quantile here (e.g., 0.75 for the upper quartile)
  upper_quartile <- apply(scenario_data, 2, quantile, probs = upper_quantile)
  
  # Create a data frame for the current scenario
  scenario_df <- data.frame(
    Space = 1:100,
    Scenario = factor(scenario_num),
    MeanAge = mean_age,
    LowerQuantile = lower_quartile,
    UpperQuantile = upper_quartile
  )
  
  # Append data for the current scenario to the plot_data data frame
  plot_data1 <- rbind(plot_data1, scenario_df)
}

#MARGARET YOU CHANGED TO BE AVG1 INSTEAD OF AVG2
# Create the graph with white background and no grid lines
#plot_filenames <- c()
postscript(paste("AOWFytrkCV0.2_all", ".eps"), width = 6, height = 6)
ggplot(plot_data1, aes(x = Space, y = MeanAge, color = Scenario, linetype = Scenario)) +
  geom_line(size = 1, aes(group = Scenario)) +
  geom_ribbon(aes(ymin = LowerQuantile, ymax = UpperQuantile, fill = Scenario), alpha = 0.3) +
  labs(x = "Coastline", y = "Biomass Distribution", color = "Scenario", linetype = "Scenario") +
  scale_color_manual(values = c("orange", "black", "blue")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_fill_manual(values = c("orange", "black", "blue")) +
  scale_y_continuous(limits = c(0, 5)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove grid lines
        panel.background = element_rect(fill = "white"))
dev.off()



