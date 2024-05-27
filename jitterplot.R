# Load necessary library
library(ggplot2)
library(reshape2)
library(tidyr)
library(gridExtra)
library(dplyr)
#lcod, dver, ytrk, wdow, sabl
#CV 0.2
# Create the data frame
biomass_datanew <- data.frame(
  Species = c("Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish", "Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish", "Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish"),
  xinf = c(1.4, 1.2, 0,  1.2, 0, 1.2, 1.2, 1.12, 1.05, 0, 1.05, 0, 1.05, 1.07, 1, 1, 0, 1, 0, 1, 1),
  #yinf = c(0.5,0.75,1,1.25,1.5,1.75,2,3,3.25,3.5,3.75,4,4.25,4.5,5.5,5.75,6,6.25,6.5,6.75,7),
  stringsAsFactors = FALSE
)
avg20biomass_datanew <- data.frame(
  Species = c("Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish", "Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish", "Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish"),
  xinf = c(1.2, 1.1, 0,  1.1, 0, 1.1, 1.1, 1.06, 1.025, 0, 1.025, 0, 1.025, 1.035, 1, 1, 0, 1, 0, 1, 1),
  #yinf = c(0.5,0.75,1,1.25,1.5,1.75,2,3,3.25,3.5,3.75,4,4.25,4.5,5.5,5.75,6,6.25,6.5,6.75,7),
  stringsAsFactors = FALSE
)

f50_datanew <- data.frame(
  Species = c("Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish", "Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish", "Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish"),
  xinf = c(0.75, 0.92, 0, 0.95, 0, 0.835, 1, 0.99, 1, 0, 1, 0, 1, 1,1, 1, 1, 1, 1, 1, 1),
  #yinf = c(0.5,0.75,1,1.25,1.5,1.75,2,3,3.25,3.5,3.75,4,4.25,4.5,5.5,5.75,6,6.25,6.5,6.75,7),
  stringsAsFactors = FALSE
)
avg20f50_datanew <- data.frame(
  Species = c("Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish", "Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish", "Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish"),
  xinf = c(0.875, 0.96, 0, 0.975, 0, 0.9175, 1, 0.99, 1, 0, 1, 0, 1, 1,1, 1, 1, 1, 1, 1, 1),
  #yinf = c(0.5,0.75,1,1.25,1.5,1.75,2,3,3.25,3.5,3.75,4,4.25,4.5,5.5,5.75,6,6.25,6.5,6.75,7),
  stringsAsFactors = FALSE
)

yield_datanew <- data.frame(
  Species = c("Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish", "Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish", "Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish"),
  xinf = c(0.7, 0.7, 0, 0.67, 0, 0.77, 0.75,0.9, 0.95, 0, 0.9, 0, 0.94, 0.9, 0.99, 0.99, 0, 0.95, 0, 0.98, 0.98),
  #yinf = c(0.5,0.75,1,1.25,1.5,1.75,2,3,3.25,3.5,3.75,4,4.25,4.5,5.5,5.75,6,6.25,6.5,6.75,7),
  stringsAsFactors = FALSE
)
avg20yield_datanew <- data.frame(
  Species = c("Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish", "Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish", "Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish"),
  xinf = c(0.85, 0.85, 0, 0.835, 0, 0.885, 0.875,0.95, 0.975, 0, 0.95, 0, 0.97, 0.95, 0.99, 0.99, 0, 0.975, 0, 0.99, 0.99),
  #yinf = c(0.5,0.75,1,1.25,1.5,1.75,2,3,3.25,3.5,3.75,4,4.25,4.5,5.5,5.75,6,6.25,6.5,6.75,7),
  stringsAsFactors = FALSE
)

biomass_datanew <- biomass_datanew %>%
  mutate(Species = factor(Species, levels = c("Lingcod", "Lingcod - Fixed Gear", "Dover Sole", "Yellowtail Rockfish", "Widow Rockfish", "Sablefish", "Sablefish - Fixed Gear"))) %>%
  arrange(Species)

avg20biomass_datanew <- avg20biomass_datanew %>%
  mutate(Species = factor(Species, levels = c("Lingcod", "Lingcod - Fixed Gear", "Dover Sole", "Yellowtail Rockfish", "Widow Rockfish", "Sablefish", "Sablefish - Fixed Gear"))) %>%
  arrange(Species)

# Reorder f50_datanew
f50_datanew <- f50_datanew %>%
  mutate(Species = factor(Species, levels = c("Lingcod", "Lingcod - Fixed Gear", "Dover Sole", "Yellowtail Rockfish", "Widow Rockfish", "Sablefish", "Sablefish - Fixed Gear"))) %>%
  arrange(Species)

avg20f50_datanew <- avg20f50_datanew %>%
  mutate(Species = factor(Species, levels = c("Lingcod", "Lingcod - Fixed Gear", "Dover Sole", "Yellowtail Rockfish", "Widow Rockfish", "Sablefish", "Sablefish - Fixed Gear"))) %>%
  arrange(Species)

# Reorder yield_datanew
yield_datanew <- yield_datanew %>%
  mutate(Species = factor(Species, levels = c("Lingcod", "Lingcod - Fixed Gear", "Dover Sole", "Yellowtail Rockfish", "Widow Rockfish", "Sablefish", "Sablefish - Fixed Gear"))) %>%
  arrange(Species)

avg20yield_datanew <- avg20yield_datanew %>%
  mutate(Species = factor(Species, levels = c("Lingcod", "Lingcod - Fixed Gear", "Dover Sole", "Yellowtail Rockfish", "Widow Rockfish", "Sablefish", "Sablefish - Fixed Gear"))) %>%
  arrange(Species)

biomass_datanew$new_column <- biomass_datanew$xinf - 1
f50_datanew$new_column <- f50_datanew$xinf - 1
yield_datanew$new_column <- yield_datanew$xinf - 1

avg20biomass_datanew$new_column <- avg20biomass_datanew$xinf - 1
avg20f50_datanew$new_column <- avg20f50_datanew$xinf - 1
avg20yield_datanew$new_column <- avg20yield_datanew$xinf - 1

#old jitter plot code
# # Plot for biomass
# y_limits <- c(0.6, 1.4)
# 
# # Define custom x-axis labels
# custom_labels <- c("Inside", "Near", "Far")
# 
# # Plot for biomass
# p1 <- ggplot(biomass_datanew, aes(x = yinf, y = xinf, fill = Species)) +
#   geom_point(shape = 21, size = 3) +
#   labs(title = "Normalized Average Biomass - CV 0.2", x = NULL, y = "yinf") +
#   ylim(y_limits) +
#   scale_x_continuous(breaks = c(1.25, 3.7, 6.2), labels = custom_labels) +
#   theme_minimal() +
#   ylab(NULL)
# 
# # Plot for F50
# p2 <- ggplot(f50_datanew, aes(x = yinf, y = xinf, fill = Species)) +
#   geom_point(shape = 21, size = 3) +
#   labs(title = "Harvest Rate (Relative to F50%) - CV 0.2", x = NULL, y = "yinf") +
#   ylim(y_limits) +
#   scale_x_continuous(breaks = c(1.25, 3.7, 6.2), labels = custom_labels) +
#   theme_minimal() +
#   ylab(NULL)
# 
# # Plot for yield
# p3 <- ggplot(yield_datanew, aes(x = yinf, y = xinf, fill = Species)) +
#   geom_point(shape = 21, size = 3) +
#   labs(title = "Normalized Average Yield - CV 0.2", x = NULL, y = "yinf") +
#   ylim(y_limits) +
#   scale_x_continuous(breaks = c(1.25, 3.7, 6.2), labels = custom_labels) +
#   theme_minimal() +
#   ylab(NULL)
# 
# # Arrange the plots
# grid.arrange(p1, p2, p3, ncol = 1)

###biomass plot

biomass_datanew <- biomass_datanew %>%
  mutate(distance = rep(c("inside", "near", "far"), times = 7))

filtered_data <- biomass_datanew %>%
  filter(distance == "inside" & !(Species %in% c("Sablefish - Fixed Gear", "Lingcod - Fixed Gear")))


avg20biomass_datanew <- avg20biomass_datanew %>%
  mutate(distance = rep(c("inside", "near", "far"), times = 7))

avg20filtered_data <- avg20biomass_datanew %>%
  filter(distance == "inside" & !(Species %in% c("Sablefish - Fixed Gear", "Lingcod - Fixed Gear")))


#plot
p <- ggplot(filtered_data, aes(x = new_column, y = Species, fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.75, 1) +  # Adjust x-axis limits to provide space for the bars
  xlab("Values") +
  ylab("Species") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  guides(fill = FALSE) # Remove legend

pp <- ggplot(avg20filtered_data, aes(x = new_column, y = Species, fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.75, 1) +  # Adjust x-axis limits to provide space for the bars
  xlab("Values") +
  ylab("Species") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  guides(fill = FALSE) # Remove legend

# Save the plot to an EPS file
ggsave("avg20inside_biomass.eps", plot = pp, device = "eps")

afiltered_data <- biomass_datanew %>%
  filter(distance == "near" & !(Species %in% c("Sablefish - Fixed Gear", "Lingcod - Fixed Gear")))
avg20afiltered_data <- avg20biomass_datanew %>%
  filter(distance == "near" & !(Species %in% c("Sablefish - Fixed Gear", "Lingcod - Fixed Gear")))


#plot
a <- ggplot(afiltered_data, aes(x = new_column, y = Species, fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.75, 1) +  # Adjust x-axis limits to provide space for the bars
  xlab("Values") +
  ylab("Species") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  guides(fill = FALSE) # Remove legend

aa <- ggplot(avg20afiltered_data, aes(x = new_column, y = Species, fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.75, 1) +  # Adjust x-axis limits to provide space for the bars
  xlab("Values") +
  ylab("Species") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  guides(fill = FALSE) # Remove legend
# Save the plot to an EPS file
ggsave("avg20near_biomass.eps", plot = aa, device = "eps")

ffiltered_data <- biomass_datanew %>%
  filter(distance == "far" & !(Species %in% c("Sablefish - Fixed Gear", "Lingcod - Fixed Gear")))

avg20ffiltered_data <- avg20biomass_datanew %>%
  filter(distance == "far" & !(Species %in% c("Sablefish - Fixed Gear", "Lingcod - Fixed Gear")))

#plot
f <- ggplot(ffiltered_data, aes(x = new_column, y = Species, fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.75, 1) +  # Adjust x-axis limits to provide space for the bars
  xlab("Values") +
  ylab("Species") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  guides(fill = FALSE) # Remove legend
#plot
ff <- ggplot(avg20ffiltered_data, aes(x = new_column, y = Species, fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.75, 1) +  # Adjust x-axis limits to provide space for the bars
  xlab("Values") +
  ylab("Species") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  guides(fill = FALSE) # Remove legend

# Save the plot to an EPS file
ggsave("avg20far_biomass.eps", plot = ff, device = "eps")

###yield plot
yield_datanew <- yield_datanew %>%
  mutate(distance = rep(c("inside", "near", "far"), times = 7))

sfiltered_data <- yield_datanew %>% 
  filter(distance == "inside")

sfiltered_data <- yield_datanew %>%
  filter(Species != "Sablefish - Fixed Gear" & Species != "Lingcod - Fixed Gear" & distance == "inside")

avg20yield_datanew <- avg20yield_datanew %>%
  mutate(distance = rep(c("inside", "near", "far"), times = 7))

avg20sfiltered_data <- avg20yield_datanew %>% 
  filter(distance == "inside")

avg20sfiltered_data <- avg20yield_datanew %>%
  filter(Species != "Sablefish - Fixed Gear" & Species != "Lingcod - Fixed Gear" & distance == "inside")

ssp <- ggplot(sfiltered_data, aes(x = new_column, y = Species, fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.75, 1) +  # Adjust x-axis limits to provide space for the bars
  xlab("Values") +
  ylab("Species") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  guides(fill = FALSE) # Remove legend

sspavg20 <- ggplot(avg20sfiltered_data, aes(x = new_column, y = Species, fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.75, 1) +  # Adjust x-axis limits to provide space for the bars
  xlab("Values") +
  ylab("Species") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  guides(fill = FALSE) # Remove legend

# Save the plot to an EPS file
ggsave("avg20inside_yield.eps", plot = sspavg20, device = "eps")

ssfiltered_data <- yield_datanew %>%
  filter(distance == "near" & !(Species %in% c("Sablefish - Fixed Gear", "Lingcod - Fixed Gear")))

avg20ssfiltered_data <- avg20yield_datanew %>%
  filter(distance == "near" & !(Species %in% c("Sablefish - Fixed Gear", "Lingcod - Fixed Gear")))

aa <- ggplot(ssfiltered_data, aes(x = new_column, y = Species, fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.75, 1) +  # Adjust x-axis limits to provide space for the bars
  xlab("Values") +
  ylab("Species") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  guides(fill = FALSE) # Remove legend

aaavg20 <- ggplot(avg20ssfiltered_data, aes(x = new_column, y = Species, fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.75, 1) +  # Adjust x-axis limits to provide space for the bars
  xlab("Values") +
  ylab("Species") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  guides(fill = FALSE) # Remove legend
# Save the plot to an EPS file
ggsave("avg20near_yield.eps", plot = aaavg20, device = "eps")

ddfiltered_data <- yield_datanew %>%
  filter(distance == "far" & !(Species %in% c("Sablefish - Fixed Gear", "Lingcod - Fixed Gear")))

avg20ddfiltered_data <- avg20yield_datanew %>%
  filter(distance == "far" & !(Species %in% c("Sablefish - Fixed Gear", "Lingcod - Fixed Gear")))

ff <- ggplot(ddfiltered_data, aes(x = new_column, y = Species, fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.75, 1) +  # Adjust x-axis limits to provide space for the bars
  xlab("Values") +
  ylab("Species") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  guides(fill = FALSE) # Remove legend

ffavg20 <- ggplot(avg20ddfiltered_data, aes(x = new_column, y = Species, fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.75, 1) +  # Adjust x-axis limits to provide space for the bars
  xlab("Values") +
  ylab("Species") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  guides(fill = FALSE) # Remove legend

# Save the plot to an EPS file
ggsave("20avgfar_yield.eps", plot = ffavg20, device = "eps")

#f50_datanew
f50_datanew <- f50_datanew %>%
  mutate(distance = rep(c("inside", "near", "far"), times = 7))

fnfiltered_data <- f50_datanew %>%
  filter(distance == "inside" & !(Species %in% c("Sablefish - Fixed Gear", "Lingcod - Fixed Gear")))

avg20f50_datanew <- avg20f50_datanew %>%
  mutate(distance = rep(c("inside", "near", "far"), times = 7))

avg20fnfiltered_data <- avg20f50_datanew %>%
  filter(distance == "inside" & !(Species %in% c("Sablefish - Fixed Gear", "Lingcod - Fixed Gear")))

fnf <- ggplot(fnfiltered_data, aes(x = new_column, y = Species, fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.75, 1) +  # Adjust x-axis limits to provide space for the bars
  xlab("Values") +
  ylab("Species") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  guides(fill = FALSE) # Remove legend

fnfavg20 <- ggplot(avg20fnfiltered_data, aes(x = new_column, y = Species, fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.75, 1) +  # Adjust x-axis limits to provide space for the bars
  xlab("Values") +
  ylab("Species") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  guides(fill = FALSE) # Remove legend

# Save the plot to an EPS file
ggsave("avg20inside_f50.eps", plot = fnfavg20, device = "eps")

nfnfiltered_data <- f50_datanew %>%
  filter(distance == "near" & !(Species %in% c("Sablefish - Fixed Gear", "Lingcod - Fixed Gear")))

avg20nfnfiltered_data <- avg20f50_datanew %>%
  filter(distance == "near" & !(Species %in% c("Sablefish - Fixed Gear", "Lingcod - Fixed Gear")))

nfnf <- ggplot(nfnfiltered_data, aes(x = new_column, y = Species, fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.75, 1) +  # Adjust x-axis limits to provide space for the bars
  xlab("Values") +
  ylab("Species") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  guides(fill = FALSE) # Remove legend

avg20nfnf <- ggplot(avg20nfnfiltered_data, aes(x = new_column, y = Species, fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.75, 1) +  # Adjust x-axis limits to provide space for the bars
  xlab("Values") +
  ylab("Species") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  guides(fill = FALSE) # Remove legend

# Save the plot to an EPS file
ggsave("near_f50.eps", plot = nfnf, device = "eps")

ffnfnfiltered_data <- f50_datanew %>%
  filter(distance == "far" & !(Species %in% c("Sablefish - Fixed Gear", "Lingcod - Fixed Gear")))

ffnn <- ggplot(ffnfnfiltered_data, aes(x = new_column, y = Species, fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.75, 1) +  # Adjust x-axis limits to provide space for the bars
  xlab("Values") +
  ylab("Species") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  guides(fill = FALSE) # Remove legend

# Save the plot to an EPS file
ggsave("far_f50.eps", plot = ffnn, device = "eps")

#################
#CV 0.02

# newbiomass_data <- data.frame(
#   Species = c("Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish"),
#   Inside = c(1.25, 1.05, 1.2, 1.15, 1.4, 1.1, 1.1),
#   Near = c(1.05, 1, 1.05, 1.01, 1.1, 1.05, 1.05),
#   Far = c(1, 1, 1, 1, 1, 1, 1),
#   stringsAsFactors = FALSE
# )
# 
# newf50_data <- data.frame(
#   Species = c("Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish"),
#   Inside = c(0.93, 0.935, 0.81, 1, 0.83, 0.84, 1),
#   Near = c(1, 1, 1, 1, 1, 1, 1),
#   Far = c(1, 1, 1, 1, 1, 1, 1),
#   stringsAsFactors = FALSE
# )
# 
# newyield_data <- data.frame(
#   Species = c("Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish"),
#   Inside = c(0.7, 0.7, 0.8, 0.75, 0.7, 0.9, 0.6),
#   Near = c(0.9, 0.9, 0.92, 0.9, 0.9, 0.98, 0.9),
#   Far = c(1, 1, 0.99, 0.97, 0.99, 1, 0.99),
#   stringsAsFactors = FALSE
# )

newbiomass_data <- data.frame(
  Species = c("Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish", "Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish", "Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish"),
  xinf = c(1.25, 1.1, 0, 1.1, 0, 1.2, 1.2, 1.07, 1, 0, 1.05, 0, 1.1, 1.1, 1, 1, 0, 1, 0, 1, 1),
  yinf = c(0.84,0.75,1,1.25,1.5,1.75,2,3,3.25,3.5,3.75,4,4.25,4.5,5.5,5.75,6,6.25,6.5,6.75,7),
  stringsAsFactors = FALSE
)

newf50_data <- data.frame(
  Species = c("Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish", "Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish", "Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish"),
  xinf = c(0.84, 0.93, 0, 1, 0, 0.84, 1,1, 1, 0, 1, 0, 1, 1,1, 1, 1, 1, 1, 1, 1),
  yinf = c(0.5,0.75,1,1.25,1.5,1.75,2,3,3.25,3.5,3.75,4,4.25,4.5,5.5,5.75,6,6.25,6.5,6.75,7),
  stringsAsFactors = FALSE
)

newyield_data <- data.frame(
  Species = c("Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish", "Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish", "Dover Sole", "Sablefish", "Sablefish - Fixed Gear", "Lingcod", "Lingcod - Fixed Gear", "Widow Rockfish", "Yellowtail Rockfish"),
  xinf = c(0.75, 0.7, 0, 0.75, 0, 0.75, 0.7,0.95, 0.9, 0, 0.9, 0, 0.9, 0.9, 0.99, 1, 0, 0.99, 0, 0.99, 0.99),
  yinf = c(0.5,0.75,1,1.25,1.5,1.75,2,3,3.25,3.5,3.75,4,4.25,4.5,5.5,5.75,6,6.25,6.5,6.75,7),
  stringsAsFactors = FALSE
)

#reoder
newbiomass_data <- newbiomass_data %>%
  mutate(Species = factor(Species, levels = c("Lingcod", "Lingcod - Fixed Gear", "Dover Sole", "Yellowtail Rockfish", "Widow Rockfish", "Sablefish", "Sablefish - Fixed Gear"))) %>%
  arrange(Species)

# Reorder 
newf50_data <- newf50_data %>%
  mutate(Species = factor(Species, levels = c("Lingcod", "Lingcod - Fixed Gear", "Dover Sole", "Yellowtail Rockfish", "Widow Rockfish", "Sablefish", "Sablefish - Fixed Gear"))) %>%
  arrange(Species)

# Reorder
newyield_data <- newyield_data %>%
  mutate(Species = factor(Species, levels = c("Lingcod", "Lingcod - Fixed Gear", "Dover Sole", "Yellowtail Rockfish", "Widow Rockfish", "Sablefish", "Sablefish - Fixed Gear"))) %>%
  arrange(Species)

newbiomass_data$new_column <- newbiomass_data$xinf - 1
newf50_data$new_column <- newf50_data$xinf - 1
newyield_data$new_column <- newyield_data$xinf - 1

#old jitter plot
# # Plot for biomass
# y_limits <- c(0.6, 1.4)
# 
# # Define custom x-axis labels
# custom_labels <- c("Inside", "Near", "Far")
# 
# # Plot for biomass
# a1 <- ggplot(newbiomass_data, aes(x = yinf, y = xinf, fill = Species)) +
#   geom_point(shape = 21, size = 3) +
#   labs(title = "Normalized Average Biomass - CV 0.02", x = NULL, y = "yinf") +
#   ylim(y_limits) +
#   scale_x_continuous(breaks = c(1.25, 3.7, 6.2), labels = custom_labels) +
#   theme_minimal() +
#   ylab(NULL)
# 
# # Plot for F50
# a2 <- ggplot(newf50_data, aes(x = yinf, y = xinf, fill = Species)) +
#   geom_point(shape = 21, size = 3) +
#   labs(title = "Harvest Rate (Relative to F50%) - CV 0.02", x = NULL, y = "yinf") +
#   ylim(y_limits) +
#   scale_x_continuous(breaks = c(1.25, 3.7, 6.2), labels = custom_labels) +
#   theme_minimal() +
#   ylab(NULL)
# 
# # Plot for yield
# a3 <- ggplot(newyield_data, aes(x = yinf, y = xinf, fill = Species)) +
#   geom_point(shape = 21, size = 3) +
#   labs(title = "Normalized Average Yield - CV 0.02", x = NULL, y = "yinf") +
#   ylim(y_limits) +
#   scale_x_continuous(breaks = c(1.25, 3.7, 6.2), labels = custom_labels) +
#   theme_minimal() +
#   ylab(NULL)
# 
# # Arrange the plots
# grid.arrange(a1, a2, a3, ncol = 1)


###biomass plot

newbiomass_data <- newbiomass_data %>%
  mutate(distance = rep(c("inside", "near", "far"), times = 7))

newifiltered_data <- newbiomass_data %>%
  filter(distance == "inside" & !(Species %in% c("Sablefish - Fixed Gear", "Lingcod - Fixed Gear")))

#plot
ppppp <- ggplot(newifiltered_data, aes(x = new_column, y = Species, fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.75, 1) +  # Adjust x-axis limits to provide space for the bars
  xlab("Values") +
  ylab("Species") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  guides(fill = FALSE) # Remove legend

# Save the plot to an EPS file
ggsave("cv0.02inside_biomass.eps", plot = ppppp, device = "eps")

acv0.02filtered_data <- newbiomass_data %>%
  filter(distance == "near" & !(Species %in% c("Sablefish - Fixed Gear", "Lingcod - Fixed Gear")))

#plot
aaaa <- ggplot(acv0.02filtered_data, aes(x = new_column, y = Species, fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.75, 1) +  # Adjust x-axis limits to provide space for the bars
  xlab("Values") +
  ylab("Species") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  guides(fill = FALSE) # Remove legend

# Save the plot to an EPS file
ggsave("near_cv0.02biomass.eps", plot = aaaa, device = "eps")

newcv0.02ffiltered_data <- newbiomass_data %>%
  filter(distance == "far" & !(Species %in% c("Sablefish - Fixed Gear", "Lingcod - Fixed Gear")))


#plot
ffff <- ggplot(newcv0.02ffiltered_data, aes(x = new_column, y = Species, fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.75, 1) +  # Adjust x-axis limits to provide space for the bars
  xlab("Values") +
  ylab("Species") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL)+
  guides(fill = FALSE) # Remove legend

# Save the plot to an EPS file
ggsave("far_cv0.02biomass.eps", plot = ffff, device = "eps")

###yield plot
newyield_data <- newyield_data %>%
  mutate(distance = rep(c("inside", "near", "far"), times = 7))

sssfiltered_data <- newyield_data %>%
  filter(distance == "inside" & !(Species %in% c("Sablefish - Fixed Gear", "Lingcod - Fixed Gear")))


ssssp <- ggplot(sssfiltered_data, aes(x = new_column, y = Species, fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.75, 1) +  # Adjust x-axis limits to provide space for the bars
  xlab("Values") +
  ylab("Species") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  guides(fill = FALSE) # Remove legend

# Save the plot to an EPS file
ggsave("inside_cv0.02yield.eps", plot = ssssp, device = "eps")

ddddssfiltered_data <- newyield_data %>%
  filter(distance == "near" & !(Species %in% c("Sablefish - Fixed Gear", "Lingcod - Fixed Gear")))


dddaa <- ggplot(ddddssfiltered_data, aes(x = new_column, y = Species, fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.75, 1) +  # Adjust x-axis limits to provide space for the bars
  xlab("Values") +
  ylab("Species") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  guides(fill = FALSE) # Remove legend

# Save the plot to an EPS file
ggsave("near_cv0.02yield.eps", plot = dddaa, device = "eps")

fffddfiltered_data <- newyield_data %>%
  filter(distance == "far" & !(Species %in% c("Sablefish - Fixed Gear", "Lingcod - Fixed Gear")))


rrff <- ggplot(fffddfiltered_data, aes(x = new_column, y = Species, fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.75, 1) +  # Adjust x-axis limits to provide space for the bars
  xlab("Values") +
  ylab("Species") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  guides(fill = FALSE) # Remove legend

# Save the plot to an EPS file
ggsave("far_cv0.02yield.eps", plot = rrff, device = "eps")

#f50_datanew
newf50_data <- newf50_data %>%
  mutate(distance = rep(c("inside", "near", "far"), times = 7))

ttfnfiltered_data <- newf50_data %>%
  filter(distance == "inside" & !(Species %in% c("Sablefish - Fixed Gear", "Lingcod - Fixed Gear")))


ttfnf <- ggplot(ttfnfiltered_data, aes(x = new_column, y = Species, fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.75, 1) +  # Adjust x-axis limits to provide space for the bars
  xlab("Values") +
  ylab("Species") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  guides(fill = FALSE) # Remove legend

# Save the plot to an EPS file
ggsave("inside_cv0.02f50.eps", plot = ttfnf, device = "eps")

wwnfnfiltered_data <- newf50_data %>%
  filter(distance == "near" & !(Species %in% c("Sablefish - Fixed Gear", "Lingcod - Fixed Gear")))


wwnfnf <- ggplot(wwnfnfiltered_data, aes(x = new_column, y = Species, fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.75, 1) +  # Adjust x-axis limits to provide space for the bars
  xlab("Values") +
  ylab("Species") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  guides(fill = FALSE) # Remove legend

# Save the plot to an EPS file
ggsave("near_cv0.02f50.eps", plot = wwnfnf, device = "eps")

ttffnfnfiltered_data <- newf50_data %>%
  filter(distance == "far" & !(Species %in% c("Sablefish - Fixed Gear", "Lingcod - Fixed Gear")))


ttffnn <- ggplot(ttffnfnfiltered_data, aes(x = new_column, y = Species, fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.75, 1) +  # Adjust x-axis limits to provide space for the bars
  xlab("Values") +
  ylab("Species") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  guides(fill = FALSE) # Remove legend

# Save the plot to an EPS file
ggsave("far_cv0.02f50.eps", plot = ttffnn, device = "eps")
