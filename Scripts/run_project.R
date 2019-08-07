library(ggplot2)
library(dplyr)
library(tidyr)
library(raster)
library(raster)
library(rgdal)
library(proxy)
library(here)
library(ggrepel)
here()

# Read the bioclimatic variables and the tower data
source('./Scripts/read_bioclim.R')

# Draw the cumulative plot, visual representation of K-S Test
source('./Scripts/cumulative_plot.R')

# Do representativeness analyses like eucledian distance
source('./Scripts/representativeness.R')

# Plot our analysis of the representativeness analyses
source('./Scripts/plot.R')

# Plot the number of towers in the network over time
source('./Scripts/tower_count_yearly.R')

#=====================================================================================================

# Notes:

  # Wetland percantage -> use alpha to give a visual representation of the wetlands
  
  # Make a loop to save the KS-test scores
  
  # Uniqueness of towers within the network:
      # Run the distance algorithm for tower-to-tower of the ones currently in the network 
      # (instead of tower to pixel). This would give us a uniqueness score of our current towers.
      # Run the distance algorithm for current network towers to new towers. 
      # This would give use a priority among new towers.
  
  # Calculate the distance (average or total) for the world when 
  # calculated from our current network, vs all potential tower.
