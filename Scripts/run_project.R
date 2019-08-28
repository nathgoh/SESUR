library(ggplot2)
library(dplyr)
library(tidyr)
library(raster)
library(raster)
library(rgdal)
library(proxy)
library(here)
library(ggrepel)
library(MASS)
library(vegan)
library(BBmisc)
library(rworldmap)
library(RColorBrewer)
here()

# Number of climatic variables being used for analysis
num_bio = 11

# Read the climatic variables and the tower data and extract the climatic variables
# from the location of the tower site
source('./Scripts/read_bioclim.R')

# Do representativeness analyses like eucledian distance on climatic variables
# Use these results to indicate where the flux tower network is a good/bad or the most 
# representativeness of a certain part of the world given the current network of towers 
source('./Scripts/representativeness.R')

# Do tower specific analyses
# Tower to tower analyses specifcally against towers where methane flux data have not been acquired
# yet, used for indication of optimal towers that should be added to the methane flux network
# MDS plot indication shows all the towers and which ones we have acquired or not, also gives us a visual
# of which clusters of towers would be beneficial to the network
source('./Scripts/tower_representativeness.R')

# Plot our analysis of the representativeness analyses
# All plots will be automatically saved when this script is running
# Plots will be saved in /SESUR/Outputs
source('./Scripts/plot.R')

# Plot the number of towers in the network over time
source('./Scripts/tower_count_yearly.R')

# Draw the cumulative plot, visual representation of K-S Test
# Comparison of the global climatic variables against those that are obtained where the towers
# are located
source('./Scripts/cumulative_plot.R')

#=====================================================================================================