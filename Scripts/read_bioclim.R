library(ggplot2)
library(dplyr)
library(tidyr)
library(raster)
library(raster)
library(rgdal)

library(here)
here()

#========================================================================================

bioclim_list <- list.files(path = "./Data/Bioclimatic", full.names = TRUE, pattern = ".tif")

bioclim_stack <- raster::stack(bioclim_list)

# Crop out Antartica, not relevant to the research
area_of_interest = extent(c(xmin = -180, xmax = 180, ymin = -58, ymax = 90))
bioclim_stack <- crop(bioclim_stack, area_of_interest)

# Reformat the rasters so for use with ggplot
bioclim_stack_df <- as.data.frame(as(bioclim_stack, "SpatialPixelsDataFrame")) 

#========================================================================================

# Function to read tower data and get the coordinates from the data
read_tower_data <- function(x) {
  
  # Read tower data
  flux_towers <<- read.csv(x)
  
  # Get coordinates from the tower data
  coords <<- cbind(flux_towers$Longitude, flux_towers$Latitude)
  
  # Make spatial objects
  towers_coords <<- SpatialPointsDataFrame(coords, flux_towers)
  towers_coords_df <<- data.frame(towers_coords)
  
  # Extract pixel values at towers
  towers_coords_df <<- cbind(towers_coords_df, raster::extract(bioclim_stack, towers_coords))
  
}

# Name of the tower data file
towers <- "./Data/Towers/BAMS_site_coordinates.csv"

read_tower_data(towers)

#========================================================================================

# Random sampling
bioclim_stack <- bioclim_stack(sample(nrow(bioclim_stack), 250))

# Convert bioclimatic stack data from wide format to long format
# Purpose of this is to plot
bioclim_stack_df2 <- bioclim_stack_df %>%
  gather(key = bioclim_var, value = value, wc2.0_bio_5m_01:wc2.0_bio_5m_11)

towers_coords_df2 <- towers_coords_df %>%
  gather(key = bioclim_var, value = value, wc2.0_bio_5m_01:wc2.0_bio_5m_11)
  
#========================================================================================

# Function to draw cumulative plot
# cumulative <- function(towers, plot_df, plot_data) {
#   cumulative_plot <- ggplot() +
#     stat_bin(data = towers, aes_string(x = plot_data, y = cumsum(..count..) / nrow(towers)), color = 'red', bins = 20, geom = "step") +
#     stat_bin(data = plot_df, aes_string(x = plot_data, y = cumsum(..count..) / nrow(plot_df)), color = 'blue', bins = 20, geom = "step")
#   
#   return(cumulative_plot)
# }

# Function to draw cumulative plot
cumulative <- function(towers, plot_df, plot_data) {
  cumulative_plot <- ggplot() +
    stat_ecdf(data = towers, aes_string(x = plot_data), color = 'red', geom = "step") +
    stat_ecdf(data = plot_df, aes_string(x = plot_data), color = 'blue', geom = "step") +
    xlab("") + ylab("Cumulative Frequency") +
    facet_wrap(~bioclim_var)
  
  return(cumulative_plot)
}


# LST Daytime
cumulative_daytime <- cumulative(towers_coords_df2, bioclim_stack_df2, "value")
cumulative_daytime

# LST Nighttime
cumulative_nighttime <- cumulative(towers_coords_df, lst_df, lst_night)
cumulative_nighttime

# # Cumulative plot
# ggplot() +
#   stat_bin(data=towers_coords_df, aes(x=lst_day, y=cumsum(..count..)/nrow(towers_coords_df)), color='red', bins=20, geom="step")+
#   stat_bin(data=lst_df, aes(x=lst_day, y=cumsum(..count..)/nrow(lst_df)), color='blue', bins=20, geom="step") +
#   xlab("LST Daytime (Celcius)") +
#   ggtitle("Distribution of pixels of LST Daytime")
# 
# ggplot() +
#   stat_bin(data = towers_coords_df, aes(x = lst_night, y=cumsum(..count..)/nrow(towers_coords_df)), color='red', bins=20, geom="step")+
#   stat_bin(data = lst_df, aes(x = lst_night, y = cumsum(..count..)/nrow(lst_df)), color='blue', bins=20, geom="step") +
#   xlab("LST Nighttime (Celcius)") +
#   ggtitle("Distribution of pixels of LST Nighttime")

#========================================================================================

#KS Test
ks.test(towers_coords_df$lst_day, lst_df$lst_day, "pnorm")
ks.test(towers_coords_df$lst_night, lst_df$lst_night, "pnorm")

#========================================================================================d
  
# Loop through individual towers
for (j in 1:nrow(towers_coords_df)) {
  
  # Select a tower
  tower <- towers_coords_df[j,]
  
  # Calculate eucledian distance to all other pixels for each tower
  dist <- data.frame(((lst_df$lst_day - tower$lst_day)^2 + (lst_df$lst_night - tower$lst_night)^2)^(1/2))
  names(dist) <- tower$ID
  
  # Append distance column to data frame
  lst_df <- cbind(lst_df, dist)
  
}

#========================================================================================

# Find the minimum distance in terms of LST at each pixel on earth from the network of towers
lst_df$min_dist <- apply(lst_df[,5:54], MARGIN = 1, FUN = min, na.rm = TRUE)

# Find the tower ID that has the minimum distance
lst_df$closest_tower <- as.character(c(apply(lst_df[,5:54], MARGIN = 1, FUN = which.min)))

#========================================================================================

# Map towers over the LST
ggplot() +
  geom_tile(data = lst_df, aes(x = long, y = lat, fill = as.factor(closest_tower))) +
  geom_text(data = towers_coords_df, aes(x = Longitude, y = Latitude, label = ID)) +
  geom_point(data = towers_coords_df, aes(x = Longitude, y = Latitude), color = "yellow") 


# Histogram
ggplot() +
  geom_histogram(data = lst_df, aes(x = closest_tower, fill = closest_tower), stat= "count") +
  coord_flip()
  






