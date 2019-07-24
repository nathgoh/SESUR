library(ggplot2)
library(dplyr)
library(tidyr)
library(raster)
library(raster)
library(rgdal)
library(proxy)
library(here)
here()

#========================================================================================

# Get the list of bioclimatic variables
bioclim_list <- list.files(path = "./Data/Bioclimatic", full.names = TRUE, pattern = ".tif")

# Put into a stack
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

# Random sampling to use less computation power
bioclim_stack_df <- bioclim_stack_df[sample(nrow(bioclim_stack_df), 300000), ]

# Convert bioclimatic stack data from wide format to long format
# Purpose of this is to plot
bioclim_stack_df2 <- bioclim_stack_df %>%
  gather(key = bioclim_var, value = value, wc2.0_bio_5m_01:wc2.0_bio_5m_11)

# Convert towers stack data from wide format to long format
towers_coords_df2 <- towers_coords_df %>%
  gather(key = bioclim_var, value = value, wc2.0_bio_5m_01:wc2.0_bio_5m_11)
  
#========================================================================================

# Function to draw cumulative plot
cumulative <- function(towers, plot_df, plot_data) {
  cumulative_plot <- ggplot() +
    stat_ecdf(data = towers, aes_string(x = plot_data), color = 'red', geom = "step") +
    stat_ecdf(data = plot_df, aes_string(x = plot_data), color = 'blue', geom = "step") +
    xlab("") + ylab("Cumulative Frequency") +
    facet_wrap(~bioclim_var)
  
  return(cumulative_plot)
}


# Bioclimatic variables
cumulative_bioclim <- cumulative(towers_coords_df2, bioclim_stack_df2, "value")
cumulative_bioclim

#========================================================================================

# Calculate eucledian distance to all other pixels of all the bioclimatic variables for each tower
distance <- proxy::dist(x = bioclim_stack_df[c(1:5)], y = towers_coords_df[c(8:12)], method = "Euclidean",
                      diag = FALSE, upper = FALSE)

# Reformat to be usable for analysis
distance <- as.data.frame(as.matrix.data.frame(distance))
names(distance) <- towers_coords$ID
bioclim_stack_df_wdist <- cbind(bioclim_stack_df, distance)


#========================================================================================

# Find the minimum distance in terms of the bioclimatic variables at each pixel on Earth from the network of towers
bioclim_stack_df_wdist$min_dist <- apply(bioclim_stack_df_wdist[, 8:57], MARGIN = 1, FUN = min, na.rm = TRUE)

# Find the tower that has the minimum distance
bioclim_stack_df_wdist$closest_tower <- c(apply(bioclim_stack_df_wdist[, 8:57], MARGIN = 1, FUN = which.min))

# Convert numeric to tower ID
bioclim_stack_df_wdist$closest_tower <- colnames(bioclim_stack_df_wdist[, 8:57])[bioclim_stack_df_wdist$closest_tower]

#========================================================================================

# Map towers over the bioclimatic variables
ggplot() +
  geom_tile(data = bioclim_stack_df_wdist, aes(x = x, y = y, fill = as.factor(closest_tower))) +
  geom_text(data = towers_coords_df, aes(x = Longitude, y = Latitude + 2, label = ID)) +
  geom_point(data = towers_coords_df, aes(x = Longitude, y = Latitude), shape = 21, size = 2, stroke = 1.1, color = "black")


# Histogram
ggplot() +
  geom_histogram(data = bioclim_stack_df_wdist, aes(x = closest_tower, fill = closest_tower), stat= "count") +
  coord_flip()

#========================================================================================

# Order the histogram
bioclim_stack_df_wdist_summarized <- bioclim_stack_df_wdist %>%
                                      group_by(closest_tower) %>%
                                        summarise(count = n()) %>%
                                          arrange(count)

# Factor 
bioclim_stack_df_wdist$closest_tower <- factor(bioclim_stack_df_wdist$closest_tower, levels = bioclim_stack_df_wdist_summarized$closest_tower)

# Histogram
ggplot() +
  geom_histogram(data = bioclim_stack_df_wdist, aes(x = closest_tower, fill = closest_tower), stat = "count") +
  coord_flip() +
  theme(legend.position = "none") +
  xlab("Closest Tower") +
  ylab("Number of Pixels")
  





