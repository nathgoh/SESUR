#===================================================================================================

# Get the list of bioclimatic variables
bioclim_list <- list.files(path = "./Data/Bioclimatic", full.names = TRUE, pattern = ".tif")

# Put into a stack
bioclim_stack <- raster::stack(bioclim_list)

# # Crop out Antartica, not relevant to the research
# area_of_interest = extent(c(xmin = -180, xmax = 180, ymin = -58, ymax = 90))
# bioclim_stack <- crop(bioclim_stack, area_of_interest)

# Aggrgate to a common resolution
bioclim_stack <- aggregate(bioclim_stack, fact = 6, fun = mean)

# Get merra variables
merra_list <- list.files(path = "./Data/Merra", full.names = TRUE, pattern = ".tif")

# Put into a stack
bioclim_stack <- raster::stack(bioclim_stack, raster::stack(merra_list))


#===================================================================================================

# Normalize the bioclimatic variable
scaled_data <- raster::scale(bioclim_stack)
bioclim_stack <- scaled_data

# Reformat the rasters so for use with ggplot
bioclim_stack_df <- as.data.frame(as(bioclim_stack, "SpatialPixelsDataFrame"))

# Filter for NA
bioclim_stack_df <- bioclim_stack_df %>%
  filter(!is.na(wc2.0_bio_5m_01))

#===================================================================================================

# Clean the tower data so that it is usable
source('./Scripts/clean_tower_data.R')

# Get coordinates from the tower data
coords <- cbind(towers$LOCATION_LONG, towers$LOCATION_LAT)

# Make spatial objects
towers_coords <- SpatialPointsDataFrame(coords, towers)
towers_coords_df <- data.frame(towers_coords)

# Extract pixel values at towers
towers_coords_df <- cbind(towers_coords_df, raster::extract(bioclim_stack, towers_coords))

# Get rid of any NA in the biolclimatic variables
towers_coords_df <- na.omit(towers_coords_df) 

#===================================================================================================

# # Random sampling to use less computation power
# bioclim_stack_df <- bioclim_stack_df[sample(nrow(bioclim_stack_df), 300000), ]

# Convert bioclimatic stack data from wide format to long format
# Purpose of this is to plot cumulative frequency
bioclim_stack_df2 <- bioclim_stack_df %>%
  gather(key = bioclim_var, value = value, wc2.0_bio_5m_01:tmp_avgr)

# Convert towers stack data from wide format to long format
towers_coords_df2 <- towers_coords_df %>%
  gather(key = bioclim_var, value = value, wc2.0_bio_5m_01:tmp_avgr)



