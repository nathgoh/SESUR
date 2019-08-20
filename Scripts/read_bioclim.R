#===================================================================================================

# Get the list of climatic variables
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

# Get the individual climatic variables and store it as its own separate data frame
tmp_df <- as.data.frame(as(bioclim_stack$tmp_avgr, "SpatialPixelsDataFrame"))
pre_df <- as.data.frame(as(bioclim_stack$pre_avg, "SpatialPixelsDataFrame"))

# Normalize the climatic variable
scaled_data <- raster::scale(bioclim_stack)
bioclim_stack <- scaled_data

# Reformat the rasters so for use with ggplot
bioclim_stack_df <- as.data.frame(as(bioclim_stack, "SpatialPixelsDataFrame"))

# # Normalize the climatic variables
# scaled_data <- normalize(bioclim_stack_df[, 1:num_bio], method = "range", range = c(0, 1), margin = 2L, on.constant = "quiet")
# scaled_data$x <- cbind(bioclim_stack_df$x)
# scaled_data$y <- cbind(bioclim_stack_df$y)

# Filter for NA
bioclim_stack_df <- bioclim_stack_df %>%
  filter(!is.na(wc2.0_bio_5m_01))

#===================================================================================================

# Clean the tower data so that it is usable
source('./Scripts/clean_tower_data.R')

# Get coordinates from the tower data
coords_acquired <- cbind(towers_acquired$LOCATION_LONG, towers_acquired$LOCATION_LAT)

# Make spatial objects
towers_coords_acquired <- SpatialPointsDataFrame(coords_acquired, towers_acquired)
towers_coords_df_acquired <- data.frame(towers_coords_acquired)

# Extract pixel values at towers
towers_coords_df_acquired <- cbind(towers_coords_df_acquired, raster::extract(bioclim_stack, towers_coords_acquired))

# Get rid of any NA in the biolclimatic variables
towers_coords_df_acquired <- na.omit(towers_coords_df_acquired)

#===================================================================================================

# Get coordinates from the tower data
coords_all <- cbind(towers_all$LOCATION_LONG, towers_all$LOCATION_LAT)

# Make spatial objects
towers_coords_all <- SpatialPointsDataFrame(coords_all, towers_all)
towers_coords_df_all <- data.frame(towers_coords_all)

# Extract pixel values at towers
towers_coords_df_all <- cbind(towers_coords_df_all, raster::extract(bioclim_stack, towers_coords_all))

# Get rid of any NA in the climatic variables
towers_coords_df_all <- na.omit(towers_coords_df_all) 

#===================================================================================================

# # Random sampling to use less computation power
# bioclim_stack_df <- bioclim_stack_df[sample(nrow(bioclim_stack_df), 300000), ]

# Convert bioclimatic stack data from wide format to long format
# Purpose of this is to plot cumulative frequency
bioclim_stack_df2 <- bioclim_stack_df %>%
  gather(key = bioclim_var, value = value, wc2.0_bio_5m_01:tmp_avgr)

# Convert towers stack data from wide format to long format
towers_coords_df2_acquired <- towers_coords_df_acquired %>%
  gather(key = bioclim_var, value = value, wc2.0_bio_5m_01:tmp_avgr)

#===================================================================================================