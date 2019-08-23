#===================================================================================================

# Fw = fraction wetland
Fw_max <- raster('./Data/Wetland Map/Fw_max.tif')
Fw_max[Fw_max < 0.025] <- NA

# Crop out Antartica, not relevant to the research
area_of_interest = extent(c(xmin = -180, xmax = 180, ymin = -58, ymax = 90))
bioclim_stack <- crop(bioclim_stack, area_of_interest)
Fw_max <- crop(Fw_max, area_of_interest)

# Create mask
bioclim_stack <- mask(bioclim_stack, Fw_max)
bioclim_stack_df <- as.data.frame(as(bioclim_stack, "SpatialPixelsDataFrame"))
Fw_max_df <- as.data.frame(as(Fw_max, "SpatialPixelsDataFrame"))

# Cropping and masking for individual climatic variables that I chose
tmp_stack <- crop(tmp_stack, area_of_interest)
pre_stack <- crop(pre_stack, area_of_interest)
tmp_stack <- mask(tmp_stack, Fw_max)
pre_stack <- mask(pre_stack, Fw_max)
tmp_df <- as.data.frame(as(tmp_stack, "SpatialPixelsDataFrame"))
pre_df <- as.data.frame(as(pre_stack, "SpatialPixelsDataFrame"))

# Times 12 as current dataset is average monthly precipitation
pre_df$pre_avg <- pre_df$pre_avg * 12

#===================================================================================================

# Calculate eucledian distance to all other pixels of all the climatic variables for each tower
distance <- proxy::dist(x = bioclim_stack_df[c(1:num_bio)], y = towers_coords_df_acquired[c(14: (13 + num_bio))], method = "Euclidean",
                      diag = FALSE, upper = FALSE)

# Reformat to be usable for analysis
distance <- as.data.frame(as.matrix.data.frame(distance))
names(distance) <- towers_coords_acquired$SITE_ID
bioclim_stack_df_wdist <- cbind(bioclim_stack_df, distance)

#===================================================================================================

# Find the minimum distance in terms of the climatic variables at each pixel on Earth from the network of towers
num_col = ncol(bioclim_stack_df_wdist)
bioclim_stack_df_wdist$min_dist <- apply(bioclim_stack_df_wdist[, (num_bio + 3):num_col], MARGIN = 1, FUN = min, na.rm = TRUE)

# Find the tower that has the minimum distance
bioclim_stack_df_wdist$closest_tower <- c(apply(bioclim_stack_df_wdist[, (num_bio + 3):num_col], MARGIN = 1, FUN = which.min))

# Convert numeric to tower ID
bioclim_stack_df_wdist$closest_tower <- colnames(bioclim_stack_df_wdist[, (num_bio + 3):num_col])[bioclim_stack_df_wdist$closest_tower]

#===================================================================================================

# K-means clustering
k <- kmeans(na.omit(bioclim_stack_df[, 1:num_bio]), centers = 10, nstart = 100)

# Put into stack
bioclim_stack_df_k <- na.omit(bioclim_stack_df)
bioclim_stack_df_k$k <- k$cluster

# Convert data frame into raster
k_raster <- SpatialPixelsDataFrame(bioclim_stack_df_k[, c('x', 'y')], data = bioclim_stack_df_k)
k_raster <- raster(k_raster, layer = ncol(bioclim_stack_df_k))

# Extract the cluster ID of each tower
towers_coords_df_acquired$k <- raster::extract(k_raster, towers_coords_acquired)

#===================================================================================================

# Calculate p-value based on KS-test
for_ks_t <- towers_coords_df_acquired[, 14:24]
for_ks_b <- bioclim_stack_df[, 1:11]

for (j in 1:ncol(for_ks_b)) {
  print(names(for_ks_b[j]))
  print(ks.test(for_ks_t[j], for_ks_b[, j], "pnorm"))
}




