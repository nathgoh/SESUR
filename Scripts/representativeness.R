#===========================================================================================

# Calculate eucledian distance to all other pixels of all the bioclimatic variables for each tower
distance <- proxy::dist(x = bioclim_stack_df[c(1:5)], y = towers_coords_df[c(8:12)], method = "Euclidean",
                      diag = FALSE, upper = FALSE)

# Reformat to be usable for analysis
distance <- as.data.frame(as.matrix.data.frame(distance))
names(distance) <- towers_coords$ID
bioclim_stack_df_wdist <- cbind(bioclim_stack_df, distance)

#===========================================================================================

# Find the minimum distance in terms of the bioclimatic variables at each pixel on Earth from the network of towers
bioclim_stack_df_wdist$min_dist <- apply(bioclim_stack_df_wdist[, 8:57], MARGIN = 1, FUN = min, na.rm = TRUE)

# Find the tower that has the minimum distance
bioclim_stack_df_wdist$closest_tower <- c(apply(bioclim_stack_df_wdist[, 8:57], MARGIN = 1, FUN = which.min))

# Convert numeric to tower ID
bioclim_stack_df_wdist$closest_tower <- colnames(bioclim_stack_df_wdist[, 8:57])[bioclim_stack_df_wdist$closest_tower]

#===========================================================================================

# K-means clustering
k <- kmeans(bioclim_stack_df[, 1:5], centers = 10, nstart = 100)

# Put into stack
bioclim_stack_df_k <- bioclim_stack_df
bioclim_stack_df_k$k <- k$cluster

# Convert data frame into raster
k_raster <- SpatialPixelsDataFrame(bioclim_stack_df_k[, c('x', 'y')], data = bioclim_stack_df_k)
k_raster <- raster(k_raster, layer = ncol(bioclim_stack_df_k))

# Extract the cluster ID of each tower
towers_coords_df$k <- raster::extract(k_raster, towers_coords)

