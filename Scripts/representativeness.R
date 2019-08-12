#===================================================================================================

# Fw = fraction wetland
Fw_max <- raster('./Data/Wetland Map/Fw_max.tif')
Fw_max[Fw_max < 0.01] <- NA

# Crop out Antartica, not relevant to the research
area_of_interest = extent(c(xmin = -180, xmax = 180, ymin = -58, ymax = 90))
bioclim_stack <- crop(bioclim_stack, area_of_interest)
Fw_max <- crop(Fw_max, area_of_interest)

# Create mask
bioclim_stack <- mask(bioclim_stack, Fw_max)
bioclim_stack_df <- as.data.frame(as(bioclim_stack, "SpatialPixelsDataFrame"))
Fw_max_df <- as.data.frame(as(Fw_max, "SpatialPixelsDataFrame"))

#===================================================================================================

# Calculate eucledian distance to all other pixels of all the bioclimatic variables for each tower
distance <- proxy::dist(x = bioclim_stack_df[c(1:num_bio)], y = towers_coords_df[c(14: (13 + num_bio))], method = "Euclidean",
                      diag = FALSE, upper = FALSE)

# Reformat to be usable for analysis
distance <- as.data.frame(as.matrix.data.frame(distance))
names(distance) <- towers_coords$SITE_ID
bioclim_stack_df_wdist <- cbind(bioclim_stack_df, distance)

#===================================================================================================

# Filter out towers in the same pixel (bioclimatically identical)
towers_coords_df <- towers_coords_df %>%
  distinct(wc2.0_bio_5m_01,tmp_avgr, .keep_all = TRUE)

# Calculate eucledian distance between all the towers
distance_tower <- proxy::dist(x = towers_coords_df[c(14: (13 + num_bio))], y = towers_coords_df[c(14: (13 + num_bio))], method = "Euclidean",
                        diag = FALSE, upper = FALSE)

# Reformat to be usable for analysis
distance_tower <- as.data.frame(as.matrix.data.frame(distance_tower))
names(distance_tower) <- towers_coords_df$SITE_ID
towers_coords_df_wdist <- cbind(towers_coords_df, distance_tower)

#===================================================================================================

# Calculate number of columns
num_col = ncol(towers_coords_df_wdist)

# Convert into long format, filter, then wide format
towers_coords_df2_wdist <- towers_coords_df_wdist %>%
  gather(key = "tower_to", value = "dist", (num_bio + 14):num_col) %>%
  filter(SITE_ID != tower_to) %>%
  spread(key = "tower_to", value = "dist", fill = NA)
  
# Find the minimum distance between the towers
towers_coords_df2_wdist$min_dist <- apply(towers_coords_df2_wdist[, (num_bio + 14):num_col], MARGIN = 1, FUN = min, na.rm = TRUE)

#===================================================================================================

# Convert data frame into a matrix
towers_coords_matrix_wdist <- as.matrix(towers_coords_df_wdist[1:61, (num_bio + 14):num_col])

# Calculates MDS
towers_mds = isoMDS(towers_coords_matrix_wdist)
plot(towers_mds$points, type = "n", pch = 20, cex = 3, col = adjustcolor("black", alpha = 0.3), xlab = "X", ylab = "Y") 
text(towers_mds$points, labels = towers_coords_df$SITE_ID, cex = 0.75)

#===================================================================================================

# Find the minimum distance in terms of the bioclimatic variables at each pixel on Earth from the network of towers
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
towers_coords_df$k <- raster::extract(k_raster, towers_coords)

#===================================================================================================

# Calculate p-value based on KS-test
# ks_tests <- data.frame()
# for_ks_t <- towers_coords_df[, 14:24]
# for_ks_b <- bioclim_stack_df[, 1:11]
# 
# for (j in 1:ncol(for_ks_b)) {
#   ks <- ks.test(for_ks_t[j], for_ks_b[, j], "pnorm")
# }
