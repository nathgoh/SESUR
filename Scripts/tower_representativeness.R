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