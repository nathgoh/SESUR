#=====================================================================================================

# Filter out towers in the same pixel (bioclimatically identical)
towers_coords_df_all <- towers_coords_df_all %>%
  distinct(wc2.0_bio_5m_01,tmp_avgr, .keep_all = TRUE)

# Get just the data of the bioclimatic variables 
towers_coords_df_all_mds <- normalize(towers_coords_df_all[, 17: ncol(towers_coords_df_all)], method = "range", range = c(0, 1), margin = 2L, on.constant = "quiet")

# Do MDS calculations
vare.mds <- metaMDS(comm = towers_coords_df_all_mds, distance = "euclidean", engine = "isoMDS", wascores = TRUE, autotransform = TRUE)

# Add "no" to the Data_acquired column where there is a missing place
towers_coords_df_all$Data_acquired[towers_coords_df_all$Data_acquired == ""] <- "No"
towers_coords_df_all$Data_acquired[towers_coords_df_all$Data_acquired == "yes"] <- "Yes"

# Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores <- as.data.frame(scores(vare.mds))

# Create a column of site names, from the rownames of data.scores
data.scores$site <- rownames(data.scores) 

# Add the grp variable created earlier
data.scores$grp <- towers_coords_df_all$Data_acquired 

# # Look at the data
# head(data.scores)  

# Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores <- as.data.frame(scores(vare.mds, "species"))

# Create a column of species, from the rownames of species.scores
species.scores$species <- rownames(species.scores)  

# # Look at the data
# head(species.scores) 

# # Calculate eucledian distance between all the towers
# distance_tower <- proxy::dist(x = towers_coords_df_all[c(14: (13 + num_bio))], y = towers_coords_df_all[c(14: (13 + num_bio))], method = "Euclidean",
#                               diag = FALSE, upper = FALSE)
# 
# # Reformat to be usable for analysis
# distance_tower <- as.data.frame(as.matrix.data.frame(distance_tower))
# names(distance_tower) <- towers_coords_df_all$SITE_ID
# towers_coords_df_wdist_all <- cbind(towers_coords_df_all, distance_tower)

#=====================================================================================================

# # Calculate number of columns
# num_col = ncol(towers_coords_df_wdist_all)
# 
# # Convert into long format, filter, then wide format
# towers_coords_df2_wdist_all <- towers_coords_df_wdist_all %>%
#   gather(key = "tower_to", value = "dist", (num_bio + 14):num_col) %>%
#   filter(SITE_ID != tower_to) %>%
#   spread(key = "tower_to", value = "dist", fill = NA)
# 
# # Find the minimum distance between the towers
# towers_coords_df2_wdist_all$min_dist <- apply(towers_coords_df2_wdist_all[, (num_bio + 14):num_col], MARGIN = 1, FUN = min, na.rm = TRUE)

#=====================================================================================================

# # Convert data frame into a matrix
# towers_coords_matrix_wdist_all <- as.matrix(towers_coords_df_wdist_all[1:61, (num_bio + 14):num_col])
# 
# # Calculates mds
# towers_all = isoMDS(towers_coords_matrix_wdist_all)
# plot(towers_all$points, type = "n", pch = 20, cex = 3, col = adjustcolor("black", alpha = 0.3), xlab = "X", ylab = "Y") 
# text(towers_all$points, labels = towers_coords_df_all$SITE_ID, cex = 0.75)

#=====================================================================================================