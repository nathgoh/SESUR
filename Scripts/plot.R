#=====================================================================================================

# Add wetland percentage (or fraction) to the bioclim stack
bioclim_stack_df_wdist = left_join(bioclim_stack_df_wdist, Fw_max_df, by = c('x', 'y'))

# Normalize the wetland percentage on a scale of 0 to 1
bioclim_stack_df_wdist <- bioclim_stack_df_wdist %>%
  mutate(Fw_max_scaled = (Fw_max - min(Fw_max)) / (max(Fw_max) - min(Fw_max)))

#=====================================================================================================

# Custom map theme
source('./Scripts/custom_theme.R')

# Map towers over the climatic variables
map_towers <- ggplot() +
  geom_tile(data = bioclim_stack_df_wdist, aes(x = x, y = y, fill = as.factor(closest_tower), alpha = Fw_max)) +
  geom_text_repel(data = towers_coords_df_acquired, aes(x = LOCATION_LONG, y =  LOCATION_LAT, label = SITE_ID), point.padding = NA, arrow = arrow(angle = 45, length = unit(1.25, 'mm'), ends = "last", type = "open"), segment.size = 0.2) +
  geom_point(data = towers_coords_df_acquired, aes(x = LOCATION_LONG,  y =  LOCATION_LAT), size = 1.5, color = "black") +
  labs(title = "Representativeness of the Tower Network") +
  theme_map(12)

# Map clustering of regions
map_clusters <- ggplot() +
  geom_tile(data = bioclim_stack_df_k, aes(x = x, y = y, fill = as.factor(k))) +
  geom_text(data = towers_coords_df_acquired, aes(x = LOCATION_LONG, y =  LOCATION_LAT + 2, label = SITE_ID)) +
  geom_point(data = towers_coords_df_acquired, aes(x = LOCATION_LONG,  y = LOCATION_LAT), shape = 21, size = 2, stroke = 1.1, color = "black")

# Map minimum distances
min_distances <- ggplot() +
  geom_tile(data = bioclim_stack_df_wdist, aes(x = x, y = y, fill = min_dist > 0.75)) +
  geom_text_repel(data = towers_coords_df_acquired, aes(x = LOCATION_LONG, y =  LOCATION_LAT, label = SITE_ID), point.padding = NA, arrow = arrow(angle = 45, length = unit(1.25, 'mm'), ends = "last", type = "open"), segment.size = 0.2, color = "red") +
  geom_point(data = towers_coords_df_acquired, aes(x = LOCATION_LONG, y = LOCATION_LAT), size = 1.5, color = "red") + 
  labs(title = "Global Dissimilarity of the Tower Network") + 
  theme_map(12)

# Histogram
unordered_hist <- ggplot() +
  geom_histogram(data = bioclim_stack_df_wdist, aes(x = closest_tower, fill = closest_tower), stat= "count") +
  coord_flip()

#=====================================================================================================

# Order the histogram
bioclim_stack_df_wdist_summarized <- bioclim_stack_df_wdist %>%
                                      group_by(closest_tower) %>%
                                        summarise(count = n()) %>%
                                          arrange(count)

# Factor
bioclim_stack_df_wdist$closest_tower <- factor(bioclim_stack_df_wdist$closest_tower, levels = bioclim_stack_df_wdist_summarized$closest_tower)

# Histogram
ordered_hist <- ggplot() +
  geom_histogram(data = bioclim_stack_df_wdist, aes(x = closest_tower, fill = closest_tower), stat = "count") +
  coord_flip() +
  xlab("Closest Tower") +
  ylab("Number of Pixels") +
  scale_y_continuous(expand = c(0, 0)) + 
  labs(title = "Count of the Representativeness of the Tower Network") +
  theme_hist(12)

#===================================================================================================

# Plot MDS of all towers (acquired and not acquired)
mds_plot <- ggplot() + 
  # Add the species labels (climatic variables)
  geom_text(data = species.scores, aes(x = NMDS1, y = NMDS2, label = species), alpha = 0.5, color = "black") +
  #geom_point(data = species.scores, aes(x = NMDS1, y = NMDS2, color = "black"), alpha = 0.5) +
  
  # Add the point markers (tower sites)
  geom_point(data = data.scores, aes(x = NMDS1, y = NMDS2, shape = as.factor(grp), color = as.factor(grp)), size = 2) + 
  
  # Add the site labels (tower ID's)
  geom_text_repel(data = data.scores, aes(x = NMDS1, y = NMDS2, label = towers_coords_df_all$SITE_ID, color = as.factor(grp)), size = 3, vjust = 0, hjust = 0, show.legend = FALSE) + 
  scale_colour_manual(values = c("No" = "red", "Yes" = "blue")) +
  coord_equal() +
  theme_mds(12)

#===================================================================================================

# Plot tower sites over just one of the climatic variables (tmp_avgr) Average Temperature
tmp_plot <- ggplot() +
  geom_tile(data = tmp_df, aes(x = x, y = y, fill = tmp_avgr)) +
  geom_point(data = towers_coords_df_acquired, aes(x = LOCATION_LONG, y = LOCATION_LAT), size = 1.5, color = "red") +
  labs(title = "Average Temperature", fill = "Celcius") +
  theme_single(12)

# Plot tower sites over just one of the climatic variables (pre_avgr) Average Preciptation
pre_plot <- ggplot() +
  geom_tile(data = pre_df, aes(x = x, y = y, fill = pre_avg)) +
  geom_point(data = towers_coords_df_acquired, aes(x = LOCATION_LONG, y = LOCATION_LAT), size = 1.5, color = "red") +
  labs(title = "Average Precipitation", fill = "mm") +
  theme_single(12)

#===================================================================================================

# mds_plot <- ggplot() +
#   geom_point(data = towers_mds, aes(x = points, fill = points), size = 1) +
#   scale_color_manual(values = c(towers_mds$))
# 
# plot(towers_mds$points, type = "n", pch = 20, cex = 3, col = adjustcolor("black", alpha = 0.3), xlab = "X", ylab = "Y") 
# text(towers_mds$points, labels = towers_coords_df_mds$SITE_ID, cex = 0.75)
