#=====================================================================================================

# Add wetland percentage (or fraction) to the bioclim stack
bioclim_stack_df_wdist = left_join(bioclim_stack_df_wdist, Fw_max_df, by = c('x', 'y'))

# Normalize the wetland percentage on a scale of 0 to 1
bioclim_stack_df_wdist <- bioclim_stack_df_wdist %>%
  mutate(Fw_max_scaled = (Fw_max - min(Fw_max)) / (max(Fw_max) - min(Fw_max)))

#=====================================================================================================

# Custom map theme
source('./Scripts/custom_theme.R')

# Make an outline of the countries for the plot
data(coastsCoarse)
map_outline_df <- fortify(coastsCoarse)

# Map clustering of regions
map_clusters <- ggplot() +
  geom_tile(data = bioclim_stack_df_k, aes(x = x, y = y, fill = as.factor(k))) +
  geom_text(data = towers_coords_df_acquired, aes(x = LOCATION_LONG, y =  LOCATION_LAT + 2, label = SITE_ID)) +
  geom_point(data = towers_coords_df_acquired, aes(x = LOCATION_LONG,  y = LOCATION_LAT), shape = 21, size = 2, stroke = 1.1, color = "black")

# Map minimum distances
min_distances <- ggplot() +
  geom_tile(data = bioclim_stack_df_wdist, aes(x = x, y = y, fill = min_dist)) +
  geom_tile(data = subset(bioclim_stack_df_wdist, min_dist > 0.4), aes(x = x, y = y), fill = "blue") +
  geom_text_repel(data = towers_coords_df_acquired, aes(x = LOCATION_LONG, y =  LOCATION_LAT, label = SITE_ID), point.padding = NA, arrow = arrow(angle = 45, length = unit(1.25, 'mm'), ends = "last", type = "open"), segment.size = 0.5, color = "black", size = 12) +
  geom_point(data = towers_coords_df_acquired, aes(x = LOCATION_LONG, y = LOCATION_LAT), size = 4.5, color = "black") + 
  geom_path(data = map_outline_df, aes(long, lat, group = group), color = 'grey20', size = 1) +
  scale_fill_distiller(palette = "PuRd", limits = c(0.4, 0), trans = "reverse") + 
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
  xlab("Tower ID") +
  ylab("Number of Pixels") +
  scale_y_continuous(expand = c(0, 0)) + 
  labs(title = "Count of the Representativeness of the Tower Network") +
  scale_fill_manual(values = tower_colors) +
  theme_hist(12)

# Map towers over the climatic variables
map_towers <- ggplot() +
  geom_tile(data = bioclim_stack_df_wdist, aes(x = x, y = y, fill = as.factor(closest_tower), alpha = Fw_max)) +
  geom_text_repel(data = towers_coords_df_acquired, aes(x = LOCATION_LONG, y =  LOCATION_LAT, label = SITE_ID), point.padding = NA, arrow = arrow(angle = 45, length = unit(1.25, 'mm'), ends = "last", type = "open"), segment.size = 0.5, size = 12) +
  geom_point(data = towers_coords_df_acquired, aes(x = LOCATION_LONG,  y =  LOCATION_LAT), size = 4.5, color = "black") +
  geom_path(data = map_outline_df, aes(long, lat, group = group), color = 'grey20', size = 1) +
  labs(title = "Representativeness of the Tower Network") +
  scale_fill_manual(values = tower_colors) +
  theme_map(12)

#===================================================================================================

# Plot MDS of all towers (acquired and not acquired)
mds_plot <- ggplot() + 
  # Add the species labels (climatic variables)
  geom_text(data = species.scores, aes(x = NMDS1, y = NMDS2, label = species), alpha = 0.5, color = "black", size = 9) +
  #geom_point(data = species.scores, aes(x = NMDS1, y = NMDS2, color = "black"), alpha = 0.5) +
  
  # Add the point markers (tower sites)
  geom_point(data = data.scores, aes(x = NMDS1, y = NMDS2, shape = as.factor(grp), color = as.factor(grp)), size = 4.5) + 
  
  # Add the site labels (tower ID's)
  geom_text_repel(data = data.scores, aes(x = NMDS1, y = NMDS2, label = towers_coords_df_all$SITE_ID, color = as.factor(grp)), size = 10, vjust = 0, hjust = 0, show.legend = FALSE) + 
  labs(title = "Relative Dissimiarity of All Towers") + 
  scale_colour_manual(values = c("No" = "red", "Yes" = "blue")) +
  coord_equal() +
  theme_mds(12)

#===================================================================================================

# Plot tower sites over just one of the climatic variables (tmp_avgr) Average Temperature
tmp_plot <- ggplot() +
  geom_tile(data = tmp_df, aes(x = x, y = y, fill = tmp_avgr)) +
  geom_point(data = towers_coords_df_acquired, aes(x = LOCATION_LONG, y = LOCATION_LAT), size = 6, color = "blue") +
  # scale_color_gradient(low = "yellow", high = "red") +
  scale_fill_distiller(palette = "YlOrRd", trans = "reverse") + 
  labs(title = "Average Temperature", fill = "Celcius") +
  theme_single(12)

# Plot tower sites over just one of the climatic variables (pre_avgr) Average Preciptation
pre_plot <- ggplot() +
  geom_tile(data = pre_df, aes(x = x, y = y, fill = pre_avg)) +
  geom_point(data = towers_coords_df_acquired, aes(x = LOCATION_LONG, y = LOCATION_LAT), size = 4, color = "red") +
  labs(title = "Average Precipitation", fill = "mm") +
  theme_single(12)

#===================================================================================================

# Draw the cumulative plot, visual representation of KS-Test
# Comparison of the global climatic variables against those that are obtained where the towers
# are located
source('./Scripts/cumulative_plot.R')

#===================================================================================================

# Save settings for the single climatic variable maps
ggsave(plot = tmp_plot, file = "tmp_plot.png", path = "../SESUR/Outputs", width = 800, height = 600, dpi = 800, units = "mm")
ggsave(plot = pre_plot, file = "pre_plot.png", path = "../SESUR/Outputs", width = 800, height = 600, dpi = 800, units = "mm")

# Save settings for the all the climatic variable maps
ggsave(plot = map_towers, file = "map_towers.png", path = "../SESUR/Outputs", width = 1000, height = 700, dpi = 600, units = "mm")
ggsave(plot = min_distances, file = "min_distances.png", path = "../SESUR/Outputs", width = 1000, height = 700, dpi = 600, units = "mm")

# Save settings for the MDS plot
ggsave(plot = mds_plot, file = "mds_plot.png", path = "../SESUR/Outputs", width = 1000, height = 600, dpi = 600, units = "mm")

# Save settings for the ordered histogram
ggsave(plot = ordered_hist, file = "histogram.png", path = "../SESUR/Outputs", width = 600, height = 800, dpi = 800, units = "mm")






