#=====================================================================================================

# Add wetland percentage (or fraction) to the bioclim stack
bioclim_stack_df_wdist = left_join(bioclim_stack_df_wdist, Fw_max_df, by = c('x', 'y'))

# Normalize the wetland percentage on a scale of 0 to 1
bioclim_stack_df_wdist <- bioclim_stack_df_wdist %>%
  mutate(Fw_max_scaled = (Fw_max - min(Fw_max)) / (max(Fw_max) - min(Fw_max)))

#=====================================================================================================

# Map towers over the bioclimatic variables
map_towers <- ggplot() +
  geom_tile(data = bioclim_stack_df_wdist, aes(x = x, y = y, fill = as.factor(closest_tower), alpha = Fw_max_scaled)) +
  geom_text_repel(data = towers_coords_df, aes(x = LOCATION_LONG, y =  LOCATION_LAT + 2, label = SITE_ID)) +
  geom_point(data = towers_coords_df, aes(x = LOCATION_LONG,  y =  LOCATION_LAT), shape = 21, size = 2, stroke = 1.1, color = "black")

# Map clustering of regions
map_clusters <- ggplot() +
  geom_tile(data = bioclim_stack_df_k, aes(x = x, y = y, fill = as.factor(k))) +
  geom_text(data = towers_coords_df, aes(x = LOCATION_LONG, y =  LOCATION_LAT + 2, label = SITE_ID)) +
  geom_point(data = towers_coords_df, aes(x = LOCATION_LONG,  y = LOCATION_LAT), shape = 21, size = 2, stroke = 1.1, color = "black")

# Map minimum distances
min_distances <- ggplot() +
  geom_tile(data = bioclim_stack_df_wdist, aes(x = x, y = y, fill = min_dist, alpha = Fw_max_scaled)) +
  geom_text(data = towers_coords_df, aes(x = LOCATION_LONG, y = LOCATION_LAT + 2, label = SITE_ID)) +
  geom_point(data = towers_coords_df, aes(x = LOCATION_LONG, y = LOCATION_LAT), shape = 21, size = 2, stroke = 1.1, color = "black")

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
  theme(legend.position = "none") +
  xlab("Closest Tower") +
  ylab("Number of Pixels") +
  scale_y_continuous(expand = c(0, 0))
