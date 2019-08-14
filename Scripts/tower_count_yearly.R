#===================================================================================================

# Empty data frame
output <- data.frame()

# Loop through individual rows (one row = one unique site)
for (row in 1:nrow(towers_coords_df_acquired)) {
  
  # Temp variables
  tower_site = towers_coords_df_acquired[row,]
  tower_id = tower_site[1, 'SITE_ID']
  start_year = tower_site[1, 'FLUX_MEASUREMENTS_DATE_START']
  end_year = tower_site[1, 'FLUX_MEASUREMENTS_DATE_END']
  
  # Make table of towers at n year for each row
  tower_table <- expand.grid(c(tower_id), seq(start_year, end_year))
  
  # Append the table to the data frame
  output <- bind_rows(output, tower_table)
}

# Rename columns 
names(output) <- c('SITE_ID', 'YEAR')

# Get tower count per year
tower_count_yearly <- output %>%
  group_by(YEAR) %>%
    summarise(n = n())

# Graph the lineplot for visual representation of tower network over time
plot_tower_count <- ggplot(tower_count_yearly) +
  geom_line(aes(x = YEAR, y = n)) +
  geom_point(aes(x = YEAR, y = n), size = 1, color = "black") +
  ylab("Number of towers") +
  xlab("Year")

#===================================================================================================

output_biome <- data.frame()

# Split the tower data table based on biome
split_tower_list <- split(towers_coords_df_acquired, towers_coords_df_acquired$BIOME)

# Loop through the list
for (l in split_tower_list) {
  for (row in 1:nrow(l)) {
    
    # Temp variables
    tower_site = towers_coords_df_acquired[row,]
    tower_id = tower_site[1, 'SITE_ID']
    start_year = tower_site[1, 'FLUX_MEASUREMENTS_DATE_START']
    end_year = tower_site[1, 'FLUX_MEASUREMENTS_DATE_END']
    biome = tower_site[1, 'BIOME']
    
    # Make table of towers at n year for each row
    tower_table <- expand.grid(c(tower_id), seq(start_year, end_year))
    tower_table$BIOME <- biome
    
    output_biome <- bind_rows(output_biome, tower_table)
  }
}

# Rename columns 
names(output_biome) <- c('SITE_ID', 'YEAR', 'BIOME')

# Get tower count per year
tower_count_yearly_biome <- output_biome %>%
    group_by(YEAR, BIOME) %>%
      summarise(n = n())

#===================================================================================================

gap_fill <- expand.grid(YEAR = tower_count_yearly_biome$YEAR, BIOME = tower_count_yearly_biome$BIOME)

# Fill in the missing tower count for a certain year in a certain biome
tower_count_yearly_biome <- full_join(tower_count_yearly_biome, gap_fill, by = c('YEAR' = 'YEAR', 'BIOME' = 'BIOME')) %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
    arrange(YEAR, BIOME) %>%
      group_by(YEAR, BIOME) %>%
        summarise(n = max(n))

#===================================================================================================

# Graph the lineplot for visual representation of tower network over time
plot_tower_count_biome <- ggplot(tower_count_yearly_biome) +
  geom_area(aes(x = YEAR, y = n, fill = BIOME), position = 'stack') +
  ylab("Number of towers") +
  xlab("Year") +
  scale_x_continuous(expand = c(0,0), breaks = seq(min(tower_count_yearly_biome$YEAR), max(tower_count_yearly_biome$YEAR), 5)) +
  scale_y_continuous(expand = c(0,0))
  
  
