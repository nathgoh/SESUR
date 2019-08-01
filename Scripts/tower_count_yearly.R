#============================================================================

# Empty data frame
output <- data.frame()

# Loop through individual rows (one row = one unique site)
for (row in 1:nrow(towers_coords_df)) {
  
  # Temp variables
  tower_site = towers_coords_df[row,]
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
