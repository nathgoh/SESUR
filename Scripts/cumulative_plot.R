#=====================================================================================================

# Function to draw cumulative plot
cumulative <- function(towers_acquired, plot_df, plot_data) {
  cumulative_plot <- ggplot() +
    stat_ecdf(data = towers_acquired, aes_string(x = plot_data), color = 'blue', geom = "step") +
    stat_ecdf(data = plot_df, aes_string(x = plot_data), color = 'red', geom = "step") +
    xlab("") + ylab("Cumulative Frequency") +
    labs(title = "KS-Test") + 
    facet_wrap(~bioclim_var) +
    theme_ks(12)
  
  return(cumulative_plot)
}

cumulative_plot <- cumulative(towers_coords_df2_acquired, bioclim_stack_df2, "value")

