#======================================================================================================================================

# Custom map ggplot theme to make it super pretty to my own desire :)
theme_map <- function(base_size) {
  theme_bw(base_size = base_size) +
    theme(plot.title = element_text(face = 'bold', size = 16, hjust = 0.5),
          plot.background = element_rect(fill = 'white'),
          plot.margin = margin(5, 5, 5, 5, "mm"),
           
          # Panel
          panel.grid.minor = element_line(color = '#D0D0D0'),
          panel.grid.major = element_line(color = '#D0D0D0'),
          panel.background = element_rect(fill = '#F5F5F5'),

          # Axis
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          
          # Legend
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.position="bottom",
          legend.box = "horizontal",
          legend.text = element_text(size = 8),
          legend.spacing = unit(0, "mm"),
          legend.key.size = unit(5, "mm"))
}

#======================================================================================================================================

# Custom histogram ggplot theme
theme_hist <- function(base_size) {
  theme_bw(base_size = base_size) +
    theme(plot.title = element_text(face = 'bold', size = 16, hjust = 0.5),
          plot.background = element_rect(fill = 'white'),
          plot.margin = margin(5, 5, 5, 5, "mm"),
          
          # Panel
          panel.grid.minor = element_line(color = '#D0D0D0'),
          panel.grid.major = element_line(color = '#D0D0D0'),
          panel.background = element_rect(fill = '#F5F5F5'),
          
          # Axis
          axis.title = element_text(size = 12, face = 'bold'),
          
          # Legend
          legend.position = 'none')
}