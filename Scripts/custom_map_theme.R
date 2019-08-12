#======================================================================================================================================

# Custom map ggplot theme to make it super pretty to my own desire :)
theme_map <- function(base_size = 10) {
  theme_bw(base_size = base_size) +
    theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0),
          plot.background = element_rect(fill = 'white'),
          
          # Panel
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = 'white'),
          panel.border = element_blank(),
          
          # Axis
          axis.text = element_blank(), 
          axis.title = element_blank(), 
          axis.line = element_line(colour = 'white'),
          axis.ticks = element_blank(),
          
          # Legend
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.position="bottom",
          legend.box = "horizontal",
          legend.text = element_text(size = 7),
          legend.spacing = unit(0, "mm"),
          legend.key.size = unit(3, "mm"))
}

#======================================================================================================================================