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
          legend.position= c(0.1, 0.2),
          legend.box = "horizontal",
          legend.text = element_text(size = 8),
          legend.spacing = unit(0, "mm"),
          legend.key.size = unit(5, "mm"),
          legend.box.background = element_rect(color = "black"),
          legend.background = element_blank())
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
          axis.title = element_text(size = 10, face = 'bold'),
          
          # Legend
          legend.position = 'none')
}

#======================================================================================================================================

# Custom MDS ggplot theme
theme_mds <- function(base_size) {
  theme_bw(base_size = base_size) +
    theme(plot.title = element_text(face = 'bold', size = 16, hjust = 0.5),
          plot.background = element_rect(fill = 'white'),
          plot.margin = margin(5, 5, 5, 5, "mm"),
          
          # Panel
          panel.grid.minor = element_line(color = '#D0D0D0'),
          panel.grid.major = element_line(color = '#D0D0D0'),
          panel.background = element_rect(fill = '#F5F5F5'),
          
          # Axis
          axis.text.x = element_blank(),  
          axis.text.y = element_blank(), 
          axis.ticks = element_blank(),  
          axis.title.x = element_text(size=12), 
          axis.title.y = element_text(size=12),
          
          # Legend
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.position= c(0.1, 0.2),
          legend.box = "vertical",
          legend.text = element_text(size = 8),
          legend.spacing = unit(0, "mm"),
          legend.key.size = unit(5, "mm"),
          legend.box.background = element_rect(color = "black"),
          legend.background = element_blank())

}

#===================================================================================================

# Custom single climatic ggplot theme
theme_single <- function(base_size) {
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
          legend.title = element_text(size = 8, face = 'bold', hjust = 0.5),
          legend.key = element_blank(),
          legend.position= c(0.1, 0.2),
          legend.box = "vertical",
          legend.text = element_text(size = 8),
          legend.spacing = unit(0, "mm"),
          legend.key.size = unit(5, "mm"),
          legend.box.background = element_rect(color = "black"),
          legend.background = element_blank())
}