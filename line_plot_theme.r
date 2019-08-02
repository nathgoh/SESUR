
line_plot_theme <- 
  
  theme_bw() +
  theme(
    
      ### ALL TEXT (inherited everywhere)
      text = element_text(size=3, colour='black'),
      
      ### FACET STRIP
      strip.text = element_text(size=3, face='bold',hjust= 0, vjust = -0.5),
      strip.background = element_blank(),
      
      ### LEGEND
      legend.text = element_text(size = 3),
      legend.background = element_blank(),
      legend.key.size = unit(3, "mm"),
      legend.title=element_blank(),
      #legend.position = 'top',
      legend.direction = 'vertical',
      legend.justification = "left",
      
      
      ### AXES
      axis.line  = element_blank(), #element_line(colour = "black", size=0.01),
      axis.text  = element_text(size=3, colour='black'),
      axis.ticks = element_line(colour='black', size=0.05), 
      
      
      ### PANEL
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(), 
      panel.border = element_rect(colour = "black", fill=NA, size=0.08),
      panel.background = element_rect(colour = "black", fill=NA, size=0.08)) #element_blank())
 
