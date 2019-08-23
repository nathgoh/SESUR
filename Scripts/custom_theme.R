#======================================================================================================================================

# Add color dictionary which manually designates a color for each tower site
tower_colors = {
  'CA-SCB*': '#006400'
  "DE-SfN*": "#ee82ee"
  "DE-Zrk*":"	#008b00"
  "FI-Lom*": "#473c8b"
  "FI-Si2*": "#8b7765"
  "FI-Sii*": "#8b6969"
  "IT-Cas*": "#ee5c42"
  "JP-BBY*": "#ffff00"
  "JP-Mse*": "#9acd32"
  "KR-CRK": "#40e0d0"
  "MY-MLM*": "#8b1a1a"
  "NZ-Kop*": "#00ff7f"
  "RU-Ch2*": "#ee799f"
  "RU-Che*": "#ffbbff"
  "RU-SAM*": "#912cee"
  "RU-Vrk*": "#551a8b"
  "SE-Deg*": "#ffec8b"
  "SE-St1*": "#ffaeb9"
  "SE-Sto*": "#000080"
  "US-Atq*": "#fa8072"
  "US-Beo*": "cd9b1d"
  "US-Bes*": "#ff7f50"
  "US-HRA*": "#cd5b45"
  "US-HRC*": "#caff70"
  "US-Ics*": "#6e8b3d"
  "US-Ivo*": "#bcee68"
  "US-LA1*": "#adff2f"
  "US-LA2*": "#228b22"
  "US-Los*": "#ff7f00"
  "US-MRM*": "#8b4500"
  "US-Myb*": "#ff1493"
  "US-NC4*": "#8b0a50"
  "US-NGB*": "#9932cc"
  "US-NGC*": "#8a2be2"
  "US-Orv*": "#0000ff"
  "US-OWC*": "#00008b"
  "US-Sne*": "#53868b"
  "US-Srr*": "#528b8b"
  "US-StJ*": "#97ffff"
  "US-Tw1*": "#00ced1"
  "US-Tw4*": "#1e90ff"
  "US-Twt*": "#00bfff"
  "US-Uaf*": "#00688b"
  "US-WPT*": "#104e8b"
}

#======================================================================================================================================

# Custom map ggplot theme to make it super pretty to my own desire :)
theme_map <- function(base_size) {
  theme_bw(base_size = base_size) +
    theme(plot.title = element_text(face = 'bold', size = 26, hjust = 0.5),
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
          legend.text = element_text(size = 16),
          legend.spacing = unit(0, "mm"),
          legend.key.size = unit(5, "mm"),
          legend.box.background = element_rect(color = "black"),
          legend.background = element_blank())
}

#======================================================================================================================================

# Custom histogram ggplot theme
theme_hist <- function(base_size) {
  theme_bw(base_size = base_size) +
    theme(plot.title = element_text(face = 'bold', size = 26, hjust = 0.5),
          plot.background = element_rect(fill = 'white'),
          plot.margin = margin(5, 5, 5, 5, "mm"),
          
          # Panel
          panel.grid.minor = element_line(color = '#D0D0D0'),
          panel.grid.major = element_line(color = '#D0D0D0'),
          panel.background = element_rect(fill = '#F5F5F5'),
          
          # Axis
          axis.title = element_text(size = 16, face = 'bold'),
          
          # Legend
          legend.position = 'none')
}

#======================================================================================================================================

# Custom MDS ggplot theme
theme_mds <- function(base_size) {
  theme_bw(base_size = base_size) +
    theme(plot.title = element_text(face = 'bold', size = 26, hjust = 0.5),
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
          axis.title.x = element_text(size=16), 
          axis.title.y = element_text(size=16),
          
          # Legend
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.position= c(0.07, 0.07),
          legend.box = "vertical",
          legend.text = element_text(size = 16),
          legend.spacing = unit(0, "mm"),
          legend.key.size = unit(5, "mm"),
          legend.box.background = element_rect(color = "black"),
          legend.background = element_blank())

}

#===================================================================================================

# Custom single climatic ggplot theme
theme_single <- function(base_size) {
  theme_bw(base_size = base_size) +
    theme(plot.title = element_text(face = 'bold', size = 26, hjust = 0.5),
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
          legend.title = element_text(size = 16, face = 'bold', hjust = 0.5),
          legend.key = element_blank(),
          legend.position= c(0.1, 0.2),
          legend.box = "vertical",
          legend.text = element_text(size = 16),
          legend.spacing = unit(0, "mm"),
          legend.key.size = unit(5, "mm"),
          legend.box.background = element_rect(color = "black"),
          legend.background = element_blank())
}