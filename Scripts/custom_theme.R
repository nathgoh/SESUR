#======================================================================================================================================

# Add color dictionary which manually designates a color for each tower site
tower_colors = list(
  'CA-SCB*'= '#00ff99',
  "MY-MLM*"= "#000066",
  "US-LA1*"= "#00ffff",
  "US-Los*"= "#0000ff",
  "US-Myb*"= "#464646",
  "DE-SfN*"= "#6600cc",
  "DE-Zrk*"= "#ff8000",
  "FI-Lom*"= "#00e600",
  "FI-Si2*"= "#ff00aa",
  "FI-Sii*"= "#8b6969",
  "IT-Cas*"= "#ff33ff",
  "JP-BBY*"= "#b30000",
  "JP-Mse*"= "#660066",
  "KR-CRK"= "#cc6600",
  "NZ-Kop*"= "#ff00ff",
  "RU-Ch2*"= "#cc00cc",
  "RU-Che*"= "#b366ff",
  "RU-SAM*"= "#ff33bb",
  "RU-Vrk*"= "#994d00",
  "SE-Deg*"= "#00b300",
  "SE-St1*"= "#008000",
  "SE-Sto*"= "#000080",
  "US-Atq*"= "#ff9933",
  "US-Beo*"= "#800000",
  "US-Bes*"= "#ff7f50",
  "US-HRA*"= "#ffff00",
  "US-HRC*"= "#caff70",
  "US-Ics*"= "#8c1aff",
  "US-Ivo*"= "#1aff1a",
  "US-LA2*"= "#228b22",
  "US-MRM*"= "#990099",
  "US-NC4*"= "#e60000",
  "US-NGB*"= "#9932cc",
  "US-NGC*"= "#004d00",
  "US-Orv*"= "#ff1a1a",
  "US-OWC*"= "#663300",
  "US-Sne*"= "#53868b",
  "US-Srr*"= "#cccc00",
  "US-StJ*"= "#cc00cc",
  "US-Tw1*"= "#00ced1",
  "US-Tw4*"= "#1e90ff",
  "US-Twt*"= "#00bfff",
  "US-Uaf*"= "#ff4d4d",
  "US-WPT*"= "#104e8b")


#======================================================================================================================================

# Custom map ggplot theme to make it super pretty to my own desire
theme_map <- function(base_size) {
  theme_bw(base_size = base_size) +
    theme(plot.title = element_text(face = 'bold', size = 56, hjust = 0.5),
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
          legend.position= c(0.15, 0.25),
          legend.box = "horizontal",
          legend.text = element_text(size = 36),
          legend.spacing = unit(12, "mm"),
          legend.key.size = unit(16, "mm"),
          legend.box.background = element_rect(color = "black"),
          legend.background = element_blank())
}

#======================================================================================================================================

# Custom histogram ggplot theme
theme_hist <- function(base_size) {
  theme_bw(base_size = base_size) +
    theme(plot.title = element_text(face = 'bold', size = 56, hjust = 0.5),
          plot.background = element_rect(fill = 'white'),
          plot.margin = margin(5, 5, 5, 5, "mm"),
          
          # Panel
          panel.grid.minor = element_line(color = '#D0D0D0'),
          panel.grid.major = element_line(color = '#D0D0D0'),
          panel.background = element_rect(fill = '#F5F5F5'),
          
          # Axis
          axis.title = element_text(size = 40, face = 'bold'),
          axis.text = element_text(size = 36),
          
          # Legend
          legend.position = 'none')
}

#======================================================================================================================================

# Custom MDS ggplot theme
theme_mds <- function(base_size) {
  theme_bw(base_size = base_size) +
    theme(plot.title = element_text(face = 'bold', size = 56, hjust = 0.5),
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
          axis.title.x = element_text(size=36, face = 'bold'), 
          axis.title.y = element_text(size=36, face = 'bold'),
          
          # Legend
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.position= c(0.09, 0.09),
          legend.box = "vertical",
          legend.text = element_text(size = 36),
          legend.spacing = unit(8, "mm"),
          legend.key.size = unit(18, "mm"),
          legend.box.background = element_rect(color = "black"),
          legend.background = element_blank())

}

#===================================================================================================

# Custom single climatic ggplot theme
theme_single <- function(base_size) {
  theme_bw(base_size = base_size) +
    theme(plot.title = element_text(face = 'bold', size = 56, hjust = 0.5),
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
          legend.title = element_text(size = 36, face = 'bold', hjust = 0.5),
          legend.key = element_blank(),
          legend.position= c(0.1, 0.2),
          legend.box = "vertical",
          legend.text = element_text(size = 36),
          legend.spacing = unit(10, "mm"),
          legend.key.size = unit(15, "mm"),
          legend.box.background = element_rect(color = "black"),
          legend.background = element_blank())
}

#==================================================================================================

# Custom KS-Test ggplot theme
theme_ks <- function(base_size) {
  theme_bw(base_size = base_size) +
    theme(plot.title = element_text(face = 'bold', size = 56, hjust = 0.5),
          plot.background = element_rect(fill = 'white'),
          plot.margin = margin(5, 5, 5, 5, "mm"),
          
          # Panel
          panel.grid.minor = element_line(color = '#D0D0D0'),
          panel.grid.major = element_line(color = '#D0D0D0'),
          panel.background = element_rect(fill = '#F5F5F5'))
}