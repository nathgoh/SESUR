# import libraries
library(raster)
library(dplyr)
library(tidyr)
library(ggplot2)

library(here)
here()



#===============================================================================
# Open the RDS stacks
lst_day <- readRDS("../Land Surface Temperature (LST)/Data/LST_Day_CMG_stack.rds")
lst_night <- readRDS("../Land Surface Temperature (LST)/Data/LST_Night_CMG_stack.rds")



# Subset (now it's only a single raster)
lst_day <- lst_day[[1]]
lst_night <- lst_night[[1]]


# reformat rasters  for graph in ggplot 
lst_day_df <- as.data.frame( as(lst_day, "SpatialPixelsDataFrame") )
names(lst_day_df) <- c("layer", "x", "y")





#===============================================================================

# Read in towers
towers <- read.csv("../Grid Extraction/Towers/BAMS_site_coordinates.csv")

# get coordinates
pts_coords <- cbind(towers$Longitude, towers$Latitude)
# make spatial object
towers_pts <- SpatialPointsDataFrame(pts_coords, towers)
towers_pts_df <- data.frame(towers_pts)


#==============================================================================

# Extract pixel values at towers
towers_pts_df$LST_Day <- raster::extract(lst_day, towers_pts)



#===============================================================================
# Map towers over the LST
ggplot() +
  geom_tile(data=lst_day_df, aes(x=x, y=y, fill=layer)) +
  geom_point(data=towers_pts_df, aes(x=Longitude, y= Latitude), color='red')


#===============================================================================
# cumulative eplot

ggplot() +
  stat_bin(data=towers_pts_df, aes(x=LST_Day, y=cumsum(..count..)/nrow(towers_pts_df)), color='red', bins=20, geom="step")+
  stat_bin(data=lst_day_df, aes(x=layer, y=cumsum(..count..)/nrow(lst_day_df)), color='blue', bins=20, geom="step")+

  xlab("LST Daytime (Deg Celcius)") +
  ggtitle("Distribution of pixels of LST Daytime")



#ggsave("../output/figures/histo_lst_daytime.png",
#       width=140, height=90, dpi=400, units="mm")
dev.off()
