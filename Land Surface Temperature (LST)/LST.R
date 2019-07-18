library(ggplot2)
library(dplyr)
library(tidyr)
library(raster)
library(here)
here()



# ======================================================================================
# Read in RDS stack data
lst_day <- readRDS("../Land Surface Temperature (LST)/Data/LST_Day_CMG_stack.rds")
lst_night <- readRDS("../Land Surface Temperature (LST)/Data/LST_Night_CMG_stack.rds")

# Get a subset of the RDS stack data
lst_day <- lst_day[[1]]
lst_night <- lst_night[[1]]

# Reformat data for use in ggplot
lst_day_df <- as.data.frame( as(lst_day, "SpatialPixelsDataFrame") )
names(lst_day_df) <- c("LST_day", "Longitutde", "Latitude")



# ======================================================================================
# Read tower data
towers <- read.csv("../Grid Extraction/Towers/BAMS_site_coordinates.csv")

# Get tower coordinates 
towers_coords <- cbind(towers$Longitude, towers$Latitude)

# Turn into spatial object 
towers_points <- SpatialPointsDataFrame(towers_coords, towers)
towers_points_df <- data.frame(towers_points)

# Extract pixel values at towers
towers_points_df$LST_Day <- raster::extract(lst_day, towers_points)



# ======================================================================================
# Map towers over the LST
ggplot() +
  geom_tile(data = lst_day_df, aes(x = Longitutde, y = Latitude, fill = LST_day)) +
  geom_point(data = towers_points_df, aes(x = Longitude, y = Latitude), color = 'yellow')

# Plot histogram
ggplot() +
  geom_histogram(data = lst_day_df, aes(x = LST_day), binwidth = 5) +
  xlab("LST Daytime (Deg Celcius)") +
  ggtitle("Distribution of Pixels of LST Daytime")

# Save histogram 
ggsave("../Land Surface Temperature (LST)/Output/histogram_LST_daytime.jpeg",
       width=140, height=90, dpi=400, units="mm")

# Cumulative plot
ggplot() +
  stat_bin(data=towers_points_df, aes(x=LST_Day, y=cumsum(..count..)/nrow(towers_points_df)), color='red', bins=20, geom="step")+
  stat_bin(data=lst_day_df, aes(x=LST_day, y=cumsum(..count..)/nrow(lst_day_df)), color='blue', bins=20, geom="step")+

dev.off()

