library(ggplot2)
library(dplyr)
library(tidyr)
library(raster)
library(here)
here()

#========================================================================================

# Read in RDS stack data
lst_day <- readRDS("../Land Surface Temperature (LST)/Data/LST_Day_CMG_stack.rds")
lst_night <- readRDS("../Land Surface Temperature (LST)/Data/LST_Night_CMG_stack.rds")

# Get a subset of the RDS raster data, one layer
lst_day <- mean(lst_day, na.rm = TRUE)
lst_night <- mean(lst_night, na.rm = TRUE)

area_of_interest = extent(c(xmin = -180, xmax = 180, ymin = -58, ymax = 90))
lst_night <- crop(lst_night, area_of_interest)
lst_day <- crop(lst_day, area_of_interest)

# Stack the lst_day and lst_night
lst = stack(lst_day, lst_night)
names(lst) <- c("lst_day", "lst_night")


# Reformat the rasters so for use with ggplot
lst_df <- as.data.frame(as(lst, "SpatialPixelsDataFrame"))
names(lst_df) <- c("lst_day", "lst_night", "long", "lat")

#========================================================================================

# Read tower data
flux_towers <- read.csv("../Grid Extraction/Towers/BAMS_site_coordinates.csv")

# Get coordinates from the tower data
coords <- cbind(flux_towers$Longitude, flux_towers$Latitude)

# Make spatial objects
towers_coords <- SpatialPointsDataFrame(coords, flux_towers)
towers_coords_df <- data.frame(towers_coords)

# Extract pixel values at towers
towers_coords_df<- cbind(towers_coords_df, raster::extract(lst, towers_coords))

#========================================================================================

# Draw histogram
#ggplot() +
#  geom_histogram(data = lst_day_subset_df, aes(x = lst_day), binwidth = 5) +
#  xlab("LST Daytime (Celcius)") +
#  ggtitle("Distribution of pixels of LST Daytime")
#
#ggsave("../Land Surface Temperature (LST)/Output/histogram_LST_daytime.jpeg",
#       width = 140, height = 90, dpi = 400, units = "mm")

#========================================================================================

# Cumulative plot
ggplot() +
  stat_bin(data=towers_coords_df, aes(x=lst_day, y=cumsum(..count..)/nrow(towers_coords_df)), color='red', bins=20, geom="step")+
  stat_bin(data=lst_df, aes(x=lst_day, y=cumsum(..count..)/nrow(lst_df)), color='blue', bins=20, geom="step") +
  xlab("LST Daytime (Celcius)") +
  ggtitle("Distribution of pixels of LST Daytime")

ggplot() +
  stat_bin(data=towers_coords_df, aes(x=lst_night, y=cumsum(..count..)/nrow(towers_coords_df)), color='red', bins=20, geom="step")+
  stat_bin(data=lst_df, aes(x=lst_night, y=cumsum(..count..)/nrow(lst_df)), color='blue', bins=20, geom="step") +
  xlab("LST Nighttime (Celcius)") +
  ggtitle("Distribution of pixels of LST Nighttime")

#========================================================================================

#KS Test
ks.test(towers_coords_df$lst_day, lst_df$lst_day, "pnorm")
ks.test(towers_coords_df$lst_night, lst_df$lst_night, "pnorm")

#========================================================================================d

# Loop through individual towers
for (j in 1:nrow(towers_coords_df)) {
  
  # Select a tower
  tower <- towers_coords_df[j,]
  
  # Calculate eucledian distance to all other pixels for each tower
  dist <- data.frame(((lst_df$lst_day - tower$lst_day)^2 + (lst_df$lst_night - tower$lst_night)^2)^(1/2))
  names(dist) <- tower$ID
  
  # Append distance column to data frame
  lst_df <- cbind(lst_df, dist)
  
}

#========================================================================================

# Find the minimum distance in terms of LST at each pixel on earth from the network of towers
lst_df$min_dist <- apply(lst_df[,5:54], MARGIN = 1, FUN = min, na.rm = TRUE)

# Find the tower ID that has the minimum distance
lst_df$closest_tower <- as.character(c(apply(lst_df[,5:54], MARGIN = 1, FUN = which.min)))

#========================================================================================

# Map towers over the LST
ggplot() +
  geom_tile(data = lst_df, aes(x = long, y = lat, fill = as.factor(closest_tower))) +
  geom_text(data = towers_coords_df, aes(x = Longitude, y = Latitude, label = ID)) +
  geom_point(data = towers_coords_df, aes(x = Longitude, y = Latitude), color = "yellow") 

# Histogram
ggplot() +
  geom_histogram(data = lst_df, aes(x = closest_tower, fill = closest_tower), stat= "count") +
  coord_flip()
  






