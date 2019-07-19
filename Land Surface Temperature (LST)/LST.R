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

# Get a subset of the RDS stack data
lst_day_subset <- lst_day[[1]]
lst_night_subset <- lst_night[[1]]

# Reformat the rasters so for use with ggplot
lst_day_subset_df <- as.data.frame(as(lst_day_subset, "SpatialPixelsDataFrame"))
names(lst_day_subset_df) <- c("lst_day", "long", "lat")

#========================================================================================

# Read tower data
flux_towers <- read.csv("../Grid Extraction/Towers/BAMS_site_coordinates.csv")

# Get coordinates from the tower data
coords <- cbind(flux_towers$Latitude, flux_towers$Longitude)

# Make spatial objects
towers_coords <- SpatialPointsDataFrame(coords, flux_towers)
towers_coords_df <- data.frame(towers_coords)

# Extract pixel values at towers
towers_coords_df$LST_Day <- raster::extract(lst_day_subset, towers_coords)

#========================================================================================

# Map towers
ggplot() +
  geom_tile(data = lst_day_subset_df, aes(x = long, y = lat, fill = lst_day)) +
  geom_point(data = towers_coords_df, aes(x = Longitude, y = Latitude, color = "yellow"))

#========================================================================================

# Draw histogram
ggplot() +
  geom_histogram(data = lst_day_subset_df, aes(x = lst_day), binwidth = 5) +
  xlab("LST Daytime (Celcius)") +
  ggtitle("Distribution of pixels of LST Daytime")

ggsave("../Land Surface Temperature (LST)/Output/histogram_LST_daytime.jpeg",
       width = 140, height = 90, dpi = 400, units = "mm")

#========================================================================================

# Cumulative plot
ggplot() +
  stat_bin(data=towers_coords_df, aes(x=LST_Day, y=cumsum(..count..)/nrow(towers_coords_df)), color='red', bins=20, geom="step")+
  stat_bin(data=lst_day_subset_df, aes(x=lst_day, y=cumsum(..count..)/nrow(lst_day_subset_df)), color='blue', bins=20, geom="step")

#========================================================================================

#KS Test
ks.test(towers_coords_df$LST_Day, lst_day_subset_df$lst_day, "pnorm")

# KS Visual Plot
sample1 <- towers_coords_df
sample2 <- lst_day_subset_df
group <- c(rep("sample1", length(sample1)), rep("sample2", length(sample2)))
data <- data.frame(values = c(sample1, sample2), group = group)

