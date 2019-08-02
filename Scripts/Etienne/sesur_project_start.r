# TODO: 
# Mask land surface by wetland area
# Fix the memory limit; 
# Include the other predictors
# plot of minimum distance
# Appplying some clustering


# Next Wednesday:
# - Make heatmap equivalent of Kumar table 2;  Ecoregion vs time,  Ecoregion vs Wetland type
# - Make the time series plots


# EF:  
#  - Upload other bioclimatic predictors.
#  - Rescale all predictors to 0-1 before distance calculation.

# import libraries
library(raster)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
here()

#===============================================================================

# List the files in the directory
l <- list.files(path = "../data/bioclim", pattern = ".tif", full.names = TRUE)

# Create empty stack, to which we'll append
bioclim_stack <- stack()

# Read raster, and add to stack.
s <- raster::stack(l)

# Aggregate raster to coarser image resolution
s <- aggregate(s, fact=6, fun=mean)

###  Get wetland mask
Fw_max <- raster("../data/swampsglwd/Fw_max.tif")
Fw_max[Fw_max < 0.01] <- NA  # Should be greater than a % 



s <- mask(s, Fw_max)

# Crop the stack, to remove antarctica
aoi = extent(c(xmin=-180, xmax=180, ymin=-58, ymax=90))
s <- crop(s, aoi)



# reformat rasters  for graph in ggplot
sdf <- as.data.frame( as(s, "SpatialPixelsDataFrame") )




#===============================================================================

# Read in towers
# towers <- read.csv("../data/towers/BAMS_site_coordinates.csv")
towers <- read.csv("../data/tower_sites/Sites_all_BAMS_15march2019_v2.csv", stringsAsFactors = F) 

# FILTER
towers <- towers %>%
  
  filter(IGBP %in% c("WET","CRO - Rice")) %>%
  # Could turn this filtering off to see a 'what if we had all towers scenario?"
  filter(Data_acquired == "yes") %>%
  filter(SITE_ID != "--*") %>%
  # Count Nb. of data years
  mutate(FLUX_MEASUREMENTS_DATE_END = ifelse(FLUX_MEASUREMENTS_DATE_END=="Present", 2019, FLUX_MEASUREMENTS_DATE_END)) %>%
  mutate(FLUX_MEASUREMENTS_DATE_END = as.numeric(FLUX_MEASUREMENTS_DATE_END), 
         FLUX_MEASUREMENTS_DATE_START= as.numeric(FLUX_MEASUREMENTS_DATE_START))

towers <- towers[,1:10]


# get coordinates
pts_coords <- cbind(towers$LOCATION_LONG, towers$LOCATION_LAT)

# make spatial object
towers_pts <- SpatialPointsDataFrame(pts_coords, towers)
crs(towers_pts) <- crs(s)

towers_pts_df <- data.frame(towers_pts)


#==============================================================================

# Extract pixel values at towers
towers_pts_df <- cbind(towers_pts_df, raster::extract(s, towers_pts))


towers_pts_df<- towers_pts_df %>% filter(!is.na(wc2.0_bio_5m_05))
#===============================================================================
#  Cumulative plot

# Randomly select rows
# sdf <- sdf[sample(nrow(sdf), 5000), ]


# Change table format from wide to long; the reason for this is to plot 
sdf2 <- sdf %>% gather(key=bioclim_var, value=value, wc2.0_bio_5m_01:wc2.0_bio_5m_11)
towers_pts_df2 <- towers_pts_df %>% gather(key=bioclim_var, value=value, wc2.0_bio_5m_01:wc2.0_bio_5m_11)



ggplot() +
  stat_ecdf(data=towers_pts_df2, aes(x=value), color='red', geom="step")+
  stat_ecdf(data=sdf2, aes(x=value), color='blue',geom="step")+

  xlab("") + ylab("Cumulative frequency") + 
    
  facet_wrap(~ bioclim_var)



ggsave("../output/figures/cumul_plot_histogram_bioclim.png",
       width=180, height=160, dpi=600, units="mm")
dev.off()



#===============================================================================
# calculate distance of one tower to the grid


library(proxy)
d <- proxy::dist(x = sdf[c(1:5)],  # the columns of bioclim in gridded
                 y = towers_pts_df[c(14:18)],   # the columns of bioclim in towers
                 method="Euclidean",
                 upper=FALSE,
                 diag=FALSE)


# Convert 'crossdist' obj to dataframe
d <- as.data.frame(as.matrix.data.frame(d))

# Add tower IDs as column names
names(d) <- towers_pts_df$ID

# Append distance column to df
sdf_wdist <- cbind(sdf, d)



#===============================================================================

# Get minimum distance at each pixel on earth, from the network of towers.
sdf_wdist$min_dist <- apply(sdf_wdist[,8:43] , 1, min, na.rm=TRUE)

# Get which tower ID has the minimum distance
sdf_wdist$closest_tower <-  (c(apply(sdf_wdist[,8:43] , 1, which.min)))

# convrt numeric back to tower ID
sdf_wdist$closest_tower <- colnames(sdf_wdist[,8:43])[sdf_wdist$closest_tower]

# Remove the unneeded columns
sdf_wdist <- sdf_wdist %>% select(x,y, min_dist, closest_tower)


#===============================================================================
# Map towers over the LST

#sdf_wdist <- sdf_wdist %>% filter(min_dist <= 10)

# /-----------------------------------------------------------------------------
#/   Map of min-dist
ggplot() +
  geom_tile(data=sdf_wdist, aes(x=x, y=y, fill=min_dist)) +
  
  # text labes
  geom_text(data=towers_pts_df, aes(x=LOCATION_LONG, y= LOCATION_LAT + 3 , label=SITE_ID)) +
  geom_point(data=towers_pts_df, aes(x=LOCATION_LONG, y= LOCATION_LAT), 
             shape=21, size=3, stroke=1.1, color='black') +
  scale_fill_distiller(palette = 'YlOrRd')



# /----------------------------------------------------------------------------
#/     Map of closest tower
ggplot() +
  geom_tile(data=sdf_wdist, aes(x=x, y=y, fill=as.factor(closest_tower))) +
  
  # text labes
  geom_text(data=towers_pts_df, aes(x=LOCATION_LONG, y= LOCATION_LAT + 3 , label=SITE_ID)) +
  geom_point(data=towers_pts_df, aes(x=LOCATION_LONG, y= LOCATION_LAT, fill=SITE_ID), 
             shape=21, size=3, stroke=1.1, color='black')
  #theme(legend.position = "none")
  

# /-----------------------------------------------------------------------------
#/    Make sorted histogram

# Calculating the pixel count for each closts-tower
sdf_wdist_summarized <- sdf_wdist %>%
                        group_by(closest_tower) %>%
                        summarize(count = n()) %>%
                        arrange(count)

# Impose an order for the bars
sdf_wdist$closest_tower <- factor(sdf_wdist$closest_tower, levels= sdf_wdist_summarized$closest_tower)

ggplot(sdf_wdist)+
  geom_histogram(aes(x=closest_tower, fill=closest_tower), stat="count") +
  coord_flip() +
  xlab("Closest Tower") +
  ylab("Number of pixels") +
  theme(legend.position = 'none') +
  scale_y_continuous(expand=c(0,0))







#===============================================================================
# K-means

k <- kmeans(sdf[, 1:5], 10, nstart = 50)

SpatialPixels()

sdf$k <- k$cluster

# Convert df back to raster format (2lines)
r <- SpatialPixelsDataFrame(sdf[c("x", "y")], data = sdf)
kr <- raster(r, layer= ncol(sdf))

# Extract the cluster ID of each tower.
towers_pts_df$k <- raster::extract(kr, towers_pts)


#-------------------------------------------
ggplot(sdf)+
  geom_point(aes(x=wc2.0_bio_5m_01, y=wc2.0_bio_5m_05, color=as.factor(k)))


# /-----------------------------------------------------------------------------
#/   Map of clusters
ggplot() +
  geom_tile(data=sdf, aes(x=x, y=y, fill=as.factor(k))) +
  # text labes
  geom_text(data=towers_pts_df, aes(x=Longitude, y= Latitude + 3 , label=ID)) +
  geom_point(data=towers_pts_df, aes(x=Longitude, y= Latitude), 
             shape=21, size=3, stroke=1.1, color='black')
