# /----------------------------------------------------------------------------#
#/    REad in libraries
library(raster)
library(dplyr)
library(tidyr)
library(ggplot2)


# /----------------------------------------------------------------------------#
#/    get polygon of continent outline                                     -----
library(rworldmap)
data(coastsCoarse)
coastsCoarse_df <- fortify(coastsCoarse)
coastsCoarse_df <- arrange(coastsCoarse_df, id)


# /----------------------------------------------------------------------------#
#/    Get predicted flux grid
# REPLACE THESE GRIDS WITH BIOCLIMATIC VARIABLES
# File format glossary:  brick and stack  are piles of grids (also known as raster).
#                        .nc  is a NetCDF

flux <- brick('../output/results/grid/upch4_med_nmolm2sec.nc')# , varname="upch4")
# replace negative (sink) values
flux[flux < 0] = NA

# Get date list (RDS is a R data structure).
# Change this to another format; because RDS is bad.
parseddates <- readRDS('../output/results/parsed_dates.rds')


# /----------------------------------------------------------------------------#
#/     Get tower locations                                               -------
#  CRS is a Coordinate system; that's an attribute of spatial data.

bams_towers <- read.csv("../Grid Extraction/Towers/BAMS_site_coordinates.csv")
xy <- bams_towers[,c(3,4)]

bams_towers <- data.frame(SpatialPointsDataFrame(coords = xy, data = bams_towers))
# proj4string = crs(flux)))


# /----------------------------------------------------------------------------#
#/    Get predicted grids

# reformat rasters  for graph in ggplot 
flux_df <- as(flux[[1]], "SpatialPixelsDataFrame")
flux_df <- as.data.frame(flux_df)
names(flux_df) <- c("layer", "x", "y")


# /----------------------------------------------------------------------------#
#/    Make a map of tower over background of bioclimatic grids             ----
ggplot() +
  
  # Flux grid
  geom_tile(data=flux_df, aes(x=x, y=y, fill=layer)) +
  
  # Coastline
  geom_path(data=coastsCoarse_df, aes(long, lat, group=group), color='black', size=0.07) +
  
  # Tower sites
  geom_point(data=bams_towers, aes(Longitude, Latitude), 
             color='black', fill= "green", shape=21,  size=2.4, stroke=0.1) +
  
  scale_x_continuous(limits=c(-180, 180))+
  scale_y_continuous(limits=c(-60, 90))+

  scale_fill_gradient(low="#fffcba", high="#ad0000", 
                      trans="log", 
                      breaks=c(10^-20, 0.001, 0.005, 0.01, 0.03), 
                      labels=c(0, 0.001, 0.005, 0.01, 0.03),
                      limits=c(10^-20, 0.03)) +
  
  guides(fill = guide_colorbar(nbin=8, raster=F, barwidth=15, frame.colour=c("black"),
                               frame.linewidth=1, ticks.colour="black",  direction="horizontal",
                               title = expression(paste("Tg CH"[4]*" month"^-1)))) +

  coord_equal() +
  theme(	legend.position="bottom", #c(0.15, 0.3),
         plot.margin = unit( c(-2, -3, -2, -3) , "mm"))



# Extract values of grids
e <- as.data.frame(extract(flux, xy)) 
#e <- bind_cols(e, bams_towers)

