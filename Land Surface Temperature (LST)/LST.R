library(ggplot2)
library(dplyr)
library(tidyr)
library(raster)
library(here)
here()

# Read in RDS stack data
lst_day <- readRDS("../Land Surface Temperature (LST)/Data/LST_Day_CMG_stack.rds")
lst_night <- readRDS("../Land Surface Temperature (LST)/Data/LST_Night_CMG_stack.rds")

# Get a subset of the RDS stack data
lst_day_subset <- lst_day[[1]]
lst_night_subset <- lst_night[[1]]

# Read tower data
flux_towers <- read.csv("../Grid Extraction/Towers/BAMS_site_coordinates.csv")
