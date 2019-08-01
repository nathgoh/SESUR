#===================================================================================================

# Read tower data
towers <- read.csv('./Data/Towers/Sites_all_BAMS_15march2019_v2.csv', stringsAsFactors = F)

# Clean the tower data and pick out the main things we want from the dataset
towers <- towers %>% 
  filter(IGBP %in% c("WET", "CRO - Rice")) %>%
    filter(Data_acquired == "yes") %>%
      filter(SITE_ID != "--*") %>%
        # Count number of data years
        mutate(FLUX_MEASUREMENTS_DATE_END = ifelse(FLUX_MEASUREMENTS_DATE_END=="Present", 2019, FLUX_MEASUREMENTS_DATE_END)) %>%
          mutate(FLUX_MEASUREMENTS_DATE_END = as.numeric(FLUX_MEASUREMENTS_DATE_END), FLUX_MEASUREMENTS_DATE_START= as.numeric(FLUX_MEASUREMENTS_DATE_START))
          #mutate(dataperiod = FLUX_MEASUREMENTS_DATE_END-FLUX_MEASUREMENTS_DATE_START)

towers <- towers[, 1:10]

#===================================================================================================

# Fw = fraction wetland, max is that  
Fw_max <- raster('./Data/Wetland Map/Fw_max.tif')
Fw_max[Fw_max < 0.01] <- NA

bioclim_stack <- mask(bioclim_stack, Fw_max)

# Crop out Antartica, not relevant to the research
area_of_interest = extent(c(xmin = -180, xmax = 180, ymin = -58, ymax = 90))
bioclim_stack <- crop(bioclim_stack, area_of_interest)

bioclim_stack_df <- as.data.frame(as(bioclim_stack, "SpatialPixelsDataFrame")) 

#====================================================================================================
 

