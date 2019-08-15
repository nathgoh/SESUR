

#=====================================================================================================

# Read tower data
towers_all <- read.csv('./Data/Towers/Sites_all_BAMS_15march2019_v2.csv', stringsAsFactors = F)

# Clean the tower data and pick out the main things we want from the dataset (All towers whether acquired or not)
towers_all <- towers_all %>% 
    filter(IGBP %in% c("WET", "CRO - Rice")) %>%
    filter(SITE_ID != "--*") %>%
    filter(SITE_ID != "--") %>%
    filter(SITE_ID != "--**") %>%
    filter(FLUX_MEASUREMENTS_DATE_START != "--") %>%
    filter(FLUX_MEASUREMENTS_DATE_END != "--") %>%
  # Count number of data years
      mutate(FLUX_MEASUREMENTS_DATE_END = ifelse(FLUX_MEASUREMENTS_DATE_END=="Present", 2019, FLUX_MEASUREMENTS_DATE_END)) %>%
      mutate(FLUX_MEASUREMENTS_DATE_END = as.numeric(FLUX_MEASUREMENTS_DATE_END), FLUX_MEASUREMENTS_DATE_START= as.numeric(FLUX_MEASUREMENTS_DATE_START))
# mutate(dataperiod = FLUX_MEASUREMENTS_DATE_END-FLUX_MEASUREMENTS_DATE_START)

# Get towers of only those where we have acquired data from
towers_acquired <- towers_all %>%
  filter(Data_acquired == "yes")

towers_acquired <- towers_acquired[, 1:10]
towers_all <- towers_all[, 1:13]
