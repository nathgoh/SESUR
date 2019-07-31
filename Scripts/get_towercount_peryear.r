
# Read in towers
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


#-------------------------------------------------------------------------------
### CALCULATE THE NUMBER OF TOWERS PER YEAR

# create empty dataframe
output = data.frame()

# loop through rows (each row = a site)
for (i in 1:nrow(towers_pts_df)){
  
  # Make temp variables
  site = towers_pts_df[i,]
  id = site[1,'SITE_ID']
  start = site[1,'FLUX_MEASUREMENTS_DATE_START']
  end = site[1,'FLUX_MEASUREMENTS_DATE_END']
  
  # Make table of tower x year  for each row
  e <- expand.grid(c(id), seq(start, end))
  
  # append rows to output
  output <- bind_rows(output, e)
 
  }

# Rename columns
names(output) <- c("SITE_ID", "YEAR")

# Get tower count per year
count_towers_per_years <- output %>%
                          group_by(YEAR) %>% 
                          summarise(n = n())

# Make lineplot of tower count
ggplot(count_towers_per_years) +
          geom_line(aes(x=YEAR, y=n)) +
          geom_point(aes(x=YEAR, y=n))