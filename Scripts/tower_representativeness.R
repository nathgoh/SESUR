#=====================================================================================================

# Filter out towers in the same pixel (bioclimatically identical)
towers_coords_df_all <- towers_coords_df_all %>%
  distinct(wc2.0_bio_5m_01,tmp_avgr, .keep_all = TRUE)

towers_coords_df_all_mds <- normalize(towers_coords_df_all[, 14: (13 + num_bio)], method = "range", range = c(0, 1), margin = 2L, on.constant = "quiet")

vare.mds <- metaMDS(comm = towers_coords_df_all_mds, distance = "euclidean", engine = "isoMDS", wascores = TRUE, autotransform = TRUE)

towers_coords_df_all <- towers_coords_df_all %>%
  left_join(towers_acquired, by = "SITE_ID") %>%
    mutate(acquired = ifelse(!is.na(COUNTRY.y), 1, 0)) 

data.scores <- as.data.frame(scores(vare.mds))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$grp <- towers_coords_df_all$acquired  #  add the grp variable created earlier
head(data.scores)  #look at the data

species.scores <- as.data.frame(scores(vare.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data

ggplot() + 
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5, color = "black") +  # add the species labels
  #geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2, shape=as.factor(grp),colour=as.factor(grp)),size=4) + # add the point markers
  geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=towers_coords_df_all$SITE_ID, color = as.factor(grp)),size=4,vjust=0,hjust=0) +  # add the site labels
  #scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=12), # remove x-axis labels
        axis.title.y = element_text(size=12), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())


# Calculate eucledian distance between all the towers
distance_tower <- proxy::dist(x = towers_coords_df_all[c(14: (13 + num_bio))], y = towers_coords_df_all[c(14: (13 + num_bio))], method = "Euclidean",
                              diag = FALSE, upper = FALSE)

# Reformat to be usable for analysis
distance_tower <- as.data.frame(as.matrix.data.frame(distance_tower))
names(distance_tower) <- towers_coords_df_all$SITE_ID
towers_coords_df_wdist_all <- cbind(towers_coords_df_all, distance_tower)

#=====================================================================================================

# Calculate number of columns
num_col = ncol(towers_coords_df_wdist_all)

# Convert into long format, filter, then wide format
towers_coords_df2_wdist_all <- towers_coords_df_wdist_all %>%
  gather(key = "tower_to", value = "dist", (num_bio + 14):num_col) %>%
  filter(SITE_ID != tower_to) %>%
  spread(key = "tower_to", value = "dist", fill = NA)

# Find the minimum distance between the towers
towers_coords_df2_wdist_all$min_dist <- apply(towers_coords_df2_wdist_all[, (num_bio + 14):num_col], MARGIN = 1, FUN = min, na.rm = TRUE)

#=====================================================================================================

# Convert data frame into a matrix
towers_coords_matrix_wdist_all <- as.matrix(towers_coords_df_wdist_all[1:61, (num_bio + 14):num_col])

# Calculates mds
towers_all = isoMDS(towers_coords_matrix_wdist_all)
plot(towers_all$points, type = "n", pch = 20, cex = 3, col = adjustcolor("black", alpha = 0.3), xlab = "X", ylab = "Y") 
text(towers_all$points, labels = towers_coords_df_all$SITE_ID, cex = 0.75)

#=====================================================================================================