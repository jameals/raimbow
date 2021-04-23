# check out weekly DCRB vms, whale, and risk data
# dependencies: prep_data_for_scenario_df_function.R

# 033120

# check out the fish ticket informed VMS data a bit. also see "Make confidential data summarized...Rmd"

# based on slack convo with mary 032420, use agency_code to get CA records. "agency code was the safest way to identify the state of landing since it matched with port group / port of landing geographic locations"
with(dcrb_vms_tix_analysis, table(agency_code, STATE))

# check to see if the blue whale grid cells and the VMS grid cells line up
# length(which(BLWH_5km_year_mo$GRID5KM_ID %in% dcrb_ca_vms_tix_analysis_TripInfo$GRID5KM_ID == FALSE))
# length(which(BLWH_5km_year_mo$GRID5KM_ID %in% dcrb_ca_vms_tix_analysis_TripInfo$GRID5KM_ID == TRUE))

### inspecting the weekly data

con_df_weekly_2009_2018_5km_CA <- read_rds("~/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_2009-2018_fishtix_humpback_blue_whales_grids.RDS")


# need to decide on best approach for rescaling risk. try log transform

# 033020. log10 transform, no plus group seems adequate. hump data are similarly skewed, we can just go with that. return to code above, and change to log10 for DCRBM VMS pings. still need to consider normalization to put VMS and hump data on same scale

# will need to normalize the whale and pings data for risk calculations because pings range to >600 per grid cell but whale values generally fall 0-1
# https://medium.com/@swethalakshmanan14/how-when-and-why-should-you-normalize-standardize-rescale-your-data-3f083def38ff
# https://stackoverflow.com/questions/5665599/range-standardization-0-to-1-in-r
#https://stackoverflow.com/questions/5468280/scale-a-series-between-two-points/5468527#5468527

# the dist of num pings is highly skewed (fat tail on R). most cells have <25 pings per week
with(con_df_weekly_2009_2018_5km_CA, table(Num_DCRB_VMS_pings)) # few instances of >1 5km cell with pings >100

# make some histograms

# pick up here. thinking to use straight blwh data, scale hump and vms to 0-1

### whales
# straight hump output
con_df_weekly_2009_2018_5km_CA %>% 
  ggplot(aes(x=H_Avg_Abund))+
  geom_density(fill='#33638DFF', alpha=0.8)+
  #geom_histogram(fill='#33638DFF', alpha=0.8, aes(y=after_stat(ndensity)))+
  #geom_histogram(fill='#33638DFF', alpha=0.8,  binwidth=0.05, aes(y = (..count..)/sum(..count..)))+
  labs(x='H_Avg_Abund per 5km grid cell', y='Frequency')
ggsave('H_Avg_Abund_density_plot.png', h=8,w=6)

# log10 hump output
con_df_weekly_2009_2018_5km_CA %>% 
  mutate(
    log10_H_Avg_Abund = log10(H_Avg_Abund + 1)
  ) %>%
  ggplot(aes(log10_H_Avg_Abund))+
  geom_density(fill='#33638DFF', alpha=0.8) +
  #geom_density(fill='#33638DFF', alpha=0.8, aes(y = after_stat((..count..)/sum(..count..))))+
  #geom_histogram(fill='#33638DFF', alpha=0.8, binwidth=0.02, aes(y = (..count..)/sum(..count..)))+
  labs(x='log10 H_Avg_Abund per 5km grid cell', y='Frequency')

# straight blwh output
con_df_weekly_2009_2018_5km_CA %>% 
  ggplot(aes(Blue_occurrence_mean))+
  geom_density(fill='#33638DFF', alpha=0.8)+
  #geom_histogram(fill='#33638DFF', alpha=0.8,  binwidth=0.05, aes(y = (..count..)/sum(..count..)))+
  labs(x='Blue_occurrence_mean per 5km grid cell', y='Frequency')

# log10 Blue_occurrence_mean output
con_df_weekly_2009_2018_5km_CA %>% 
  mutate(
    log10_Blue_occurrence_mean = log10(Blue_occurrence_mean + 1)
  ) %>%
  ggplot(aes(log10_Blue_occurrence_mean))+
  geom_density(fill='#33638DFF', alpha=0.8)+
  #geom_histogram(fill='#33638DFF', alpha=0.8, binwidth=0.02, aes(y = (..count..)/sum(..count..)))+
  labs(x='log10 Blue_occurrence_mean per 5km grid cell', y='Frequency')

# normalized Blue_occurrence_mean output. looks the same as Blue_occurrence_mean
con_df_weekly_2009_2018_5km_CA %>% 
  ggplot(aes(normalized_Blue_occurrence_mean))+
  geom_density(fill='#33638DFF', alpha=0.8)+
  #geom_histogram(fill='#33638DFF', alpha=0.8, binwidth=0.02, aes(y = (..count..)/sum(..count..)))+
  labs(x='normalized Blue_occurrence_mean per 5km grid cell')

# straight DRCB VMS output
con_df_weekly_2009_2018_5km_CA %>% 
  ggplot(aes(Num_DCRB_VMS_pings, stat(density)))+
  geom_histogram(fill='#33638DFF', alpha=0.8, binwidth=5)+
  labs(x='DCRB VMS pings per 5km grid cell')

# straight DRCB VMS output, zoomed into small values
con_df_weekly_2009_2018_5km_CA %>% 
  ggplot(aes(Num_DCRB_VMS_pings, stat(density)))+
  geom_histogram(fill='#33638DFF', alpha=0.8, binwidth=2)+
  labs(x='DCRB VMS pings per 5km grid cell') +
  xlim(0,25)

# normalized_Num_DCRB_VMS_pings. not any better
con_df_weekly_2009_2018_5km_CA %>% 
  ggplot(aes(normalized_Num_DCRB_VMS_pings))+
  geom_density(fill='#33638DFF')+
  labs(x='Normalized DCRB VMS pings per 5km grid cell')

# create a log10(x+1)
con_df_weekly_2009_2018_5km_CA %>%
  mutate(
    log10_Num_DCRB_VMS_pings = log10(Num_DCRB_VMS_pings + 1)
  ) %>%
  ggplot(aes(log10_Num_DCRB_VMS_pings))+
  geom_histogram(fill='#33638DFF', alpha=0.8, binwidth=0.1)+
  #geom_density(fill='#33638DFF')+
  labs(x='log10 DCRB VMS pings per 5km grid cell')

# create a cubetransform
con_df_weekly_2009_2018_5km_CA %>%
  mutate(
    cubetransformed_Num_DCRB_VMS_pings = (Num_DCRB_VMS_pings)^(1/3)
  ) %>%
  ggplot(aes(cubetransformed_Num_DCRB_VMS_pings))+
  geom_density(fill='#33638DFF')+
  labs(x='cubetransformed DCRB VMS pings per 5km grid cell')

# create a plus group and plot
con_df_weekly_2009_2018_5km_CA %>% 
  mutate(
    Num_DCRB_VMS_pings_plus_group = ifelse(Num_DCRB_VMS_pings >=100, 100, Num_DCRB_VMS_pings)
  ) %>%
  ggplot(aes(Num_DCRB_VMS_pings_plus_group))+
  geom_density(fill='#33638DFF')+
  labs(x='DCRB VMS pings with 100+ group per 5km grid cell')

# create a log10(x+1) plus group and plot
con_df_weekly_2009_2018_5km_CA %>% 
  mutate(
    Num_DCRB_VMS_pings_plus_group = ifelse(Num_DCRB_VMS_pings >=100, 100, Num_DCRB_VMS_pings),
    log10_Num_DCRB_VMS_pings_plus_group = log10(Num_DCRB_VMS_pings_plus_group + 1)
  ) %>%
  ggplot(aes(log10_Num_DCRB_VMS_pings_plus_group))+
  geom_density(fill='#33638DFF')+
  labs(x='log10 DCRB VMS pings with 100+ group per 5km grid cell')

# create a cube transformed plus group and plot
con_df_weekly_2009_2018_5km_CA %>% 
  mutate(
    Num_DCRB_VMS_pings_plus_group = ifelse(Num_DCRB_VMS_pings >=100, 100, Num_DCRB_VMS_pings),
    cubetransformed_Num_DCRB_VMS_pings_plus_group = (Num_DCRB_VMS_pings_plus_group)^(1/3)
  ) %>%
  ggplot(aes(cubetransformed_Num_DCRB_VMS_pings_plus_group))+
  geom_density(fill='#33638DFF')+
  labs(x='cubetransformed DCRB VMS pings with 100+ group per 5km grid cell')


# check to see whether whale values are mostly NAs
# blwh
sum(is.na(con_df_weekly_2009_2018_5km_CA$Blue_occurrence_mean))/nrow(con_df_weekly_2009_2018_5km_CA) # 0%

# humps
sum(is.na(con_df_weekly_years_5km_CA$H_Avg_Abund))/nrow(con_df_weekly_years_5km_CA) # 6%
View(con_df_weekly_years_5km_CA[which(is.na(con_df_weekly_years_5km_CA$H_Avg_Abund)==TRUE),]) # only 8 points prior to 2019 have NA values. need to add back 2019 hump predictions
sum(is.na(con_df_weekly_2009_2018_5km_CA$H_Avg_Abund))/nrow(con_df_weekly_2009_2018_5km_CA) # 0.01%


# make some maps of where NA values occur
grd <- sf::read_sf("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/5x5 Grid/regions_master_final_lamb.shp")

# background map
states <- st_as_sf(map("state", fill = TRUE, plot=FALSE)) %>% 
  st_transform(st_crs(grd))

# humps
# 2018 and NAs only
# this join takes a few min
grd_con_df_weekly_years_5km_CA_na_2010_hump <- grd %>%
  left_join(con_df_weekly_years_5km_CA) %>%
  filter(year == 2010) %>%
  filter(is.na(H_Avg_Abund)) %>%
  mutate(
    empty_cell = 1
  )

write_rds(grd_con_df_weekly_years_5km_CA_na_2018_hump, "~/Documents/RAIMBOW/Processed Data/VMS/grd_con_df_weekly_years_5km_CA_na_2018_hump.RDS")

grd_con_df_weekly_years_5km_CA_na_2018_hump <- read_rds("~/Documents/RAIMBOW/Processed Data/VMS/grd_con_df_weekly_years_5km_CA_na_2018_hump.RDS")

vms_bbox <- st_bbox(grd_con_df_weekly_years_5km_CA_na_2018_hump %>% 
                      st_as_sf()
)

# plot where hump 2018 NAs occur
p_hump_na_2018 <- ggplot() + 
  geom_sf(data=grd_con_df_weekly_years_5km_CA_na_2018_hump, 
          aes(fill=empty_cell)
  ) +
  geom_sf(data=states,col=NA,fill='gray50') +
  ggtitle("2018 hump NAs") +
  coord_sf(xlim=c(vms_bbox[1],vms_bbox[3]),ylim=c(vms_bbox[2],vms_bbox[4]))
p_hump_na_2018
ggsave(here::here('tradeoffs','hump_na_2018.png'),p_hump_na_2018,h=8,w=6)

# blues
# 2014 and NAs only
# this join takes a few min
grd_con_df_weekly_years_5km_CA_na_2014 <- grd %>%
  left_join(con_df_weekly_years_5km_CA) %>%
  filter(year == 2014) %>%
  filter(is.na(Blue_occurrence_mean)) %>%
  mutate(
    empty_cell = 1
  )

#filter(yr == 2017 & mth == 01) %>%
#filter(STATE == "CA")

write_rds(grd_con_df_weekly_years_5km_CA_na_2014, "~/Documents/RAIMBOW/Processed Data/VMS/grd_con_df_weekly_years_5km_CA_na_2014.RDS")

grd_con_df_weekly_years_5km_CA_na_2014 <- read_rds("~/Documents/RAIMBOW/Processed Data/VMS/grd_con_df_weekly_years_5km_CA_na_2014.RDS")

vms_bbox <- st_bbox(grd_con_df_weekly_years_5km_CA_na_2014 %>% 
                      st_as_sf()
)

# plot where blwh 2014 NAs occur
p_blwh_na_2014 <- ggplot() + 
  geom_sf(data=grd_con_df_weekly_years_5km_CA_na_2014, 
          aes(fill=empty_cell)
  ) +
  geom_sf(data=states,col=NA,fill='gray50') +
  coord_sf(xlim=c(vms_bbox[1],vms_bbox[3]),ylim=c(vms_bbox[2],vms_bbox[4]))
p_blwh_na_2014
ggsave(here::here('tradeoffs','blwh_na_2014.png'),p_blwh_na_2014,h=8,w=6)

# 2018 and NAs only
# this join takes a few min
grd_con_df_weekly_years_5km_CA_na_2018 <- grd %>%
  left_join(con_df_weekly_years_5km_CA) %>%
  filter(year == 2018) %>%
  filter(is.na(Blue_occurrence_mean)) %>%
  mutate(
    empty_cell = 1
  )

#filter(yr == 2017 & mth == 01) %>%
#filter(STATE == "CA")

write_rds(grd_con_df_weekly_years_5km_CA_na_2018, "~/Documents/RAIMBOW/Processed Data/VMS/grd_con_df_weekly_years_5km_CA_na_2018.RDS")

grd_con_df_weekly_years_5km_CA_na_2018 <- read_rds("~/Documents/RAIMBOW/Processed Data/VMS/grd_con_df_weekly_years_5km_CA_na_2018.RDS")

vms_bbox <- st_bbox(grd_con_df_weekly_years_5km_CA_na_2018 %>% 
                      st_as_sf()
)

# plot where blwh 2018 NAs occur
p_blwh_na_2018 <- ggplot() + 
  geom_sf(data=grd_con_df_weekly_years_5km_CA_na_2018, 
          aes(fill=empty_cell)
  ) +
  geom_sf(data=states,col=NA,fill='gray50') +
  coord_sf(xlim=c(vms_bbox[1],vms_bbox[3]),ylim=c(vms_bbox[2],vms_bbox[4]))
p_blwh_na_2018
ggsave(here::here('tradeoffs','blwh_na_2018.png'),p_blwh_na_2018,h=8,w=6)

# 2018 and valid blwh values only
# this join takes a few min
grd_con_df_weekly_years_5km_CA_nona_2018 <- grd %>%
  left_join(con_df_weekly_years_5km_CA) %>%
  filter(year == 2018) %>%
  filter(is.na(Blue_occurrence_mean)==FALSE) %>%
  mutate(
    empty_cell = 1
  )

write_rds(grd_con_df_weekly_years_5km_CA_nona_2018, "~/Documents/RAIMBOW/Processed Data/VMS/grd_con_df_weekly_years_5km_CA_nona_2018.RDS")

grd_con_df_weekly_years_5km_CA_nona_2018 <- read_rds("~/Documents/RAIMBOW/Processed Data/VMS/grd_con_df_weekly_years_5km_CA_nona_2018.RDS")

vms_bbox <- st_bbox(grd_con_df_weekly_years_5km_CA_nona_2018 %>% 
                      st_as_sf()
)

# plot where blwh 2018 non NAs occur
p_blwh_nona_2018 <- ggplot() + 
  geom_sf(data=grd_con_df_weekly_years_5km_CA_nona_2018, 
          aes(fill=empty_cell)
  ) +
  geom_sf(data=states,col=NA,fill='gray50') +
  ggtitle("2018 blwh not NAs") +
  coord_sf(xlim=c(vms_bbox[1],vms_bbox[3]),ylim=c(vms_bbox[2],vms_bbox[4]))
p_blwh_nona_2018
ggsave(here::here('tradeoffs','blwh_nona_2018.png'),p_blwh_nona_2018,h=8,w=6)



