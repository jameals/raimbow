## Mapping functions for WDFW logbook data 
# creating depth distribution plots

library(tidyverse)
library(lubridate)
library(here)
library(sf)
library(raster)
library(fasterize)
select <- dplyr::select
library(rnaturalearth)
library(viridis)
library(magrittr)
library(gridExtra)
library(nngeo)

# ggplot theme
plot_theme <-   theme_minimal()+
  theme(text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=14),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        axis.text.x.bottom = element_text(angle=45),
        legend.position = c(0.8,0.3),
        title=element_text(size=12),
        legend.title = element_text(size=10),
        panel.grid.major = element_line(color="gray50",linetype=3))
theme_set(plot_theme)
options(dplyr.summarise.inform = FALSE)

###########################################################################################
#Plotting (simulated) pots by depth bin


#from raw logs, need to run place_traps function to create the traps along each line and get their depth
#running place_traps on the full logs dataframe takes really long, so ran it once and saved it as traps_sf_for_all_logs_and_seasons_2009-2019.rds and uploaded it to Kiteworks
#traps_sf_all_logs <- read_rds(here::here('wdfw', 'data','traps_sf_for_all_logs_and_seasons_2009-2019.rds'))
#the depth plotting could also be done using the new df traps_g_license_logs_2013_2019.rds - output of script 1
traps_sf_all_logs <- read_rds(here::here('wdfw', 'data','traps_g_license_logs_2013_2019.rds'))

#jameal
traps_sf_all_logs <- read_rds('/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Logbook-VMS/WA logbooks - mapping for CP/traps_g_license_logs_2013_2019.rds')

glimpse(traps_sf_all_logs)

logs_all <- traps_sf_all_logs %>% 
  st_set_geometry(NULL) %>% 
  mutate(m=month(SetDate),d=day(SetDate),period=ifelse(d<=15,1,2)) %>% 
  mutate(m = month.name[m], period = ifelse(period==1,"first half","second half")) %>% 
  mutate(season = str_sub(SetID,1,9)) %>% 
  mutate(season_month = paste0(season,"_",m))
#dataset has highly negative values (~ -30000) to denote port and bay areas - remove those. 
#Also note that place_traps function already removes depths >200m as crab fishing at deeper depths is not likely
logs_all %<>% filter(depth > -1000)

logs_all %<>% mutate(m = factor(m, levels = c('December','January','February','March','April','May','June','July','August','September','October','November')))

glimpse(logs_all)


###########################################################################################
#JAMEAL'S DEPTH PLOTTING

# quick check on embayments
range(logs_all[(which(logs_all$is_port_or_bay==TRUE)),'depth'])

# summarise the # pots fished by grid cell for each 2 wk interval. use 2 wk interval because that is the temporal scale at which the place_traps() function was applied
traps_grd_depth_interval <- logs_all %>%
  group_by(GRID5KM_ID, depth, season_month, period) %>%
  summarise(
    total_pots = sum(PotsFished, na.rm=TRUE)
  ) %>%
  ungroup() 

glimpse(traps_grd_depth_interval)

# summarise across all seasons and intervals for each depth
# drop duplicated grid cells for now
traps_grd_depth <- traps_grd_depth_interval %>%
  filter(
    GRID5KM_ID != 117310 &
      GRID5KM_ID != 117311 &
      GRID5KM_ID != 117640 & 
      GRID5KM_ID != 120280 &
      GRID5KM_ID != 120610 &
      GRID5KM_ID != 120940 &
      GRID5KM_ID != 122258 &
      GRID5KM_ID != 122259 &
      GRID5KM_ID != 122588
  ) %>%
  group_by(depth) %>%
  summarise(
  mean_pots = mean(total_pots, na.rm=TRUE)
  ) %>%
  ungroup()
glimpse(traps_grd_depth)

# have a quick look at pots vs depth
ggplot(traps_grd_depth, aes(x=depth, y=mean_pots)) +
  geom_point()

# add a perc_traps and cumperc to the df
traps_grd_depth <- traps_grd_depth %>%
  mutate(
    perc_traps = 100*(mean_pots/sum(mean_pots))
  ) %>%
  # calculate cumulative sum
  arrange(-depth) %>% 
  mutate(cumperc=cumsum(perc_traps))

# cumulative dist plot
p1 <- traps_grd_depth %>% 
  ggplot(aes(x=-depth,y=cumperc))+
  geom_line()+
  #geom_hline(aes(yintercept = 90), colour="blue", linetype=2)+
  labs(x="Depth (m)",y="Cumulative % Traps") +
  ggtitle("Distribution of crab pots by depth,\nall years and seasons")
p1

ggsave(here::here('wdfw','plots','Cumulative distribution of traps by depth all years and seasons.png'), p1)

# summarise for winter v spring seasons and intervals for each depth
# drop duplicated grid cells for now

spsum <- c("May","June","July","August","September")
traps_grd_depth_season <- logs_all %>%
  mutate(
    win_or_spsum = case_when(
      m %in% spsum ~ "SprSum",
      TRUE ~ "Winter"
    )
  ) %>%
  group_by(GRID5KM_ID, depth, win_or_spsum, season_month, period) %>%
  summarise(
    total_pots = sum(PotsFished, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  filter(
    GRID5KM_ID != 117310 &
      GRID5KM_ID != 117311 &
      GRID5KM_ID != 117640 & 
      GRID5KM_ID != 120280 &
      GRID5KM_ID != 120610 &
      GRID5KM_ID != 120940 &
      GRID5KM_ID != 122258 &
      GRID5KM_ID != 122259 &
      GRID5KM_ID != 122588
  ) %>%
  group_by(depth, win_or_spsum) %>%
  summarise(
    mean_pots = mean(total_pots, na.rm=TRUE)
  ) %>%
  ungroup()
glimpse(traps_grd_depth_season)

# have a quick look at pots vs depth
ggplot(traps_grd_depth_season, aes(x=depth, y=mean_pots, colour = win_or_spsum)) +
  geom_point()

# add a perc_traps and cumperc to the df
traps_grd_depth_season_perc <- traps_grd_depth_season %>%
  # group_by(win_or_spsum) %>%
  # group_split() %>%
  split(list(.$win_or_spsum)) %>%
  purrr::map_dfr(
    .f = function(x) { x %>% mutate(perc_traps = 100*(mean_pots/sum(mean_pots)))}
  ) %>%
  # calculate cumulative sum
  split(list(.$win_or_spsum)) %>%
  purrr::map_dfr(
    .f = function(x) { x %>% arrange(-depth) %>% 
        mutate(cumperc=cumsum(perc_traps))}
  )

# quick check
glimpse(traps_grd_depth_season_perc) 
traps_grd_depth_season_perc %>% group_by(win_or_spsum) %>% summarise(perc_traps = sum(perc_traps),maxcumperc = max(cumperc))

# cumulative dist plot
p2 <- traps_grd_depth_season_perc %>% 
  ggplot(aes(x=-depth,y=cumperc, colour = win_or_spsum))+
  geom_line()+
  #geom_hline(aes(yintercept = 90), colour="blue", linetype=2)+
  labs(x="Depth (m)",y="Cumulative % Traps") +
  ggtitle("Distribution of crab pots by depth,\nall years in Dec-Apr vs May-Sep") + 
  theme(legend.position = ("top"),legend.title=element_blank())
p2

ggsave(here::here('wdfw','plots','Cumulative distribution of traps by depth all years, win v sprsum.png'), p2)


###########################################################################################
#LEENA'S VERSION OF JAMEAL'S DEPTH PLOTTING

# quick check on embayments
range(logs_all[(which(logs_all$is_port_or_bay==TRUE)),'depth'])

# summarise the # pots fished by grid cell for each 2 wk interval. use 2 wk interval because that is the temporal scale at which the place_traps() function was applied
traps_grd_depth_interval <- logs_all %>%
  #group_by(GRID5KM_ID, depth, season_month, period) %>% #Don't group by grid
  group_by(depth, season_month, period) %>%
  summarise(
    total_pots = sum(PotsFished, na.rm=TRUE)
  ) %>%
  ungroup() 

glimpse(traps_grd_depth_interval)

# summarise across all seasons and intervals for each depth
# drop duplicated grid cells for now
traps_grd_depth <- traps_grd_depth_interval %>%
  #filter( #No need to filter for these
   # GRID5KM_ID != 117310 &
    #  GRID5KM_ID != 117311 &
    #  GRID5KM_ID != 117640 & 
    #  GRID5KM_ID != 120280 &
    #  GRID5KM_ID != 120610 &
    #  GRID5KM_ID != 120940 &
    #  GRID5KM_ID != 122258 &
    #  GRID5KM_ID != 122259 &
    #  GRID5KM_ID != 122588
  #) %>%
  group_by(depth) %>%
  summarise(
    #mean_pots = mean(total_pots, na.rm=TRUE) #work in sums, not means
    total_pots = sum(total_pots, na.rm=TRUE)
  ) %>%
  ungroup()
glimpse(traps_grd_depth)

# have a quick look at pots vs depth
#ggplot(traps_grd_depth, aes(x=depth, y=mean_pots)) + #work in sums, not means
ggplot(traps_grd_depth, aes(x=depth, y=total_pots)) +
  geom_point()

# add a perc_traps and cumperc to the df
traps_grd_depth <- traps_grd_depth %>%
  mutate(
    perc_traps = 100*(total_pots/sum(total_pots))
  ) %>%
  # calculate cumulative sum
  arrange(-depth) %>% 
  mutate(cumperc=cumsum(perc_traps))

# cumulative dist plot
p1 <- traps_grd_depth %>% 
  ggplot(aes(x=-depth,y=cumperc))+
  geom_line()+
  #geom_hline(aes(yintercept = 90), colour="blue", linetype=2)+
  labs(x="Depth (m)",y="Cumulative % Traps") +
  ggtitle("Distribution of crab pots by depth,\nall years and seasons")
p1

ggsave(here::here('wdfw','plots','Cumulative distribution of traps by depth all years and seasons_LEENA VERSION.png'), p1)

# summarise for winter v spring seasons and intervals for each depth
# drop duplicated grid cells for now

spsum <- c("May","June","July","August","September")
traps_grd_depth_season <- logs_all %>%
  mutate(
    win_or_spsum = case_when(
      m %in% spsum ~ "SprSum",
      TRUE ~ "Winter"
    )
  ) %>%
  #group_by(GRID5KM_ID, depth, win_or_spsum, season_month, period) %>% #Don't group by grid
  group_by(depth, win_or_spsum, season_month, period) %>%
  summarise(
    total_pots = sum(PotsFished, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  # filter( #No need to filter for these
  #   GRID5KM_ID != 117310 &
  #     GRID5KM_ID != 117311 &
  #     GRID5KM_ID != 117640 & 
  #     GRID5KM_ID != 120280 &
  #     GRID5KM_ID != 120610 &
  #     GRID5KM_ID != 120940 &
  #     GRID5KM_ID != 122258 &
  #     GRID5KM_ID != 122259 &
  #     GRID5KM_ID != 122588
  # ) %>%
  group_by(depth, win_or_spsum) %>%
  summarise(
    #mean_pots = mean(total_pots, na.rm=TRUE) #work in sums, not means
    total_pots = sum(total_pots, na.rm=TRUE)
  ) %>%
  ungroup()
glimpse(traps_grd_depth_season)

# have a quick look at pots vs depth
#ggplot(traps_grd_depth_season, aes(x=depth, y=mean_pots, colour = win_or_spsum)) +
ggplot(traps_grd_depth_season, aes(x=depth, y=total_pots, colour = win_or_spsum)) +
  geom_point()

# add a perc_traps and cumperc to the df
traps_grd_depth_season_perc <- traps_grd_depth_season %>%
  # group_by(win_or_spsum) %>%
  # group_split() %>%
  split(list(.$win_or_spsum)) %>%
  purrr::map_dfr(
    #.f = function(x) { x %>% mutate(perc_traps = 100*(mean_pots/sum(mean_pots)))}
    .f = function(x) { x %>% mutate(perc_traps = 100*(total_pots/sum(total_pots)))}
  ) %>%
  # calculate cumulative sum
  split(list(.$win_or_spsum)) %>%
  purrr::map_dfr(
    .f = function(x) { x %>% arrange(-depth) %>% 
        mutate(cumperc=cumsum(perc_traps))}
  )

# quick check
glimpse(traps_grd_depth_season_perc) 
traps_grd_depth_season_perc %>% group_by(win_or_spsum) %>% summarise(perc_traps = sum(perc_traps),maxcumperc = max(cumperc))

# cumulative dist plot
p2 <- traps_grd_depth_season_perc %>% 
  ggplot(aes(x=-depth,y=cumperc, colour = win_or_spsum))+
  geom_line()+
  #geom_hline(aes(yintercept = 90), colour="blue", linetype=2)+
  labs(x="Depth (m)",y="Cumulative % Traps") +
  ggtitle("Distribution of crab pots by depth,\nall years in Dec-Apr vs May-Sep") + 
  theme(legend.position = ("top"),legend.title=element_blank())
p2

ggsave(here::here('wdfw','plots','Cumulative distribution of traps by depth all years, win v sprsum_LEENA VERSION.png'), p2)



###########################################################################################################
#### LEENA'S ORIGINAL TEST PLOTTING BELOW -- OLD

#Testing some plotting

ggplot(logs_all, aes(depth)) +
  geom_histogram()

#simple histogram by season
ids <- unique(logs_all$season)
plot_list = list()
for (i in 1:length(ids)) {
  p = ggplot(subset(logs_all, season == ids[i]), aes(depth)) +
    geom_histogram(binwidth = 5) +
    scale_y_continuous(breaks=seq(0, 255000, 50000),limits=c(0,255000))+
    ggtitle((paste(ids[i])))
  plot_list[[i]] = p
}
plot_list


#simple bar chart by season, by depth bin
ids <- unique(logs_all$season)
plot_list = list()
for (i in 1:length(ids)) {
  p = ggplot(subset(logs_all, season == ids[i]), aes(depth)) +
    geom_bar() +
    scale_x_binned() +
    scale_y_continuous(breaks=seq(0, 255000, 50000),limits=c(0,255000))+
    ggtitle((paste(ids[i])))
  plot_list[[i]] = p
}
plot_list 


#stacked histogram of depth by month in season
ids <- unique(logs_all$season)
plot_list = list()
for (i in 1:length(ids)) {
  p = ggplot(subset(logs_all, season == ids[i]), aes(depth, fill = m)) +
    geom_histogram(binwidth = 5) +
    scale_y_continuous(breaks=seq(0, 255000, 50000),limits=c(0,255000))+
    ggtitle((paste(ids[i]))) +
    theme(legend.position = c(0.3,0.6))
  plot_list[[i]] = p
}
plot_list


#blake's mod of stacked histogram of depth by month in season
#orders months in legend starting with dec, all plot legends have all months listed and switched "m" in legend to "Month"
ids <- unique(logs_all$season)
plot_list = list()
for (i in 1:length(ids)) {
  p = ggplot(subset(logs_all, season == ids[i]), aes(depth, fill = m)) +
    geom_histogram(binwidth = 10) +
    scale_y_continuous(breaks=seq(0, 350000, 50000),limits=c(0,350000))+
    ggtitle((paste(ids[i]))) +
    theme(legend.position = c(0.3,0.6))+
    scale_fill_discrete(name = "Month",drop = FALSE)
  plot_list[[i]] = p
}
plot_list

#frequency polygon of depth by month in season
ids <- unique(logs_all$season)
plot_list = list()
for (i in 1:length(ids)) {
  p = ggplot(subset(logs_all, season == ids[i]), aes(depth, colour = m)) +
    geom_freqpoly(binwidth = 5) +
    scale_y_continuous(breaks=seq(0, 255000, 50000),limits=c(0,255000))+
    ggtitle((paste(ids[i]))) +
    theme(legend.position = c(0.3,0.6))
  plot_list[[i]] = p
}
plot_list

# bar chart of proportions instead of counts by season, by depth bin
ids <- unique(logs_all$season)
plot_list = list()
for (i in 1:length(ids)) {
  p = ggplot(subset(logs_all, season == ids[i])) +
    geom_bar(aes(x=depth, y=stat(prop))) +
    scale_x_binned() +
    scale_y_continuous(breaks=seq(0.0, 0.5, 0.1),limits=c(0.0,0.5))+
    ggtitle((paste(ids[i])))
  plot_list[[i]] = p
}
plot_list 

#to save to a pdf.
pdf("NAME.pdf")
for (i in 1:length(ids)) {
  print(plot_list[[i]])
}
dev.off()
