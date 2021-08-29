## This script has been modified from the mapping functions for WDFW logbook data to fit OR data:
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


#---------------------------------------------------------------------------------------------------------

# Plotting (simulated) pots by depth bin

# From raw logbook data, need to run place_traps function (script 1) to create the traps along each line and get their depths
# running code fro script 1 on the full logs (2013-2018) dataframe takes a long time, so ran it once and saved it as RDS 
traps_g_all_logs <- read_rds(here::here('wdfw', 'data', 'OR', 'OR_traps_g_all_logs_2013_2018_SpatialFlag_filtered.rds'))
# Also a version exists for OR where haven't yet filtered for SpatialFlag
#traps_g_all_logs <- read_rds(here::here('wdfw', 'data', 'OR', 'OR_traps_g_all_logs_2013_2018.rds'))

traps_g_all_logs <- read_rds(here::here('wdfw', 'data', 'OR', 'OR_traps_g_all_logs_2007_2018_SpatialFlag_filtered.rds'))


glimpse(traps_g_all_logs)

logs_all <- traps_g_all_logs %>% 
  st_set_geometry(NULL) %>% 
  mutate(m=month(SetDate),d=day(SetDate),period=ifelse(d<=15,1,2)) %>% 
  mutate(m = month.name[m], period = ifelse(period==1,"first half","second half")) %>% 
  mutate(season = str_sub(SetID,1,9)) %>% 
  mutate(season_month = paste0(season,"_",m))
# dataset has highly negative values (~ -30000) to denote port and bay areas - remove those. 
# Also note that place_traps function (script 1) already removes depths deeper than 200m as crab fishing at deeper depths is not likely
logs_all %<>% filter(depth > -1000)

logs_all %<>% mutate(m = factor(m, levels = c('December','January','February','March','April','May','June','July','August','September','October','November')))

glimpse(logs_all)


#---------------------------------------------------------------

# Cumulative distribution of pots by depth - all years and seasons (no winter vs spring/summer breakdown)

pots_by_depth_all_data <- logs_all %>%
  count(depth) %>% 
  ungroup() %>% 
  rename(pots=n) %>% 
  # do cumulative counts
  mutate(depth=-depth) %>% 
  arrange(depth) %>% 
  mutate(cumulative_pots=cumsum(pots),perc_pots=cumulative_pots/last(cumulative_pots)*100)
glimpse(pots_by_depth_all_data)

depth_dist_all_data <- pots_by_depth_all_data %>% 
  ggplot(aes(x=depth,y=perc_pots))+
  geom_line(size=1)+
  #geom_hline(aes(yintercept = 90), colour="blue", linetype=2)+
  scale_x_continuous(breaks=seq(0, 200, 20),limits=c(0,200))+
  labs(x="Depth (m)",y="Cumulative % Traps") +
  ggtitle("Distribution of crab pots by depth,\nall years and seasons") + 
  theme(legend.position = ("top"),legend.title=element_blank())
depth_dist_all_data

#ggsave(here('wdfw','plots', 'OR', paste0('Cumulative distribution of pots by depth_all years and seasons','.png')),depth_dist_all_data,w=12,h=10)
#ggsave(here('wdfw','plots', 'OR', paste0('Cumulative distribution of pots by depth_all years and seasons_2007_2018','.png')),depth_dist_all_data,w=12,h=10)


# Cumulative distribution of pots by depth - by season
pots_by_depth_by_season <- logs_all %>%
  count(season, depth) %>% 
  ungroup() %>% 
  rename(pots=n) %>% 
  # do cumulative counts
  mutate(depth=-depth) %>% 
  group_by(season) %>%
  arrange(depth) %>% 
  mutate(cumulative_pots=cumsum(pots),perc_pots=cumulative_pots/last(cumulative_pots)*100)
glimpse(pots_by_depth_by_season)

depth_dist_by_season <- pots_by_depth_by_season %>% 
  ggplot(aes(x=depth,y=perc_pots, colour = season, group=season))+
  geom_line(size=1)+
  scale_colour_brewer(palette = "PRGn") +
  #geom_hline(aes(yintercept = 90), colour="blue", linetype=2)+
  scale_x_continuous(breaks=seq(0, 200, 20),limits=c(0,200))+
  labs(x="Depth (m)",y="Cumulative % Traps") +
  ggtitle("Distribution of crab pots by depth,\nall years by season") + 
  theme(legend.position = ("top"),legend.title=element_blank())
depth_dist_by_season

#ggsave(here('wdfw','plots', 'OR', paste0('Cumulative distribution of pots by depth_all years_by season','.png')),depth_dist_by_season,w=12,h=10)



#slightly different plot, showing early seasons with 100% data entry in different colour scale to make them stand out more 
years_with_full_log_entry <- c('2007-2008', '2008-2009', '2009-2010', '2010-2011')

data1 <-  pots_by_depth_by_season %>% 
  filter(season %in% years_with_full_log_entry)
data2 <-  pots_by_depth_by_season %>% 
  filter(!(season %in% years_with_full_log_entry))

#https://cran.r-project.org/web/packages/khroma/vignettes/tol.html
cols <- c(
  #seasons with 100% logs entered
  "firebrick1", "orangered", "orange", "gold", 
  #seasons when 30% logs entered
  "#762A83", "#9970AB", "#C2A5CF", "#E7D4E8", "#D9F0D3", "#ACD39E", "#5AAE61","#1B7837")

depth_dist_by_season_x <- 
  ggplot()+
  #make line thickness reflect the area in use in above line 'size=totarea' (good for trap density plotting)
  geom_line(data1, mapping=aes(x=depth,y=perc_pots, group=season, colour=season), size = 1) +
  geom_line(data2, mapping=aes(x=depth,y=perc_pots, group=season, colour=season), size = 1) +
  scale_colour_manual(values = cols) +
  #geom_hline(aes(yintercept = 90), colour="blue", linetype=2)+
  scale_x_continuous(breaks=seq(0, 200, 20),limits=c(0,200))+
  labs(x="Depth (m)",y="Cumulative % Traps") +
  ggtitle("Distribution of crab pots by depth,\nall years by season") + 
  guides(color = guide_legend(override.aes = list(size = 2))) + #this will make legend for the years look better
  theme(legend.position = ("top"),legend.title=element_blank())
depth_dist_by_season_x

#ggsave(here('wdfw','plots', 'OR', paste0('Cumulative distribution of pots by depth_all years_by season_2007_2018_diff colours for 100 vs 30 percent data entry','.png')),depth_dist_by_season_x,w=12,h=10)

  
#--------------------------

# Break things down by winter vs spring/summer

spsum <- c("May","June","July","August","September") #THIS MIGHT DIFFER FOR OR

# Cumulative distribution of pots by depth - all data (all years) - winter vs spring/summer
pots_by_depth_spsumvswinter <- logs_all %>%
  mutate(
    win_or_spsum = case_when(
      m %in% spsum ~ "SprSum",
      TRUE ~ "Winter"
    )
  ) %>%
  count(win_or_spsum,depth) %>% 
  ungroup() %>% 
  rename(pots=n) %>% 
  # do cumulative counts
  mutate(depth=-depth) %>% 
  group_by(win_or_spsum) %>% 
  arrange(depth) %>% 
  mutate(cumulative_pots=cumsum(pots),perc_pots=cumulative_pots/last(cumulative_pots)*100)
glimpse(pots_by_depth_spsumvswinter)

depth_dist_spsumvswinter <- pots_by_depth_spsumvswinter %>% 
  ggplot(aes(x=depth,y=perc_pots, colour = win_or_spsum))+
  geom_line(size=1)+
  #geom_hline(aes(yintercept = 90), colour="blue", linetype=2)+
  scale_x_continuous(breaks=seq(0, 200, 20),limits=c(0,200))+
  labs(x="Depth (m)",y="Cumulative % Traps") +
  ggtitle("Distribution of crab pots by depth, all years \nin Dec-Apr (Winter) vs May-Sep (SprSum)") + 
  theme(legend.position = ("top"),legend.title=element_blank())
depth_dist_spsumvswinter

#ggsave(here('wdfw','plots', 'OR', paste0('Cumulative distribution of pots by depth_all years_win v sprsum_2007_2018','.png')),depth_dist_spsumvswinter,w=12,h=10)



# Cumulative distribution of pots by depth - by season - winter vs spring/summer

pots_by_depth_spsumvswinter_byseason <- logs_all %>%
  mutate(
    win_or_spsum = case_when(
      m %in% spsum ~ "SprSum",
      TRUE ~ "Winter"
    )
  ) %>%
  count(season, win_or_spsum,depth) %>% 
  ungroup() %>% 
  rename(pots=n) %>% 
  # do cumulative counts
  mutate(depth=-depth) %>% 
  group_by(season, win_or_spsum) %>% 
  arrange(depth) %>% 
  mutate(cumulative_pots=cumsum(pots),perc_pots=cumulative_pots/last(cumulative_pots)*100)
glimpse(pots_by_depth_spsumvswinter_byseason)

ids <- unique(pots_by_depth_spsumvswinter_byseason$season)
plot_list = list()
for (i in 1:length(ids)) {
  p = ggplot(subset(pots_by_depth_spsumvswinter_byseason, season == ids[i]), aes(color=win_or_spsum)) +
    geom_line(aes(x=depth,y=perc_pots), size=1)+
    #geom_hline(aes(yintercept = 90), colour="blue", linetype=2)+
    scale_x_continuous(breaks=seq(0, 200, 20),limits=c(0,200))+
    labs(x="Depth (m)",y="Cumulative % Traps") +
    theme(legend.position = ("top"),legend.title=element_blank()) +
    ggtitle((paste(ids[i])))
  plot_list[[i]] = p
}
plot_list

plot_out <- cowplot::plot_grid(plotlist = plot_list, nrow = 2)
#ggsave(here('wdfw','plots', 'OR', paste0('Cumulative distribution of pots by depth_all years_by season_win v sprsum','.png')),plot_out,w=14,h=10)
#ggsave(here('wdfw','plots', 'OR', paste0('Cumulative distribution of pots by depth_all years_by season_win v sprsum_2007_2018','.png')),plot_out,w=14,h=10)


#-------------------------------------------------------------
