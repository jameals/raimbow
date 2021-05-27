## Mapping functions for WDFW logbook data 
# creating ts plots

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

##################################################

##USING M1 ONLY
#getting traps_g for full logs takes a long time to run, so saved it as RDS, which can be found in Kiteworks folder
traps_g_for_all_logs_full_seasons <- read_rds(here::here('wdfw', 'data','traps_g_for all logs full seasons.rds'))
traps_g <- traps_g_for_all_logs_full_seasons
#For now look at 2013-2019
traps_g <- traps_g %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019'))

traps_g <- traps_g %>% 
  mutate(
    season = str_sub(SetID,1,9),
    month_name = month(SetDate, label=TRUE, abbr = FALSE),
    season_month = paste0(season,"_",month_name),
    month_interval = paste0(month_name, 
                            "_", 
                            ifelse(day(SetDate)<=15,1,2)
    ),
    season_month_interval = paste0(season, 
                                   "_", 
                                   month_interval)
  )



##For df with M1 and M2 will have to bring in adj_summtraps (result from script 2)



##FOLLOWING CODE CURRENTLY USING M1 ONLY
#OPTION 1: group by season_month 
summtraps5km <- traps_g %>% 
  st_set_geometry(NULL) %>%
  filter(!is.na(GRID5KM_ID)) %>% 
  # count the total number of traps in each grid cell in each set
  group_by(season_month, Vessel,GRID5KM_ID,grd_x,grd_y,SetID,AREA) %>% 
  summarise(
    ntraps_vessel_set_cell=n()
  ) %>% 
  # average the number of pots per vessel per grid cell
  ungroup() %>% 
  group_by(season_month, Vessel,GRID5KM_ID,grd_x,grd_y,AREA) %>% 
  summarise(
    ntraps_vessel_cell=mean(ntraps_vessel_set_cell)#,
    #sd_traps_vessel_cell=sd(ntraps_vessel_set_cell) # want to come back and think about how to aggregate uncertainty
  ) %>% 
  # finally, sum the total traps per grid cell, across vessels
  ungroup() %>% 
  group_by(season_month, GRID5KM_ID,grd_x,grd_y,AREA) %>% 
  summarise(
    tottraps=sum(ntraps_vessel_cell),
    num_vessels = length(unique(Vessel)) #add count of unique vessels
  ) %>% 
  # trap density is total traps divided by area (in sq. km) of each cell
  mutate(
    trapdens=tottraps/(AREA/1e6)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(tottraps))
glimpse(summtraps5km)

# now we want a summary for each season_month based on the above for all of WA
summtrapsWA <- summtraps5km %>%
  group_by(season_month) %>%  
  summarise(
    tottraps = sum(tottraps),
    number_obs = n(), #no. of grid cells in that season_month that had traps in them
    totarea = sum(AREA/1e6), #in km2
    meantrapdens = mean(trapdens),
    sdtrapdens = sd(trapdens),
    mediantrapdens = median(trapdens),
    percentile_975th = quantile(trapdens, probs=0.975, na.rm=TRUE),
    percentile_75th = quantile(trapdens, probs=0.75, na.rm=TRUE),
    percentile_25th = quantile(trapdens, probs=0.25, na.rm=TRUE),
    percentile_025th = quantile(trapdens, probs=0.025, na.rm=TRUE),
  )
glimpse(summtrapsWA)

summtrapsWA <- summtrapsWA %>%
  separate(season_month, into = c("season", "month_name"), sep = "_") %>%
  mutate(season_month = paste0(season,"_",month_name)) %>%
  mutate(month_name = factor(month_name, levels = c('December','January','February','March','April','May','June','July','August','September','October','November'))) %>% 
  filter(!is.na(month_name)) 

#could look into using bins (categorical variable) to specify line width in plot (curently continuous variable)
#summtrapsWA <- summtrapsWA %>% 
#mutate(number_obs_bins = cut(number_obs, breaks = c(0,50,100,150,200,250,300,350,400,450)),
#       number_obs_bins = as.factor(number_obs_bins))
#and then in plotting code change geom_line call to this:
#geom_line(aes(size=factor(number_obs_bins)))
#the problem is that with lots of bins it's hard to tell the width difference between them - unless can manually edit the widths...

#PLOT for Option 1
logs_ts <- ggplot(summtrapsWA, aes(x= month_name, y= meantrapdens, colour=season,  group=season))+
  #make line width reflect the area/no. of grid cells used
  geom_line(aes(size=totarea),lineend = "round") + #size=number_obs; size=totarea
  scale_colour_brewer(palette = "PRGn") +
  #scale_colour_viridis_d(option = "plasma") + 
  ylab("Mean of trapdens across \ngrid cells for entire WA") +
  xlab("Month") + #Month_1st or 2nd half
  #scale_y_continuous(breaks=seq(0, 60000, 10000),limits=c(0,60000))+
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme(legend.title = element_blank(),
        #title = element_text(size = 32),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),#element_text(hjust = 1,size = 12, angle = 90),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        #legend.position = c(0.9, 0.8) +
        legend.position="bottom"
  )
logs_ts


#OPTION 2: group by season_month_interval -- not as good as option 1
summtraps5km <- traps_g %>% 
  st_set_geometry(NULL) %>%
  filter(!is.na(GRID5KM_ID)) %>% 
  # count the total number of traps in each grid cell in each set
  group_by(season_month_interval, Vessel,GRID5KM_ID,grd_x,grd_y,SetID,AREA) %>%  
  summarise(
    ntraps_vessel_set_cell=n()
  ) %>% 
  # average the number of pots per vessel per grid cell
  ungroup() %>% 
  group_by(season_month_interval, Vessel,GRID5KM_ID,grd_x,grd_y,AREA) %>% 
  summarise(
    ntraps_vessel_cell=mean(ntraps_vessel_set_cell)#,
    #sd_traps_vessel_cell=sd(ntraps_vessel_set_cell) # want to come back and think about how to aggregate uncertainty
  ) %>% 
  # finally, sum the total traps per grid cell, across vessels
  ungroup() %>% 
  group_by(season_month_interval, GRID5KM_ID,grd_x,grd_y,AREA) %>% 
  summarise(
    tottraps=sum(ntraps_vessel_cell),
    num_vessels = length(unique(Vessel)) #add count of unique vessels
  ) %>% 
  # trap density is total traps divided by area (in sq. km) of each cell
  mutate(
    trapdens=tottraps/(AREA/1e6)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(tottraps))
glimpse(summtraps5km)

# now we want a summary for each season_month_interval based on the above for all of WA
summtrapsWA <- summtraps5km %>%
  group_by(season_month_interval) %>%  
  summarise(
    tottraps = sum(tottraps),
    number_obs = n(), #no. of grid cells in that season_month that had traps in them
    totarea = sum(AREA/1e6), #in km2
    meantrapdens = mean(trapdens),
    sdtrapdens = sd(trapdens),
    mediantrapdens = median(trapdens),
    percentile_975th = quantile(trapdens, probs=0.975, na.rm=TRUE),
    percentile_75th = quantile(trapdens, probs=0.75, na.rm=TRUE),
    percentile_25th = quantile(trapdens, probs=0.25, na.rm=TRUE),
    percentile_025th = quantile(trapdens, probs=0.025, na.rm=TRUE),
  )
glimpse(summtrapsWA)

summtrapsWA <- summtrapsWA %>%
  separate(season_month_interval, into = c("season", "month_name", "period"), sep = "_") %>%
  mutate(season_month = paste0(season,"_",month_name)) %>%
  mutate(month_name = factor(month_name, levels = c('December','January','February','March','April','May','June','July','August','September','October','November'))) %>% 
  filter(!is.na(month_name)) %>% 
  mutate(season_month_interval = paste0(season_month,"_",period)) %>% 
  mutate(month_interval = paste0(month_name,"_",period)) %>%
  mutate(month_interval = factor(month_interval, levels = c('December_1','December_2','January_1','January_2','February_1','February_2','March_1','March_2','April_1', 'April_2','May_1','May_2','June_1','June_2','July_1','July_2','August_1','August_2','September_1','September_2','October_1','October_2','November_1','November_2')))

#PLOT for Option 2 - not very good looking, lines overlap too much
logs_ts <- ggplot(summtrapsWA, aes(x= month_interval, y= mediantrapdens, colour=season,  group=season))+
  #make line width reflect the area/no. of grid cells used
  geom_line(aes(size=number_obs),lineend = "round") + #size=number_obs; size=totarea
  scale_colour_brewer(palette = "PRGn") +
  #scale_colour_viridis_d(option = "plasma") + 
  ylab("Median of trapdens across \ngrid cells for entire WA") +
  xlab("Month_1st or 2nd half") + #Month_1st or 2nd half
  #scale_y_continuous(breaks=seq(0, 60000, 10000),limits=c(0,60000))+
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme(legend.title = element_blank(),
        #title = element_text(size = 32),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),#element_text(hjust = 1,size = 12, angle = 90),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        #legend.position = c(0.9, 0.8) +
        legend.position="bottom"
  )
logs_ts

#if want to export as png
png(here::here(
  "wdfw",
  "DRAFT_mean of trapdens across grid cells for entire WA.png"), 
  width = 7.5, height = 5, units = "in", res = 300
)
logs_ts
invisible(dev.off())

