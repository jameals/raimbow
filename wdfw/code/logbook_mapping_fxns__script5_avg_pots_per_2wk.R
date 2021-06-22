## Mapping functions for WDFW logbook data 
# Investigate the average pots per vessel per 2wk interval

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

#########################################

# Investigate the average pots per vessel per 2wk interval

# Start with traps_g df for all seasons (traps are simulated and joined to grid)
# getting traps_g for full logs takes a long time to run, so saved it as RDS, which can be found in Kiteworks folder
#traps_g_for_all_logs_full_seasons <- read_rds(here::here('wdfw', 'data','traps_g_for all logs full seasons.rds'))
# or use the new df traps_g_license_logs_2013_2019.rds
traps_g_license_logs_2013_2019 <- read_rds(here::here('wdfw', 'data','traps_g_license_logs_2013_2019.rds'))

#traps_g <- traps_g_for_all_logs_full_seasons
traps_g <- traps_g_license_logs_2013_2019

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

# For now look at 2013-2019, traps_g_license_logs_2013_2019.rds is already filtered for these years
#traps_g <- traps_g %>% 
# filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019'))

# modifying the summtraps code from script 2 that adjusts for double counting
testdf <- traps_g %>% 
  st_set_geometry(NULL) %>%
  filter(!is.na(GRID5KM_ID)) %>% 
  # count the total number of traps in each grid cell in each set
  group_by(season_month_interval, Vessel, License,GRID5KM_ID,grd_x,grd_y,SetID,AREA) %>%  
  summarise(
    ntraps_vessel_set_cell=n()
  ) %>% 
  # average the number of pots per vessel per grid cell
  ungroup() %>% 
  group_by(season_month_interval, Vessel, License, GRID5KM_ID,grd_x,grd_y,AREA) %>% 
  summarise(
    ntraps_vessel_cell=mean(ntraps_vessel_set_cell)) %>% 
  # finally, sum the total traps per Vessel, across all grid cells in the 2-week period in question
  ungroup() %>% 
  group_by(season_month_interval, Vessel, License) %>% 
  summarise(
    M1_tottraps=sum(ntraps_vessel_cell))
glimpse(testdf)


#bring in 'raw' logs 
logs <- read_csv(here('wdfw', 'data','WDFW-Dcrab-logbooks-compiled_stackcoords_2009-2019.csv'),col_types = 'ccdcdccTcccccdTddddddddddddddddiddccddddcddc')
logs %<>% filter(is.na(FishTicket1) | FishTicket1 != "Q999999") 

#sum raw PotsFished, and get info on how many landings a vessel did in a 2-week period
logsdf <- logs %>% 
  mutate(
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

#For now look at 2013-2019
logsdf <- logsdf %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019')) 

#Sum the raw/unadjusted PotsFished for vessel by 2-week period
testdf2  <- logsdf %>% 
  distinct(SetID, .keep_all= TRUE) %>% #note in raw logs data are repeated - has a row for start and end of stringline
  group_by(season_month_interval, Vessel) %>%  
  summarise(
    sum_PotsFished=sum(PotsFished,na.rm=T) #need to include na.rm=T statement
  )
glimpse(testdf2)


logsdf <- logsdf %>% 
  # count the total number of fishtickets landed by vessel in 2-week period
  group_by(season_month_interval, Vessel) %>%  
  summarise(
    count_FishTicket=n_distinct(FishTicket1)) 
glimpse(logsdf)


#join the datasets
testdf %<>% 
  left_join(testdf2,by=c("season_month_interval","Vessel")) %>% 
  left_join(logsdf, by=c("season_month_interval","Vessel")) %>% 
  separate(season_month_interval, into = c("season", "month_name", "interval"), sep = "_") %>%
  filter(!is.na(month_name)) %>% 
  mutate(month_interval = paste0(month_name,"_",interval)) %>%
  mutate(month_interval = factor(month_interval, levels = c('December_1','December_2','January_1','January_2','February_1','February_2','March_1','March_2','April_1', 'April_2','May_1','May_2','June_1','June_2','July_1','July_2','August_1','August_2','September_1','September_2','October_1','October_2','November_1','November_2'))) %>% 
  arrange(Vessel,season, month_interval)
glimpse(testdf)


#Read in and join license & pot limit info
WA_pot_limit_info <- read_csv(here::here('wdfw', 'data','WA_pot_limit_info_May2021.csv'))

WA_pot_limit_info %<>%
  rename(License = License_ID)

#join Pot_Limit info. 
testdf %<>%
  left_join(WA_pot_limit_info,by=c("License"))
glimpse(testdf)

testdf %<>% 
  select(season, month_name, interval, month_interval, Vessel, License, M1_tottraps, sum_PotsFished,Pot_Limit, count_FishTicket) %>% 
  #Vessels pot limit in a 2-week interval is same as what is assumed to be its fished pot count using M2 method
  rename(Pot_Limit_or_M2 = Pot_Limit)
glimpse(testdf)
# this summary df showcases the difference between pot counts via M1 or M2, an between summing raw pot count from logbooks

#-------------------------------------------------------------------------------

# some checks
# calculating an estimate for lines in water as the sum of pot limits for those vessels that were active in a given time period 

check_lines_in_water <- testdf %>% 
  group_by(season, month_name, `Pot_Limit`) %>% 
  na.omit() %>% 
  summarise(numberoflicenses=n_distinct(License), na.rm=TRUE) %>% 
  mutate(check_PotsFished=sum(numberoflicenses * `Pot_Limit`)) %>% 
  select(season, month_name, check_PotsFished) %>% 
  distinct() %>% 
  collect()

check_lines_in_water <- check_lines_in_water %>%
  mutate(month_name = factor(month_name, levels = c('December','January','February','March','April','May','June','July','August','September','October','November')))  

check_plot <- ggplot(check_lines_in_water, aes(x= month_name, y= check_PotsFished /1000, colour=season,  group=season))+
  geom_line(size=1.5, lineend = "round") + 
  scale_colour_brewer(palette = "PRGn") +
  ylab("tottraps(1000s) across \ngrid entire WA") +
  xlab("Month") + #Month_1st or 2nd half
  scale_y_continuous(breaks=seq(0, 70, 10),limits=c(0,70))+
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
check_plot

#------------------------------

# number of vessels that were active in each month as per logbook data

active_vessels_by_month <- testdf %>% 
  group_by(season, month_name) %>% 
  na.omit() %>% 
  summarise(
    n_unique_licenses=n_distinct(License), na.rm=TRUE)

#write_csv(active_vessels_by_month,here::here('wdfw','data',"active_vessels_by_month.csv"))

