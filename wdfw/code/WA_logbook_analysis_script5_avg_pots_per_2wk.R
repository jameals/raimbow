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
library(ggpubr)

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
#traps_g_license_logs_2013_2020 <- read_rds(here::here('wdfw', 'data','traps_g_license_all_logs_2013_2020.rds'))
traps_g_license_logs_2013_2020 <- read_rds(here::here('wdfw', 'data','traps_g_all_logs_2014_2020_clipped_to_WA_waters_20220126.rds'))

#traps_g <- traps_g_for_all_logs_full_seasons
traps_g <- traps_g_license_logs_2013_2020

#new iteration of df already has these
# traps_g <- traps_g %>% 
#   mutate(
#     season = str_sub(SetID,1,9),
#     month_name = month(SetDate, label=TRUE, abbr = FALSE),
#     season_month = paste0(season,"_",month_name),
#     month_interval = paste0(month_name, 
#                             "_", 
#                             ifelse(day(SetDate)<=15,1,2)
#     ),
#     season_month_interval = paste0(season, 
#                                    "_", 
#                                    month_interval)
#   )

# For now look at 2013-2019, traps_g_license_logs_2013_2019.rds is already filtered for these years
#traps_g <- traps_g %>% 
# filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019'))

# modifying the summtraps code from script 2 that adjusts for double counting
testdf <- traps_g %>% 
  #st_set_geometry(NULL) %>%
  filter(!is.na(GRID5KM_ID)) %>% 
  # count the total number of traps in each grid cell in each set
  group_by(season_month_interval, Vessel, License,GRID5KM_ID,grd_x,grd_y,SetID2,AREA) %>%  
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
logs <- read_csv(here('wdfw', 'data','WDFW-Dcrab-logbooks-compiled_stackcoords_2009-2020.csv'),col_types = 'ccdcdccTcccccdTddddddddddddddddiddccddddcddc')
#want to keep effort in WA waters by OR vessels
#logs %<>% filter(is.na(FishTicket1) | FishTicket1 != "Q999999") 

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

#For now look at 2013-2020
logsdf <- logsdf %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019', '2019-2020')) 

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

# apply 2019 summer pot limit reduction, which took effect July 1 and was in effect through the end of the season (Sept. 15)
# apply 2020 summer pot limit reduction (May-Sep)
## create season_month column
testdf %<>%
  mutate(season_month = paste0(season,"_",month_name))
## make a new column for summer pot limit reduction
testdf %<>% 
  mutate(Pot_Limit_SummerReduction = Pot_Limit)
## split df to pre and post reduction periods
df1 <- testdf %>%
  filter(!season_month %in% c('2018-2019_July', '2018-2019_August', '2018-2019_September',
                              '2019-2020_May', '2019-2020_June', '2019-2020_July', '2019-2020_August', '2019-2020_September'))
df2 <- testdf %>%
  filter(season_month %in% c('2018-2019_July', '2018-2019_August', '2018-2019_September',
                             '2019-2020_May', '2019-2020_June', '2019-2020_July', '2019-2020_August', '2019-2020_September'))
## adjust pot limit post 1 July 2019
df2 %<>% 
  mutate(Pot_Limit_SummerReduction = ifelse(Pot_Limit_SummerReduction==500, 330, 200))
## join dfs back together  
testdf <- rbind(df1,df2)

testdf %<>% 
  select(season, month_name, season_month, interval, month_interval, Vessel, License, M1_tottraps, sum_PotsFished, Pot_Limit_SummerReduction, count_FishTicket) %>% 
  #Vessels pot limit in a 2-week interval is same as what is assumed to be its fished pot count using M2 method
  rename(Pot_Limit_or_M2 = Pot_Limit_SummerReduction)
glimpse(testdf)
# this summary df showcases the difference between pot counts via M1 or M2, an between summing raw pot count from logbooks

#-------------------------------------------------------------------------------

# some checks
# calculating an estimate for lines in water as the sum of pot limits for those vessels that were active in a given time period 

check_lines_in_water <- testdf %>% 
  group_by(season, month_name, Pot_Limit_or_M2) %>% 
  na.omit() %>% 
  summarise(numberoflicenses=n_distinct(License), na.rm=TRUE) %>% 
  mutate(check_PotsFished=sum(numberoflicenses * Pot_Limit_or_M2)) %>% 
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
#ggsave(here('wdfw','plots', paste0('number of lines in water as sum of pot limits of all unique active vessels per month__WA_waters_only_2wk_input_file_20220120','.png')),check_plot,w=12,h=10)

#------------------------------

# number of vessels that were active in each month as per logbook data

active_vessels_by_month <- testdf %>% 
  group_by(season, month_name) %>% 
  na.omit() %>% 
  summarise(
    n_unique_licenses=n_distinct(License), na.rm=TRUE)

#write_csv(active_vessels_by_month,here::here('wdfw','data',"active_vessels_by_month_2013_2020.csv"))

active_vessels_by_month <- active_vessels_by_month %>%
  mutate(month_name = factor(month_name, levels = c('December','January','February','March','April','May','June','July','August','September','October','November')))  


vessels_by_month_plot <- ggplot(active_vessels_by_month, aes(x= month_name, y= n_unique_licenses, colour=season,  group=season))+
  geom_line(size=1.5, lineend = "round") + 
  scale_colour_brewer(palette = "PRGn") +
  ylab("No. active vessels per month in WA \n(unique vessels in logs)") +
  xlab("Month") + #Month_1st or 2nd half
  scale_y_continuous(breaks=seq(0, 165, 20),limits=c(0,165))+
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
vessels_by_month_plot
#ggsave(here('wdfw','plots', paste0('test number of active vessels by month_WA_waters_only_2wk_input_file_20220209','.png')),vessels_by_month_plot,w=12,h=10)


active_vessels_by_season <- testdf %>% 
  group_by(season) %>% 
  na.omit() %>% 
  summarise(
    n_unique_licenses=n_distinct(License), na.rm=TRUE)
#UPDATED, does include effort in WA waters landed in OR:
# season      n_unique_licenses
# 2013-2014   161
# 2014-2015   165
# 2015-2016   158
# 2016-2017   168
# 2017-2018   158
# 2018-2019   162
# 2019-2020   148

#OLD, doesn't include effort in WA waters landed in OR:
# season      n_unique_licenses
# 2013-2014   158
# 2014-2015   159
# 2015-2016   153
# 2016-2017   163
# 2017-2018   153
# 2018-2019   159
# 2019-2020   137




# number of vessels that were active in May-Sep of each year as per logbook data

active_vessels_in_MaySep_by_season <- testdf %>% 
  #comparing 2020 to pre-regs
  filter(season != '2018-2019') %>% 
  #only interested in May-Sep
  filter(month_name %in% c('May', 'June', 'July', 'August', 'September')) %>% 
  #group only by season
  group_by(season, Pot_Limit_or_M2) %>% 
  na.omit() %>% 
  summarise(
    n_unique_licenses=n_distinct(License), na.rm=TRUE)


vessels_in_MaySep_by_season_plot <- ggplot(active_vessels_in_MaySep_by_season, aes(x= season, y= n_unique_licenses, group=1))+
  geom_line(size=1.5, lineend = "round") + 
  ylab("No. active vessels in May-Sep in WA \n(unique vessels in logs)") +
  xlab("Season") + 
  theme(legend.title = element_blank(),
        #title = element_text(size = 32),
        legend.text = element_text(size=20),
        axis.text.x = element_text(hjust = 1,size = 20, angle = 90),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        #legend.position = c(0.9, 0.8) +
        legend.position="bottom"
  )
vessels_in_MaySep_by_season_plot
#26.2% drop in the number of unique vessels that appear in logs between May-Sep from 2018 to 2020

active_vessels_in_MaySep_by_season_v2 <- testdf %>% 
  #comparing 2020 to pre-regs
  #filter(season != '2018-2019') %>% 
  #only interested in May-Sep
  filter(month_name %in% c('May', 'June', 'July', 'August', 'September')) %>% 
  #also group by month
  group_by(season, month_name) %>% 
  na.omit() %>% 
  summarise(
    n_unique_licenses=n_distinct(License), na.rm=TRUE)

active_vessels_in_MaySep_by_season_v2 <- active_vessels_in_MaySep_by_season_v2 %>%
  mutate(month_name = factor(month_name, levels = c('May','June','July','August','September','October','November')))  


vessels_in_MaySep_by_season_plot_v2 <- ggplot(active_vessels_in_MaySep_by_season_v2, aes(x= month_name, y= n_unique_licenses, colour=season,  group=season))+
  geom_line(size=2, lineend = "round") + 
  scale_colour_brewer(palette = "PRGn") +
  ylab("No. active vessels by month in WA \n(unique vessels in logs)") +
  xlab("Month") + 
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme(panel.background = element_rect(fill = 'gray92'),
        legend.title = element_blank(),
        #title = element_text(size = 32),
        legend.text = element_text(size=20),
        axis.text.x = element_text(hjust = 1,size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        #legend.position = c(0.9, 0.8) +
        legend.position="bottom"
  )
vessels_in_MaySep_by_season_plot_v2


vessels_by_season_plot <- ggplot()+
  geom_line(data=active_vessels_in_MaySep_by_season, aes(x= season, y= n_unique_licenses), group=1 ,size=1.5, lineend = "round") + 
  geom_point(data=active_vessels_in_MaySep_by_season, aes(x= season, y= n_unique_licenses), group=1 ,size=3.5, lineend = "round") + 
  
  geom_line(data=active_vessels_in_JulSep_by_season, aes(x= season, y= n_unique_licenses), group=1 ,size=1.5, color='red',lineend = "round") + 
  geom_point(data=active_vessels_in_JulSep_by_season, aes(x= season, y= n_unique_licenses), group=1 ,size=3.5, color='red',lineend = "round") + 
  
  ylab("No. active vessels in WA \n(unique vessels in logs)") +
  xlab("Season") + 
  theme(legend.title = element_blank(),
        #title = element_text(size = 32),
        legend.text = element_text(size=20),
        axis.text.x = element_text(hjust = 1,size = 20, angle = 90),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        #legend.position = c(0.9, 0.8) +
        legend.position="bottom"
  )
vessels_by_season_plot

path_figures <- "C:/Users/Leena.Riekkola/Projects/raimbow/whalepreds_aggregate/figures"

png(paste0(path_figures, "/ts_number_unique_vessels_in_logs_JulSep_MaySep_by_month.png"), width = 16, height = 12, units = "in", res = 300)
ggarrange(vessels_by_season_plot,
          vessels_in_MaySep_by_season_plot_v2,
          ncol=1,
          nrow=2,
          #legend="top",
          #labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())


# number of vessels that were active in Jul-Sep of each year as per logbook data

active_vessels_in_JulSep_by_season <- testdf %>% 
  #comparing 2019 to pre-regs
  filter(season != '2019-2020') %>% 
  #only interested in May-Sep
  filter(month_name %in% c('July', 'August', 'September')) %>% 
  #group only by season
  group_by(season, Pot_Limit_or_M2) %>% 
  na.omit() %>% 
  summarise(
    n_unique_licenses=n_distinct(License), na.rm=TRUE)


vessels_in_JulSep_by_season_plot <- ggplot(active_vessels_in_JulSep_by_season, aes(x= season, y= n_unique_licenses, group=1))+
  geom_line(size=1.5, lineend = "round") + 
  ylab("No. active vessels in Jul-Sep in WA \n(unique vessels in logs)") +
  xlab("Season") + 
  theme(legend.title = element_blank(),
        #title = element_text(size = 32),
        legend.text = element_text(size=20),
        axis.text.x = element_text(hjust = 1,size = 20, angle = 90),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        #legend.position = c(0.9, 0.8) +
        legend.position="bottom"
  )
vessels_in_JulSep_by_season_plot


active_vessels_in_JulSep_by_season_v2 <- testdf %>% 
  #comparing 2019 to pre-regs
  filter(season != '2019-2020') %>% 
  #only interested in Jul-Sep
  filter(month_name %in% c('July', 'August', 'September')) %>% 
  #also group by month
  group_by(season, month_name) %>% 
  na.omit() %>% 
  summarise(
    n_unique_licenses=n_distinct(License), na.rm=TRUE)

active_vessels_in_JulSep_by_season_v2 <- active_vessels_in_JulSep_by_season_v2 %>%
  mutate(month_name = factor(month_name, levels = c('July','August','September')))  


vessels_in_JulSep_by_season_plot_v2 <- ggplot(active_vessels_in_JulSep_by_season_v2, aes(x= month_name, y= n_unique_licenses, colour=season,  group=season))+
  geom_line(size=1.5, lineend = "round") + 
  scale_colour_brewer(palette = "PRGn") +
  ylab("No. active vessels in Jul-Sep in WA \n(unique vessels in logs)") +
  xlab("Month") + 
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme(legend.title = element_blank(),
        #title = element_text(size = 32),
        legend.text = element_text(size=20),
        axis.text.x = element_text(hjust = 1,size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        #legend.position = c(0.9, 0.8) +
        legend.position="bottom"
  )
vessels_in_JulSep_by_season_plot_v2



##boxplots
active_vessels_in_MaySep_by_season #-- pooled unique vessels across all of May-Sep
active_vessels_in_JulSep_by_season #-- pooled unique vessels across all of Jul-Sep

pooled_active_vessels_in_JulSep_by_season <- active_vessels_in_JulSep_by_season %>% 
  mutate(Pot_Limit_or_M2 = case_when(Pot_Limit_or_M2 == 300  ~ 300,
                                     Pot_Limit_or_M2 == 500  ~ 500,
                                     Pot_Limit_or_M2 == 200  ~ 300,
                                     Pot_Limit_or_M2 == 330  ~ 500)) %>% 
  mutate(pre_post_reg = 
         ifelse(season == '2018-2019', "2018-2019", "pre-reg"))

pooled_active_vessels_in_MaySep_by_season <- active_vessels_in_MaySep_by_season %>% 
  mutate(Pot_Limit_or_M2 = case_when(Pot_Limit_or_M2 == 300  ~ 300,
                                     Pot_Limit_or_M2 == 500  ~ 500,
                                     Pot_Limit_or_M2 == 200  ~ 300,
                                     Pot_Limit_or_M2 == 330  ~ 500)) %>% 
  mutate(pre_post_reg = 
           ifelse(season == '2019-2020', "2019-2020", "pre-reg"))


box_pooled_unique_vessels_pre_reg_vs_2018_2019 <- ggplot() +
  geom_violin(data = pooled_active_vessels_in_JulSep_by_season %>% filter(pre_post_reg=='pre-reg'), aes(x = pre_post_reg, y = n_unique_licenses, group=Pot_Limit_or_M2, color=factor(Pot_Limit_or_M2)), lwd=2) +
  #geom_jitter(data = pooled_active_vessels_in_JulSep_by_season %>% filter(pre_post_reg=='pre-reg'), aes(x = pre_post_reg, y = n_unique_licenses),width = 0.15) +
  #geom_dotplot(data = pooled_active_vessels_in_JulSep_by_season %>% filter(pre_post_reg=='pre-reg'), aes(x = pre_post_reg, y = n_unique_licenses),binaxis='y', stackdir='center', dotsize=1)+
  geom_point(data = pooled_active_vessels_in_JulSep_by_season %>% filter(pre_post_reg=='2018-2019'), aes(x = pre_post_reg, y = n_unique_licenses, group=Pot_Limit_or_M2, color=factor(Pot_Limit_or_M2)), size=8) +
  scale_x_discrete(labels=c("pre-reg" = "pre-regulations", "2018-2019" = "2019"), limits = rev, ) +
  scale_color_manual(values=c("#8bd8bd", "#243665")) +
  scale_fill_manual(values=c("#8bd8bd", "#243665")) +
  ylab("No. unique vessels") + 
  xlab("") +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        #legend.position = c(.15, .85),
        legend.position = 'none',
        axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0),
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
box_pooled_unique_vessels_pre_reg_vs_2018_2019

path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
png(paste0(path_figures, "/number_of_unique_vessels_prePreg_vs_2019.png"), width = 22, height = 14, units = "in", res = 400)
ggarrange(box_pooled_unique_vessels_pre_reg_vs_2018_2019,
          ncol=1,
          nrow=1
          #legend="top",
          #labels="auto",
          #vjust=8,
          #hjust=-0.2
)
invisible(dev.off())



box_pooled_unique_vessels_pre_reg_vs_2019_2020 <- ggplot() +
  geom_violin(data = pooled_active_vessels_in_MaySep_by_season %>% filter(pre_post_reg=='pre-reg'), aes(x = pre_post_reg, y = n_unique_licenses, group=Pot_Limit_or_M2, color=factor(Pot_Limit_or_M2)), lwd=2) +
  #geom_jitter(data = pooled_active_vessels_in_MaySep_by_season %>% filter(pre_post_reg=='pre-reg'), aes(x = pre_post_reg, y = n_unique_licenses),width = 0.15) +
  #geom_dotplot(data = pooled_active_vessels_in_MaySep_by_season %>% filter(pre_post_reg=='pre-reg'), aes(x = pre_post_reg, y = n_unique_licenses),binaxis='y', stackdir='center', dotsize=1)+
  geom_point(data = pooled_active_vessels_in_MaySep_by_season %>% filter(pre_post_reg=='2019-2020'), aes(x = pre_post_reg, y = n_unique_licenses, group=Pot_Limit_or_M2, color=factor(Pot_Limit_or_M2)), size=8) +
  scale_color_manual(values=c("#8bd8bd", "#243665")) +
  scale_fill_manual(values=c("#8bd8bd", "#243665")) +
  ylab("") + 
  xlab("") +
  scale_x_discrete(labels=c("pre-reg" = "pre-regulations", "2019-2020" = "2020"), limits = rev, ) +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 50),
        legend.position = c(.9, .9),
        axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0),
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
box_pooled_unique_vessels_pre_reg_vs_2019_2020

path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
png(paste0(path_figures, "/number_of_unique_vessels_prePreg_vs_2020_v2.png"), width = 22, height = 14, units = "in", res = 400)
ggarrange(box_pooled_unique_vessels_pre_reg_vs_2019_2020,
          ncol=1,
          nrow=1,
          legend="bottom"
          #labels="auto",
          #vjust=8,
          #hjust=-0.2
)
invisible(dev.off())




active_vessels_in_MaySep_by_season_v2 <- testdf %>% 
  mutate(Pot_Limit_or_M2 = case_when(Pot_Limit_or_M2 == 300  ~ 300,
                                     Pot_Limit_or_M2 == 500  ~ 500,
                                     Pot_Limit_or_M2 == 200  ~ 300,
                                     Pot_Limit_or_M2 == 330  ~ 500)) %>% 
  filter(month_name %in% c('May', 'June', 'July', 'August', 'September')) %>% 
  #also group by month
  group_by(season, month_name, Pot_Limit_or_M2) %>% 
  na.omit() %>% 
  summarise(
    n_unique_licenses=n_distinct(License), na.rm=TRUE)

active_vessels_in_MaySep_by_season_v2 <- active_vessels_in_MaySep_by_season_v2 %>%
  mutate(month_name = factor(month_name, levels = c('May','June','July','August','September','October','November'))) %>% 
  mutate(pre_post_reg = 
           ifelse(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018'), "pre-regulation", season)) %>% 
  filter(!(month_name == "May" & season =='2018-2019' | month_name == "June" & season =='2018-2019')) %>% 
  mutate(pre_post_reg = case_when(
    pre_post_reg =='pre-regulation' ~ 'pre-regulation',
    pre_post_reg == '2018-2019' ~ '2019',
    pre_post_reg == '2019-2020' ~ '2020')
    )


active_vessels_in_MaySep_by_season_v2$Pot_Limit_or_M2 <- as.factor(active_vessels_in_MaySep_by_season_v2$Pot_Limit_or_M2)
# specify the factor levels in the order you want
active_vessels_in_MaySep_by_season_v2$pre_post_reg <- factor(active_vessels_in_MaySep_by_season_v2$pre_post_reg, levels = c("pre-regulation", "2019", "2020"))

vessels_in_MaySep_by_season_plot_v2 <- ggplot()+
  geom_violin(data = active_vessels_in_MaySep_by_season_v2 %>%  filter(pre_post_reg=='pre-regulation'), aes(x= month_name, y= n_unique_licenses, fill=pre_post_reg), lwd=2.2) + 
  scale_fill_manual(values=c("white"), guide = guide_legend(order = 1)) +
  
  geom_point(data = active_vessels_in_MaySep_by_season_v2 %>%  filter(pre_post_reg!='pre-regulation'), aes(x= month_name, y= n_unique_licenses, color=pre_post_reg, shape=pre_post_reg), size=9) + 
  scale_color_manual(values=c("black", "black")) +
  scale_x_discrete(labels=c("May"="May","June"="Jun","July"="Jul","August"="Aug","September"="Sep")) +
  facet_wrap(~Pot_Limit_or_M2)+
  ylab("Number of active vessels") +
  xlab("Month") + 
  #guides(color = guide_legend(override.aes = list(size = 2))) +
  theme_bw()+
  theme(#panel.background = element_rect(fill = 'gray92'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = 2),
        axis.ticks.length=unit(.25, "cm"),
        axis.ticks=element_line(size=2, colour = 'black'),
        
        legend.title = element_blank(),
        #title = element_text(size = 32),
        legend.text = element_text(size=50),
        axis.text.x = element_text(hjust = 0.5,size = 50, color='black'),
        axis.text.y = element_text(size = 50, color='black'),
        axis.title = element_text(size = 50),
        legend.position = c(0.9, 0.8),
        #legend.position="right",
        strip.text.x = element_text(
            size = 50, color = "black", face = "bold"),
        strip.background = element_rect(
          color="black", fill="white", size=1.5, linetype="solid"),
        strip.placement = "left",
        plot.margin = unit(c(0,0,0,30), "pt")
        
  )
vessels_in_MaySep_by_season_plot_v2

path_figures <- "C:/Users/lrie0/Documents/Projects/NOAA data/maps_ts_whales/figures"
png(paste0(path_figures, "/number_of_unique_vessels_prePreg_vs_2019_2020_byt_license_category_UPDATED.png"), width = 30, height = 14, units = "in", res = 500)
ggarrange(vessels_in_MaySep_by_season_plot_v2,
          ncol=1,
          nrow=1
          #legend="bottom"
          #labels="auto",
          #vjust=8,
          #hjust=-0.2
)
invisible(dev.off())



#------------------------------------------
#above saved active_vessels_by_month.csv
#into that joined WDFW estimated number of active vessels per month from their CP draft (WDFW would have calculated the estimate from fishtickets)
#by getting the ratio of our estimated active vessels to WDFW estimate, this will serve as an indicator of compliance rate
#note that the WDFW plot that the numbers came from only went to 2019-2019 season
# could do our own version of this by looking at number of unique vessels in our joined VMS-fishticket df

#overall WDFW estimates compliance to be 78-82 in a season

active_vessels_by_month_with_compliance <- read_csv(here('wdfw', 'data','active_vessels_by_month_with compliance.csv'))

active_vessels_by_month_with_compliance <- active_vessels_by_month_with_compliance %>%
  mutate(month = factor(month, levels = c('December','January','February','March','April','May','June','July','August','September','October','November')))  

compliance_plot <- ggplot(active_vessels_by_month_with_compliance, aes(x= month, y= ratio_NMFS_to_WDFW_compliance, colour=season,  group=season))+
  geom_line(size=1.5, lineend = "round") + 
  scale_colour_brewer(palette = "PRGn") +
  ylab("Ratio NMFS estimated no. active vessels to WDFW estimate \ni.e. compliance rate") +
  xlab("Month") + #Month_1st or 2nd half
  #scale_y_continuous(breaks=seq(0, 70, 10),limits=c(0,70))+
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
compliance_plot
#ggsave(here('wdfw','plots', paste0('test ratio estimated no of uniq vessels to WDFW estimate_ie compliance rate_WA_waters_only_2wk_input_file_20220120','.png')),compliance_plot,w=12,h=10)


#------------------------------

# investigate unique vessels that were active in each month as per logbook data, while also retaining PotLimit info

#adjust summer potlimit reduction back to the original permit groupings
testdf_adjusted <- testdf %>% 
  mutate(Permit_tier = case_when(
    Pot_Limit_or_M2==330 | Pot_Limit_or_M2==500 ~ 500,
    Pot_Limit_or_M2==200 | Pot_Limit_or_M2==300 ~ 300)
     ) %>%  
  na.omit()

active_vessels_by_month_2 <- unique(testdf_adjusted[,c('season', 'month_name','season_month','Vessel', 'Permit_tier')])

active_vessels_by_month_3 <- active_vessels_by_month_2 %>% 
  group_by(season, month_name, Permit_tier) %>% 
  summarise(
    n_unique_vessels=n_distinct(Vessel), na.rm=TRUE)

active_vessels_by_month_3 <- active_vessels_by_month_3 %>%
  mutate(
    month_name = factor(month_name, levels = c('December','January','February','March','April','May','June','July','August','September','October','November')),
    Permit_tier = factor(Permit_tier, levels = c('300','500'))
    )  

vessels_by_month_plot_x <- ggplot(active_vessels_by_month_3, aes(x=month_name, y=n_unique_vessels, group=Permit_tier, colour=Permit_tier))+
  geom_line(size=1.5, lineend = "round") + 
  facet_wrap(~ season) +
  #scale_colour_brewer(palette = "PRGn") +
  ylab("No. active vessels (WA)") +
  xlab("Month") + #Month_1st or 2nd half
  scale_y_continuous(breaks=seq(0, 110, 20),limits=c(0,110))+
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(), #element_text(hjust = 1,size = 12, angle = 90),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        #legend.position = c(0.9, 0.8) +
        legend.position="bottom"
  )
vessels_by_month_plot_x
#ggsave(here('wdfw','plots', paste0('Number of active vessels per month by permit tier groups_WA_waters_only_2wk_input_file_20220120','.png')),vessels_by_month_plot_x,w=12,h=10)



bar_chart <- ggplot(active_vessels_by_month_3, aes(x = month_name, y = n_unique_vessels, fill = Permit_tier)) +
  geom_col(position = "fill") +
  facet_wrap(~ season) +
  ylab("Proportion of active vessels (WA)") +
  xlab("Month") + 
  scale_y_continuous(breaks = seq(0, 1, by = 0.20))+
  theme(legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(), #element_text(hjust = 1,size = 12, angle = 90),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        #legend.position = c(0.9, 0.8) +
        legend.position="bottom"
  )
bar_chart
#ggsave(here('wdfw','plots', paste0('Prop of active vessels per month by permit tier groups_WA_waters_only_2wk_input_file_20220209','.png')),bar_chart,w=12,h=10)

