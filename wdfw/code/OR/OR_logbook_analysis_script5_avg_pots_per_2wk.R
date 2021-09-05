## This script has been modified from the mapping functions for WDFW logbook data to fit OR data:
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
traps_g_license_logs_2013_2018 <- read_rds(here::here('wdfw', 'data', 'OR', 'OR_traps_g_all_logs_2013_2018_SpatialFlag_filtered.rds'))
# or use the df where haven't filtered for spatial flag
#traps_g_license_logs_2013_2018 <- read_rds(here::here('wdfw', 'data', 'OR', 'OR_traps_g_all_logs_2013_2018.rds'))

traps_g_license_logs_2007_2018 <- read_rds(here::here('wdfw', 'data', 'OR', 'OR_traps_g_all_logs_2007_2018_SpatialFlag_filtered.rds'))


#traps_g <- traps_g_license_logs_2013_2018
traps_g <- traps_g_license_logs_2007_2018

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

# For now look at 2013-2018, traps_g_license_logs_2013_2018.rds is already filtered for these years
#traps_g <- traps_g %>% 
# filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018'))

# Change few column names to match OR and WA data and code
traps_g %<>%
  rename(License=PermitNumber, Pot_Limit=Potlimit)

# modifying the summtraps code from script 2 that adjusts for double counting
testdf <- traps_g %>% 
  st_set_geometry(NULL) %>%
  filter(!is.na(GRID5KM_ID)) %>% 
  # count the total number of traps in each grid cell in each set
  group_by(season_month_interval, Vessel, License, Pot_Limit, GRID5KM_ID,grd_x,grd_y,SetID,AREA) %>%  
  summarise(
    ntraps_vessel_set_cell=n()
  ) %>% 
  # average the number of pots per vessel per grid cell
  ungroup() %>% 
  group_by(season_month_interval, Vessel, License, Pot_Limit, GRID5KM_ID,grd_x,grd_y,AREA) %>% 
  summarise(
    ntraps_vessel_cell=mean(ntraps_vessel_set_cell)) %>% 
  # finally, sum the total traps per Vessel, across all grid cells in the 2-week period in question
  ungroup() %>% 
  group_by(season_month_interval, Vessel, License, Pot_Limit) %>% 
  summarise(
    M1_tottraps=sum(ntraps_vessel_cell))
glimpse(testdf)


#bring in 'raw' logs 
#logs <- read_csv(here('wdfw', 'data', 'OR', 'ODFW-Dcrab-logbooks-compiled_stackcoords_license_2013-2018_2021-08-17.csv'))
logs <- read_csv(here('wdfw', 'data', 'OR', 'ODFW-Dcrab-logbooks-compiled_stackcoords_license_2007-2018_20210830.csv'))
#logs %<>% filter(is.na(FishTicket1) | FishTicket1 != "Q999999") #This is WA spesific, but need to check if OR has something similar

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

#For now look at 2013-2018
#logsdf <- logsdf %>% 
#  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018')) 

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
  #OR data has one column with multiple FishTicket numbers, while WA had split these into several fishticket columns
  #All OR FishTicket number in the column are separated by ;, max is 5 fishtickets though most have 1-3
  separate(FishTicket, c("FishTicket1","FishTicket2", "FishTicket3", "FishTicket4", "FishTicket5"), ";") %>% 
  group_by(season_month_interval, Vessel) %>%  
  summarise(
    #Need to check for OR what it means when a stringline has multiple FishTicket numbers, is the first one the only one that matters?
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

#For WA data read in and join license & pot limit info here, but for OR that is already done

# #WA SPECIFIC - NEED TO FIND OUT IF OR HAS HAD ANY SUMMER POT LIMIT REDUCTIONS
# # apply 2019 summer pot limit reduction, which took effect July 1, 2019 
# # and was in effect through the end of the season (Sept. 15, 2019)
# ## create season_month column
# testdf %<>%
#   mutate(season_month = paste0(season,"_",month_name))
# ## make a new column for summer pot limit reduction
# testdf %<>% 
#   mutate(Pot_Limit_SummerReduction = Pot_Limit)
# ## split df to pre and post reduction periods
# df1 <- testdf %>%
#   filter(!season_month %in% c('2018-2019_July', '2018-2019_August', '2018-2019_September'))
# df2 <- testdf %>%
#   filter(season_month %in% c('2018-2019_July', '2018-2019_August', '2018-2019_September'))
# ## adjust pot limit post 1 July 2019
# df2 %<>% 
#   mutate(Pot_Limit_SummerReduction = ifelse(Pot_Limit_SummerReduction==500, 330, 200))
# ## join dfs back together  
# testdf <- rbind(df1,df2)

testdf %<>% 
  mutate(season_month = paste0(season,"_",month_name)) %>% #need to run this line if didn't run the above for summer pot limit reduction
  select(season, month_name, season_month, interval, month_interval, Vessel, License, M1_tottraps, sum_PotsFished, Pot_Limit, count_FishTicket) %>% #Pot_Limit_SummerReduction
  #Vessels pot limit in a 2-week interval is same as what is assumed to be its fished pot count using M2 method
  rename(Pot_Limit_or_M2 = Pot_Limit) #Pot_Limit_SummerReduction
glimpse(testdf)
# this summary df showcases the difference between pot counts via M1 or M2, an between summing raw pot count from logbooks
#With OR data the difference between M1 and M2/PotLimit might be due to them only entering 30% of logbook data

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
  mutate(month_name = factor(month_name, levels = c('December','January','February','March','April','May','June','July','August')))  

check_plot <- ggplot(check_lines_in_water, aes(x= month_name, y= check_PotsFished /1000, colour=season,  group=season))+
  geom_line(size=1.5, lineend = "round") + 
  scale_colour_brewer(palette = "PRGn") +
  ylab("tottraps(1000s) across \ngrid entire OR") +
  xlab("Month") + #Month_1st or 2nd half
  #scale_y_continuous(breaks=seq(0, 90, 10),limits=c(0,90))+
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
#This will differ from the lines in the water plot done in script 3 due to 30% entry rate of OR log data


#slightly different plot, showing early seasons with 100% data entry in different colour scale to make them stand out more 
years_with_full_log_entry <- c('2007-2008', '2008-2009', '2009-2010', '2010-2011')

data1 <-  check_lines_in_water %>% 
  filter(season %in% years_with_full_log_entry)
data2 <-  check_lines_in_water %>% 
  filter(!(season %in% years_with_full_log_entry))

#https://cran.r-project.org/web/packages/khroma/vignettes/tol.html
cols <- c(
  #seasons with 100% logs entered
  "firebrick1", "orangered", "orange", "gold", 
  #seasons when 30% logs entered
  "#762A83", "#9970AB", "#C2A5CF", "#E7D4E8", "#D9F0D3", "#ACD39E", "#5AAE61","#1B7837")

check_plot_x <- 
  ggplot()+
  geom_line(data1, mapping=aes(x=month_name, y=check_PotsFished/1000, colour=season,  group=season),size=1.5, lineend = "round") +
  geom_line(data2, mapping=aes(x=month_name, y=check_PotsFished/1000, colour=season,  group=season),size=1.5, lineend = "round") +
  scale_colour_manual(values = cols) +
  ylab("Total traps (1000) in water \nfor entire OR") +
  xlab("Month") + 
  #scale_y_continuous(breaks=seq(0, 100, 20),limits=c(0,100))+
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
check_plot_x
#ggsave(here('wdfw','plots', 'OR', paste0('test lines in water as the sum of pot limits for active vessels per month_2007-2018','.png')),check_plot_x,w=12,h=10)

#------------------------------

# number of vessels that were active in each month as per logbook data

active_vessels_by_month <- testdf %>% 
  group_by(season, month_name) %>% 
  na.omit() %>% 
  summarise(
    n_unique_licenses=n_distinct(License), na.rm=TRUE)

#write_csv(active_vessels_by_month,here::here('wdfw','data',"active_vessels_by_month.csv"))

active_vessels_by_month <- active_vessels_by_month %>%
  mutate(month_name = factor(month_name, levels = c('December','January','February','March','April','May','June','July','August','September','October','November')))  


vessels_by_month_plot <- ggplot(active_vessels_by_month, aes(x= month_name, y= n_unique_licenses, colour=season,  group=season))+
  geom_line(size=1.5, lineend = "round") + 
  scale_colour_brewer(palette = "PRGn") +
  ylab("No. active vessels across \ngrid entire OR") +
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
vessels_by_month_plot
#ggsave(here('wdfw','plots', 'OR', paste0('test number of active vessels by month_2007-2010-2013-2018','.png')),vessels_by_month_plot,w=12,h=10)


#slightly different plot, showing early seasons with 100% data entry in different colour scale to make them stand out more 
years_with_full_log_entry <- c('2007-2008', '2008-2009', '2009-2010', '2010-2011')

data1 <-  active_vessels_by_month %>% 
  filter(season %in% years_with_full_log_entry)
data2 <-  active_vessels_by_month %>% 
  filter(!(season %in% years_with_full_log_entry))

#https://cran.r-project.org/web/packages/khroma/vignettes/tol.html
cols <- c(
  #seasons with 100% logs entered
  "firebrick1", "orangered", "orange", "gold", 
  #seasons when 30% logs entered
  "#762A83", "#9970AB", "#C2A5CF", "#E7D4E8", "#D9F0D3", "#ACD39E", "#5AAE61","#1B7837")

vessels_by_month_plot_x <- 
  ggplot()+
  geom_line(data1, mapping=aes(x= month_name, y= n_unique_licenses, colour=season,  group=season),size=1.5, lineend = "round") +
  geom_line(data2, mapping=aes(x= month_name, y= n_unique_licenses, colour=season,  group=season),size=1.5, lineend = "round") +
  scale_colour_manual(values = cols) +
  ylab("No. active vessels across \ngrid entire OR") +
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
vessels_by_month_plot_x
#ggsave(here('wdfw','plots', 'OR', paste0('test number of active vessels by month_2007-2018','.png')),vessels_by_month_plot_x,w=12,h=10)




#------------------------------
# Adjust this WA code for OR
# investigate unique vessels that were active in each month as per logbook data, while also retaining PotLimit info

#adjust summer potlimit reduction back to the original permit groupings
#OR hasn't had pot limit summer reduction just yet
testdf_adjusted <- testdf %>% 
  mutate(Permit_tier = case_when(
    Pot_Limit_or_M2==500 ~ 500,
    Pot_Limit_or_M2==300 ~ 300,
    Pot_Limit_or_M2==200 ~ 200)
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
    Permit_tier = factor(Permit_tier, levels = c('200', '300','500'))
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
#ggsave(here('wdfw','plots','OR', paste0('Number of active vessels per month by permit tier groups','.png')),vessels_by_month_plot_x,w=12,h=10)



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
#ggsave(here('wdfw','plots','OR', paste0('Prop of active vessels per month by permit tier groups','.png')),bar_chart,w=12,h=10)


