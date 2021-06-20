## Mapping functions for WDFW logbook data 
# creating ts plots of trap counts and densities

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
library(cowplot)

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


#------------------------------------------------------------------------------


##For df with M1 and M2 summaries bring in adj_summtraps (result from script 2)
adj_summtraps <- read_rds(here::here('wdfw', 'data','adj_summtraps.rds'))


#------------------------------------------------------------------------------------


#Plotting time series of trap COUNTS and DENSITIES on a 2-WEEKLY time step

# we want a summary for each season_month_interval (i.e. 2-week) for all of WA
M2_summtrapsWA_2w <- adj_summtraps %>%
  group_by(season_month_interval) %>%  
  summarise(
    M1_tottraps = sum(M1_tottraps), #summing total traps like this is fine when working on 2-week step
    M2_tottraps = sum(M2_tottraps),
    M2_number_obs = n(), #no. of grid cells in that season_month that had traps in them
    totarea = sum(AREA/1e6), #in km2
    M1_meantrapdens = mean(M1_trapdens),
    M2_meantrapdens = mean(M2_trapdens),
    M1_sdtrapdens = sd(M1_trapdens),
    M2_sdtrapdens = sd(M2_trapdens),
    M1_mediantrapdens = median(M1_trapdens),
    M2_mediantrapdens = median(M2_trapdens),
    M1_percentile_975th = quantile(M1_trapdens, probs=0.975, na.rm=TRUE),
    M1_percentile_75th = quantile(M1_trapdens, probs=0.75, na.rm=TRUE),
    M1_percentile_25th = quantile(M1_trapdens, probs=0.25, na.rm=TRUE),
    M1_percentile_025th = quantile(M1_trapdens, probs=0.025, na.rm=TRUE),
    M2_percentile_975th = quantile(M2_trapdens, probs=0.975, na.rm=TRUE),
    M2_percentile_75th = quantile(M2_trapdens, probs=0.75, na.rm=TRUE),
    M2_percentile_25th = quantile(M2_trapdens, probs=0.25, na.rm=TRUE),
    M2_percentile_025th = quantile(M2_trapdens, probs=0.025, na.rm=TRUE)
  )
glimpse(M2_summtrapsWA_2w)

M2_summtrapsWA_2w <- M2_summtrapsWA_2w %>%
  separate(season_month_interval, into = c("season", "month_name", "period"), sep = "_") %>%
  mutate(season_month = paste0(season,"_",month_name)) %>%
  mutate(month_name = factor(month_name, levels = c('December','January','February','March','April','May','June','July','August','September','October','November'))) %>% 
  filter(!is.na(month_name)) %>% 
  mutate(season_month_interval = paste0(season_month,"_",period)) %>% 
  mutate(month_interval = paste0(month_name,"_",period)) %>%
  mutate(month_interval = factor(month_interval, levels = c('December_1','December_2','January_1','January_2','February_1','February_2','March_1','March_2','April_1', 'April_2','May_1','May_2','June_1','June_2','July_1','July_2','August_1','August_2','September_1','September_2','October_1','October_2','November_1','November_2')))

#PLOT trap COUNTS (y=M2_tottraps/1000) and DENSITIES (y=M2_meantrapdens) on a 2-WEEKLY time step 
logs_ts_2week <- ggplot(M2_summtrapsWA_2w, aes(x=month_interval, y=M2_meantrapdens, colour=season,  group=season))+
    #make line thickness reflect the area OR the no. of grid cells in use (good for trap density plotting)
    geom_line(aes(size=totarea^2),lineend = "round") + #using totarea^2 instead of totarea produces better looking plots
    #OR 
    #have a constant line thickness (good when plotting no. of traps/lines in water)
    #geom_line(size=1.5, lineend = "round") + 
  scale_colour_brewer(palette = "PRGn") +
  ylab("Mean trap density across \ngrid cells for entire WA") +
  xlab("Month_1st or 2nd half") + 
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme(legend.title = element_blank(),
        #title = element_text(size = 32),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),#element_text(hjust = 1,size = 12, angle = 90),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position="bottom"
  )
logs_ts_2week
#ggsave(here('wdfw','plots',paste0('Plot of mean M2 trap densities_2weekly','.png')),logs_ts,w=12,h=10)


#-----------------------------------------------------------------------------------------

#Plotting time series of trap COUNTS on a MONTHLY time step

#once trap counts have been summarised across all grid cells on a 2-weekly step, 
#then can average them to get to monthly step for accurate summaries of total trap counts
M2_summtrapsWA_month <- M2_summtrapsWA_2w %>%
  group_by(season_month) %>%  
  summarise(
    M1_mean_tottraps = mean(M1_tottraps), 
    M2_mean_tottraps = mean(M2_tottraps)
  )
glimpse(M2_summtrapsWA_month)

M2_summtrapsWA_month <- M2_summtrapsWA_month %>%
  separate(season_month, into = c("season", "month_name"), sep = "_") %>%
  mutate(season_month = paste0(season,"_",month_name)) %>%
  mutate(month_name = factor(month_name, levels = c('December','January','February','March','April','May','June','July','August','September','October','November'))) %>% 
  filter(!is.na(month_name)) 
glimpse(M2_summtrapsWA_month)

#PLOT for trap COUNTS on a MONTHLY time step 
logs_ts_month <- ggplot(M2_summtrapsWA_month, aes(x= month_name, y=M2_mean_tottraps/1000, colour=season,  group=season))+
  #make line width reflect the area/no. of grid cells used
  geom_line(size=1.5, lineend = "round") + 
  scale_colour_brewer(palette = "PRGn") +
  ylab("M2_mean_tottraps (1000) across \ngrid cells for entire WA") +
  xlab("Month") + 
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
logs_ts_month
#ggsave(here('wdfw','plots',paste0('Plot of mean M2 trap counts','.png')),logs_ts,w=12,h=10)



#Plotting time series of trap DENSITIES on a MONTHLY time step

# we want a summary for each season_month for all of WA
M2_summtrapsWA_month_dens <- adj_summtraps %>%
  group_by(season_month) %>%  
  summarise(
    #note that just summing trap counts as done on 2-w step is wrong as it would 'double count' the same pots of a vessel from both halves of the month
    #see the above code for plotting trap COUNTS, and use this code to plot trap DENSITIES on a 1-month step
    number_obs = n(), #no. of grid cells in that season_month that had traps in them
    totarea = sum(AREA/1e6), #in km2
    M1_meantrapdens = mean(M1_trapdens),
    M2_meantrapdens = mean(M2_trapdens),
    M1_sdtrapdens = sd(M1_trapdens),
    M2_sdtrapdens = sd(M2_trapdens),
    M1_mediantrapdens = median(M1_trapdens),
    M2_mediantrapdens = median(M2_trapdens),
    M1_percentile_975th = quantile(M1_trapdens, probs=0.975, na.rm=TRUE),
    M1_percentile_75th = quantile(M1_trapdens, probs=0.75, na.rm=TRUE),
    M1_percentile_25th = quantile(M1_trapdens, probs=0.25, na.rm=TRUE),
    M1_percentile_025th = quantile(M1_trapdens, probs=0.025, na.rm=TRUE),
    M2_percentile_975th = quantile(M2_trapdens, probs=0.975, na.rm=TRUE),
    M2_percentile_75th = quantile(M2_trapdens, probs=0.75, na.rm=TRUE),
    M2_percentile_25th = quantile(M2_trapdens, probs=0.25, na.rm=TRUE),
    M2_percentile_025th = quantile(M2_trapdens, probs=0.025, na.rm=TRUE)
  )
glimpse(M2_summtrapsWA_month_dens)

M2_summtrapsWA_month_dens <- M2_summtrapsWA_month_dens %>%
  separate(season_month, into = c("season", "month_name"), sep = "_") %>%
  mutate(season_month = paste0(season,"_",month_name)) %>%
  mutate(month_name = factor(month_name, levels = c('December','January','February','March','April','May','June','July','August','September','October','November'))) %>% 
  filter(!is.na(month_name)) 

#try ^2 totareas for nicer changes in line thickness
#could look into using bins (categorical variable) to specify line width in plot (currently continuous variable)
#summtrapsWA <- summtrapsWA %>% 
#mutate(number_obs_bins = cut(number_obs, breaks = c(0,50,100,150,200,250,300,350,400,450)),
#       number_obs_bins = as.factor(number_obs_bins))
#and then in plotting code change geom_line call to this:
#geom_line(aes(size=factor(number_obs_bins)))
#the problem is that with lots of bins it's hard to tell the width difference between them - unless can manually edit the widths...

#PLOT for trap DENSITITES on a monthly time step 
logs_ts_month_dens <- ggplot(M2_summtrapsWA_month_dens, aes(x= month_name, y= M2_meantrapdens, colour=season,  group=season))+
    #make line thickness reflect the area OR the no. of grid cells in use (good for trap density plotting)
    geom_line(aes(size=totarea^2),lineend = "round") + 
    #OR 
    #have a constant line thickness (good when plotting no. of traps/lines in water)
    #geom_line(size=1.5, lineend = "round") + 
  scale_colour_brewer(palette = "PRGn") +
  ylab("M2 mean of trapdens across \ngrid cells for entire WA") +
  xlab("Month") + 
  #scale_y_continuous(breaks=seq(0, 60000, 10000),limits=c(0,60000))+
  #scale_y_continuous(breaks=seq(2, 16, 2),limits=c(2,16))+
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
logs_ts_month_dens
#ggsave(here('wdfw','plots',paste0('Plot of mean M2 trapdensities','.png')),logs_ts,w=12,h=10)



#----------------------------------------------------------------------------------------------------------------

#You can also incorporate 2.5, 25, 75 and 97.5 percentiles to ts plots

#M2
test <- M2_summtrapsWA %>% filter(season=='2018-2019')
test_ts_6 <- ggplot()+
  #make line width reflect the area/no. of grid cells used
  geom_line(data=test, aes(x=month_name, y= M2_meantrapdens, size=totarea^2), group=1, lineend = "round") + #size=number_obs; size=totarea
  geom_line(data=test, aes(x=month_name, y= M2_percentile_975th), group=1) +
  geom_line(data=test, aes(x=month_name, y= M2_percentile_025th), group=1) +
  ylab("M2 trapdens across \ngrid cells for entire WA") +
  xlab("Month") + #Month_1st or 2nd half
  #guides(color = guide_legend(override.aes = list(size = 2))) +
  ggtitle('2018-2019') +
  theme(legend.title = element_blank(),
        #title = element_text(size = 32),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),#element_text(hjust = 1,size = 12, angle = 90),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        #legend.position = c(0.9, 0.8) +
        legend.position="bottom" 
  )
test_ts_6

map_out <- plot_grid(test_ts_1,test_ts_2,test_ts_3,test_ts_4,test_ts_5,test_ts_6,nrow=2)
# saving
ggsave(here('wdfw','plots',paste0('M2 ts plots with 2.5 and 97.5 percentiles','.png')),map_out,w=12,h=10)



#M1 & M2 on the same scale
test <- M2_summtrapsWA %>% filter(season=='2018-2019')
test_ts_6 <- ggplot()+
  #make line width reflect the area/no. of grid cells used
  geom_line(data=test, aes(x=month_name, y= M1_meantrapdens, size=totarea^2), group=1, lineend = "round", color='blue') + #size=number_obs; size=totarea
  geom_line(data=test, aes(x=month_name, y= M1_percentile_975th), group=1, color='blue') +
  geom_line(data=test, aes(x=month_name, y= M1_percentile_025th), group=1, color='blue') +
  geom_line(data=test, aes(x=month_name, y= M2_meantrapdens, size=totarea^2), group=1, lineend = "round", color='black') + #size=number_obs; size=totarea
  geom_line(data=test, aes(x=month_name, y= M2_percentile_975th), group=1, color='black') +
  geom_line(data=test, aes(x=month_name, y= M2_percentile_025th), group=1, color='black') +
  ylab("trapdens across \ngrid cells for entire WA") +
  xlab("Month") + #Month_1st or 2nd half
  #guides(color = guide_legend(override.aes = list(size = 2))) +
  ggtitle('2018-2019') +
  theme(legend.title = element_blank(),
        #title = element_text(size = 32),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),#element_text(hjust = 1,size = 12, angle = 90),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        #legend.position = c(0.9, 0.8) +
        legend.position="bottom" 
  )
test_ts_6

map_out <- plot_grid(test_ts_1,test_ts_2,test_ts_3,test_ts_4,test_ts_5,test_ts_6,nrow=2)
# saving
ggsave(here('wdfw','plots',paste0('M1 M2 ts plots on same sclae with 2.5 and 97.5 percentiles_alldblgridsremoved','.png')),map_out,w=12,h=10)


test <- M2_summtrapsWA %>% filter(season=='2018-2019')
test_ts_6 <- ggplot()+
  #make line width reflect the area/no. of grid cells used
  #geom_line(data=test, aes(x=month_name, y= M1_meantrapdens, size=totarea^2), group=1, lineend = "round", color='blue') + #size=number_obs; size=totarea
  #geom_line(data=test, aes(x=month_name, y= M1_percentile_75th), group=1, color='blue') +
  #geom_line(data=test, aes(x=month_name, y= M1_percentile_25th), group=1, color='blue') +
  geom_line(data=test, aes(x=month_name, y= M2_meantrapdens, size=totarea^2), group=1, lineend = "round", color='black') + #size=number_obs; size=totarea
  geom_line(data=test, aes(x=month_name, y= M2_percentile_75th), group=1, color='black') +
  geom_line(data=test, aes(x=month_name, y= M2_percentile_25th), group=1, color='black') +
  ylab("trapdens across \ngrid cells for entire WA") +
  xlab("Month") + #Month_1st or 2nd half
  #guides(color = guide_legend(override.aes = list(size = 2))) +
  ggtitle('2018-2019') +
  theme(legend.title = element_blank(),
        #title = element_text(size = 32),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),#element_text(hjust = 1,size = 12, angle = 90),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        #legend.position = c(0.9, 0.8) +
        legend.position="bottom" 
  )
test_ts_6

map_out <- plot_grid(test_ts_1,test_ts_2,test_ts_3,test_ts_4,test_ts_5,test_ts_6,nrow=2)
# saving
ggsave(here('wdfw','plots',paste0('M2 ts plots with 25 and 75 percentiles_alldblgridsremoved','.png')),map_out,w=12,h=10)
