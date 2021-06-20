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


#------------------------------------------------------------------------------


##For df with M1 and M2 summaries bring in adj_summtraps (result from script 2)
adj_summtraps <- read_rds(here::here('wdfw', 'data','adj_summtraps.rds'))


#------------------------------------------------------------------------------

# Making line thickness in ts plots reflect the area in use

# If you want the ts plots to reflect how fishery is spread across a larger area in early season,
# but contracts in space later in season, use: size=totarea in plotting code
# column 'totarea' in the df is in km2

# note though that line thickness point size in plots currently ranges between 1 and 6 (code: scale_size(range = c(1, 6)))
# while the difference between the largest area and smallest are in use is much larger
# for 2-weekly summary df, the min of total area (totarea column) is 113.1 sq.km, and max is 9547.9 sq.km
# for 1-monthly summary df, the min of total area (totarea column) is 712.8 sq.km, and max is 18961.2 sq.km

# Therefore the legend may not me very informative
# consider leaving legend out (code: scale_size(range = c(1, 6), guide = FALSE)))
# and explaining line thickness in figure legend (e.g. by mentioning min and max values)
# OR work on the legend further than current code to make it more informative

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
logs_ts_2week <- ggplot(M2_summtrapsWA_2w, aes(x=month_interval, y=M2_meantrapdens, colour=season, group=season, size=totarea))+
    #make line thickness reflect the area in use in above line 'size=totarea' (good for trap density plotting)
    geom_line(lineend = "round") + 
    scale_size(range = c(1, 6), guide = FALSE) + #specify min and max size, guide=FALSE here will remove line thickness from legend
    #OR 
    #have a constant line thickness (good when plotting no. of traps/lines in water)
    #geom_line(size=1.5, lineend = "round") + 
  scale_colour_brewer(palette = "PRGn") +
  ylab("Mean trap density across \ngrid cells for entire WA") +
  xlab("Month_1st or 2nd half") + 
  guides(color = guide_legend(override.aes = list(size = 2))) + #this will make legend for the years look better
  theme(legend.title = element_blank(),
        #title = element_text(size = 32),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),#element_text(hjust = 1,size = 12, angle = 90),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position="bottom"
  )
logs_ts_2week
#ggsave(here('wdfw','plots',paste0('Plot of mean M2 trap densities_2weekly','.png')),logs_ts_2week,w=12,h=10)


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

#PLOT for trap COUNTS (thousands) on a MONTHLY time step 
logs_ts_month <- ggplot(M2_summtrapsWA_month, aes(x= month_name, y=M2_mean_tottraps/1000, colour=season,  group=season))+
  #make line width be consistent - tho see other code in script for how to make this vary by area used
  geom_line(size=1.5, lineend = "round") + 
  scale_colour_brewer(palette = "PRGn") +
  ylab("Total traps (1000) in water \nfor entire WA") +
  xlab("Month") + 
  scale_y_continuous(breaks=seq(0, 70, 10),limits=c(0,70))+
  guides(color = guide_legend(override.aes = list(size = 2))) + #this will make legend for the years look better
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
#ggsave(here('wdfw','plots',paste0('Mean M2 trap counts_lines in water_by month','.png')),logs_ts_month,w=12,h=10)



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

#PLOT for trap DENSITITES on a MONTHLY time step 
logs_ts_month_dens <- ggplot(M2_summtrapsWA_month_dens, aes(x= month_name, y= M2_meantrapdens, colour=season,  group=season, size=totarea))+
  #make line thickness reflect the area in use in above line 'size=totarea' (good for trap density plotting)
  geom_line(lineend = "round") + 
  scale_size(range = c(1, 6), guide = FALSE) + #specify min and max size, guide=FALSE here will remove line thickness from legend
  #OR 
  #have a constant line thickness (good when plotting no. of traps/lines in water)
  #geom_line(size=1.5, lineend = "round") + 
  scale_colour_brewer(palette = "PRGn") +
  ylab("Mean trap density across \ngrid cells for entire WA") +
  xlab("Month") + 
  #scale_y_continuous(breaks=seq(0, 60000, 10000),limits=c(0,60000))+
  #scale_y_continuous(breaks=seq(2, 16, 2),limits=c(2,16))+
  guides(color = guide_legend(override.aes = list(size = 2))) + #this will make legend for the years look better
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
#ggsave(here('wdfw','plots',paste0('Plot of mean M2 trapdensities','.png')),logs_ts_month_dens,w=12,h=10)




#----------------------------------------------------------------------------------------------------------------

#You can also incorporate 2.5, 25, 75 and 97.5 percentiles to ts plots

#M2 - all seasons on the same scale
ids <- unique(M2_summtrapsWA_month_dens$season)
plot_list = list()

for (i in 1:length(ids)) {
  p = ggplot(subset(M2_summtrapsWA_month_dens, season == ids[i])) +
    geom_line(aes(x=month_name, y=M2_meantrapdens, size=totarea), group=1, lineend = "round", color='black') + 
    scale_size(range = c(1, 6)) +
    geom_line(aes(x=month_name, y=M2_percentile_75th), group=1, color='black') +
    geom_line(aes(x=month_name, y=M2_percentile_25th), group=1, color='black') +
    scale_y_continuous(breaks=seq(0, 20, 5),limits=c(0,20))+
    ylab("Trap density \n(no. of traps/sq.km) for entire WA") +
    xlab("Month") + 
    ggtitle((paste(ids[i]))) +
    theme(legend.title = element_blank(),
          #title = element_text(size = 32),
          legend.text = element_text(size=12),
          axis.text.x = element_blank(),#element_text(hjust = 1,size = 12, angle = 90),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12),
          #legend.position = c(0.9, 0.8) +
          legend.position="none" #use "none" here to fully hide/remove legend
    )
  plot_list[[i]] = p
}
plot_list

map_out <- cowplot::plot_grid(plotlist = plot_list,nrow = 2)
# saving
#ggsave(here('wdfw','plots',paste0('M2 mean trap dens, 25th and 75th percentiles for all WA by season','.png')),map_out,w=12,h=10)

