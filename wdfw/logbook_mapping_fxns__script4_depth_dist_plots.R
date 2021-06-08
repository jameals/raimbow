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

#use the full set of logbook data for this (no breakdown by particular year or period etc) 
#from raw logs, need to run place_traps function to create the traps along each line and get their depth
#running place_traps on the full logs dataframe takes really long, so ran it once and saved it as traps_sf_for_all_logs_and_seasons_2009-2019.rds
#and uploaded it to Kiteworks

traps_sf_all_logs <- read_rds(here::here('wdfw', 'data','traps_sf_for_all_logs_and_seasons_2009-2019.rds'))
#the depth plotting could also be done using the new df traps_g_license_logs_2013_2019.rds - output of script 1
#traps_sf_all_logs <- read_rds(here::here('wdfw', 'data','traps_g_license_logs_2013_2019.rds'))

logs_all <- traps_sf_all_logs %>% 
  st_set_geometry(NULL) %>% 
  mutate(m=month(SetDate),d=day(SetDate),period=ifelse(d<=15,1,2)) %>% 
  mutate(m = month.name[m], period = ifelse(period==1,"first half","second half")) %>% 
  mutate(season = str_sub(SetID,1,9)) %>% 
  mutate(season_month = paste0(season,"_",m))
#dataset has higly negative values (~ -30000) to denote port and bay areas - remove those. 
#Also note that place_traps function already removes depths <200m as crab fishing at deeper depths is not likely
logs_all %<>% filter(depth > -1000)

logs_all %<>% mutate(m = factor(m, levels = c('December','January','February','March','April','May','June','July','August','September','October','November'))) 

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
