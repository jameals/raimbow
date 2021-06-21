## Mapping functions for WDFW logbook data 
# histogram etc of trap densities for winter vs summer

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
library(scales)
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


#----------------------------------

## Cleaned and summarized, simulated crab trap data
adj_summtraps <- read_rds(here::here('wdfw','data','adj_summtraps.rds'))


#Looks like GRID5KM_IDs 117310, 117311, 117640, 120280, 120610, 120940, 122258, 122259, 122588 
#exists in the data twice but with different AREA values
#have a look at them
adj_summtraps <- adj_summtraps %>% 
  filter(GRID5KM_ID %in% c(117310, 117311, 117640, 120280, 120610, 120940, 122258, 122259, 122588))
#test to see what happens if remove grid cell
adj_summtraps <- adj_summtraps %>% 
  filter(!GRID5KM_ID %in% c(117310, 117311, 117640, 120280, 120610, 120940, 122258, 122259, 122588))


# create a column in df to indicate whether data are 'winter' or 'summer' 
# summer is data after May 1 
# the 'periods' included in summer are:
# May_1, May-2, June_1, June_2, July_1, July_2, August_1, August_2, September_1, September_2
adj_summtraps_wintersummer <- adj_summtraps %>% 
  mutate(wintersummer = 
           ifelse(month_interval %in% c('May_1', 'May_2', 'June_1', 'June_2', 'July_1', 'July_2', 'August_1', 'August_2', 'September_1','September_2')
                  ,'summer', 'winter'))




# bar chart of proportions -- all data
p1 <- adj_summtraps_wintersummer %>% 
  ggplot(aes(color=wintersummer, fill=wintersummer)) +
  geom_bar(aes(x=M1_trapdens, y=stat(prop)), position = "dodge") +
  scale_x_binned() + 
  ggtitle('Bar chart of proportions - M1 trap densities in winter/summer')
p1

p2 <- adj_summtraps_wintersummer %>% 
  ggplot(aes(color=wintersummer, fill=wintersummer)) +
  geom_bar(aes(x=M2_trapdens, y=stat(prop)), position = "dodge") +
  scale_x_binned() + #you can specify x-axis break here, e.g.: breaks=seq(0, 125, 5)
  ggtitle('Bar chart of proportions - M2 trap densities in winter/summer')
p2

map_out <- plot_grid(p1,p2,nrow=1)
ggsave(here('wdfw','plots',paste0('Plot of trap densitites_by season','.png')),map_out,w=12,h=10)




################
#by crab season
#bar chart of proportions  
p13 <- adj_summtraps_wintersummer %>% 
  filter(season=='2018-2019') %>% 
  ggplot(aes(color=wintersummer, fill=wintersummer)) +
  geom_bar(aes(x=M1_trapdens, y=stat(prop)), position = "dodge") +
  scale_x_binned(breaks=seq(0, 85, 5)) + #n.breaks = 15
  theme(legend.position = "none") +
  ggtitle('2018-2019 - M1')
p13

p14 <- adj_summtraps_wintersummer %>% 
  filter(season=='2018-2019') %>% 
  ggplot(aes(color=wintersummer, fill=wintersummer)) +
  geom_bar(aes(x=M2_trapdens, y=stat(prop)), position = "dodge") +
  scale_x_binned(breaks=seq(0, 145, 5)) +
  theme(legend.position = "none") +
  ggtitle('2018-2019 - M2')
p14

map_out <- plot_grid(p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,nrow=6)
ggsave(here('wdfw','plots',paste0('Plot of trap densitites by crab season_alldblgridsremoved','.png')),map_out,w=12,h=10)
