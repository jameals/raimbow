# WA fishery footprint area by season

#-----------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(rnaturalearth) #devtools::install_github("ropensci/rnaturalearthhires")
library(sf)
library(rgeos)
library(viridis)
library(ggpubr)
library(scales)
library(ggridges)

#-----------------------------------------------------------------------------------

# bring in gridded WA logbook data, with trap density calculated per grid per 2-week period
# look at May_Sep only
path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2013_2020.rds"
x.fish_WA <- readRDS(path.fish_WA) %>% 
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N'))

x.fish_WA_MaySep <- x.fish_WA %>% 
  filter(is_May_Sep == "Y") 



x.fish_WA_MaySep_footprints <- x.fish_WA_MaySep %>% 
  group_by(season) %>% 
  distinct(GRID5KM_ID, .keep_all = TRUE) %>% 
  summarise(total_area_km2 = sum(AREA)/1e6)

#  season    total_area (km2)
# 2013-2014      4048
# 2014-2015      3012
# 2015-2016      2867
# 2016-2017      5533
# 2017-2018      6322
# 2018-2019      4747
# 2019-2020      3231

mean_area <- mean(x.fish_WA_MaySep_footprints$total_area_km2)

x.fish_WA_MaySep_footprints$season <- factor(x.fish_WA_MaySep_footprints$season, 
                                             levels = c('2013-2014', '2014-2015', '2015-2016', '2016-2017', '2017-2018', '2018-2019', '2019-2020'))



WA_fishery_footprint_area <- ggplot(x.fish_WA_MaySep_footprints, aes(x=season)) + 
  geom_line(aes(y = total_area_km2, group = 1), color = "black") + 
  geom_point(aes(y = total_area_km2, group = 1), color = "black", size=2) + 
  geom_hline(yintercept= mean_area, linetype="dashed", color = "red") +
  ylab("Area (km2)") + 
  xlab("Season") +
  ggtitle("May-Sep fishery footprint area\nred line = mean area across 2013-2020") +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 12, angle = 60),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size=12),
        strip.background = element_blank(),
        strip.placement = "left"
  )
WA_fishery_footprint_area









