# test - finding cut off point for bw data


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
library(gridExtra)
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

#-----------------------------------------------------------------------------------

# bring in RDS from Briana that has coastwide sightings data with extracted model prediction values.
path_bw_prediction_values <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/PredictionValues_CoastWide_OldNew.rds"
bw_prediction_values_raw <- readRDS(path_bw_prediction_values) 




path.grid.5km.lno <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Grid_5km_landerased.rds"
grid.5km.lno <- readRDS(path.grid.5km.lno) # 5km grid, land erased

path_figures <- "C:/Users/Leena.Riekkola/Projects/raimbow/whalepreds_aggregate/figures" #or use this if do want to upload to GitHub


#-----------------------------------------------------------------------------------
# quick map to visualise the data

# grab a base map
rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")),   ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>%
    filter(admin %in% c("Canada", "Mexico")) %>%
    st_geometry() %>%
    st_transform(st_crs(grid.5km.lno))
)

#bbox
bbox = c(-133,30,-115,49) 


map_bw_prediction_values_raw <- ggplot() + 
  geom_sf(data=sf::st_as_sf(bw_prediction_values_raw), 
          aes(fill=old_new,
              col=old_new
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  ggtitle("bw sightings") +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  theme_minimal() + #theme_classic() +
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3),
        axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
        strip.text = element_text(size=14),
        title=element_text(size=16)
  )
map_bw_prediction_values_raw

#-----------------------------------------------------------------------------------

# new column to indicate the State in which the sighting occurred

# WA-OR border 46.26
# OR-CA border 42.00

bw_prediction_values <- bw_prediction_values_raw %>%
  mutate(state = case_when(
    lat > 46.26  ~ 'WA',
    lat < 46.26 & lat > 42.00 ~ 'OR',
    lat < 42.00  ~ 'CA'
  ))

map_bw_prediction_values <- ggplot() + 
  geom_sf(data=sf::st_as_sf(bw_prediction_values), 
          aes(fill=state,
              col=state
          )
  ) +
  geom_sf(data=rmap.base,col=NA,fill='gray50') +
  ggtitle("bw sightings") +
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4])) +
  theme_minimal() + #theme_classic() +
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3),
        axis.text.x.bottom = element_text(angle=45, vjust = 0.5),
        strip.text = element_text(size=14),
        title=element_text(size=16)
  )
map_bw_prediction_values


# there are some sightings with NA for predicted value - remove those
bw_prediction_values_noNA <- bw_prediction_values %>%
  filter(!is.na(pred_value))
  
  
  
#-----------------------------------------------------------------------------------

# make density plots (geom_density) to see where the majority of sightings 
# or larger whale aggregations occur
# there are only 5 points in WA waters
# look at thresholds for the entire coast, or for OR and WA combined 


coastwide <- bw_prediction_values_noNA %>% 
  ggplot(aes(pred_value))+ #,fill=season_segment,col=season_segment
  geom_density(alpha=0.5)+
  labs(x="prediction value",y="density")+ #,fill="Season Portion",col="Season Portion"
  geom_vline(xintercept=0.7,col='red',linetype=2)+
  ggtitle("coastwide (dashed line at 0.7)") +
  theme_bw()+
  theme(#panel.grid.major=element_blank(),
        panel.grid.major = element_line(color="gray90"),
        panel.grid.minor=element_blank(),
        #axis.text.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
  )
coastwide


WA_and_OR <- bw_prediction_values_noNA %>% 
  filter(state == 'WA' | state =='OR') %>% 
  ggplot(aes(pred_value))+ #,fill=season_segment,col=season_segment
  geom_density(alpha=0.5)+
  labs(x="prediction value",y="density")+ #,fill="Season Portion",col="Season Portion"
  #geom_vline(xintercept=0.6,col='red',linetype=2)+
  geom_vline(xintercept=0.65,col='red',linetype=2)+
  geom_vline(xintercept=0.7,col='red',linetype=2)+
  ggtitle("WA and OR (dashed lines at 0.65 and 0.7)") +
  theme_bw()+
  theme(#panel.grid.major=element_blank(),
    panel.grid.major = element_line(color="gray90"),
    panel.grid.minor=element_blank(),
    #axis.text.y=element_blank(),
    plot.title = element_text(hjust = 0.5),
  )
WA_and_OR





# bring in group sizing -- WA and OR don't have large groups
# in WA and OR, group sizes 4, 5,6 only 1 record each. but no NAs in group size

WA_and_OR_group_size <- bw_prediction_values_noNA %>% 
  filter(state == 'WA' | state =='OR') %>% 
  ggplot(aes(pred_value,fill=as.factor(group_size),col=as.factor(group_size)))+ #,fill=group_size,col=group_size
  geom_density(alpha=0.5)+
  labs(x="prediction value",y="density",fill="group size",col="group size")+ #,fill="group_size",col="group_size"
  #geom_vline(xintercept=0.6,col='red',linetype=2)+
  geom_vline(xintercept=0.65,col='red',linetype=2)+
  geom_vline(xintercept=0.7,col='red',linetype=2)+
  ggtitle("WA and OR (dashed lines at 0.65 and 0.7)\ngroup sizes 4, 5, 6 only one record each") +
  theme_bw()+
  theme(#panel.grid.major=element_blank(),
    panel.grid.major = element_line(color="gray90"),
    panel.grid.minor=element_blank(),
    #axis.text.y=element_blank(),
    plot.title = element_text(hjust = 0.5),
  )
WA_and_OR_group_size



# # save
# png(paste0(path_figures, "/model predicted values at observed sightings_coastwide_WA and OR.png"), width = 14, height = 10, units = "in", res = 300)
# ggarrange(coastwide,
#           WA_and_OR,
#           WA_and_OR_group_size,
#           ncol=2,
#           nrow=2,
#           legend="right",
#           #labels="auto",
#           vjust=8,
#           hjust=0
# )
# invisible(dev.off())



gt <- arrangeGrob(coastwide, WA_and_OR,
                  WA_and_OR_group_size,   # bar plot spaning two columns
                  ncol = 2, nrow = 2, 
                  layout_matrix = rbind(c(1,2), c(3,3)))

p <- as_ggplot(gt)  # transform to a ggplot
p

png(paste0(path_figures, "/model predicted values at observed sightings_coastwide_WA and OR_v2.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(p)
invisible(dev.off())
