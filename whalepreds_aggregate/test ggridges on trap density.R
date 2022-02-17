#test ggridges on trap density

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

# bring in gridded WA logbook data, with trap density calculated per grid per 2-week step or 1-month step
path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds"
path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_1mon_step.rds"

path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step_REGS_IN_EVERY_SEASON.rds"


x.fish_WA <- readRDS(path.fish_WA) %>% 
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N')) %>%
  #Grid ID 122919 end up having very high trap densities in few months 
  #(e.g., 244pots/km2 in May 2013-2014 season, also high in July 2013-2014
  #this is because the grid is split across land, and few points happen to fall in a very tiny area
  #remove it
filter(GRID5KM_ID != 122919)



#the above data is filtered to be only effort that is in WA waters
#but may have been landed in either WA or OR

# In 2018-19 season there was one vessel in WA logbooks that reportedly fished really far south in OR waters, 
# but only few string lines
# this seems like such an outlier that we will remove it here
# x.fish_WA_filtered <-  x.fish_WA %>% 
#   filter(!GRID5KM_ID %in% c(102128, 102458, 102787, 102788, 103117, 103118))


#summary info of trap densities in May-Sep
summary_x_fish_WA <- x.fish_WA %>% 
  filter(is_May_Sep == 'Y') %>%  
  group_by(season) %>% 
  summarise(max_trap_dens = max(M2_trapdens),
            trap_dens_95th = quantile(M2_trapdens, probs=0.95, na.rm=TRUE),
            trap_dens_99th = quantile(M2_trapdens, probs=0.99, na.rm=TRUE)
            )
summary_pre_reg_trap_dens <- summary_x_fish_WA %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018')) %>% 
  summarise(
    avg_max_trap_dens_pre_reg = mean(max_trap_dens)
  )
# on 2-week step: 59.4   on 1-month step: 60.5
#% change from pre-regs average to 2018-19:
#on 2-week step:
(61.83287-59.4)/59.4*100 #4.095741
#on 1-month step:
(58.97082-60.5)/60.5*100 #-2.52757

#on 2-week step:
#% change from pre-regs average to 2019-20:
(48.77437-59.4)/59.4*100  #-17.88827
#on 1-month step:
(46.66595-60.5)/60.5*100  #-22.8662


#Pre-regulations average 99th percentile of trap density 
#on 2-week step: 42.8   on 1-month step: 45.7
#% change from pre-reg average to 2018-19: 
#on 2-week step: -12.0 
#on 1-month step: -21.4

#2019-20: 
#on 2-week step:-30.6
#on 1-month step:-34.4

#-----------------------------------------------------------------------------------
#The geom geom_density_ridges calculates density estimates from the provided data 
#and then plots those, using the ridgeline visualization. 

#'rel_min_height' = Lines with heights below this cutoff will be removed. 
#'#The cutoff is measured relative to the overall maximum, so rel_min_height=0.01 would remove 
#'everything that is 1\ ridgelines. Default is 0, so nothing is removed. 
#-----------------------------------------------------------------------------------

# May-Sep only
# data left on 2-week time step

x.fish_WA_MaySep <-  x.fish_WA %>% 
  filter(is_May_Sep == "Y") %>% 
  mutate(month_name = factor(month_name, levels = c('September', 'August', 'July', 'June', 'May')))

x.fish_WA_MaySep$season <- factor(x.fish_WA_MaySep$season, levels = c('2019-2020', '2018-2019', '2017-2018', '2016-2017', '2015-2016', '2014-2015', '2013-2014'))


###########################################################################
## THIS IS PERHAPS THE BEST LOOKING ONE
ggplot(x.fish_WA_MaySep, aes(x = M2_trapdens, y = season, height = ..density..)) + 
  geom_density_ridges(stat = "density", 
                      rel_min_height = 0.005, 
                      fill = "#0072B250", 
                      scale = 1.5) + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  #coord_cartesian(clip = "off") +
  xlab("Trap density [pots/km2] (May-Sep)") +
  theme_ridges(grid = TRUE, center_axis_labels = TRUE)
#Trailing tails can be cut off using the rel_min_height aesthetic. 
#This aesthetic sets a percent cutoff relative to the highest point of any of the density curves. 
#A value of 0.01 usually works well, but you may have to modify this parameter for different datasets
# cuts out anything that is below 1% of the top height of the distribution


#colour each quantile
#https://r-charts.com/distribution/ggridges/
#colour palettes: https://colorhunt.co/
pot_density_ridges_quantiles <- ggplot(x.fish_WA_MaySep, aes(x = M2_trapdens, y = season, fill = stat(quantile))) +
  stat_density_ridges(quantile_lines = TRUE,
                      calc_ecdf = TRUE,
                      geom = "density_ridges_gradient",
                      quantiles = c(0.25, 0.5, 0.75),
                      rel_min_height = 0.005,
                      scale = 1.5) +
  scale_fill_manual(name = "Quantile", values = c("#E8DED2", "#A3D2CA", "#5EAAA8", "#056676"),
                    labels = c("0-25%", "25-50%","50-75%", "75-100%")) + 
  #xlim(0,72)+
  scale_x_continuous(limits = c(0, 72), expand = c(0, 0))+
  xlab("Pot density [pots/km2] (May-Sep)") +
  theme_ridges(grid = TRUE, center_axis_labels = TRUE)
pot_density_ridges_quantiles

path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
png(paste0(path_figures, "/pot_density_ridges_quantiles_by crab season.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(pot_density_ridges_quantiles,
          ncol=1,
          nrow=1,
          legend="right",
          #labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())

##############################################################


# If make ggridgeplots on full seasons, no clear difference in the last two
x.fish_WA$season <- factor(x.fish_WA$season, levels = c('2019-2020', '2018-2019', '2017-2018', '2016-2017', '2015-2016', '2014-2015', '2013-2014'))
ggplot(x.fish_WA, aes(x = M2_trapdens, y = season, height = ..density..)) + 
  geom_density_ridges(stat = "density", rel_min_height = 0.001, fill = "#0072B250", scale = 1.5) + #, scale = 1.5
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  #coord_cartesian(clip = "off") +
  xlab("Trap density [pots/km2] ") +
  theme_ridges(grid = TRUE, center_axis_labels = TRUE)



mid <- mean(x.fish_WA_MaySep$M2_trapdens)

ridgeplot_WA_MaySep <- ggplot(x.fish_WA_MaySep, aes(x = M2_trapdens, y = season, fill = ..x..)) + #fill = stat(x)
  geom_density_ridges_gradient(scale = 1.25, rel_min_height = 0.005) + # changing rel_min_height will change the outlook of the plot
  #scale_fill_viridis_c(name = "Trap density [pots/km2]", option = "C") +
  #scale_fill_gradientn(colours = colorspace::diverge_hcl(7)) +
  #scale_fill_gradient2(midpoint = mid, low = "blue", mid = "white", high = "red" )+
  scale_fill_gradient(low = "white", high = "red" )+
  scale_x_continuous(limits = c(0, 65),expand = c(0, 0)) +
  #scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  #labs(title = 'Trap density in WA logbooks, 2013-2020 May-Sep') + 
  theme_ridges(grid = TRUE, center_axis_labels = TRUE)
ridgeplot_WA_MaySep



mid <- mean(log(x.fish_WA_MaySep$M2_trapdens))

ridgeplot_WA_MaySep <- ggplot(x.fish_WA_MaySep, aes(x = log(M2_trapdens), y = season, fill = ..x..)) + #fill = stat(x)
  geom_density_ridges_gradient(scale = 1.25, rel_min_height = 0.005) + # changing rel_min_height will change the outlook of the plot
  #scale_fill_viridis_c(name = "Trap density [pots/km2]", option = "C") +
  #scale_fill_gradientn(colours = colorspace::diverge_hcl(7)) +
  scale_fill_gradient2(midpoint = mid, low = "blue", mid = "white", high = "red" )+
  scale_x_continuous(limits = c(0, 5),expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = 'Trap density in WA logbooks, 2013-2020 May-Sep') + 
  theme_ridges()
ridgeplot_WA_MaySep



ridgeplot_WA_MaySep <- ggplot(x.fish_WA_MaySep, aes(x = M2_trapdens, y = season)) + #fill = stat(x)
  geom_density_ridges(
    jittered_points = TRUE,
    position = position_points_jitter(width = 0.05, height = 0),
    point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7
  ) + 
  theme_ridges()
ridgeplot_WA_MaySep





ggplot(x.fish_WA_MaySep, aes(x = M2_trapdens, y = season, fill = season, color = season)) +
  geom_density_ridges(rel_min_height = 0.005) +
  scale_fill_cyclical(name = "Cycle", values = c("#99E6FF", "#4CA6FF")) +
  scale_color_cyclical(name = "Cycle", values = c(1, 4)) +
  scale_x_continuous(limits = c(0, 65), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Trap density [pots/km2] (May-Sep)") +
  theme_ridges(grid = TRUE, center_axis_labels = TRUE)

#breaking things down by month doesn't really help visually
ggplot(x.fish_WA_MaySep, aes(x = M2_trapdens, y = season, height = stat(density))) + 
  geom_density_ridges(stat = "density", rel_min_height = 0.005, fill = "#0072B250") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  facet_grid(~ month_name) +
  #coord_cartesian(clip = "off") +
  xlab("Trap density [pots/km2]") +
  theme_ridges(grid = TRUE, center_axis_labels = TRUE)




# ggplot(x.fish_WA_MaySep, aes(x = M2_trapdens, y = season, fill = paste(season, month_name))) +
#   geom_density_ridges(
#     alpha = 0.8, color = 'white', from = 0, to = 65
#   ) +
#   labs(
#     x = "trap density",
#     y = "season"
#   )+
#   scale_y_discrete(expand = c(0,0)) +
#   scale_x_continuous(expand = c(0,0))+
#   scale_fill_cyclical(
#     breaks = c('2013-2014 May', '2013-2014 June', '2013-2014 July', '2013-2014 August', '2013-2014 September'),
#     labels = c(`2013-2014 May` = 'May', 
#                `2013-2014 June` = 'June', 
#                `2013-2014 July` = 'July', 
#                `2013-2014 August` = 'August', 
#                `2013-2014 September` = 'September'),
#     values = c("#ff0000", "#0000ff", "#ff8080","#8080ff", "#faa7a7", "#9d9dfa", "#fae3e3", "#d9d9fc"),
#     name = "month", guide = "legend"
#                ) +
#   theme_ridges(grid = FALSE)
  

#the australian athletes example code
# still don't think breaking down by month is helpful
ggplot(x.fish_WA_MaySep, aes(x = M2_trapdens, y = season, 
                             color = month_name, point_color = month_name, fill = month_name)) + #fill = stat(x)
  geom_density_ridges(
    jittered_points = TRUE, scale = 0.95,
    rel_min_height = 0.005,
    position = position_points_jitter(width = 0.05, height = 0),
    point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7
  ) + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = c("#442288", "#6CA2EA", "#B5D33D","#FED23F", "#EB7D5B"),
                    labels = c('may', 'june', 'july', 'august', 'september') 
  ) +
  scale_color_manual(values = c("#442288", "#6CA2EA", "#B5D33D","#FED23F", "#EB7D5B"), guide = "none") +
  coord_cartesian(clip = "off")+
  guides(fill = guide_legend(
    override.aes = list(fill = c("#442288", "#6CA2EA", "#B5D33D","#FED23F", "#EB7D5B"),
                        color = NA, point_color = NA)
  ))+
  theme_ridges()


#-----------------------------------------------------------------------
path.grid.5km <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/five_km_grid_polys_geo.shp"
path.grid.5km.lno <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Grid_5km_landerased.rds"
path.grid.depth <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/weighted_mean_NGDC_depths_for_5km_gridcells.csv"

grid.5km <- st_read(path.grid.5km, quiet = TRUE) # 5km grid
grid.5km.lno <- readRDS(path.grid.5km.lno) # 5km grid, land erased
#glimpse(grid.5km.lno)
grid.depth <- read.csv(path.grid.depth) %>% 
  rename(GRID5KM_ID = Gridcell_ID, depth = AWM_depth_m)

# join 5km grid with depths
grid.key <- left_join(grid.5km %>% st_drop_geometry(), 
                      grid.depth, by = "GRID5KM_ID") # These values come from Blake, and are the average weighted mean (AWM) depth values in meter. Also from Blake: using the weighted mean values is critical for handling grid cells that partially overlap with land, as well as for cells that straddle any isobaths used as depth boundaries.
#this also works without having to drop geometry:
grid.key <- left_join(grid.5km,grid.depth, by = "GRID5KM_ID")
#glimpse(grid.key)

grid_depth_and_fish <- left_join(x.fish_WA_MaySep, grid.key,  by = "GRID5KM_ID")

grid_depth_and_fish_bins <-  grid_depth_and_fish %>% 
  mutate(Bins = cut(depth, breaks = c(-500, -400, -300, -200, -100, 0.1))) 

ggplot(grid_depth_and_fish_bins, aes(x = M2_trapdens, y = season, height = ..density..)) + 
  geom_density_ridges(stat = "density", rel_min_height = 0.005, fill = "#0072B250", scale = 1.2) + #, scale = 1.5
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  #coord_cartesian(clip = "off") +
  facet_grid(~ Bins) +
  xlab("Trap density [pots/km2] (May-Sep)") +
  theme_ridges(grid = TRUE, center_axis_labels = TRUE)

#-----------------------------------------------------------------------








# trap density on a 1-month step 
# this will require using the df on a 2-weekly step (adj_summtraps) and
# taking the average trap density for each grid cell for the desired time period
# --  is it correct to take averages, or is the above plotting on 2-weekly more accurate -- plots pretty similar tho
x.fish_WA_MaySep_month <- x.fish_WA_MaySep %>% 
  group_by(season_month,GRID5KM_ID, grd_x, grd_y, AREA) %>% 
  summarise( 
    number_obs = n(), #no. of grid cells in that season_month that had traps in them 
    mean_M2_trapdens = mean(M2_trapdens)
  )  %>%
  separate(season_month, into = c("season", "month_name"), sep = "_") %>%
  mutate(season_month = paste0(season,"_",month_name)) %>%
  mutate(month_name = factor(month_name, levels = c('December','January','February','March','April','May','June','July','August','September','October','November'))) %>% 
  filter(!is.na(month_name)) 
glimpse(x.fish_WA_MaySep_month)

x.fish_WA_MaySep_month$season <- factor(x.fish_WA_MaySep_month$season, levels = c('2019-2020', '2018-2019', '2017-2018', '2016-2017', '2015-2016', '2014-2015', '2013-2014'))

ggplot(x.fish_WA_MaySep_month, aes(x = mean_M2_trapdens, y = season, height = stat(density))) + 
  geom_density_ridges(stat = "density", rel_min_height = 0.005, fill = "#0072B250") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  #coord_cartesian(clip = "off") +
  xlab("Trap density [pots/km2]") +
  theme_ridges(grid = TRUE, center_axis_labels = TRUE)

ggplot(x.fish_WA_MaySep_month, aes(x = log2(mean_M2_trapdens), y = season, height = stat(density))) + 
  geom_density_ridges(stat = "density", rel_min_height = 0.005, fill = "#0072B250") +
  #scale_x_continuous(expand = c(0, 0)) +
  #scale_y_discrete(expand = c(0, 0)) +
  #coord_cartesian(clip = "off") +
  xlab("Log of trap density [pots/km2]") +
  theme_ridges(grid = TRUE, center_axis_labels = TRUE)

