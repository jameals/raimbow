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

# bring in gridded WA logbook data, with trap density calculated per grid per 2-week period
path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2013_2020.rds"
x.fish_WA <- readRDS(path.fish_WA) %>% 
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N'))


# In 2018-19 season there was one vessel in WA logbooks that reportedly fished really far south in OR waters, 
# but only few string lines
# this seems like such an outlier that we will remove it here
x.fish_WA_filtered <-  x.fish_WA %>% 
  filter(!GRID5KM_ID %in% c(102128, 102458, 102787, 102788, 103117, 103118))




## HERE WOULD ALSO REMOVE GRIDS THAT ARE IN OR WATERS (BUT WA LOGBOOKS)
## AND ADD IN OR LOGS THAT WHERE IN WA GRIDS



#-----------------------------------------------------------------------------------

# If make ggridgeplots on full seasons, no clear difference in the last two

# May-Sep only
# data left on 2-week time step

x.fish_WA_MaySep <-  x.fish_WA_filtered %>% 
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N')) %>% 
  filter(is_May_Sep == "Y") 
  
x.fish_WA_MaySep$season <- factor(x.fish_WA_MaySep$season, levels = c('2019-2020', '2018-2019', '2017-2018', '2016-2017', '2015-2016', '2014-2015', '2013-2014'))

mid <- mean(x.fish_WA_MaySep$M2_trapdens)

ridgeplot_WA_MaySep <- ggplot(x.fish_WA_MaySep, aes(x = M2_trapdens, y = season, fill = ..x..)) + #fill = stat(x)
  geom_density_ridges_gradient(scale = 1.25, rel_min_height = 0.005) + # changing rel_min_height will change the outlook of the plot
  #scale_fill_viridis_c(name = "Trap density [pots/km2]", option = "C") +
  #scale_fill_gradientn(colours = colorspace::diverge_hcl(7)) +
  scale_fill_gradient2(midpoint = mid, low = "blue", mid = "white", high = "red" )+
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

ggplot(x.fish_WA_MaySep, aes(x = M2_trapdens, y = season, height = stat(density))) + 
  geom_density_ridges(stat = "density", rel_min_height = 0.005, fill = "#0072B250") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  #coord_cartesian(clip = "off") +
  xlab("Trap density [pots/km2]") +
  theme_ridges(grid = TRUE, center_axis_labels = TRUE)






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

