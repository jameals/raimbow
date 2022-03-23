#testing a glm on risk

#-----------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(rnaturalearth) #devtools::install_github("ropensci/rnaturalearthhires")
library(sf)
library(rgeos)
library(viridis)
library(ggpubr)

#-----------------------------------------------------------------------------------
#bring in some grids
path.grid.5km <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/five_km_grid_polys_geo.shp"
path.grid.5km.lno <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Grid_5km_landerased.rds"
path.grid.depth <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/weighted_mean_NGDC_depths_for_5km_gridcells.csv"

grid.5km <- st_read(path.grid.5km, quiet = TRUE) # 5km grid
grid.5km.lno <- readRDS(path.grid.5km.lno) # 5km grid, land erased
#glimpse(grid.5km.lno)
grid.depth <- read.csv(path.grid.depth) %>% 
  rename(GRID5KM_ID = Gridcell_ID, depth = AWM_depth_m)


path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures" #not uploading to GitHub
#path_figures <- "C:/Users/Leena.Riekkola/Projects/raimbow/whalepreds_aggregate/figures" #or use this if do want to upload to GitHub
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------

#fishing effort

path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds"

x.fish_WA <- readRDS(path.fish_WA)
#Grid ID 122919 end up having very high trap densities in few months 
#(e.g., 244pots/km2 in May 2013-2014 season, also high in July 2013-2014
#this is because the grid is split across land, and few points happen to fall in a very tiny area
#remove it
x.fish_WA <- x.fish_WA %>% filter(GRID5KM_ID != 122919)
# get avg traps dens per grid cell for each yr month to allow matching with whale data
x.fish_WA2 <- x.fish_WA %>%
  group_by(season_month, GRID5KM_ID, grd_x, grd_y) %>% #remove AREA as grouping factor here
  summarise( 
    number_obs = n(), #no. of grid cells in that season_month that had traps in them 
    mean_M2_trapdens = mean(M2_trapdens), 
  )

# make column for year month for fishing data to allow matching with whale data
x.fish_WA_MaySep <- x.fish_WA2 %>%
  separate(season_month, into = c("season", "month_name"), sep = "_") %>%
  mutate(month = match(month_name, month.name)) %>% #month becomes one digit number
  mutate(month = sprintf("%02d", as.numeric(month))) %>% #change month to two digit number
  #create column for summer season (May-Sep) in case want to filter to that at some point
  mutate(is_May_Sep = 
           ifelse(month %in% c('05', '06', '07', '08', '09')
                  ,'Y', 'N'))

x.fish_WA_MaySep$month_name <- factor(x.fish_WA_MaySep$month_name, 
                                  levels = c('December', 'January', 'February', 'March', 'April', 'May', 
                                             'June', 'July', 'August', 'September'))


#-----------------------------------------------------------------------------------


#whale data

#HW data 2009-July 2019
#path.hump <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_long_monthly.rds"
#New hw data pull 2009 to 2020
path.hump_2009_2020 <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/Humpback_5km_long_MONTHLY2009_2020_20211028.rds"
#bw 2009-Jul 2019
path.blue <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/BlueWhale_5km_long_monthly.rds"
#New bw data pull Aug 2019 to Sep 2021
path.blue_2019_2021 <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/data/BlueWhale_5km_long_monthly_2019Aug_2021Sep.rds"


#hw output 2009-2020
x.hump_2009_2020 <- readRDS(path.hump_2009_2020) %>%
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Humpback_dens_mean) #Humpback_dens_se
glimpse(x.hump_2009_2020)


#bw output 2009-July 2019
x.blue <- readRDS(path.blue) %>%
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Blue_occurrence_mean, Blue_occurrence_se)
glimpse(x.blue)

#bw output Aug 2019-Sep 2021
x.blue_2019_2021 <- readRDS(path.blue_2019_2021) %>%
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Blue_occurrence_mean, Blue_occurrence_se)
glimpse(x.blue_2019_2021) #why does data end June 2021??

#join the 2 bw dfs
x.blue.all <- rbind(x.blue, x.blue_2019_2021)


# join blue and hump whale outputs
#x.whale <- full_join(x.hump, x.blue, 
x.whale <- full_join(x.hump_2009_2020, x.blue.all, 
                     by = c("GRID5KM_ID", "year_month")) %>% # full_join ensures we retain cells with hump but not blue predictions and vice versa
  left_join(st_drop_geometry(grid.5km.lno), by = "GRID5KM_ID") # adds grid cell area

x.whale_crab_season <- x.whale %>% 
  separate(year_month, into = c("year", "month"), sep = "_") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(season_start = ifelse(month == "12", year, year-1)) %>% 
  mutate(season_end = ifelse(month == "12", year+1, year)) %>% 
  mutate(season = paste0(season_start,"-",season_end))


x.whale_crab_season_May_Sep <-  x.whale_crab_season %>% 
  filter(month %in% c('05', '06', '07', '08', '09')) %>% 
  select(-season_start, -season_end)
#-----------------------------------------------------------------------------------
# 
# # if working in the realised fishery footprint:
# 
# #join whale data to fishing data
# fishing_whale_joined <- left_join(x.fish_WA_MaySep, x.whale_crab_season, by=c("GRID5KM_ID", "season", "month")) %>% 
#   select(-season_start, -season_end, -area_km_lno)
# 
# #calculate risk  metric
# risk_fishing_whale_joined <- fishing_whale_joined %>%
#   mutate(
#     hump_risk = Humpback_dens_mean * mean_M2_trapdens,
#     blue_risk = Blue_occurrence_mean * mean_M2_trapdens
#   ) %>% 
#   #if there is no whale data in grid, then risk is NA, as out of bounds of whale model
#   mutate(hump_risk = 
#            ifelse(is.na(Humpback_dens_mean), NA, hump_risk),
#          blue_risk = 
#            ifelse(is.na(Blue_occurrence_mean), NA, blue_risk)
#   ) 
# 
# #add column to denote pre-post regulation
# risk_fishing_whale_joined <- risk_fishing_whale_joined %>% 
#   mutate(
#   pre_post_reg = case_when(
#     season == '2018-2019' & month %in% c('07', '08', '09') ~ "xpost-reg",  #and 'x' to name so that pre-reg comes first
#     season == '2019-2020' & month %in% c('05', '06', '07', '08', '09') ~ "xpost-reg")) %>% 
#   mutate(pre_post_reg = ifelse(is.na(pre_post_reg), 'pre-reg', pre_post_reg))
#     
# 
# risk_fishing_whale_joined$month <- as.numeric(risk_fishing_whale_joined$month)

#-----------------------------------------------------------------------------------
#if work in consistent study area

#'study area' created in QGIS, to encompass all fished grids plus 'buffer' (grids that could be fished)
#read in 'study area' (grid)
study_area <- read_sf(here::here('wdfw','data', 'study_area.shp'))
glimpse(study_area)
#plot(study_area)

study_area_grids_id <- sort(unique(study_area$GRID5KM_ID)) 

study_area_df <- as.data.frame(study_area_grids_id) %>% 
  rename(GRID5KM_ID = study_area_grids_id)


#the study area grid needs to have all season-month combos for May-Sep
season <- c("2013-2014", "2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019", "2019-2020")
month <- as.factor(c("05", "06", "07", "08", "09"))
season_month_combos <- crossing(season, month)
study_area_df_with_all_season_month_combos <- crossing(study_area_df, season_month_combos) %>%
  #and add to that the column to denote study area
  mutate(study_area = 'Y')


#join whale data to study area grid
study_area_whale <- full_join(study_area_df_with_all_season_month_combos, x.whale_crab_season_May_Sep, by=c("GRID5KM_ID", "season", "month"))

#join fishing data to study area grid with whale data
study_area_whale_fishing <- left_join(study_area_whale, x.fish_WA_MaySep, by=c("GRID5KM_ID", "season", "month")) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))


#calculate risk  metric
risk_whales_WA_MaySep <- study_area_whale_fishing %>%
  mutate(
    hump_risk = Humpback_dens_mean * mean_M2_trapdens,
    blue_risk = Blue_occurrence_mean * mean_M2_trapdens
  ) %>% 
  #if there is no fishing data in grid, then risk is 0, as there is no fishing
  mutate(hump_risk = 
           ifelse(is.na(mean_M2_trapdens), 0, hump_risk),
         blue_risk = 
           ifelse(is.na(mean_M2_trapdens), 0, blue_risk)
  ) %>%
  #if there is no whale data in grid, then risk is NA, as out of bounds of whale model
  mutate(hump_risk = 
           ifelse(is.na(Humpback_dens_mean), NA, hump_risk),
         blue_risk = 
           ifelse(is.na(Blue_occurrence_mean), NA, blue_risk)
  ) %>%
  mutate(is_May_Sep = 
           ifelse(month %in% c('05', '06', '07', '08', '09')
                  ,'Y', 'N')) %>% 
  mutate(
    pre_post_reg = case_when(
      season == '2018-2019' & month %in% c('07', '08', '09') ~ "xpost-reg",  #and 'x' to name so that pre-reg comes first
      season == '2019-2020' & month %in% c('05', '06', '07', '08', '09') ~ "xpost-reg")) %>% 
  mutate(pre_post_reg = ifelse(is.na(pre_post_reg), 'pre-reg', pre_post_reg))



#-----------------------------------------------------------------------------------

#glm


#choosing family for glm:
#https://stats.stackexchange.com/questions/190763/how-to-decide-which-glm-family-to-use
#"If you are dealing with continuous non-negative outcome, then you could consider the Gamma distribution, 
#or Inverse Gaussian distribution."

#https://stats.stackexchange.com/questions/67547/when-to-use-gamma-glms
#"Given skewed positive data I will often find myself trying gamma and lognormal models 
#(in GLM context log link, normal or Gaussian family) and choosing which works better."





#Jameal message on Slack: focus on jul-sep data only, and consider a model that is 
#summed_risk ~ month + year + pre_post_reg. in this model pre_post_reg includes the 2018-19 and 2019-20 seasons 
#and won't be perfectly correlated. the downside is we lose 2 months of data annually for the 
#comparison of 2019-20 to 2013-2018. so i would suggest you run a separate glm using may-sep data 
#for 2013-18 and 2019-20 data only. that model would be summed_risk ~ month + year,  and  
#a sig effect of the 2019-20 year would indicate an influence of the regulations and/or internannual variability.


# Jul-Sep, all seasons, summed data across all grids in each month
risk_whales_WA_JulSep_summed <- risk_whales_WA_MaySep %>% 
  filter(month %in% c('07', '08', '09')) %>% 
  filter(season != '2019-2020') %>% 
  filter(study_area == 'Y')  %>% 
  group_by(season, month, pre_post_reg) %>%
  summarise(sum_hump_risk = sum(hump_risk, na.rm = T),
            sum_blue_risk = sum(blue_risk, na.rm = T))

hist(risk_whales_WA_JulSep_summed$sum_hump_risk)
hist(risk_whales_WA_JulSep_summed$sum_blue_risk)

#risk_whales_WA_JulSep_summed <- risk_whales_WA_JulSep_summed %>% 
#  mutate(month_num = as.integer(month))

library(ggcorrplot)
model.matrix(~0+., data=risk_whales_WA_JulSep_summed) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

#retain month in glm even tho it is not significant in Jul-Sep comparison
#in the Jul-Sep glm log link, or other family types, doesn't improve things from basic gaussian
#if use Gamma, results don't make sense/match the plots
mod1_hump <- glm(sum_hump_risk ~ month + pre_post_reg,
               family=gaussian, data=risk_whales_WA_JulSep_summed, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_hump)
hist(mod1_hump$residuals)

scatter.smooth(fitted(mod1_hump), residuals(mod1_hump, type = "pearson"),
               #mgp = c(2.2, 1, 0),
               ylab = "Residuals (Pearson)",
               xlab = "Predicted")
title("Residual plot", line = 0.7)
abline(h = 0, col="blue")

plot(mod1_hump)



# #just pre-post-reg as the only variable
# mod1_hump <- glm(sum_hump_risk ~ pre_post_reg,
#                  family=gaussian, data=risk_whales_WA_JulSep_summed, na.action = na.omit) #family = gaussian(link = "log")
# summary(mod1_hump)
# hist(mod1_hump$residuals)
# 
# scatter.smooth(fitted(mod1_hump), residuals(mod1_hump, type = "pearson"),
#                #mgp = c(2.2, 1, 0),
#                ylab = "Residuals (Pearson)",
#                xlab = "Predicted")
# title("Residual plot", line = 0.7)
# abline(h = 0, col="blue")
# 
# plot(mod1_hump)

library(car)
qqPlot(mod1_hump$residuals)

# #Kruskal-Wallis is testing for differences in the medians
# kruskal.test(sum_hump_risk ~ pre_post_reg, data = risk_whales_WA_JulSep_summed)
# #testing the equality of means/medians in two independent samples
# #the Mann-Whitney test is commonly regarded as a test of population medians
# wilcox.test(sum_hump_risk ~ pre_post_reg, data = risk_whales_WA_JulSep_summed)
# 
# risk_whales_WA_JulSep_summed %>% 
#   group_by(pre_post_reg) %>%  
#   summarise(median_hump_risk = median(sum_hump_risk),
#             mean_hump_risk = mean(sum_hump_risk),
#             median_blue_risk = median(sum_blue_risk),
#             mean_blue_risk = mean(sum_blue_risk))
#when 2020 is included
#pre_post_reg median_hump_risk  median_blue_risk  mean_hump_risk  mean_blue_risk
#pre-reg        16.137996         260.6932          17.044269       253.3776
#xpost-reg      4.843725          197.1785          5.030699          190.7951
##HW: median: -70.0%    mean: -70.5
##BW: -24.4%

#when 2020 is excluded
#pre_post_reg median_hump_risk  median_blue_risk  mean_hump_risk  mean_blue_risk
#pre-reg        16.137996         260.6932            17.044269       253.3776
#xpost-reg      4.237895          231.1482            3.897254        222.8370
##HW: median -73.7%,      mean -77.1%
##BW: -11.3%,             mean -12.1%


mod1_blue <- glm(sum_blue_risk ~ month + pre_post_reg,
                 family=gaussian, data=risk_whales_WA_JulSep_summed, na.action = na.omit)
# mod1_blue <- glm(sum_blue_risk ~ pre_post_reg,
#                  family=gaussian, data=risk_whales_WA_JulSep_summed, na.action = na.omit)
summary(mod1_blue)
hist(mod1_blue$residuals)

plot(mod1_blue)

scatter.smooth(fitted(mod1_hump), residuals(mod1_blue, type = "pearson"),
               #mgp = c(2.2, 1, 0),
               ylab = "Residuals (Pearson)",
               xlab = "Predicted")
title("Residual plot", line = 0.7)
abline(h = 0, col="blue")

qqPlot(mod1_blue$residuals)


#wilcox.test(sum_blue_risk ~ pre_post_reg, data = risk_whales_WA_JulSep_summed)



#separate glm using may-sep data for 2013-18 and 2019-20 data only.
#that model would be summed_risk ~ month + year
risk_whales_WA_MaySep_summed <- risk_whales_WA_MaySep %>% 
  filter(season != '2018-2019') %>% 
  filter(study_area == 'Y')  %>% 
  group_by(season, month, pre_post_reg) %>%
  summarise(sum_hump_risk = sum(hump_risk, na.rm = T),
            sum_blue_risk = sum(blue_risk, na.rm = T))

#risk_whales_WA_MaySep_summed <- risk_whales_WA_MaySep_summed %>% 
#  mutate(month_num = as.integer(month))

hist(risk_whales_WA_MaySep_summed$sum_hump_risk)
hist(risk_whales_WA_MaySep_summed$sum_blue_risk)

model.matrix(~0+., data=risk_whales_WA_MaySep_summed) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)


mod2_hump <- glm(sum_hump_risk ~ month + pre_post_reg,
                 family=gaussian, data=risk_whales_WA_MaySep_summed, na.action = na.omit) #family = gaussian(link = "inverse")
summary(mod2_hump) #interpreting significant intercept: It means you have enough evidence to say that the intercept isn't 0.
#We typically don't care if the intercept is significant or not. It's important to have in the model but unless there is a good reason to typically you don't interpret the significance test of the intercept
hist(mod2_hump$residuals)
plot(mod2_hump)

scatter.smooth(fitted(mod2_hump), residuals(mod2_hump, type = "pearson"),
               #mgp = c(2.2, 1, 0),
               ylab = "Residuals (Pearson)",
               xlab = "Predicted")
title("Residual plot", line = 0.7)
abline(h = 0, col="blue")

qqPlot(mod2_hump$residuals)

# wilcox.test(sum_hump_risk ~ pre_post_reg, data = risk_whales_WA_MaySep_summed)
# 
# risk_whales_WA_MaySep_summed %>% 
#   group_by(pre_post_reg) %>%  
#   summarise(median_hump_risk = median(sum_hump_risk), 
#             mean_hump_risk = mean(sum_hump_risk),
#             median_blue_risk = median(sum_blue_risk),
#             mean_blue_risk = mean(sum_blue_risk))
#	comparing pre-reg to 2020 only
#pre_post_reg median_hump_risk  median_blue_risk    mean_hump_risk  mean_blue_risk
#pre-reg        17.811136         180.8941            18.795108         192.7972
#xpost-reg      7.527816          157.0247            9.150597          149.8488
##HW: median -57.7,   mean -51.3
##BW: median -13.2,   mean -22.3



#in the May-Sep glm log link is maybe better than basic gaussian
mod2_blue <- glm(sum_blue_risk ~ month + pre_post_reg ,
                 family=gaussian, data=risk_whales_WA_MaySep_summed, na.action = na.omit) #family = gaussian(link = "inverse")
summary(mod2_blue)
hist(mod2_blue$residuals)

scatter.smooth(fitted(mod2_blue), residuals(mod2_blue, type = "pearson"),
               #mgp = c(2.2, 1, 0),
               ylab = "Residuals (Pearson)",
               xlab = "Predicted")
title("Residual plot", line = 0.7)
abline(h = 0, col="blue")

plot(mod2_blue)
qqPlot(mod2_blue$residuals)

#wilcox.test(sum_blue_risk ~ pre_post_reg, data = risk_whales_WA_MaySep_summed)









































#-----------------------------------------------------------------------------------

#what if we standardise after summing...?
# Jul-Sep, all seasons, summed data across all grids in each month
risk_whales_WA_JulSep_summed <- risk_whales_WA_MaySep %>% 
  filter(month %in% c('07', '08', '09')) %>% 
  filter(study_area == 'Y')  %>% 
  group_by(season, month, pre_post_reg) %>%
  summarise(sum_hump_risk = sum(hump_risk, na.rm = T),
            sum_blue_risk = sum(blue_risk, na.rm = T))


sd(risk_whales_WA_JulSep_summed$sum_hump_risk)
mean(risk_whales_WA_JulSep_summed$sum_hump_risk)
min(risk_whales_WA_JulSep_summed$sum_hump_risk)
max(risk_whales_WA_JulSep_summed$sum_hump_risk)

risk_whales_WA_JulSep_summed_standardized <- risk_whales_WA_JulSep_summed %>% 
  mutate(hw_stand_risk = (sum_hump_risk-13.61182)/8.672362, #use sd in the standardization 
         hw_stand_risk2 = (sum_hump_risk-3.118496)/(36.88234-3.118496) #normalize to range from 1-0
         )

risk_whales_WA_JulSep_summed_standardized <- risk_whales_WA_JulSep_summed_standardized %>% 
  mutate(month_num = as.integer(month))


hist(risk_whales_WA_JulSep_summed_standardized$hw_stand_risk2)
hist(risk_whales_WA_JulSep_summed_standardized$sum_blue_risk)



#what if we standardise before summing...?
# Jul-Sep, all seasons, summed data across all grids in each month

sd(risk_whales_WA_MaySep$hump_risk, na.rm=T )
mean(risk_whales_WA_MaySep$hump_risk, na.rm=T )
min(risk_whales_WA_MaySep$hump_risk, na.rm=T )
max(risk_whales_WA_MaySep$hump_risk, na.rm=T )

risk_whales_WA_MaySep_standardized <- risk_whales_WA_MaySep %>% 
  mutate(hw_stand_risk = (hump_risk-0.001051685)/0.02671779, #use sd in the standardization 
         hw_stand_risk2 = (hump_risk-0)/(2.596234-0) #normalize to range from 1-0
  )

risk_whales_WA_MaySep_standardized_summed <- risk_whales_WA_MaySep_standardized %>% 
  filter(month %in% c('07', '08', '09')) %>% 
  filter(study_area == 'Y')  %>% 
  group_by(season, month, pre_post_reg) %>%
  summarise(sum_hump_risk_stand = sum(hw_stand_risk, na.rm = T),
            sum_hump_risk_stand2 = sum(hw_stand_risk2, na.rm = T))



risk_whales_WA_MaySep_standardized_summed <- risk_whales_WA_MaySep_standardized_summed %>% 
  mutate(month_num = as.integer(month))


hist(risk_whales_WA_MaySep_standardized_summed$sum_hump_risk_stand)
hist(risk_whales_WA_MaySep_standardized_summed$sum_hump_risk_stand2)



library(ggcorrplot)
model.matrix(~0+., data=risk_whales_WA_JulSep_summed_standardized) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

#in the Jul-Sep glm log link doesn't improve things from basic gaussian
mod3_hump <- glm(sum_hump_risk_stand2 ~ month_num + pre_post_reg,
                 family=gaussian, data=risk_whales_WA_MaySep_standardized_summed, na.action = na.omit) #family = gaussian(link = "inverse")
summary(mod3_hump)
hist(mod3_hump$residuals)

scatter.smooth(fitted(mod3_hump), residuals(mod3_hump, type = "pearson"),
               #mgp = c(2.2, 1, 0),
               ylab = "Residuals (Pearson)",
               xlab = "Predicted")
title("Residual plot", line = 0.7)
abline(h = 0, col="blue")
##the residual plot for either standardization isn't any different to the non-standardized version

plot(mod3_hump)




















#-----------------------------------------------------------------------------------

#probably want to focus on summer season only

#HW
#risk_whales_WA_MaySep -- constant study area
#risk_fishing_whale_joined -- fished grids only

risk_fishing_whale_joined_MaySep <- risk_whales_WA_MaySep %>% 
  filter(is_May_Sep=='Y') 


hist(risk_fishing_whale_joined_MaySep$hump_risk)


m1_hump <- glm(hump_risk ~ month_name + season + pre_post_reg,
               family=Gamma, data=risk_fishing_whale_joined_MaySep, na.action = na.omit)
summary(m1_hump)


#compare Jul-Sep only -- 
#getting NA in summary for whatever is last variable,
#because pre_post_reg and season are linearly correlated/dependent

risk_fishing_whale_joined_JulSep <-  risk_fishing_whale_joined %>% 
  filter(month %in% (7:9)) 
  

risk_fishing_whale_joined_JulSep$month_name <- droplevels(risk_fishing_whale_joined_JulSep$month_name)

m3_hump <- glm(hump_risk ~  month_name +  pre_post_reg  + season+1,
               family=gaussian, data=risk_fishing_whale_joined_JulSep, na.action = na.omit)
summary(m3_hump)



#sum across grids
risk_fishing_whale_joined_MaySep_sum_month <- risk_fishing_whale_joined_MaySep %>% 
  filter(season != '2018-2019') %>% 
  #if used fished only grids df:
  #group_by(season, month_name, pre_post_reg) %>% 
  #if used constant study area:
  filter(study_area == 'Y')  %>% 
  group_by(season, month, pre_post_reg) %>%
  summarise(sum_hump_risk = sum(hump_risk, na.rm = T),
            sum_blue_risk = sum(blue_risk, na.rm = T))

hist(risk_fishing_whale_joined_MaySep_sum_month$sum_hump_risk)
hist(risk_fishing_whale_joined_MaySep_sum_month$sum_blue_risk)

#perfect correlation with pre-post-reg and season 2019-2020 as all months have regs
library(ggcorrplot)
model.matrix(~0+., data=risk_fishing_whale_joined_MaySep_sum_month) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

m2_hump <- glm(sum_hump_risk ~ month_name + season + pre_post_reg,
               family=gaussian, data=risk_fishing_whale_joined_MaySep_sum_month, na.action = na.omit)
summary(m2_hump)

#pre_post_reg and season are linearly dependent (colinear) -- remove season
m2b_hump <- glm(sum_hump_risk ~ pre_post_reg + month_name,
               family=gaussian, data=risk_fishing_whale_joined_MaySep_sum_month, na.action = na.omit)
summary(m2b_hump)

#just pre-post-reg
m2c_hump <- glm(sum_hump_risk ~ pre_post_reg,
                family=gaussian, data=risk_fishing_whale_joined_MaySep_sum_month, na.action = na.omit)
summary(m2c_hump)

#compare Jul-Sep only -- 
risk_fishing_whale_joined_JulSep_sum <-  risk_whales_WA_MaySep %>% 
  #if used fished only grids df:
  #filter(month %in% (7:9)) %>% 
  #if used constant study area:
  filter(month %in% c('07', '08','09')) %>% 
  filter(study_area == 'Y')  %>% 
  filter(season != '2019-2020') %>% 
  group_by(season, month, pre_post_reg) %>% 
  summarise(sum_hump_risk = sum(hump_risk, na.rm = T),
            sum_blue_risk = sum(blue_risk, na.rm = T))

#risk_fishing_whale_joined_JulSep_sum$month_name <- droplevels(risk_fishing_whale_joined_JulSep_sum$month_name)

hist(risk_fishing_whale_joined_JulSep_sum$sum_hump_risk)
hist(risk_fishing_whale_joined_JulSep_sum$sum_blue_risk)


library(ggcorrplot)
model.matrix(~0+., data=risk_fishing_whale_joined_JulSep_sum) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)


m4_hump <- glm(sum_hump_risk ~ month_name + pre_post_reg + season ,
               family=gaussian, data=risk_fishing_whale_joined_JulSep_sum, na.action = na.omit)
summary(m4_hump)

m4b_hump <- glm(sum_hump_risk ~  pre_post_reg + month_name,
               family=gaussian, data=risk_fishing_whale_joined_JulSep_sum, na.action = na.omit)
summary(m4b_hump)

m4c_hump <- glm(sum_hump_risk ~  pre_post_reg,
                family=gaussian, data=risk_fishing_whale_joined_JulSep_sum, na.action = na.omit)
summary(m4c_hump)


#BW
hist(risk_fishing_whale_joined_MaySep$blue_risk)

m1_blue <- glm(blue_risk ~ month_name + season + pre_post_reg,
               family=Gamma, data=risk_fishing_whale_joined_MaySep)
summary(m1_blue)
  

hist(risk_fishing_whale_joined_MaySep_sum_month$sum_blue_risk)

m2_blue <- glm(sum_blue_risk ~ month_name + season + pre_post_reg,
               family=gaussian, data=risk_fishing_whale_joined_MaySep_sum_month, na.action = na.omit)
summary(m2_blue)

#################################################################################
# For the 'Option 2' boxplots -- maybe instead of summing across months to have one value
#per grid, we need to sum across grids and have one value per month (as done above for glm)

##Jul-Sep
plot_subset_2018_2019_box <- risk_fishing_whale_joined_MaySep_sum_month %>% 
  filter(month_name %in% c('July', 'August', 'September')) %>% 
  filter(season %in% c('2018-2019')) %>% 
  mutate(pre_post_reg =  "2018-2019") %>% 
  mutate(pre_post_reg = as.factor(pre_post_reg))

plot_subset_pre_reg_box <- risk_fishing_whale_joined_MaySep_sum_month %>% 
  filter(month_name %in% c('July', 'August', 'September')) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018')) %>% 
  mutate(pre_post_reg = as.factor(pre_post_reg))

box_hump_risk_Jul_Sep_pre_reg_vs_2018_2019 <- ggplot() +
  geom_boxplot(data = plot_subset_pre_reg_box, aes(x = pre_post_reg, y = sum_hump_risk )) +
  geom_boxplot(data = plot_subset_2018_2019_box, aes(x = pre_post_reg, y = sum_hump_risk )) +
  ylab("humpback Whale Risk Jul-Sep") + 
  xlab("") +
  scale_x_discrete(limits = rev) +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        strip.text = element_text(size=20),
        strip.background = element_blank(),
        strip.placement = "left"
  )
box_hump_risk_Jul_Sep_pre_reg_vs_2018_2019


##May-Sep
MaySep_plot_subset_2019_2020_box <- risk_fishing_whale_joined_MaySep_sum_month %>% 
  filter(month_name %in% c('May', 'June', 'July', 'August', 'September')) %>% 
  filter(season %in% c('2019-2020')) %>% 
  mutate(pre_post_reg =  "2019-2020") %>% 
  mutate(pre_post_reg = as.factor(pre_post_reg))

MaySep_plot_subset_pre_reg_box <- risk_fishing_whale_joined_MaySep_sum_month %>% 
  filter(month_name %in% c('May', 'June', 'July', 'August', 'September')) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018')) %>% 
  mutate(pre_post_reg = as.factor(pre_post_reg))


box_hump_risk_Jul_Sep_pre_reg_vs_2019_2020_MaySep <- ggplot() +
  geom_boxplot(data = MaySep_plot_subset_pre_reg_box, aes(x = pre_post_reg, y = sum_hump_risk)) +
  geom_boxplot(data = MaySep_plot_subset_2019_2020_box, aes(x = pre_post_reg, y = sum_hump_risk)) +
  ylab("humpback Whale Risk May-Sep") + 
  xlab("") +
  scale_x_discrete(limits = rev) +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        strip.text = element_text(size=20),
        strip.background = element_blank(),
        strip.placement = "left"
  )
box_hump_risk_Jul_Sep_pre_reg_vs_2019_2020_MaySep

