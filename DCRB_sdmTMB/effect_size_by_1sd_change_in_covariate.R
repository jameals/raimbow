
#-------------------------------------------------------------------------------------------------

library(here)
library(ggplot2)
library(tidyverse)
library(sdmTMB)
library(sf)
library(ggcorrplot)
library(mgcv)
library(ggeffects)
library(tictoc)
library(plotmo)
library(viridis)
library(ggridges)
library(ggpubr)

#this was needed for sdmTMB to work
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)

#-------------------------------------------------------------------------------------------------

###WINTER

#-------------------------------------------------------------------------------------------------

#read in winter data - the version where z-scoring is done across winter only
winter <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_winter_20230324.rds'))
glimpse(winter) 

winter$month_name_f <- factor(winter$month_name, levels = c("December", "January", "February", "March", "April"))

# Add UTM columns (zone 10)
winter = add_utm_columns(winter, ll_names = c("grd_x", "grd_y"))


mesh_winter <- make_mesh(winter, xy_cols = c("X","Y"), cutoff = 10)
mesh_winter$mesh$n


fit19b_winter <-  read_rds(here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC','winter','after fixing fuel and crab price',"fit19b_winter.rds"))

#SST
dummy_SST <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','winter',"dummy_df_SST.csv"))
dummy_SST$half_month_of_seasonf <- as.factor(dummy_SST$half_month_of_seasonf)
predictions_SST <- predict(fit19b_winter, newdata = dummy_SST)


#depth
dummy_depth <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','winter',"dummy_df_depth.csv"))
dummy_depth$half_month_of_seasonf <- as.factor(dummy_depth$half_month_of_seasonf)
predictions_depth <- predict(fit19b_winter, newdata = dummy_depth, `se_fit` = TRUE)

ggplot(data=predictions_depth, aes(x=z_depth_point_mean, y=est, group=1)) +
  geom_line()+
  geom_ribbon(aes(ymin=est-est_se*qnorm(0.975), ymax=est+est_se*qnorm(0.975), alpha=0.2))+
  geom_point()+
  #scale_color_grey() + 
  theme_classic()


predictions_depth_v2_winter <- predictions_depth %>% 
  mutate(depth = case_when(z_depth_point_mean == -2.0 ~ -193, 
                           z_depth_point_mean == -1.5 ~ -167, 
                           z_depth_point_mean == -1.0 ~ -140, 
                           z_depth_point_mean == -0.5 ~ -114, 
                           z_depth_point_mean == 0.0 ~ -87, 
                           z_depth_point_mean == 0.5 ~ -61, 
                           z_depth_point_mean == 1.0 ~ -34, 
                           z_depth_point_mean == 1.5 ~ -8)) %>%  
  #z_depth_point_mean == 2.0  ~ 0)) %>%  #raw data doesn't go as far as 2 on z scale 
  mutate(est_backtransformed = exp(est),
         est_se_backtransformed = exp(est_se))

#backtransformed ribbon uses 95%CI
depth_plot_winter <-ggplot(data=predictions_depth_v2_winter, aes(x=depth, y=est_backtransformed, group=1)) +
  geom_ribbon(aes(ymin=est_backtransformed-est_se_backtransformed*qnorm(0.975), ymax=est_backtransformed+est_se_backtransformed*qnorm(0.975), alpha=0.5), fill="#68a2b9")+
  geom_line(size = 1.2)+
  geom_point(size = 2.4)+
  #scale_color_grey() + 
  #ylim(0,820)+ #if wanted to have all depth plots in same scale
  ylab("Predicted no. of pots") +
  xlab("Depth (m)") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position='none',
    axis.text.x = element_text(size = 45, colour = 'black'),
    axis.text.y = element_text(size = 45, colour = 'black'),
    axis.title = element_text(size = 50),
    axis.line = element_line(colour = 'black', size = 2),
    axis.ticks.length=unit(.25, "cm"),
    axis.ticks=element_line(size=2, colour = 'black'),
    strip.text = element_text(size=50, colour = 'black'),
    strip.background = element_blank(),
    strip.placement = "left",
    plot.margin = unit(c(0,0,0,30), "pt")
  ) 
depth_plot_winter

# #export for main text figure - depth curve
# path_figures <- "C:/Users/lrie0/OneDrive/NOAA/Riekkola et al - predicting fishing effort/Figures"
# png(paste0(path_figures, "/depth_plot_winter.png"), width = 20, height = 14, units = "in", res = 500)
# ggarrange(depth_plot_winter,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())



#fishing state
dummy_fishing_state <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','winter',"dummy_df_fishing_state.csv"))
dummy_fishing_state$half_month_of_seasonf <- as.factor(dummy_fishing_state$half_month_of_seasonf)
predictions_dummy_fishing_state <- predict(fit19b_winter, newdata = dummy_fishing_state)


#season
dummy_season <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','winter',"dummy_df_season.csv"))
dummy_season$half_month_of_seasonf <- as.factor(dummy_season$half_month_of_seasonf)
predictions_dummy_season <- predict(fit19b_winter, newdata = dummy_season)


#half-month of season
dummy_HMOS <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','winter',"dummy_df_HMOS.csv"))
dummy_HMOS$half_month_of_seasonf <- as.factor(dummy_HMOS$half_month_of_seasonf)
predictions_dummy_HMOS <- predict(fit19b_winter, newdata = dummy_HMOS)


#wind
dummy_wind <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','winter',"dummy_df_wind.csv"))
dummy_wind$half_month_of_seasonf <- as.factor(dummy_wind$half_month_of_seasonf)
predictions_dummy_wind <- predict(fit19b_winter, newdata = dummy_wind)


#depth sd
dummy_depth_sd <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','winter',"dummy_df_depth_sd.csv"))
dummy_depth_sd$half_month_of_seasonf <- as.factor(dummy_depth_sd$half_month_of_seasonf)
predictions_dummy_depth_sd <- predict(fit19b_winter, newdata = dummy_depth_sd, `se_fit` = TRUE)

ggplot(data=predictions_dummy_depth_sd, aes(x=z_depth_point_sd, y=est, group=1)) +
  geom_line()+
  geom_ribbon(aes(ymin=est-est_se*qnorm(0.975), ymax=est+est_se*qnorm(0.975), alpha=0.2))+
  geom_point()+
  #scale_color_grey() + 
  theme_classic()


#fault lines
dummy_faults <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','winter',"dummy_df_faults.csv"))
dummy_faults$half_month_of_seasonf <- as.factor(dummy_faults$half_month_of_seasonf)
predictions_dummy_faults <- predict(fit19b_winter, newdata = dummy_faults)


#canyons
dummy_canyons <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','winter',"dummy_df_canyons.csv"))
dummy_canyons$half_month_of_seasonf <- as.factor(dummy_canyons$half_month_of_seasonf)
predictions_dummy_canyons <- predict(fit19b_winter, newdata = dummy_canyons)


#distance to port
dummy_port_dist <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','winter',"dummy_df_port_dist.csv"))
dummy_port_dist$half_month_of_seasonf <- as.factor(dummy_port_dist$half_month_of_seasonf)
predictions_dummy_port_dist <- predict(fit19b_winter, newdata = dummy_port_dist)


#fuel price
dummy_fuel <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','winter',"dummy_df_fuel.csv"))
dummy_fuel$half_month_of_seasonf <- as.factor(dummy_fuel$half_month_of_seasonf)
predictions_dummy_fuel <- predict(fit19b_winter, newdata = dummy_fuel, `se_fit` = TRUE)

ggplot(data=predictions_dummy_fuel, aes(x=z_weighted_fuel_pricegal, y=est, group=1)) +
  geom_line()+
  geom_ribbon(aes(ymin=est-est_se*qnorm(0.975), ymax=est+est_se*qnorm(0.975), alpha=0.2))+
  geom_point()+
  #scale_color_grey() + 
  theme_classic()

  
#crab price
dummy_crab <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','winter',"dummy_df_crab.csv"))
dummy_crab$half_month_of_seasonf <- as.factor(dummy_crab$half_month_of_seasonf)
predictions_dummy_crab <- predict(fit19b_winter, newdata = dummy_crab)


#bottom O2
dummy_O2 <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','winter',"dummy_df_O2.csv"))
dummy_O2$half_month_of_seasonf <- as.factor(dummy_O2$half_month_of_seasonf)
predictions_dummy_O2 <- predict(fit19b_winter, newdata = dummy_O2)


#distance to closed area
dummy_dist_to_closed <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','winter',"dummy_df_dist_to_closed.csv"))
dummy_dist_to_closed$half_month_of_seasonf <- as.factor(dummy_dist_to_closed$half_month_of_seasonf)
predictions_dummy_dist_to_closed <- predict(fit19b_winter, newdata = dummy_dist_to_closed)



#-------------------------------------------------------------------------------------------------

###SUMMER

#-------------------------------------------------------------------------------------------------

#read in summer data - the version where z-scoring is done across summer only
summer <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_summer_20230324.rds'))
glimpse(summer) 

summer$month_name_f <- factor(summer$month_name, levels = c("May", "June", "July", "August", "September"))

# Add UTM columns (zone 10)
summer = add_utm_columns(summer, ll_names = c("grd_x", "grd_y"))


mesh_summer <- make_mesh(summer, xy_cols = c("X","Y"), cutoff = 10)
mesh_summer$mesh$n


fit19b_summer <-  read_rds(here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC','summer','after fixing fuel and crab ppp',"fit19b_summer.rds"))

#SST
dummy_SST <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer' ,"dummy_df_SST.csv"))
dummy_SST$half_month_of_seasonf <- as.factor(dummy_SST$half_month_of_seasonf)
predictions <- predict(fit19b_summer, newdata = dummy_SST)


#depth
dummy_depth <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_depth.csv"))
dummy_depth$half_month_of_seasonf <- as.factor(dummy_depth$half_month_of_seasonf)
predictions_depth <- predict(fit19b_summer, newdata = dummy_depth, `se_fit` = TRUE)


predictions_depth_v2_summer <- predictions_depth %>% 
  mutate(depth = case_when(z_depth_point_mean == -2.0 ~ -190, 
                           z_depth_point_mean == -1.5 ~ -163, 
                           z_depth_point_mean == -1.0 ~ -137, 
                           z_depth_point_mean == -0.5 ~ -111, 
                           z_depth_point_mean == 0.0 ~ -84, 
                           z_depth_point_mean == 0.5 ~ -58, 
                           z_depth_point_mean == 1.0 ~ -31, 
                           z_depth_point_mean == 1.5 ~ -5)) %>%  
  #z_depth_point_mean == 2.0  ~ 0)) %>%  #raw data doesn't go as far as 2 on z scale 
  mutate(est_backtransformed = exp(est),
         est_se_backtransformed = exp(est_se))

#backtransformed ribbon uses 95%CI
depth_plot_all_summer <- ggplot(data=predictions_depth_v2_summer, aes(x=depth, y=est_backtransformed, group=1)) +
  geom_ribbon(aes(ymin=est_backtransformed-est_se_backtransformed*qnorm(0.975), ymax=est_backtransformed+est_se_backtransformed*qnorm(0.975), alpha=0.2), fill="#68a2b9")+
  geom_line(size = 1.2)+
  geom_point(size = 2.4)+
  #scale_color_grey() + 
  #ylim(0,820)+ #if wanted to have all depth plots in same scale
  ylab("Predicted no. of pots") +
  xlab("Depth (m)") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position='none',
    axis.text.x = element_text(size = 45, colour = 'black'),
    axis.text.y = element_text(size = 45, colour = 'black'),
    axis.title = element_text(size = 50),
    axis.line = element_line(colour = 'black', size = 2),
    axis.ticks.length=unit(.25, "cm"),
    axis.ticks=element_line(size=2, colour = 'black'),
    strip.text = element_text(size=50, colour = 'black'),
    strip.background = element_blank(),
    strip.placement = "left",
    plot.margin = unit(c(0,0,0,30), "pt")
  ) 
depth_plot_all_summer


# #export for main text figure - depth curve
# path_figures <- "C:/Users/lrie0/OneDrive/NOAA/Riekkola et al - predicting fishing effort/Figures"
# png(paste0(path_figures, "/depth_plot_all_summer.png"), width = 20, height = 14, units = "in", res = 500)
# ggarrange(depth_plot_all_summer,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())



#fishing state
dummy_fishing_state <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_fishing_state.csv"))
dummy_fishing_state$half_month_of_seasonf <- as.factor(dummy_fishing_state$half_month_of_seasonf)
predictions_dummy_fishing_state <- predict(fit19b_summer, newdata = dummy_fishing_state)


#WA pot reduction
dummy_WA_pot_reduction <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_WA_pot_reduction.csv"))
dummy_WA_pot_reduction$half_month_of_seasonf <- as.factor(dummy_WA_pot_reduction$half_month_of_seasonf)
predictions_dummy_WA_pot_reduction <- predict(fit19b_summer, newdata = dummy_WA_pot_reduction)


#season
dummy_season <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_season.csv"))
dummy_season$half_month_of_seasonf <- as.factor(dummy_season$half_month_of_seasonf)
predictions_dummy_season <- predict(fit19b_summer, newdata = dummy_season)


#half-month of season
dummy_HMOS <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_HMOS.csv"))
dummy_HMOS$half_month_of_seasonf <- as.factor(dummy_HMOS$half_month_of_seasonf)
predictions_dummy_HMOS <- predict(fit19b_summer, newdata = dummy_HMOS)


#wind
dummy_wind <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_wind.csv"))
dummy_wind$half_month_of_seasonf <- as.factor(dummy_wind$half_month_of_seasonf)
predictions_dummy_wind <- predict(fit19b_summer, newdata = dummy_wind)


#depth sd
dummy_depth_sd <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_depth_sd.csv"))
dummy_depth_sd$half_month_of_seasonf <- as.factor(dummy_depth_sd$half_month_of_seasonf)
predictions_dummy_depth_sd <- predict(fit19b_summer, newdata = dummy_depth_sd)


#fault lines
dummy_faults <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_faults.csv"))
dummy_faults$half_month_of_seasonf <- as.factor(dummy_faults$half_month_of_seasonf)
predictions_dummy_faults <- predict(fit19b_summer, newdata = dummy_faults)


#canyons
dummy_canyons <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_canyons.csv"))
dummy_canyons$half_month_of_seasonf <- as.factor(dummy_canyons$half_month_of_seasonf)
predictions_dummy_canyons <- predict(fit19b_summer, newdata = dummy_canyons)


#distance to port
dummy_port_dist <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_port_dist.csv"))
dummy_port_dist$half_month_of_seasonf <- as.factor(dummy_port_dist$half_month_of_seasonf)
predictions_dummy_port_dist <- predict(fit19b_summer, newdata = dummy_port_dist)


#fuel price
dummy_fuel <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_fuel.csv"))
dummy_fuel$half_month_of_seasonf <- as.factor(dummy_fuel$half_month_of_seasonf)
predictions_dummy_fuel <- predict(fit19b_summer, newdata = dummy_fuel, `se_fit` = TRUE)

ggplot(data=predictions_dummy_fuel, aes(x=z_weighted_fuel_pricegal, y=est, group=1)) +
  geom_line()+
  geom_ribbon(aes(ymin=est-est_se*qnorm(0.975), ymax=est+est_se*qnorm(0.975), alpha=0.2))+
  geom_point()+
  #scale_color_grey() + 
  theme_classic()


#crab price
dummy_crab <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_crab.csv"))
dummy_crab$half_month_of_seasonf <- as.factor(dummy_crab$half_month_of_seasonf)
predictions_dummy_crab <- predict(fit19b_summer, newdata = dummy_crab)


#bottom O2
dummy_O2 <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_O2.csv"))
dummy_O2$half_month_of_seasonf <- as.factor(dummy_O2$half_month_of_seasonf)
predictions_dummy_O2 <- predict(fit19b_summer, newdata = dummy_O2)


#distance to closed area
dummy_dist_to_closed <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_dist_to_closed.csv"))
dummy_dist_to_closed$half_month_of_seasonf <- as.factor(dummy_dist_to_closed$half_month_of_seasonf)
predictions_dummy_dist_to_closed <- predict(fit19b_summer, newdata = dummy_dist_to_closed)



#-------------------------------------------------------------------------------------------------

###ALL DATA

#-------------------------------------------------------------------------------------------------


#read in all data - the version where z-scoring is done across all data
all_data <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_all_data_20230324.rds'))
glimpse(all_data) 

all_data$month_name_f <- factor(all_data$month_name, levels = c("December", "January", "February", "March", "April",
                                                                "May", "June", "July", "August", "September"))

# Add UTM columns (zone 10)
all_data = add_utm_columns(all_data, ll_names = c("grd_x", "grd_y"))


mesh_all_data <- make_mesh(all_data, xy_cols = c("X","Y"), cutoff = 10)
mesh_all_data$mesh$n

fit16b_all_data <-  read_rds(here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC', 'all data', 'after fixes',"fit16b_all_data.rds"))

#depth
dummy_depth <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','all data',"dummy_df_depth.csv"))
#dummy_dist_to_closed_interaction$month_name_f <- as.factor(dummy_dist_to_closed_interaction$month_name_f)
predictions_depth <- predict(fit16b_all_data, newdata = dummy_depth, `se_fit` = TRUE)

predictions_depth_v2 <- predictions_depth %>% 
  mutate(depth = case_when(z_depth_point_mean == -2.0 ~ -192, 
                           z_depth_point_mean == -1.5 ~ -165, 
                           z_depth_point_mean == -1.0 ~ -139, 
                           z_depth_point_mean == -0.5 ~ -112, 
                           z_depth_point_mean == 0.0 ~ -86, 
                           z_depth_point_mean == 0.5 ~ -59, 
                           z_depth_point_mean == 1.0 ~ -32, 
                           z_depth_point_mean == 1.5 ~ -6)) %>%  
  #z_depth_point_mean == 2.0  ~ 0)) %>%  #raw data doesn't go as far as 2 on z scale 
  mutate(est_backtransformed = exp(est),
         est_se_backtransformed = exp(est_se))

#backtransformed ribbon uses 95%CI
depth_plot_all_data <- ggplot(data=predictions_depth_v2, aes(x=depth, y=est_backtransformed, group=1)) +
  geom_ribbon(aes(ymin=est_backtransformed-est_se_backtransformed*qnorm(0.975), ymax=est_backtransformed+est_se_backtransformed*qnorm(0.975), alpha=0.5), fill="#68a2b9")+
  geom_line(size = 1.2)+
  geom_point(size = 2.4)+
  #scale_color_grey() + 
  #ylim(0,820)+ #if wanted to have all depth plots in same scale
  ylab("Predicted no. of pots") +
  xlab("Depth (m)") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position='none',
    axis.text.x = element_text(size = 45, colour = 'black'),
    axis.text.y = element_text(size = 45, colour = 'black'),
    axis.title = element_text(size = 50),
    axis.line = element_line(colour = 'black', size = 2),
    axis.ticks.length=unit(.25, "cm"),
    axis.ticks=element_line(size=2, colour = 'black'),
    strip.text = element_text(size=50, colour = 'black'),
    strip.background = element_blank(),
    strip.placement = "left",
    plot.margin = unit(c(0,0,0,30), "pt")
  ) 
depth_plot_all_data

# #export for main text figure - depth curve
# path_figures <- "C:/Users/lrie0/OneDrive/NOAA/Riekkola et al - predicting fishing effort/Figures"
# png(paste0(path_figures, "/depth_plot_all_data.png"), width = 20, height = 14, units = "in", res = 500)
# ggarrange(depth_plot_winter,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())




dummy_SST <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','all data',"dummy_df_SST.csv"))
dummy_SST$month_name_f <- as.factor(dummy_SST$month_name_f)
predictions_SST <- predict(fit16b_all_data, newdata = dummy_SST)

dummy_month <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','all data',"dummy_df_month.csv"))
dummy_month$month_name_f <- as.factor(dummy_month$month_name_f)
predictions_month <- predict(fit16b_all_data, newdata = dummy_month, `se_fit` = TRUE)

#% change 
#Dec-Jan: 22.53077
#Jan-Feb: 18.70284
#Feb-Mar: 13.21732
#Mar-Apr: -1.431653
#Apr-May: 5.881991
#May-Jun: -11.97779
#Jun-Jul: -6.061324
#Jul-Aug: 12.94559
#Aug-Sep: 35.02503

predictions_month <- predictions_month %>% 
  mutate(month_name_f = factor(month_name_f, 
                               levels = c('December','January','February','March','April','May','June','July','August','September')))  
ggplot(data=predictions_month, aes(x=month_name_f, y=est, group=1)) +
  geom_line()+
  #geom_ribbon(aes(ymin=est-est_se, ymax=est+est_se, alpha=0.2))+
  geom_point()+
  theme_classic()

#the 'increasing trend' in this plot, its not that the total number of pots increases, but maybe it's capturing
#the shrinkage of the fishing area, and that's why pots pr grid goes up?
#could this need a companion plot of avg number of unique grids used in a given month? and number of active vessels?

plot(predictions_month$month_name_f,predictions_month$est)



dummy_year <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','all data',"dummy_df_year.csv"))
dummy_year$month_name_f <- as.factor(dummy_year$month_name_f)
dummy_year$season <- as.factor(dummy_year$season)

predictions_year <- predict(fit16b_all_data, newdata = dummy_year) #, `se_fit` = TRUE

plot(predictions_year$season,predictions_year$est)



#fuel price
dummy_fuel <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','all data',"dummy_df_fuel.csv"))
dummy_fuel$month_name_f <- as.factor(dummy_fuel$month_name_f)
predictions_dummy_fuel <- predict(fit16b_all_data, newdata = dummy_fuel, `se_fit` = TRUE)

ggplot(data=predictions_dummy_fuel, aes(x=z_weighted_fuel_pricegal, y=est, group=1)) +
  geom_line()+
  geom_ribbon(aes(ymin=est-est_se*qnorm(0.975), ymax=est+est_se*qnorm(0.975), alpha=0.2))+
  geom_point()+
  #scale_color_grey() + 
  theme_classic()


#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
####MARGINAL EFFECT OF SEASON
#-------------------------------------------------------------------------------------------------
##summer - season effect

dummy_season <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_HMOS_fixed_effect_season.csv"))
dummy_season$half_month_of_seasonf <- as.factor(dummy_season$half_month_of_seasonf)
dummy_season$season <- as.factor(dummy_season$season)

predictions_season <- predict(fit19b_summer, newdata = dummy_season)

plot(predictions_season$season,predictions_season$est)


##winter - season effect

dummy_season <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','winter',"dummy_df_HMOS_fixed_effect_season.csv"))
dummy_season$half_month_of_seasonf <- as.factor(dummy_season$half_month_of_seasonf)
dummy_season$season <- as.factor(dummy_season$season)

predictions_season <- predict(fit19b_winter, newdata = dummy_season, `se_fit` = TRUE)

plot(predictions_season$season,predictions_season$est)

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right
ggplot(predictions_season, aes(x=season, y=est,group=1)) + 
  geom_errorbar(aes(ymin=est-est_se, ymax=est+est_se), colour="black", width=.1, position=pd) +
  geom_point(position=pd, size=3)

# Predictions at original data locations
#predictions_winter <- predict(fit19b_winter) #file becomes too big if include , `se_fit` = TRUE
#so don't use that approach




#######################################
#plotting marginal effect for season

fit19b_winter <-  read_rds(here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC','winter','after fixing fuel and crab price',"fit19b_winter.rds"))
fit19b_summer <-  read_rds(here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC','summer','after fixing fuel and crab ppp',"fit19b_summer.rds"))
fit16b_all_data <-  read_rds(here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC', 'all data', 'after fixes',"fit16b_all_data.rds"))
## also need to load the base dfs
#glimpse(winter)
#glimpse(summer)
#glimpse(all_data)


#tweaking Eric's 'plot_log' function
g_winter <- ggeffect(fit19b_winter, "season [all]", back.transform = FALSE)
g_winter$conf.low <- log(g_winter$conf.low)
g_winter$conf.high <- log(g_winter$conf.high)
g_winter$predicted <- log(g_winter$predicted)
g_winter$group <- "winter"

g_summer <- ggeffect(fit19b_summer, "season [all]", back.transform = FALSE)
g_summer$conf.low <- log(g_summer$conf.low)
g_summer$conf.high <- log(g_summer$conf.high)
g_summer$predicted <- log(g_summer$predicted)
g_summer$group <- "summer"

g_all_data <- ggeffect(fit16b_all_data, "season [all]", back.transform = FALSE)
g_all_data$conf.low <- log(g_all_data$conf.low)
g_all_data$conf.high <- log(g_all_data$conf.high)
g_all_data$predicted <- log(g_all_data$predicted)
g_all_data$group <- "all data"

g_data <- rbind(g_winter, g_summer, g_all_data)

plot_season_ME <- ggplot() + 
  geom_errorbar(data=g_data, aes(x=x, ymin=conf.low, ymax=conf.high, group=group, color=group), width=.1, position=position_dodge(width = 0.5)) +
  geom_point(data=g_data, aes(x=x, y=predicted, group=group, color=group), size=3, position=position_dodge(width = 0.5)) +
  theme_classic()
plot_season_ME


# plot_season_ME <- ggplot() + 
#   geom_errorbar(data=g_winter, aes(x=x, ymin=conf.low, ymax=conf.high), colour="#68a2b9", width=.1, position=position_dodge()) +
#   geom_point(data=g_winter, aes(x=x, y=predicted), size=3, position=pd, colour='#68a2b9') +
#   geom_errorbar(data=g_summer, aes(x=x, ymin=conf.low, ymax=conf.high), colour="#99d9d9", width=.1, position=position_dodge()) +
#   geom_point(data=g_summer, aes(x=x, y=predicted), size=3, position=pd, colour='#99d9d9') +
#   theme_classic()
# plot_season_ME


#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
####MARGINAL EFFECT OF MONTH AND HMOS
#-------------------------------------------------------------------------------------------------
#SUMMER - VISUALISING HMOS EFFECT

dummy_HMOS_test <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_HMOS_fixed_effect_all_HMOS.csv"))
dummy_HMOS_test$half_month_of_seasonf <- as.factor(dummy_HMOS_test$half_month_of_seasonf)
predictions_HMOS_all <- predict(fit19b_summer, newdata = dummy_HMOS_test)
#predictions_HMOS_all_v2 <- predictions_HMOS_all %>% mutate(effect = (exp(est)-1)*100)
plot(predictions_HMOS_all$half_month_of_seasonf,predictions_HMOS_all$est)
#plot(predictions_HMOS_all_v2$half_month_of_seasonf,predictions_HMOS_all_v2$effect)

dummy_HMOS_test_v2 <- dummy_HMOS_test %>% filter(half_month_of_seasonf %in% c("10", "11", "12", "13", "14", "15", "16", "17", "18", "19"))
predictions_HMOS_all_v3 <- predict(fit19b_summer, newdata = dummy_HMOS_test_v2)
predictions_HMOS_all_v4 <- predictions_HMOS_all_v3 %>% mutate(effect = (exp(est)-1)*100)
plot(predictions_HMOS_all_v4$half_month_of_seasonf,predictions_HMOS_all_v4$effect)


#WINTER - VISUALISING HMOS EFFECT

dummy_HMOS_test <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','winter',"dummy_df_HMOS_fixed_effect_all_HMOS.csv"))
dummy_HMOS_test$half_month_of_seasonf <- as.factor(dummy_HMOS_test$half_month_of_seasonf)
predictions_HMOS_all <- predict(fit19b_summer, newdata = dummy_HMOS_test)
#predictions_HMOS_all_v2 <- predictions_HMOS_all %>% mutate(effect = (exp(est)-1)*100)
plot(predictions_HMOS_all$half_month_of_seasonf,predictions_HMOS_all$est)
#plot(predictions_HMOS_all_v2$half_month_of_seasonf,predictions_HMOS_all_v2$effect)







#SUMMER - VISUALISING HMOS EFFECT

dummy_testtest <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_vary_year_HMOS_steady.csv"))
dummy_testtest$half_month_of_seasonf <- as.factor(dummy_testtest$half_month_of_seasonf)
predictions_testtest <- predict(fit19b_summer, newdata = dummy_testtest)

plot(predictions_testtest$yearn,predictions_testtest$est)

ggplot(data=predictions_testtest, aes(x=yearn, y=est, group=half_month_of_seasonf)) +
  geom_line(aes(color=half_month_of_seasonf))+
  geom_point(aes(color=half_month_of_seasonf))+
  #scale_color_grey() + 
  #scale_color_continuous()+
  scale_color_hue(c = 40)+
  #scale_color_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  #scale_fill_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  theme_classic()

ggplot(data=predictions_testtest, aes(x=half_month_of_seasonf, y=est, group=season)) +
  geom_line(aes(color=season))+
  geom_point(aes(color=season))+
  #scale_color_grey() + 
  #scale_color_continuous()+
  scale_color_hue(c = 40)+
  #scale_color_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  #scale_fill_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  theme_classic()


#WINTER - VISUALISING HMOS EFFECT

dummy_testtest <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','winter',"dummy_vary_year_HMOS_steady.csv"))
dummy_testtest$half_month_of_seasonf <- as.factor(dummy_testtest$half_month_of_seasonf)
predictions_testtest <- predict(fit19b_summer, newdata = dummy_testtest)

#plot(predictions_testtest$yearn,predictions_testtest$est)

ggplot(data=predictions_testtest, aes(x=yearn, y=est, group=half_month_of_seasonf)) +
  geom_line(aes(color=half_month_of_seasonf))+
  geom_point(aes(color=half_month_of_seasonf))+
  #scale_color_grey() + 
  #scale_color_continuous()+
  scale_color_hue(c = 40)+
  #scale_color_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  #scale_fill_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  theme_classic()

ggplot(data=predictions_testtest, aes(x=half_month_of_seasonf, y=est, group=season)) +
  geom_line(aes(color=season))+
  geom_point(aes(color=season))+
  #scale_color_grey() + 
  #scale_color_continuous()+
  scale_color_hue(c = 40)+
  #scale_color_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  #scale_fill_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  theme_classic()


#ALL DATA - VISUALISING MONTH EFFECT

dummy_testtest <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','all data',"dummy_vary_year_month_steady.csv"))
#dummy_testtest$month_name_f <- as.factor(dummy_testtest$month_name_f)
dummy_testtest$month_name_f <- factor(dummy_testtest$month_name_f, levels = c("December", "January", "February", "March", "April",
                                                                "May", "June", "July", "August", "September"))

predictions_testtest <- predict(fit16b_all_data, newdata = dummy_testtest)

datax <- predictions_testtest %>%
  group_by(month_name_f) %>%
  top_n(1, season) 

ggplot(data=predictions_testtest, aes(x=season, y=est, group=month_name_f)) +
  geom_line(aes(color=month_name_f, label = month_name_f))+
  geom_point(aes(color=month_name_f))+
  #scale_color_grey() + 
  #scale_color_continuous()+
  scale_color_hue(c = 40)+
  geom_text_repel(
    aes(label = month_name_f), data = datax,
    size = 3) +
  #scale_color_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  #scale_fill_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  theme_classic()


datax <- predictions_testtest %>%
  group_by(season) %>%
  top_n(1, month_name_f) 

ggplot(data=predictions_testtest, aes(x=month_name_f, y=est, group=season)) +
  geom_line(aes(color=season))+
  geom_point(aes(color=season))+
  #scale_color_grey() + 
  #scale_color_continuous()+
  scale_color_hue(c = 40)+
  geom_text_repel(
    aes(label = season), data = datax,
    size = 3) +
  #scale_color_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  #scale_fill_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  theme_classic()





#tweaking Eric's 'plot_log' function
#winter and summer use HMOS (while all data uses month)
gg_winter <- ggeffect(fit19b_winter, "half_month_of_seasonf [all]", back.transform = FALSE)
gg_winter$conf.low <- log(gg_winter$conf.low)
gg_winter$conf.high <- log(gg_winter$conf.high)
gg_winter$predicted <- log(gg_winter$predicted)
gg_winter$group <- "winter"

gg_summer <- ggeffect(fit19b_summer, "half_month_of_seasonf [all]", back.transform = FALSE)
gg_summer$conf.low <- log(gg_summer$conf.low)
gg_summer$conf.high <- log(gg_summer$conf.high)
gg_summer$predicted <- log(gg_summer$predicted)
gg_summer$group <- "summer"

gg_data <- rbind(gg_winter, gg_summer)

plot_HMOS_ME <- ggplot() + 
  geom_errorbar(data=gg_data, aes(x=x, ymin=conf.low, ymax=conf.high, group=group, color=group), width=.1, position=position_dodge(width = 0.5)) +
  geom_point(data=gg_data, aes(x=x, y=predicted, group=group, color=group), size=3, position=position_dodge(width = 0.5)) +
  theme_classic()
plot_HMOS_ME


# all data uses month
gg_all_data <- ggeffect(fit16b_all_data, "month_name_f [all]", back.transform = FALSE)
gg_all_data$conf.low <- log(gg_all_data$conf.low)
gg_all_data$conf.high <- log(gg_all_data$conf.high)
gg_all_data$predicted <- log(gg_all_data$predicted)
gg_all_data$group <- "all data"

plot_month_ME <- ggplot() + 
  geom_errorbar(data=gg_all_data, aes(x=x, ymin=conf.low, ymax=conf.high, group=1), width=.1, position=position_dodge(width = 0.5)) +
  geom_point(data=gg_all_data, aes(x=x, y=predicted, group=group, color=1), position=position_dodge(width = 0.5)) +
  theme_classic()
plot_month_ME
##this plot doesn't seem quite right, it's to flat, no change from month to month


dummy_month_effect <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','all data',"dummy_df_month.csv"))
dummy_month_effect$month_name_f <- factor(dummy_month_effect$month_name_f, levels = c("December", "January", "February", "March", "April",
                                                                              "May", "June", "July", "August", "September"))

predictions_month_effect <- predict(fit16b_all_data, newdata = dummy_month_effect, `se_fit` = TRUE)

ggplot(predictions_month_effect, aes(x=month_name_f, y=est, group=1)) + 
  geom_errorbar(aes(ymin=est-est_se, ymax=est+est_se), colour="black", width=.1) +
  geom_point(position=pd, size=3)+
  theme_classic()





#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------

########################visualising interaction###################################################

#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
##interaction between depth and bottom O2

#############################SUMMER
#the low, med, high bottom O2 values should come from the summer only df
dummy_depth_O2_interaction <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','summer',"dummy_df_depth_O2_interaction.csv"))
dummy_depth_O2_interaction$half_month_of_seasonf <- as.factor(dummy_depth_O2_interaction$half_month_of_seasonf)
predictions_depth_O2_interaction <- predict(fit19b_summer, newdata = dummy_depth_O2_interaction, `se_fit` = TRUE)

#ribbon uses SE
ggplot(data=predictions_depth_O2_interaction, aes(x=z_depth_point_mean, y=est, group=as.factor(z_bottom_O2_avg))) +
  geom_line(aes(color=as.factor(z_bottom_O2_avg)))+
  geom_ribbon(aes(ymin=est-est_se, ymax=est+est_se,color=as.factor(z_bottom_O2_avg), fill = as.factor(z_bottom_O2_avg)), alpha=0.2)+
  geom_point(aes(color=as.factor(z_bottom_O2_avg)))+
  #scale_color_grey() + 
  scale_color_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  scale_fill_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  theme_classic()

#ribbon uses 95%CI
ggplot(data=predictions_depth_O2_interaction, aes(x=z_depth_point_mean, y=est, group=as.factor(z_bottom_O2_avg))) +
  geom_line(aes(color=as.factor(z_bottom_O2_avg)))+
  geom_ribbon(aes(ymin=est-est_se*qnorm(0.975), ymax=est+est_se*qnorm(0.975),color=as.factor(z_bottom_O2_avg), fill = as.factor(z_bottom_O2_avg)), alpha=0.2)+
  geom_point(aes(color=as.factor(z_bottom_O2_avg)))+
  #scale_color_grey() + 
  scale_color_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  scale_fill_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  theme_classic()


##back transform estimate and CI, change bottom O2 & depth values from z-scale to original
#-0.7253 --> 35.455
#-0.3027 --> 54.759
#0.4734 --> 90.213

predictions_depth_O2_interaction_v2 <- predictions_depth_O2_interaction %>% 
  mutate(bottom_O2 = case_when(z_bottom_O2_avg == -0.7253 ~ 35.455,
                               z_bottom_O2_avg == -0.3027 ~ 54.759,
                               z_bottom_O2_avg == 0.4734  ~ 90.213)) %>% 
  mutate(depth = case_when(z_depth_point_mean == -2.0 ~ -190, 
                           z_depth_point_mean == -1.5 ~ -163, 
                           z_depth_point_mean == -1.0 ~ -137, 
                           z_depth_point_mean == -0.5 ~ -111, 
                           z_depth_point_mean == 0.0 ~ -84, 
                           z_depth_point_mean == 0.5 ~ -58, 
                           z_depth_point_mean == 1.0 ~ -31, 
                           z_depth_point_mean == 1.5 ~ -5)) %>%  
                           #z_depth_point_mean == 2.0  ~ 0)) %>%  #raw data doesn't go as far as 2 on z scale 
  mutate(est_backtransformed = exp(est),
         est_se_backtransformed = exp(est_se))

#backtransformed ribbon uses 95%CI
ggplot(data=predictions_depth_O2_interaction_v2, aes(x=depth, y=est_backtransformed, group=as.factor(bottom_O2))) +
  geom_line(aes(color=as.factor(bottom_O2)))+
  geom_ribbon(aes(ymin=est_backtransformed-est_se_backtransformed*qnorm(0.975), ymax=est_backtransformed+est_se_backtransformed*qnorm(0.975),color=as.factor(bottom_O2), fill = as.factor(bottom_O2)), alpha=0.2)+
  geom_point(aes(color=as.factor(bottom_O2)))+
  #scale_color_grey() + 
  scale_color_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  scale_fill_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  ylab("Predicted no. of pots") +
  xlab("Depth (m)") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title.align = .5,
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1, units = "cm"),
    axis.text.x = element_text(size = 12, colour = 'black'),
    axis.text.y = element_text(size = 12, colour = 'black'),
    axis.title = element_text(size = 14),
    axis.line = element_line(colour = 'black', size = 0.7),
    axis.ticks.length=unit(.1, "cm"),
    axis.ticks=element_line(size=0.7, colour = 'black'),
    strip.text = element_text(size=12, colour = 'black'),
    strip.background = element_blank(),
    strip.placement = "left"
  ) 




#########################WINTER
#the low, med, high bottom O2 values should come from the summer only df -- looks like there had been a mistake with those values -- used summer values
#now fixed
dummy_depth_O2_interaction <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','winter',"dummy_df_depth_O2_interaction.csv"))
dummy_depth_O2_interaction$half_month_of_seasonf <- as.factor(dummy_depth_O2_interaction$half_month_of_seasonf)
predictions_depth_O2_interaction <- predict(fit19b_winter, newdata = dummy_depth_O2_interaction, `se_fit` = TRUE)

#ribbon uses SE
ggplot(data=predictions_depth_O2_interaction, aes(x=z_depth_point_mean, y=est, group=as.factor(z_bottom_O2_avg))) +
  geom_line(aes(color=as.factor(z_bottom_O2_avg)))+
  geom_ribbon(aes(ymin=est-est_se, ymax=est+est_se,color=as.factor(z_bottom_O2_avg), fill = as.factor(z_bottom_O2_avg)), alpha=0.2)+
  geom_point(aes(color=as.factor(z_bottom_O2_avg)))+
  #scale_color_grey() + 
  scale_color_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  scale_fill_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  theme_classic()

#ribbon uses 95%CI
ggplot(data=predictions_depth_O2_interaction, aes(x=z_depth_point_mean, y=est, group=as.factor(z_bottom_O2_avg))) +
  geom_line(aes(color=as.factor(z_bottom_O2_avg)))+
  geom_ribbon(aes(ymin=est-est_se*qnorm(0.975), ymax=est+est_se*qnorm(0.975),color=as.factor(z_bottom_O2_avg), fill = as.factor(z_bottom_O2_avg)), alpha=0.2)+
  geom_point(aes(color=as.factor(z_bottom_O2_avg)))+
  #scale_color_grey() + 
  scale_color_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  scale_fill_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  theme_classic()

#boxplot -- but not enough instance to make a box
# p <- ggplot(predictions_depth_O2_interaction, aes(as.factor(z_depth_point_mean), est, colour = as.factor(z_bottom_O2_avg))) + geom_boxplot()
# p


##back transform estimate and CI, change bottom O2 & depth values from z-scale to original
#-0.8209 --> 59.519
#-0.2169 --> 99.075
#0.6517 --> 155.954

predictions_depth_O2_interaction_v2 <- predictions_depth_O2_interaction %>% 
  mutate(bottom_O2 = case_when(z_bottom_O2_avg == -0.8209 ~ 59.519,
                               z_bottom_O2_avg == -0.2169 ~ 99.075,
                               z_bottom_O2_avg == 0.6517  ~ 155.954)) %>% 
  mutate(depth = case_when(z_depth_point_mean == -2.0 ~ -193, 
                           z_depth_point_mean == -1.5 ~ -166, 
                           z_depth_point_mean == -1.0 ~ -140, 
                           z_depth_point_mean == -0.5 ~ -113, 
                           z_depth_point_mean == 0.0 ~ -87, 
                           z_depth_point_mean == 0.5 ~ -60, 
                           z_depth_point_mean == 1.0 ~ -34, 
                           z_depth_point_mean == 1.5 ~ -7)) %>%   
  #z_depth_point_mean == 2.0  ~ 0)) %>%  #raw data doesn't go as far as 2 on z scale 
  mutate(est_backtransformed = exp(est),
         est_se_backtransformed = exp(est_se))

#backtransformed ribbon uses 95%CI
ggplot(data=predictions_depth_O2_interaction_v2, aes(x=depth, y=est_backtransformed, group=as.factor(bottom_O2))) +
  geom_line(aes(color=as.factor(bottom_O2)))+
  geom_ribbon(aes(ymin=est_backtransformed-est_se_backtransformed*qnorm(0.975), ymax=est_backtransformed+est_se_backtransformed*qnorm(0.975),color=as.factor(bottom_O2), fill = as.factor(bottom_O2)), alpha=0.2)+
  geom_point(aes(color=as.factor(bottom_O2)))+
  #scale_color_grey() + 
  scale_color_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  scale_fill_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  ylab("Predicted no. of pots") +
  xlab("Depth (m)") +
  theme_classic() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title.align = .5,
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1, units = "cm"),
    axis.text.x = element_text(size = 12, colour = 'black'),
    axis.text.y = element_text(size = 12, colour = 'black'),
    axis.title = element_text(size = 14),
    axis.line = element_line(colour = 'black', size = 0.7),
    axis.ticks.length=unit(.1, "cm"),
    axis.ticks=element_line(size=0.7, colour = 'black'),
    strip.text = element_text(size=12, colour = 'black'),
    strip.background = element_blank(),
    strip.placement = "left"
  ) 


#-------------------------------------------------------------------------------------------------

##ALL DATA

#-------------------------------------------------------------------------------------------------

#read in all data - the version where z-scoring is done across all data
all_data <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_all_data_20230324.rds'))
glimpse(all_data) 

all_data$month_name_f <- factor(all_data$month_name, levels = c("December", "January", "February", "March", "April",
                                                                "May", "June", "July", "August", "September"))

# Add UTM columns (zone 10)
all_data = add_utm_columns(all_data, ll_names = c("grd_x", "grd_y"))


mesh_all_data <- make_mesh(all_data, xy_cols = c("X","Y"), cutoff = 10)
mesh_all_data$mesh$n


#-------------------------------------------------------------------------------------------------

fit16b_all_data <-  read_rds(here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC','all data','after fixes',"fit16b_all_data.rds"))


dummy_dist_to_closed_interaction <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','all data',"dummy_dist_to_closed_effect.csv"))
dummy_dist_to_closed_interaction$month_name_f <- as.factor(dummy_dist_to_closed_interaction$month_name_f)

#predict with new data:
predictions <- predict(fit16b_all_data, newdata = dummy_dist_to_closed_interaction, `se_fit` = TRUE)
predictions_v2 <- predictions %>% mutate(effect = (exp(est)-1)*100)

ggplot(data=predictions, aes(x=z_dist_to_closed_km, y=est, group=OR_WA_waters)) +
  geom_line(aes(color=OR_WA_waters))+
  geom_ribbon(aes(ymin=est-est_se, ymax=est+est_se,color=OR_WA_waters, fill = OR_WA_waters), alpha=0.2)+
  geom_point(aes(color=OR_WA_waters))+
  scale_color_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  scale_fill_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  theme_classic()


#View(predictions_v2)
#for WA, between 0 and 1 of covariate 1.6% increase in effort
#for OR, between 0 and 1 of covariate 7.9% decrease in effort


##back transform estimate and CI, change dist to closed values from z-scale to original
predictions_x <- predictions %>% 
  mutate(dist_to_closed_km = case_when(z_dist_to_closed_km == -1.0 ~ 32, 
                           z_dist_to_closed_km == -0.5 ~ 127, 
                           z_dist_to_closed_km == 0.0 ~ 222, 
                           z_dist_to_closed_km == 0.5 ~ 316, 
                           z_dist_to_closed_km == 1.0 ~ 411, 
                           z_dist_to_closed_km == 1.5 ~ 505)) %>%   
  mutate(est_backtransformed = exp(est),
         est_se_backtransformed = exp(est_se))

#backtransformed plot
ggplot(data=predictions_x, aes(x=dist_to_closed_km, y=est_backtransformed, group=OR_WA_waters)) +
  geom_line(aes(color=OR_WA_waters))+
  geom_ribbon(aes(ymin=est_backtransformed-est_se_backtransformed, ymax=est_backtransformed+est_se_backtransformed,color=OR_WA_waters, fill = OR_WA_waters), alpha=0.2)+
  geom_point(aes(color=OR_WA_waters))+
  scale_color_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  scale_fill_manual(values = c("#fde725", "#5ec962",  "#3b528b"))+
  ylab("Predicted no. of pots") +
  xlab("Distance to closed area (km)") +
  theme_classic() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title.align = .5,
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1, units = "cm"),
    axis.text.x = element_text(size = 12, colour = 'black'),
    axis.text.y = element_text(size = 12, colour = 'black'),
    axis.title = element_text(size = 14),
    axis.line = element_line(colour = 'black', size = 0.7),
    axis.ticks.length=unit(.1, "cm"),
    axis.ticks=element_line(size=0.7, colour = 'black'),
    strip.text = element_text(size=12, colour = 'black'),
    strip.background = element_blank(),
    strip.placement = "left"
  ) 


# predictionsv2 <- predict(fit16b_all_data)
# predictionsv2$resids <- residuals(fit16b_all_data)
# 
# ggplot(data=predictionsv2, aes(x=z_dist_to_closed_km, y=resids, group=OR_WA_waters)) +
#   geom_line(aes(color=OR_WA_waters))+
#   geom_point(aes(color=OR_WA_waters))+
#   scale_color_grey() + 
#   theme_classic()






##-----------------------------------------------------------------
#investigating result that 'WA has more pots' 
#does it really, or is it just a marginal effect -- all other things being equal a grid in WA has more pots
#suspect this would be due to WA having a shorter coastline
##-----------------------------------------------------------------
all_data_subset <- all_data %>% filter(tottraps <250)
ggplot(data=all_data_subset, aes(x=tottraps, group=OR_WA_waters, fill=OR_WA_waters)) +
  geom_density(adjust=1.5) +
  theme_classic() +
  facet_grid(~ OR_WA_waters) +
  theme(
    legend.position="right",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank()
  )

#using a subset of data
ggplot(all_data_subset, aes(x = tottraps, y = OR_WA_waters, fill = OR_WA_waters)) +
  geom_density_ridges() +
  theme_ridges(grid = TRUE, center_axis_labels = TRUE) +
  xlab("No. of pots") +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 40),
        legend.position = "none",
        #legend.position = c(.8, .5),
        axis.text.x = element_text(hjust = 0.5, size = 40, angle = 0),
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left",
        panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.title.y=element_blank()
  )

#using all_data df
ggplot(all_data, aes(x = tottraps, y = OR_WA_waters, fill = OR_WA_waters)) +
  geom_density_ridges() +
  theme_ridges(grid = TRUE, center_axis_labels = TRUE) +
  xlab("No. of pots") +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 40),
        legend.position = "none",
        #legend.position = c(.8, .5),
        axis.text.x = element_text(hjust = 0.5, size = 40, angle = 0),
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left",
        panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.title.y=element_blank()
  )





##-----------------------------------------------------------------
##-----------------------------------------------------------------
      #investigating fuel variable more
##-----------------------------------------------------------------


##weighting predictions ('cap' predictions)

#fuel price changed in all ports (baseline price, doubled)
dummy_fuel <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','winter',"winter_test_data_fuel_changes_in_all_ports.csv"))
dummy_fuel$half_month_of_seasonf <- as.factor(dummy_fuel$half_month_of_seasonf)
predictions_dummy_fuel <- predict(fit19b_winter, newdata = dummy_fuel) #, `se_fit` = TRUE

predictions_dummy_fuel_v2 <- predictions_dummy_fuel %>% mutate(bck_trns_est = exp(est)) %>% 
  select(GRID5KM_ID:tottraps, weighted_fuel_pricegal, z_weighted_fuel_pricegal,fuel_change:bck_trns_est) 


#Both, Feb_2 2017-18 at base fuel price and at doubled fuel price need to be weighted so traps total 172,800 (at each step - so)
predictions_dummy_fuel_base <- predictions_dummy_fuel_v2 %>% filter(fuel_change=="base") %>% 
  #each grid would be some % of the total pots
  mutate(percent_tottrap = tottraps/sum(tottraps)*100) %>% 
  mutate(percent_preds = bck_trns_est/sum(bck_trns_est)*100) %>%
  #divide percent_pred with 100 to get it as proportion
  mutate(weighted = percent_preds/100*172800) #172800 was total estimated pots for Feb_2 2017-2018 from landings

predictions_dummy_fuel_doubled <- predictions_dummy_fuel_v2 %>% filter(fuel_change=="doubled") %>% 
  #each grid would be some % of the total pots
  mutate(percent_tottrap = tottraps/sum(tottraps)*100) %>% 
  mutate(percent_preds = bck_trns_est/sum(bck_trns_est)*100) %>%
  #divide percent_pred with 100 to get it as proportion
  mutate(weighted = percent_preds/100*172800) #172800 was total estimated pots for Feb_2 2017-2018 from landings


#read in restricted study area shapefile
study_area <- read_sf(here::here('DCRB_sdmTMB','data','restricted_study_area.shp')) 
# #plot(study_area)
predictions_dummy_fuel_base_sf <- predictions_dummy_fuel_base %>% left_join(study_area, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA) 
predictions_dummy_fuel_doubled_sf <- predictions_dummy_fuel_doubled %>% left_join(study_area, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA) 
fuel_mapping_sf <- rbind(predictions_dummy_fuel_base_sf, predictions_dummy_fuel_doubled_sf)
#export shapefile for QGIS
#st_write(fuel_mapping_sf, "fuel_mapping_sf_weighted_20230417.shp")

#difference between the 2 layers
predictions_dummy_fuel_base_sf <- predictions_dummy_fuel_base_sf %>% select(GRID5KM_ID, grd_x, grd_y, weighted,geometry) %>% 
  rename(weighted_base = weighted)
predictions_dummy_fuel_doubled_sf <- predictions_dummy_fuel_doubled_sf %>% select(GRID5KM_ID, grd_x, grd_y, weighted,geometry) %>% 
  rename(weighted_doubled = weighted)
fuel_mapping_sf <- predictions_dummy_fuel_base_sf %>% left_join(predictions_dummy_fuel_doubled_sf) %>% 
  mutate(difference = weighted_base-weighted_doubled)
#export shapefile for QGIS
#st_write(fuel_mapping_sf, "difference_weighted_base_minus_doubled_20230417.shp")


##decided that the below is not actually necessary if just mapping things. if don't fix repeating grids and
#e.g. sum pots, then get a wrong result. but for our mapping purposes we don't need to do it
#get rid of duplicated IDs
##BASE
# predictions_dummy_fuel_base_sf <- predictions_dummy_fuel_base %>% left_join(study_area, by=c('GRID5KM_ID')) %>% 
#   select(-NGDC_GRID, -ORIG_AREA) 
# ##fix cases with repeating grids, as 'weighted' gets repeated for each piece of a grid
# #grid that appears 3 times: 86945
# grid_86945 <- predictions_dummy_fuel_base_sf %>% filter(GRID5KM_ID == 86945) %>% 
#   mutate(weighted = weighted/3)
# #grids that appear twice
# grids_twice <- predictions_dummy_fuel_base_sf %>% filter(GRID5KM_ID %in% c(89582, 89913, 96184, 96514, 96515, 96845, 
#                                                             98826, 98827, 99157, 100808, 101138, 105429, 
#                                                             105759, 107079, 112031, 112361, 112691, 117310, 
#                                                             117311, 117639, 117640, 117970, 118960, 119290, 
#                                                             119950, 120280, 120610, 120940, 122258, 122259, 
#                                                             122588, 122589, 122919, 129512, 129842)) %>% 
#   mutate(weighted = weighted/2)
# grids_ok <- predictions_dummy_fuel_base_sf %>% filter(!GRID5KM_ID %in% c(89582, 89913, 96184, 96514, 96515, 96845, 
#                                                          98826, 98827, 99157, 100808, 101138, 105429, 
#                                                          105759, 107079, 112031, 112361, 112691, 117310, 
#                                                          117311, 117639, 117640, 117970, 118960, 119290, 
#                                                          119950, 120280, 120610, 120940, 122258, 122259, 
#                                                          122588, 122589, 122919, 129512, 129842, 86945))
# fuel_mapping_sf_fix_base <- rbind(grids_ok, grids_twice, grid_86945)
# 
# ##DOUBLED
# predictions_dummy_fuel_doubled_sf <- predictions_dummy_fuel_doubled %>% left_join(study_area, by=c('GRID5KM_ID')) %>% 
#   select(-NGDC_GRID, -ORIG_AREA) 
# ##fix cases with repeating grids, as 'weighed' gets repeated for each piece of a grid
# #grid that appears 3 times: 86945
# grid_86945 <- predictions_dummy_fuel_doubled_sf %>% filter(GRID5KM_ID == 86945) %>% 
#   mutate(weighted = weighted/3)
# grids_twice <- predictions_dummy_fuel_doubled_sf %>% filter(GRID5KM_ID %in% c(89582, 89913, 96184, 96514, 96515, 96845, 
#                                                                            98826, 98827, 99157, 100808, 101138, 105429, 
#                                                                            105759, 107079, 112031, 112361, 112691, 117310, 
#                                                                            117311, 117639, 117640, 117970, 118960, 119290, 
#                                                                            119950, 120280, 120610, 120940, 122258, 122259, 
#                                                                            122588, 122589, 122919, 129512, 129842)) %>% 
#   mutate(weighted = weighted/2)
# grids_ok <- predictions_dummy_fuel_doubled_sf %>% filter(!GRID5KM_ID %in% c(89582, 89913, 96184, 96514, 96515, 96845, 
#                                                                          98826, 98827, 99157, 100808, 101138, 105429, 
#                                                                          105759, 107079, 112031, 112361, 112691, 117310, 
#                                                                          117311, 117639, 117640, 117970, 118960, 119290, 
#                                                                          119950, 120280, 120610, 120940, 122258, 122259, 
#                                                                          122588, 122589, 122919, 129512, 129842, 86945))
# fuel_mapping_sf_fix_doubled <- rbind(grids_ok, grids_twice, grid_86945)
# 
# 
# fuel_mapping_sf <- rbind(fuel_mapping_sf_fix_base, fuel_mapping_sf_fix_doubled)
# #export shapefile for QGIS
# #st_write(fuel_mapping_sf, "fuel_mapping_sf_weighted.shp")









##this is old and hasn't been 'updated'
## fuel price doubled in TLA only
dummy_fuel <-  read_csv(here::here('DCRB_sdmTMB', 'data','dummy dfs','winter',"winter_test_data_fuel_changes_in_TLA_port_only.csv"))
dummy_fuel$half_month_of_seasonf <- as.factor(dummy_fuel$half_month_of_seasonf)
predictions_dummy_fuel <- predict(fit19b_winter, newdata = dummy_fuel) #, `se_fit` = TRUE

predictions_dummy_fuel_v2 <- predictions_dummy_fuel %>% mutate(bck_trns_est = exp(est)) %>% 
  select(GRID5KM_ID:tottraps, weighted_fuel_pricegal, z_weighted_fuel_pricegal,fuel_change:bck_trns_est)


scale_values <- function(x){(x-min(x))/(max(x)-min(x))}

predictions_dummy_fuel_base <- predictions_dummy_fuel_v2 %>% filter(fuel_change=="base") %>% 
  mutate(bck_trns_est_scaled = scale_values(bck_trns_est))

predictions_dummy_fuel_doubled <- predictions_dummy_fuel_v2 %>% filter(fuel_change=="TLA changed") %>% 
  mutate(bck_trns_est_scaled = scale_values(bck_trns_est))

predictions_dummy_fuel_v2 <- rbind(predictions_dummy_fuel_base, predictions_dummy_fuel_doubled)


#read in restricted study area shapefile
study_area <- read_sf(here::here('DCRB_sdmTMB','data','restricted_study_area.shp'))
#plot(study_area)


fuel_mapping_sf <- predictions_dummy_fuel_v2 %>% left_join(study_area, by=c('GRID5KM_ID')) %>% 
  select(-NGDC_GRID, -ORIG_AREA)

#export shapefile for QGIS
#st_write(fuel_mapping_sf, "fuel_mapping_TLA_changed_sf.shp")




