#sdmTMB runs


#here are some of the sort of questions we'd like to be able to answer:

#Which predictors have the biggest effect on crab fishing effort distribution in space? 
#Are static variables better predictors than dynamic variables? Are environmental variables better predictors than economic variables?
  
#How static/variable is the overall fleet footprint. How predictable is footprint from year to year?
  
#What parts of the study area are most variable or consistent between time steps/years?
#Which months are most variable or most consistent?

#-------------------------------------------------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(sdmTMB)
library(sf)
library(ggcorrplot)
library(mgcv)
library(ggeffects)
library(tictoc)

#this was needed for sdmTMB to work
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)

#-------------------------------------------------------------------------------------------------

# run models based on Eric's "examples" document in repo

#read in df
#no need to filter, that has been done
#but make sure set UTM zone

##NOTE THAT NEED TO RUN ALL DATA, SUMMER, AND WINTER DATASETS


d <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_all_data.rds'))
#z-scoring has been done across all data (winter and summer)
glimpse(d) #none z-scored column have been retained

d$yearn <- as.numeric(substr(d$season,1,4))
d$yearf <- as.factor(d$yearn)

# Add UTM columns (zone 10)
d = add_utm_columns(d, ll_names = c("grd_x", "grd_y"))

# try smooth over months
d$month_n <- 1
d$month_n[which(d$month_name=="January")] = 2
d$month_n[which(d$month_name=="February")] = 3
d$month_n[which(d$month_name=="March")] = 4
d$month_n[which(d$month_name=="April")] = 5
d$month_n[which(d$month_name=="May")] = 6
d$month_n[which(d$month_name=="June")] = 7
d$month_n[which(d$month_name=="July")] = 8
d$month_n[which(d$month_name=="August")] = 9
d$month_n[which(d$month_name=="September")] = 10


#summer
summer <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_summer.rds'))
#z-scoring has been done across summer only
glimpse(summer) #none z-scored column have been retained

summer$yearn <- as.numeric(substr(summer$season,1,4))
summer$yearf <- as.factor(summer$yearn)

# try smooth over months
summer$month_n <- 5
summer$month_n[which(summer$month_name=="June")] = 6
summer$month_n[which(summer$month_name=="July")] = 7
summer$month_n[which(summer$month_name=="August")] = 8
summer$month_n[which(summer$month_name=="September")] = 9

# Add UTM columns (zone 10)
summer = add_utm_columns(summer, ll_names = c("grd_x", "grd_y"))



#winter

winter <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_winter.rds'))
#z-scoring has been done across summer only
glimpse(winter) #none z-scored column have been retained

winter$yearn <- as.numeric(substr(winter$season,1,4))
winter$yearf <- as.factor(winter$yearn)

# try smooth over months
winter$month_n <- 1
winter$month_n[which(winter$month_name=="January")] = 2
winter$month_n[which(winter$month_name=="February")] = 3
winter$month_n[which(winter$month_name=="March")] = 4
winter$month_n[which(winter$month_name=="April")] = 5

# Add UTM columns (zone 10)
winter = add_utm_columns(winter, ll_names = c("grd_x", "grd_y"))
#-------------------------------------------------------------------------------------------------

#Initial model

#fit0 = covariates only

mesh <- make_mesh(d, xy_cols = c("X","Y"), cutoff = 10)
mesh$mesh$n

tic()
fit0_all_data <- sdmTMB(tottraps ~ 0 + 
                 season +
                 month_name + 
                 OR_WA_waters +
                 WA_pot_reduction +
                 poly(z_SST_avg,2) +
                 poly(z_wind_avg,2) +
                 poly(z_depth_point_mean,2) +
                 poly(z_depth_point_sd,2) +
                 poly(z_faults_km,2) +
                 poly(z_dist_canyon_km,2) +
                 poly(z_weighted_dist,2) +
                 poly(z_weighted_fuel_pricegal,2) +
                 poly(z_weighted_crab_ppp,2) +
                 poly(z_bottom_O2_avg,2) +
                 poly(z_dist_to_closed_km ,2),
               family = tweedie(),
               mesh = mesh,
               spatial = "off",
               spatiotemporal = "off",
               data = d,
               time = "yearf")
toc() #3.6min

#sanity(fit0_all_data)
#no warning about convergence issues. some red Xs in sanity check (e.g. "`b_j` gradient > 0.001")
AIC(fit0_all_data)
#1073744

#-------------------------------------------------------------------------------------------------

#Adding spatial and spatiotemporal fields (seasons)

tic()
fit1_all_data <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_name + 
                          OR_WA_waters +
                          WA_pot_reduction +
                          poly(z_SST_avg,2) +
                          poly(z_wind_avg,2) +
                          poly(z_depth_point_mean,2) +
                          poly(z_depth_point_sd,2) +
                          poly(z_faults_km,2) +
                          poly(z_dist_canyon_km,2) +
                          poly(z_weighted_dist,2) +
                          poly(z_weighted_fuel_pricegal,2) +
                          poly(z_weighted_crab_ppp,2) +
                          poly(z_bottom_O2_avg,2) +
                          poly(z_dist_to_closed_km ,2),
                        family = tweedie(),
                        mesh = mesh,
                        spatial = "on",
                        spatiotemporal = "off",
                        data = d,
                        time = "yearf")
toc() #12.5min

#sanity(fit1_all_data)
#no warning about convergence issues. some red Xs in sanity check (e.g. "`b_j` gradient > 0.001")
AIC(fit1_all_data)
#1028092



tic()
fit2_all_data <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_name + 
                          OR_WA_waters +
                          WA_pot_reduction +
                          poly(z_SST_avg,2) +
                          poly(z_wind_avg,2) +
                          poly(z_depth_point_mean,2) +
                          poly(z_depth_point_sd,2) +
                          poly(z_faults_km,2) +
                          poly(z_dist_canyon_km,2) +
                          poly(z_weighted_dist,2) +
                          poly(z_weighted_fuel_pricegal,2) +
                          poly(z_weighted_crab_ppp,2) +
                          poly(z_bottom_O2_avg,2) +
                          poly(z_dist_to_closed_km ,2),
                        family = tweedie(),
                        mesh = mesh,
                        spatial = "on",
                        spatiotemporal = "iid",
                        data = d,
                        time = "yearf")
toc() #12.5min

#sanity(fit2_all_data)
#no warning about convergence issues. 1 red Xs in sanity check (e.g. "`ln_kappa` gradient > 0.001")
AIC(fit2_all_data)
#1014316
#-------------------------------------------------------------------------------------------------

#model with only spatial and spatiotemporal fields (seasons), no covariates

tic()
fit1b_all_data <- sdmTMB(tottraps ~ 0,
                        family = tweedie(),
                        mesh = mesh,
                        spatial = "on",
                        spatiotemporal = "off",
                        data = d,
                        time = "yearf")
toc() #0.6min

#sanity(fit1b_all_data)
#no warning about convergence issues. No red Xs in sanity check
AIC(fit1b_all_data)
#1058927


#2b
tic()
fit2b_all_data <- sdmTMB(tottraps ~ 0,
                         family = tweedie(),
                         mesh = mesh,
                         spatial = "on",
                         spatiotemporal = "iid",
                         data = d,
                         time = "yearf")
toc() #1.3min

#sanity(fit2b_all_data)
#no warning about convergence issues. No red Xs in sanity check
AIC(fit2b_all_data)
#1047840

#-------------------------------------------------------------------------------------------------

#Just as a test, we can see if changing month to a smooth improves the fit

tic()
fit3_all_data <- sdmTMB(tottraps ~ 0 + 
                          s(month_n, k = 8) + # <- new, what is k supposed to be?
                          season +
                          #month_name + 
                          OR_WA_waters +
                          WA_pot_reduction +
                          poly(z_SST_avg,2) +
                          poly(z_wind_avg,2) +
                          poly(z_depth_point_mean,2) +
                          poly(z_depth_point_sd,2) +
                          poly(z_faults_km,2) +
                          poly(z_dist_canyon_km,2) +
                          poly(z_weighted_dist,2) +
                          poly(z_weighted_fuel_pricegal,2) +
                          poly(z_weighted_crab_ppp,2) +
                          poly(z_bottom_O2_avg,2) +
                          poly(z_dist_to_closed_km ,2),
                        family = tweedie(),
                        mesh = mesh,
                        spatial = "on",
                        spatiotemporal = "iid",
                        data = d,
                        time = "yearf")
toc() #12.8min k=10 // 12.3 when k=8

#sanity(fit3_all_data)
# k=10: Warning message: The model may not have converged. Maximum final gradient: 0.0148537011631085. Some red Xs
#k=8 no convergency warnings. Few red Xs in sanity check
AIC(fit3_all_data)
#k=10: 1014351 --  same if k=8


#-------------------------------------------------------------------------------------------------






#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------

                              ##SUMMER DATA

#Initial model
#fit0 = covariates only

mesh <- make_mesh(summer, xy_cols = c("X","Y"), cutoff = 10)
mesh$mesh$n

tic()
fit0_summer <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_name + 
                          OR_WA_waters +
                          WA_pot_reduction +
                          poly(z_SST_avg,2) +
                          poly(z_wind_avg,2) +
                          poly(z_depth_point_mean,2) +
                          poly(z_depth_point_sd,2) +
                          poly(z_faults_km,2) +
                          poly(z_dist_canyon_km,2) +
                          poly(z_weighted_dist,2) +
                          poly(z_weighted_fuel_pricegal,2) +
                          poly(z_weighted_crab_ppp,2) +
                          poly(z_bottom_O2_avg,2) +
                          poly(z_dist_to_closed_km ,2),
                        family = tweedie(),
                        mesh = mesh,
                        spatial = "off",
                        spatiotemporal = "off",
                        data = summer,
                        time = "yearf")
toc() #0.8min

#sanity(fit0_summer)
#Warning message:The model may not have converged. Maximum final gradient: 0.0150379936285089
#Couple red Xs in sanity check
AIC(fit0_summer)
#287161

#-------------------------------------------------------------------------------------------------

#Adding spatial and spatiotemporal fields (seasons)

tic()
fit1_summer <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_name + 
                          OR_WA_waters +
                          WA_pot_reduction +
                          poly(z_SST_avg,2) +
                          poly(z_wind_avg,2) +
                          poly(z_depth_point_mean,2) +
                          poly(z_depth_point_sd,2) +
                          poly(z_faults_km,2) +
                          poly(z_dist_canyon_km,2) +
                          poly(z_weighted_dist,2) +
                          poly(z_weighted_fuel_pricegal,2) +
                          poly(z_weighted_crab_ppp,2) +
                          poly(z_bottom_O2_avg,2) +
                          poly(z_dist_to_closed_km ,2),
                        family = tweedie(),
                        mesh = mesh,
                        spatial = "on",
                        spatiotemporal = "off",
                        data = summer,
                        time = "yearf")
toc() #3.8min

#sanity(fit1_summer)
#no warning about convergence issues. No red Xs in sanity check
AIC(fit1_summer)
#272600



tic()
fit2_summer <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_name + 
                          OR_WA_waters +
                          WA_pot_reduction +
                          poly(z_SST_avg,2) +
                          poly(z_wind_avg,2) +
                          poly(z_depth_point_mean,2) +
                          poly(z_depth_point_sd,2) +
                          poly(z_faults_km,2) +
                          poly(z_dist_canyon_km,2) +
                          poly(z_weighted_dist,2) +
                          poly(z_weighted_fuel_pricegal,2) +
                          poly(z_weighted_crab_ppp,2) +
                          poly(z_bottom_O2_avg,2) +
                          poly(z_dist_to_closed_km ,2),
                        family = tweedie(),
                        mesh = mesh,
                        spatial = "on",
                        spatiotemporal = "iid",
                        data = summer,
                        time = "yearf")
toc() #5.1min

#sanity(fit2_summer)
#no warning about convergence issues. No red Xs in sanity check
AIC(fit2_summer)
#265942
#-------------------------------------------------------------------------------------------------

#model with only spatial and spatiotemporal fields (seasons), no covariates

tic()
fit1b_summer <- sdmTMB(tottraps ~ 0,
                         family = tweedie(),
                         mesh = mesh,
                         spatial = "on",
                         spatiotemporal = "off",
                         data = summer,
                         time = "yearf")
toc() #0.2min

#sanity(fit1b_summer)
#no warning about convergence issues. No red Xs in sanity check
AIC(fit1b_summer)
#277485



tic()
fit2b_summer <- sdmTMB(tottraps ~ 0,
                         family = tweedie(),
                         mesh = mesh,
                         spatial = "on",
                         spatiotemporal = "iid",
                         data = summer,
                         time = "yearf")
toc() #0.6min

#sanity(fit2b_summer)
#Warning message: The model may not have converged. Maximum final gradient: 0.0108565926496453
#Couple red Xs in sanity check
AIC(fit2b_summer)
#271531

#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------

                                  ##WINTER DATA

#Initial model
#fit0 = covariates only

mesh <- make_mesh(winter, xy_cols = c("X","Y"), cutoff = 10)
mesh$mesh$n

tic()
fit0_winter <- sdmTMB(tottraps ~ 0 + 
                        season +
                        month_name + 
                        OR_WA_waters +
                        #WA_pot_reduction +  #not relevant in winter
                        poly(z_SST_avg,2) +
                        poly(z_wind_avg,2) +
                        poly(z_depth_point_mean,2) +
                        poly(z_depth_point_sd,2) +
                        poly(z_faults_km,2) +
                        poly(z_dist_canyon_km,2) +
                        poly(z_weighted_dist,2) +
                        poly(z_weighted_fuel_pricegal,2) +
                        poly(z_weighted_crab_ppp,2) +
                        poly(z_bottom_O2_avg,2) +
                        poly(z_dist_to_closed_km ,2),
                      family = tweedie(),
                      mesh = mesh,
                      spatial = "off",
                      spatiotemporal = "off",
                      data = winter,
                      time = "yearf")
toc() #2.2min

#sanity(fit0_winter)
#no warning about convergence issues. Some red Xs in sanity check
AIC(fit0_winter)
#774067

#-------------------------------------------------------------------------------------------------

#Adding spatial and spatiotemporal fields (seasons)

tic()
fit1_winter <- sdmTMB(tottraps ~ 0 + 
                        season +
                        month_name + 
                        OR_WA_waters +
                        #WA_pot_reduction +    #not relevant in winter
                        poly(z_SST_avg,2) +
                        poly(z_wind_avg,2) +
                        poly(z_depth_point_mean,2) +
                        poly(z_depth_point_sd,2) +
                        poly(z_faults_km,2) +
                        poly(z_dist_canyon_km,2) +
                        poly(z_weighted_dist,2) +
                        poly(z_weighted_fuel_pricegal,2) +
                        poly(z_weighted_crab_ppp,2) +
                        poly(z_bottom_O2_avg,2) +
                        poly(z_dist_to_closed_km ,2),
                      family = tweedie(),
                      mesh = mesh,
                      spatial = "on",
                      spatiotemporal = "off",
                      data = winter,
                      time = "yearf")
toc() #7min

#sanity(fit1_winter)
#Warning message:The model may not have converged. Maximum final gradient: 0.0167330325325095
#Couple red Xs in sanity check
AIC(fit1_winter)
#739449



tic()
fit2_winter <- sdmTMB(tottraps ~ 0 + 
                        season +
                        month_name + 
                        OR_WA_waters +
                        #WA_pot_reduction +      #not relevant in winter
                        poly(z_SST_avg,2) +
                        poly(z_wind_avg,2) +
                        poly(z_depth_point_mean,2) +
                        poly(z_depth_point_sd,2) +
                        poly(z_faults_km,2) +
                        poly(z_dist_canyon_km,2) +
                        poly(z_weighted_dist,2) +
                        poly(z_weighted_fuel_pricegal,2) +
                        poly(z_weighted_crab_ppp,2) +
                        poly(z_bottom_O2_avg,2) +
                        poly(z_dist_to_closed_km ,2),
                      family = tweedie(),
                      mesh = mesh,
                      spatial = "on",
                      spatiotemporal = "iid",
                      data = winter,
                      time = "yearf")
toc() #8min

#sanity(fit2_winter)
#no warning about convergence issues. Some red Xs in sanity check
AIC(fit2_winter)
#728431
#-------------------------------------------------------------------------------------------------

#model with only spatial and spatiotemporal fields (seasons), no covariates

tic()
fit1b_winter <- sdmTMB(tottraps ~ 0,
                       family = tweedie(),
                       mesh = mesh,
                       spatial = "on",
                       spatiotemporal = "off",
                       data = winter,
                       time = "yearf")
toc() #0.5min

#sanity(fit1b_winter)
#no warning about convergence issues. No red Xs in sanity check
AIC(fit1b_winter)
#744916



tic()
fit2b_winter <- sdmTMB(tottraps ~ 0,
                       family = tweedie(),
                       mesh = mesh,
                       spatial = "on",
                       spatiotemporal = "iid",
                       data = winter,
                       time = "yearf")
toc() #1.1min

#sanity(fit2b_winter)
#no warning about convergence issues. No red Xs in sanity check
AIC(fit2b_winter)
#734885
#-------------------------------------------------------------------------------------------------


































#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------


