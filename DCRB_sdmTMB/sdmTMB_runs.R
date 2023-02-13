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
library(plotmo)

#this was needed for sdmTMB to work
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)

#-------------------------------------------------------------------------------------------------

set.seed(1)

#-------------------------------------------------------------------------------------------------
# run models based on Eric's "examples" document in repo

#read in df
#no need to filter, that has been done
#also more recent dfs already have steps for numeric and factorial year, numerical months (for smooths)
#but make sure set UTM zone

##NOTE THAT NEED TO RUN ALL DATA, SUMMER, AND WINTER DATASETS


d_all_data <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_all_data_20230209.rds'))
#z-scoring has been done across all data (winter and summer)
glimpse(d_all_data) 

d_all_data$month_name_f <- factor(d_all_data$month_name, levels = c("December", "January", "February", "March", "April", 
                                                   "May", "June", "July", "August", "September"))

# Add UTM columns (zone 10)
d_all_data = add_utm_columns(d_all_data, ll_names = c("grd_x", "grd_y"))



#summer
summer <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_summer_20230209.rds')) 
#z-scoring has been done across summer only
glimpse(summer)

summer$month_name_f <- factor(summer$month_name, levels = c("May", "June", "July", "August", "September"))

# Add UTM columns (zone 10)
summer = add_utm_columns(summer, ll_names = c("grd_x", "grd_y"))



#winter
winter <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_winter_20230209.rds'))
#z-scoring has been done across summer only
glimpse(winter) 

winter$month_name_f <- factor(winter$month_name, levels = c("December", "January", "February", "March", "April"))

# Add UTM columns (zone 10)
winter = add_utm_columns(winter, ll_names = c("grd_x", "grd_y"))

#-------------------------------------------------------------------------------------------------
##in the current round of models, the default is to not use polynomial terms, and to use the basic z-scored values
#except for e.g. season/year/month that will be factorials
#-------------------------------------------------------------------------------------------------

#Initial model

#fit0 = covariates only

mesh <- make_mesh(d_all_data, xy_cols = c("X","Y"), cutoff = 10)
mesh$mesh$n

tic()
fit0_all_data <- sdmTMB(tottraps ~ 0 + 
                 season +
                 month_name_f + 
                 OR_WA_waters +
                 WA_pot_reduction +
                 z_SST_avg +
                 z_wind_avg +
                 z_depth_point_mean +
                 z_depth_point_sd +
                 z_faults_km +
                 z_dist_canyon_km +
                 z_weighted_dist +
                 z_weighted_fuel_pricegal +
                 z_weighted_crab_ppp +
                 z_bottom_O2_avg +
                 z_dist_to_closed_km,
               family = tweedie(),
               mesh = mesh,
               spatial = "off",
               spatiotemporal = "off",
               data = d_all_data,
               time = "yearf")
toc() #4min

#sanity(fit0_all_data)
#When seed set & no polynomials: The model may not have converged. Maximum final gradient: 0.0268904298444301 
#some red Xs in sanity check - b_js, thetaf, ln_phi
#sanity(fit0_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, thetaf, ln_phi
AIC(fit0_all_data)
#1076517

#-------------------------------------------------------------------------------------------------

#Adding spatial and spatiotemporal fields (seasons)

tic()
fit1_all_data <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_name_f + 
                          OR_WA_waters +
                          WA_pot_reduction +
                          z_SST_avg +
                          z_wind_avg +
                          z_depth_point_mean +
                          z_depth_point_sd +
                          z_faults_km +
                          z_dist_canyon_km +
                          z_weighted_dist +
                          z_weighted_fuel_pricegal +
                          z_weighted_crab_ppp +
                          z_bottom_O2_avg +
                          z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh,
                        spatial = "on",
                        spatiotemporal = "off",
                        data = d_all_data,
                        time = "yearf")
toc() #117.6min

#sanity(fit1_all_data)
#seed set or no polynomials: The model may not have converged. Maximum final gradient: 0.264274485057823
#some red Xs in sanity check (b_js, ln_tau, ln_phi)
#sanity(fit1_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, ln_tau, ln_phi
AIC(fit1_all_data)
#1030530



tic()
fit2_all_data <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_name_f + 
                          OR_WA_waters +
                          WA_pot_reduction +
                          z_SST_avg +
                          z_wind_avg +
                          z_depth_point_mean +
                          z_depth_point_sd +
                          z_faults_km +
                          z_dist_canyon_km +
                          z_weighted_dist +
                          z_weighted_fuel_pricegal +
                          z_weighted_crab_ppp +
                          z_bottom_O2_avg +
                          z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh,
                        spatial = "on",
                        spatiotemporal = "iid",
                        data = d_all_data,
                        time = "yearf")
toc() #13.3min

#sanity(fit2_all_data)
#when seed set and no polynomials: The model may not have converged. Maximum final gradient: 0.146955463494238
#some red Xs: b_js, ln_tau
#sanity(fit2_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, ln_tau
AIC(fit2_all_data)
#1017844
#-------------------------------------------------------------------------------------------------

#model with only spatial and spatiotemporal fields (seasons), no covariates

tic()
fit1b_all_data <- sdmTMB(tottraps ~ 0,
                        family = tweedie(),
                        mesh = mesh,
                        spatial = "on",
                        spatiotemporal = "off",
                        data = d_all_data,
                        time = "yearf")
toc() #0.6min

#sanity(fit1b_all_data)
#when seed set and no polynomials: no warning about convergence issues. 
#No red Xs in sanity check
#sanity(fit1b_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
AIC(fit1b_all_data)
#1058927


#2b
tic()
fit2b_all_data <- sdmTMB(tottraps ~ 0,
                         family = tweedie(),
                         mesh = mesh,
                         spatial = "on",
                         spatiotemporal = "iid",
                         data = d_all_data,
                         time = "yearf")
toc() #1.3min

#sanity(fit2b_all_data)
#when seed set and no polynomials:no warning about convergence issues. 
#No red Xs in sanity check 
#sanity(fit2b_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
AIC(fit2b_all_data)
#1047840


#2c
tic()
fit2c_all_data <- sdmTMB(tottraps ~ 0,
                         family = tweedie(),
                         mesh = mesh,
                         spatial = "on",
                         spatiotemporal = "ar1",
                         data = d_all_data,
                         time = "yearf")
toc() #4.4min

#when seed set at no polys: The model may not have converged. Maximum final gradient: 0.17503815124403
#sanity(fit2c_all_data)
#Red Xs: ln_tau, ln_kappa, thetaf, ln_phi, ar1_phi
#sanity(fit2c_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#still have ln_tau, thetaf, ln_phi
AIC(fit2c_all_data)
#1047657
summary(fit2c_all_data)
#AR1 correlation (rho): 0.56

#-------------------------------------------------------------------------------------------------

#Just as a test, we can see if changing month to a smooth improves the fit

tic()
fit3_all_data <- sdmTMB(tottraps ~ 0 + 
                          s(month_n) + # <- new, let k be automatically selected
                          season +
                          #month_name + 
                          OR_WA_waters +
                          WA_pot_reduction +
                          z_SST_avg +
                          z_wind_avg +
                          z_depth_point_mean +
                          z_depth_point_sd +
                          z_faults_km +
                          z_dist_canyon_km +
                          z_weighted_dist +
                          z_weighted_fuel_pricegal +
                          z_weighted_crab_ppp +
                          z_bottom_O2_avg +
                          z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh,
                        spatial = "on",
                        spatiotemporal = "iid",
                        data = d_all_data,
                        time = "yearf")
toc() #12.6min 

#when seed set and no polynomials: The model may not have converged. Maximum final gradient: 0.15756103918822 
#sanity(fit3_all_data)
#Some red Xs (b_js, ln_kappa, thetaf, ln_phi)
#sanity(fit3_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, ln_kappa, thetaf, ln_phi
AIC(fit3_all_data)
#1017878 


#------------------------------------

# Just as a test, we can see if changing year/season to a smooth improves the fit

tic()
fit4_all_data <- sdmTMB(tottraps ~ 0 + 
                          month_name_f + 
                          s(yearn) + # <- new, let k be automatically selected
                          #season +
                          OR_WA_waters +
                          WA_pot_reduction +
                          z_SST_avg +
                          z_wind_avg +
                          z_depth_point_mean +
                          z_depth_point_sd +
                          z_faults_km +
                          z_dist_canyon_km +
                          z_weighted_dist +
                          z_weighted_fuel_pricegal +
                          z_weighted_crab_ppp +
                          z_bottom_O2_avg +
                          z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh,
                        spatial = "on",
                        spatiotemporal = "iid",
                        data = d_all_data,
                        time = "yearf")
toc() #15.1min 

#when seed is set and no polynomials: The model may not have converged: non-positive-definite Hessian matrix
#sanity(fit4_all_data)
#Lots of red Xs
#sanity(fit4_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
# Still lots of red Xs
AIC(fit4_all_data)
#1017833 

#--------------------------------------
#change the IID spatiotemporal fields in model 2 to “AR1” to test the autoregressive structure
# Is there support for AR1 spatiotemporal fields?
# Using the best model (fit2)

tic()
fit5_all_data <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_name_f + 
                          OR_WA_waters +
                          WA_pot_reduction +
                          z_SST_avg +
                          z_wind_avg +
                          z_depth_point_mean +
                          z_depth_point_sd +
                          z_faults_km +
                          z_dist_canyon_km +
                          z_weighted_dist +
                          z_weighted_fuel_pricegal +
                          z_weighted_crab_ppp +
                          z_bottom_O2_avg +
                          z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh,
                        spatial = "on",
                        spatiotemporal = "ar1", # <- new
                        data = d_all_data,
                        time = "yearf")
toc() #40.4min

#sanity(fit5_all_data)
#when seed set and no polynomial terms: The model may not have converged. Maximum final gradient: 0.133647741153254 
#Some red Xs (b_js, ln_tau)
#sanity(fit5_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Some red Xs (b_j only)
AIC(fit5_all_data)
#1017657
#summary(fit5_all_data)
#Spatiotemporal AR1 correlation (rho): 0.55

#--------------------------------------
#Adding spatial and spatiotemporal fields (months)

# switch indexing of spatiotemporal fields to “month_name” and again can try the spatiotemporal fields as IID or AR1.

# tic()
# fit6_all_data <- update(fit2_all_data,
#                time = "month_n")
# toc()  #13.7min


tic()
fit6_all_data <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_name_f + 
                          OR_WA_waters +
                          WA_pot_reduction +
                          z_SST_avg +
                          z_wind_avg +
                          z_depth_point_mean +
                          z_depth_point_sd +
                          z_faults_km +
                          z_dist_canyon_km +
                          z_weighted_dist +
                          z_weighted_fuel_pricegal +
                          z_weighted_crab_ppp +
                          z_bottom_O2_avg +
                          z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh,
                        spatial = "on",
                        spatiotemporal = "iid",
                        data = d_all_data,
                        time = "month_n")
toc()  #13.7min

#when seed set and no polynomials: The model may not have converged. Maximum final gradient: 0.0975291024016594 
#sanity(fit6_all_data)
#some red Xs (b_js, ln_kappa)
#sanity(fit6_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Some red Xs (b_j only)
AIC(fit6_all_data)
#1015953

#plots <- plot_diag(fit6_all_data)





# tic()
# fit7_all_data <- update(fit2_all_data,
#                time = "month_n",
#                spatiotemporal = "ar1")
# toc()  #43min

tic()
fit7_all_data <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_name_f + 
                          OR_WA_waters +
                          WA_pot_reduction +
                          z_SST_avg +
                          z_wind_avg +
                          z_depth_point_mean +
                          z_depth_point_sd +
                          z_faults_km +
                          z_dist_canyon_km +
                          z_weighted_dist +
                          z_weighted_fuel_pricegal +
                          z_weighted_crab_ppp +
                          z_bottom_O2_avg +
                          z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh,
                        spatial = "on",
                        spatiotemporal = "ar1",
                        data = d_all_data,
                        time = "month_n")
toc() #min

#when seed set and no polynomials: The model may not have converged. Maximum final gradient: 0.0541188746935877
#sanity(fit7_all_data)
#some red Xs (b_js, thetaf, ln_phi, ln_tau, sigma_O)
#sanity(fit7_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#some red Xs (b_js, thetaf, ln_phi, ln_tau, sigma_O)
AIC(fit7_all_data)
#1015198
#summary(fit7_all_data) 
#Spatiotemporal AR1 correlation (rho): 0.98



#index by half_month step
d_all_data$half_month_n <- 1
d_all_data$half_month_n[which(d_all_data$half_month=="December_2")] = 2
d_all_data$half_month_n[which(d_all_data$half_month=="January_1")] = 3
d_all_data$half_month_n[which(d_all_data$half_month=="January_2")] = 4
d_all_data$half_month_n[which(d_all_data$half_month=="February_1")] = 5
d_all_data$half_month_n[which(d_all_data$half_month=="February_2")] = 6
d_all_data$half_month_n[which(d_all_data$half_month=="March_1")] = 7
d_all_data$half_month_n[which(d_all_data$half_month=="March_2")] = 8
d_all_data$half_month_n[which(d_all_data$half_month=="April_1")] = 9
d_all_data$half_month_n[which(d_all_data$half_month=="April_2")] = 10
d_all_data$half_month_n[which(d_all_data$half_month=="May_1")] = 11
d_all_data$half_month_n[which(d_all_data$half_month=="May_2")] = 12
d_all_data$half_month_n[which(d_all_data$half_month=="June_1")] = 13
d_all_data$half_month_n[which(d_all_data$half_month=="June_2")] = 14
d_all_data$half_month_n[which(d_all_data$half_month=="July_1")] = 15
d_all_data$half_month_n[which(d_all_data$half_month=="July_2")] = 16
d_all_data$half_month_n[which(d_all_data$half_month=="August_1")] = 17
d_all_data$half_month_n[which(d_all_data$half_month=="August_2")] = 18
d_all_data$half_month_n[which(d_all_data$half_month=="September_1")] = 19


tic()
fit8_all_data <- update(fit2_all_data,
                        time = "half_month_n") #as numeric
toc()  #15.5min

#when seed set and no polynomials: had some warning but not copied down here
#sanity(fit8_all_data)
#red Xs (b_js, ln_tau, thetaf)
#sanity(fit8_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, ln_tau, thetaf
AIC(fit8_all_data)
#1016081
#results are same if half_month is factor AS LONG AS LEVELS ARE ORDERED CORRECTLY

#--------------------------------------

tic()
fit9_all_data <- sdmTMB(tottraps ~ 0 + 
                          s(half_month_n) + # <- new, let k be automatically selected
                          season +
                          #month_name + 
                          OR_WA_waters +
                          WA_pot_reduction +
                          z_SST_avg +
                          z_wind_avg +
                          z_depth_point_mean +
                          z_depth_point_sd +
                          z_faults_km +
                          z_dist_canyon_km +
                          z_weighted_dist +
                          z_weighted_fuel_pricegal +
                          z_weighted_crab_ppp +
                          z_bottom_O2_avg +
                          z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh,
                        spatial = "on",
                        spatiotemporal = "iid",
                        data = d_all_data,
                        time = "yearf")
toc() #12.7min 

#Warning message: The model may not have converged. Maximum final gradient: 0.0772079237766903
#sanity(fit9_all_data)
#Red Xs: b_j, ln_tau
#sanity(fit9_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_j, ln_tau
AIC(fit9_all_data)
# 1017473


#--------------------------------------


tic()
fit10a_all_data <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_name_f + 
                          OR_WA_waters +
                          WA_pot_reduction +
                          z_SST_avg +
                          z_wind_avg +
                          poly(z_depth_point_mean,2) +
                          z_depth_point_sd +
                          z_faults_km +
                          z_dist_canyon_km +
                          z_weighted_dist +
                          z_weighted_fuel_pricegal +
                          z_weighted_crab_ppp +
                          z_bottom_O2_avg +
                          z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh,
                        spatial = "on",
                        spatiotemporal = "iid",
                        data = d_all_data,
                        time = "month_n")
toc() #15 min

#The model may not have converged. Maximum final gradient: 0.0447868798046311
#sanity(fit10a_all_data)
#Red Xs: b_js, ln_tau, thetaf 
#sanity(fit10a_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, thetaf 
AIC(fit10a_all_data)
# 1013714

tic()
fit10b_all_data <- sdmTMB(tottraps ~ 0 + 
                            season +
                            month_name_f + 
                            OR_WA_waters +
                            WA_pot_reduction +
                            z_SST_avg +
                            z_wind_avg +
                            z_depth_point_mean +
                            z_depth_point_sd +
                            z_faults_km +
                            z_dist_canyon_km +
                            z_weighted_dist +
                            z_weighted_fuel_pricegal +
                            z_weighted_crab_ppp +
                            poly(z_bottom_O2_avg,2) +
                            z_dist_to_closed_km,
                          family = tweedie(),
                          mesh = mesh,
                          spatial = "on",
                          spatiotemporal = "iid",
                          data = d_all_data,
                          time = "month_n")
toc() #13min

#The model may not have converged. Maximum final gradient: 0.0491395900269096
#sanity(fit10b_all_data)
#Red Xs: b_js, ln_tau, ln_kappa
#sanity(fit10b_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, ln_tau, ln_kappa
AIC(fit10b_all_data)
# 1015954


tic()
fit10c_all_data <- sdmTMB(tottraps ~ 0 + 
                            season +
                            month_name_f + 
                            OR_WA_waters +
                            WA_pot_reduction +
                            z_SST_avg +
                            z_wind_avg +
                            poly(z_depth_point_mean,2) +
                            z_depth_point_sd +
                            z_faults_km +
                            z_dist_canyon_km +
                            z_weighted_dist +
                            z_weighted_fuel_pricegal +
                            z_weighted_crab_ppp +
                            poly(z_bottom_O2_avg,2) +
                            z_dist_to_closed_km,
                          family = tweedie(),
                          mesh = mesh,
                          spatial = "on",
                          spatiotemporal = "iid",
                          data = d_all_data,
                          time = "month_n")
toc() #14min

#No warnings
#sanity(fit10c_all_data)
#Red Xs: b_js only
#sanity(fit10c_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: none
AIC(fit10c_all_data)
# 1013716

#plots <- plot_diag(fit10c_all_data)

#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------

                              ##SUMMER DATA

#Initial model
#fit0 = covariates only

mesh_summer <- make_mesh(summer, xy_cols = c("X","Y"), cutoff = 10)
mesh_summer$mesh$n

tic()
fit0_summer <- sdmTMB(tottraps ~ 0 + 
                        season +
                        month_name_f + 
                        OR_WA_waters +
                        WA_pot_reduction +
                        z_SST_avg +
                        z_wind_avg +
                        z_depth_point_mean +
                        z_depth_point_sd +
                        z_faults_km +
                        z_dist_canyon_km +
                        z_weighted_dist +
                        z_weighted_fuel_pricegal +
                        z_weighted_crab_ppp +
                        z_bottom_O2_avg +
                        z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh_summer,
                        spatial = "off",
                        spatiotemporal = "off",
                        data = summer,
                        time = "yearf")
toc() #0.8min

#when seed is set and no polynomials: The model may not have converged. Maximum final gradient: 0.0123570464511804
#sanity(fit0_summer)
#Red Xs: b_js, thetaf, ln_phi
#sanity(fit0_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, ln_phi
AIC(fit0_summer)
#287759.8

#-------------------------------------------------------------------------------------------------

#Adding spatial and spatiotemporal fields (seasons)

tic()
fit1_summer <- sdmTMB(tottraps ~ 0 + 
                        season +
                        month_name_f + 
                        OR_WA_waters +
                        WA_pot_reduction +
                        z_SST_avg +
                        z_wind_avg +
                        z_depth_point_mean +
                        z_depth_point_sd +
                        z_faults_km +
                        z_dist_canyon_km +
                        z_weighted_dist +
                        z_weighted_fuel_pricegal +
                        z_weighted_crab_ppp +
                        z_bottom_O2_avg +
                        z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh_summer,
                        spatial = "on",
                        spatiotemporal = "off",
                        data = summer,
                        time = "yearf")
toc() #3.8min

#when seed set and no polynomials: The model may not have converged. Maximum final gradient: 0.0246840868351694 
#sanity(fit1_summer)
#Red Xs: b_js, ln_phi 
#sanity(fit1_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, ln_phi 
AIC(fit1_summer)
#273554.1



tic()
fit2_summer <- sdmTMB(tottraps ~ 0 + 
                        season +
                        month_name_f + 
                        OR_WA_waters +
                        WA_pot_reduction +
                        z_SST_avg +
                        z_wind_avg +
                        z_depth_point_mean +
                        z_depth_point_sd +
                        z_faults_km +
                        z_dist_canyon_km +
                        z_weighted_dist +
                        z_weighted_fuel_pricegal +
                        z_weighted_crab_ppp +
                        z_bottom_O2_avg +
                        z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh_summer,
                        spatial = "on",
                        spatiotemporal = "iid",
                        data = summer,
                        time = "yearf")
toc() #5.1min

#when seed set and no polynomials:The model may not have converged. Maximum final gradient: 0.105323135497564
#sanity(fit2_summer)
#Red Xs: b_js, ln_tau, thetaf,ln_phi
#sanity(fit2_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, ln_tau, thetaf,ln_phi
AIC(fit2_summer)
#267717.8
#-------------------------------------------------------------------------------------------------

#model with only spatial and spatiotemporal fields (seasons), no covariates

tic()
fit1b_summer <- sdmTMB(tottraps ~ 0,
                         family = tweedie(),
                         mesh = mesh_summer,
                         spatial = "on",
                         spatiotemporal = "off",
                         data = summer,
                         time = "yearf")
toc() #0.2min


#when set seed and no polynomials: no warning about convergence issues. 
#sanity(fit1b_summer)
#No red Xs
#sanity(fit1b_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
AIC(fit1b_summer)
#277485.1



tic()
fit2b_summer <- sdmTMB(tottraps ~ 0,
                         family = tweedie(),
                         mesh = mesh_summer,
                         spatial = "on",
                         spatiotemporal = "iid",
                         data = summer,
                         time = "yearf")
toc() #0.6min

#when seed set and no polynomials: The model may not have converged. Maximum final gradient: 0.0108447213867819
#sanity(fit2b_summer)
#red Xs: ln_tau, ln_kappa, ln_phi
#sanity(fit2b_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#red Xs: ln_tau, ln_phi
AIC(fit2b_summer)
#271531.5



#2c
tic()
fit2c_summer <- sdmTMB(tottraps ~ 0,
                       family = tweedie(),
                       mesh = mesh_summer,
                       spatial = "on",
                       spatiotemporal = "ar1",
                       data = summer,
                       time = "yearf")
toc() #1.4min

#when seed set and no polynomials: The model may not have converged. Maximum final gradient: 0.072301593333485
#sanity(fit2c_summer)
#red Xs: ln_tau, thetaf, ln_phi
#sanity(fit2c_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#red Xs: ln_tau, ln_phi
AIC(fit2c_summer)
#271460.9
summary(fit2c_summer)
#AR1 correlation (rho): 0.46




#2d
tic()
fit2d_summer <- sdmTMB(tottraps ~ 0,
                       family = tweedie(),
                       mesh = mesh_summer,
                       spatial = "on",
                       spatiotemporal = "ar1",
                       data = summer,
                       time = "month_name_f")
toc() #1.3sec

#when seed set at no polys: no warnings
#sanity(fit2d_summer)
#Red Xs: ln_phi
#sanity(fit2d_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#none
AIC(fit2d_summer)
#272759.7
summary(fit2d_summer)
#AR1 correlation (rho): 0.97




#2e
tic()
fit2e_summer <- sdmTMB(tottraps ~ 0,
                       family = tweedie(),
                       mesh = mesh_summer,
                       spatial = "on",
                       spatiotemporal = "ar1",
                       data = summer,
                       time = "month_of_seasonf")
toc() #1.5
min

#when seed set at no polys: no warnings
#sanity(fit2e_summer)
#Red Xs: None
#sanity(fit2e_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#none 
AIC(fit2e_summer)
#272614.4
summary(fit2e_summer)
#AR1 correlation (rho): 0.97

#plots <- plot_diag(fit2e_summer)
###PLOTS LOOKED WEIRD BECAUSE SMAs OPEN, AND MAY IS 1 MONTH OF SEASON FOR THEM



#2f
tic()
fit2f_summer <- sdmTMB(tottraps ~ 0,
                       family = tweedie(),
                       mesh = mesh_summer,
                       spatial = "on",
                       spatiotemporal = "ar1",
                       data = summer,
                       time = "half_month_of_seasonf")
toc() #6min

#when seed set at no polys: no warnings
#sanity(fit2f_summer)
#Red Xs: none
#sanity(fit2f_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#none
AIC(fit2f_summer)
#272146.1
summary(fit2f_summer)
#AR1 correlation (rho): 0.98

#plots <- plot_diag(fit2f_summer)



#-------------------------------------

#Just as a test, we can see if changing month to a smooth improves the fit

tic()
fit3_summer <- sdmTMB(tottraps ~ 0 + 
                          s(month_n, k = 4) + # <- new,
                          season +
                          #month_name_f + 
                          OR_WA_waters +
                          WA_pot_reduction +
                        z_SST_avg +
                        z_wind_avg +
                        z_depth_point_mean +
                        z_depth_point_sd +
                        z_faults_km +
                        z_dist_canyon_km +
                        z_weighted_dist +
                        z_weighted_fuel_pricegal +
                        z_weighted_crab_ppp +
                        z_bottom_O2_avg +
                        z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh_summer,
                        spatial = "on",
                        spatiotemporal = "iid",
                        data = summer,
                        time = "yearf")
toc() #4.8min 
#with summmer model if try to let k be automatically selected, get error:
#Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : A term has fewer unique covariate combinations than specified maximum degrees of freedom

#when seed set and no polynomials: The model may not have converged. Maximum final gradient: 0.0160984928631862
#sanity(fit3_summer)
#Red Xs: b_js only
#sanity(fit3_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js only
AIC(fit3_summer)
#267727.9




tic()
fit3b_summer <- sdmTMB(tottraps ~ 0 + 
                        s(month_n, k = 4) + # <- new,
                        season +
                        #month_name_f + 
                        OR_WA_waters +
                        WA_pot_reduction +
                        z_SST_avg +
                        z_wind_avg +
                        z_depth_point_mean +
                        z_depth_point_sd +
                        z_faults_km +
                        z_dist_canyon_km +
                        z_weighted_dist +
                        z_weighted_fuel_pricegal +
                        z_weighted_crab_ppp +
                        z_bottom_O2_avg +
                        z_dist_to_closed_km,
                      family = tweedie(),
                      mesh = mesh_summer,
                      spatial = "on",
                      spatiotemporal = "ar1", #ar1
                      data = summer,
                      time = "yearf")
toc() #10min 

#when seed set and no polynomials: The model may not have converged. Maximum final gradient: 0.116710570962219
#sanity(fit3b_summer)
#Red Xs: b_js, bs, thetaf, ln_phi
#sanity(fit3b_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, thetaf, ln_phi
AIC(fit3b_summer)
#267651.9
summary(fit3b_summer)
#Spatiotemporal AR1 correlation (rho): 0.47


#-------------------------------------

# Just as a test, we can see if changing year/season to a smooth improves the fit

tic()
fit4_summer <- sdmTMB(tottraps ~ 0 + 
                        month_name_f + 
                          s(yearn, k = 10) + # <- new, let k be automatically selected
                          #season +
                          OR_WA_waters +
                          WA_pot_reduction +
                        z_SST_avg +
                        z_wind_avg +
                        z_depth_point_mean +
                        z_depth_point_sd +
                        z_faults_km +
                        z_dist_canyon_km +
                        z_weighted_dist +
                        z_weighted_fuel_pricegal +
                        z_weighted_crab_ppp +
                        z_bottom_O2_avg +
                        z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh_summer,
                        spatial = "on",
                        spatiotemporal = "iid",
                        data = summer,
                        time = "yearf")
toc() #5.9min 
#this time fine to let k be automatically selected
#seed set and no polynomials: The model may not have converged. Maximum final gradient: 0.0363551898475372
#sanity(fit4_summer)
#Red Xs: b_js, ln_tau, ln_smooth_sigma
#sanity(fit4_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, ln_smooth_sigma
AIC(fit4_summer)
#267710.2

#-------------------------------------
#change the IID spatiotemporal fields in model 2 to “AR1” to test the autoregressive structure
# Is there support for AR1 spatiotemporal fields?
# Using the best model (fit2)

tic()
fit5_summer <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_name_f + 
                          OR_WA_waters +
                          WA_pot_reduction +
                        z_SST_avg +
                        z_wind_avg +
                        z_depth_point_mean +
                        z_depth_point_sd +
                        z_faults_km +
                        z_dist_canyon_km +
                        z_weighted_dist +
                        z_weighted_fuel_pricegal +
                        z_weighted_crab_ppp +
                        z_bottom_O2_avg +
                        z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh_summer,
                        spatial = "on",
                        spatiotemporal = "ar1", # <- new
                        data = summer,
                        time = "yearf")
toc() #14min

#when seed set and no polynomials: The model may not have converged. Maximum final gradient: 0.144498983789155
#sanity(fit5_summer)
#red Xs: b_js, ln_tau, ln_phi, ar1_phi
#sanity(fit5_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, ln_tau, ln_phi
AIC(fit5_summer)
#265861
#summary(fit5_summer)
#Spatiotemporal AR1 correlation (rho): 0.47


#-------------------------------------

#Adding spatial and spatiotemporal fields (months)

# switch indexing of spatiotemporal fields to “month_name” and again can try the spatiotemporal fields as IID or AR1.

tic()
fit6_summer <- update(fit2_summer,
                        time = "month_n")
toc()  #4.5min

#when seed est and no polynomial terms: The model may not have converged. Maximum final gradient: 0.0685854251422118
#sanity(fit6_summer)
#Red Xs: b_js, thetaf, ln_phi
#sanity(fit6_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, thetaf, ln_phi
AIC(fit6_summer)
#271878.2






tic()
fit7_summer <- update(fit2_summer,
                        time = "month_n",
                        spatiotemporal = "ar1")
toc()  #7.5min

#when seed set and no polynomials: The model may not have converged. Maximum final gradient: 0.0304410014337826
#sanity(fit7_summer)
#Some red Xs (b_js, ln_tau, ar1_phi)
#sanity(fit7_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still red Xs: b_js, ln_tau, ar1_phi
AIC(fit7_summer)
#271799.8
#summary(fit7_summer) 
#Spatiotemporal AR1 correlation (rho): 0.97

#-------------------------------------

#index by half_month step
summer$half_month_n <- 1
summer$half_month_n[which(summer$half_month=="May_2")] = 2
summer$half_month_n[which(summer$half_month=="June_1")] = 3
summer$half_month_n[which(summer$half_month=="June_2")] = 4
summer$half_month_n[which(summer$half_month=="July_1")] = 5
summer$half_month_n[which(summer$half_month=="July_2")] = 6
summer$half_month_n[which(summer$half_month=="August_1")] = 7
summer$half_month_n[which(summer$half_month=="August_2")] = 8
summer$half_month_n[which(summer$half_month=="September_1")] = 9


tic()
fit8_summer <- update(fit2_summer,
                        time = "half_month_n") #as numeric
toc()  #5.4min

#when seed set and no polynomials: The model may not have converged. Maximum final gradient: 0.0465512301905608
#sanity(fit8_summer)
#Red Xs: b_js, ln_tau, ln_kappa, ln_phi
#sanity(fit8_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, ln_tau, ln_phi
AIC(fit8_summer)
#271748.7


#-------------------------------------














#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------

                                  ##WINTER DATA

#Initial model
#fit0 = covariates only

mesh_winter <- make_mesh(winter, xy_cols = c("X","Y"), cutoff = 10)
mesh_winter$mesh$n

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
                      mesh = mesh_winter,
                      spatial = "off",
                      spatiotemporal = "off",
                      data = winter,
                      time = "yearf")
toc() #2.2min

# when seed set and no polynomials: The model may not have converged. Maximum final gradient: 0.01975976529072
#sanity(fit0_winter)
#Some red Xs in sanity check (b_js, thetaf, ln_phi)
#sanity(fit0_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still X for thetaf, ln_phi
AIC(fit0_winter)
#774067.9

#-------------------------------------------------------------------------------------------------

#Adding spatial and spatiotemporal fields (seasons)

tic()
fit1_winter <- sdmTMB(tottraps ~ 0 + 
                        season +
                        month_name_f + 
                        OR_WA_waters +
                        #WA_pot_reduction +    #not relevant in winter
                        z_SST_avg +
                        z_wind_avg +
                        z_depth_point_mean +
                        z_depth_point_sd +
                        z_faults_km +
                        z_dist_canyon_km +
                        z_weighted_dist +
                        z_weighted_fuel_pricegal +
                        z_weighted_crab_ppp +
                        z_bottom_O2_avg +
                        z_dist_to_closed_km,
                      family = tweedie(),
                      mesh = mesh_winter,
                      spatial = "on",
                      spatiotemporal = "off",
                      data = winter,
                      time = "yearf")
toc() #6.7min

#when seed set and no polynomials: The model may not have converged. Maximum final gradient: 0.113602006008465
#sanity(fit1_winter)
#Couple red Xs in sanity check (b_j, thetaf, ln_phi)
#sanity(fit1_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still X for b_j, thetaf, ln_phi
AIC(fit1_winter)
#741447



tic()
fit2_winter <- sdmTMB(tottraps ~ 0 + 
                        season +
                        month_name_f + 
                        OR_WA_waters +
                        #WA_pot_reduction +    #not relevant in winter
                        z_SST_avg +
                        z_wind_avg +
                        z_depth_point_mean +
                        z_depth_point_sd +
                        z_faults_km +
                        z_dist_canyon_km +
                        z_weighted_dist +
                        z_weighted_fuel_pricegal +
                        z_weighted_crab_ppp +
                        z_bottom_O2_avg +
                        z_dist_to_closed_km,
                      family = tweedie(),
                      mesh = mesh_winter,
                      spatial = "on",
                      spatiotemporal = "iid",
                      data = winter,
                      time = "yearf")
toc() #8min

#when set seed and no polynomials: The model may not have converged. Maximum final gradient: 0.0885550831947178
#sanity(fit2_winter)
#Some red Xs in sanity check (b_js, ln_tau, ln_kappa)
#sanity(fit2_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still Xs for b_js, ln_tau, ln_kappa
AIC(fit2_winter)
#731319.1
#-------------------------------------------------------------------------------------------------

#model with only spatial and spatiotemporal fields (seasons), no covariates

tic()
fit1b_winter <- sdmTMB(tottraps ~ 0,
                       family = tweedie(),
                       mesh = mesh_winter,
                       spatial = "on",
                       spatiotemporal = "off",
                       data = winter,
                       time = "yearf")
toc() #0.5min

#when seed set and no polynomials: no warnings
#sanity(fit1b_winter)
#No red Xs in sanity check
#sanity(fit1b_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
AIC(fit1b_winter)
#744916.7



tic()
fit2b_winter <- sdmTMB(tottraps ~ 0,
                       family = tweedie(),
                       mesh = mesh_winter,
                       spatial = "on",
                       spatiotemporal = "iid",
                       data = winter,
                       time = "yearf")
toc() #1.1min

#when seed set and no polynomials: no warnings
#sanity(fit2b_winter)
#No red Xs in sanity check
#sanity(fit2b_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
AIC(fit2b_winter)
#734885.2


#2c
tic()
fit2c_winter <- sdmTMB(tottraps ~ 0,
                         family = tweedie(),
                         mesh = mesh_winter,
                         spatial = "on",
                         spatiotemporal = "ar1",
                         data = winter,
                         time = "yearf")
toc() #4.4min

#when seed set at no polys: no warnings
#sanity(fit2c_winter)
#Red Xs: None
#sanity(fit2c_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
AIC(fit2c_winter)
#734728.5
summary(fit2c_winter)
#AR1 correlation (rho): 0.51

#plots <- plot_diag(fit2c_winter)


#2d
tic()
fit2d_winter <- sdmTMB(tottraps ~ 0,
                       family = tweedie(),
                       mesh = mesh_winter,
                       spatial = "on",
                       spatiotemporal = "ar1",
                       data = winter,
                       time = "month_name_f")
toc() #84sec

#when seed set at no polys: no warnings
#sanity(fit2d_winter)
#Red Xs: thetaf, ln_phi
#sanity(fit2d_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#No red Xs
AIC(fit2d_winter)
#738692.8
summary(fit2d_winter)
#AR1 correlation (rho): 0.99

#plots <- plot_diag(fit2d_winter)


#2e
tic()
fit2e_winter <- sdmTMB(tottraps ~ 0,
                       family = tweedie(),
                       mesh = mesh_winter,
                       spatial = "on",
                       spatiotemporal = "ar1",
                       data = winter,
                       time = "month_of_seasonf")
toc() #2.4min

#when seed set at no polys: no warnings
#sanity(fit2e_winter)
#Red Xs: thetaf, ln_phi
#sanity(fit2e_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still ln_phi
AIC(fit2e_winter)
#738350.2
summary(fit2e_winter)
#AR1 correlation (rho): 0.99

#plots <- plot_diag(fit2e_winter)



#2f
tic()
fit2f_winter <- sdmTMB(tottraps ~ 0,
                       family = tweedie(),
                       mesh = mesh_winter,
                       spatial = "on",
                       spatiotemporal = "ar1",
                       data = winter,
                       time = "half_month_of_seasonf")
toc() #2.8min

#when seed set at no polys: no warnings
#sanity(fit2f_winter)
#Red Xs: thetaf, ln_phi
#sanity(fit2f_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#No red Xs
AIC(fit2f_winter)
#736912.7
summary(fit2f_winter)
#AR1 correlation (rho): 1.0

#plots <- plot_diag(fit2f_winter)
#--------------------------------------

#Just as a test, we can see if changing month to a smooth improves the fit

tic()
fit3_winter <- sdmTMB(tottraps ~ 0 + 
                        s(month_n, k = 4) + # <- new,
                        season +
                        #month_name_f + 
                        OR_WA_waters +
                        #WA_pot_reduction +   #not relevant in winter
                        z_SST_avg +
                        z_wind_avg +
                        z_depth_point_mean +
                        z_depth_point_sd +
                        z_faults_km +
                        z_dist_canyon_km +
                        z_weighted_dist +
                        z_weighted_fuel_pricegal +
                        z_weighted_crab_ppp +
                        z_bottom_O2_avg +
                        z_dist_to_closed_km,
                      family = tweedie(),
                      mesh = mesh_winter,
                      spatial = "on",
                      spatiotemporal = "iid",
                      data = winter,
                      time = "yearf")
toc() #7min 

#when seed set and no polynomials: The model may not have converged. Maximum final gradient: 0.154623119063547
#sanity(fit3_winter)
#X for b_js, ln_tau, thetaf, ln_phi
#sanity(fit3_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still X for b_js, ln_tau, thetaf, ln_phi
AIC(fit3_winter)
#731335.5

#--------------------------------------

# Just as a test, we can see if changing year/season to a smooth improves the fit

tic()
fit4_winter <- sdmTMB(tottraps ~ 0 + 
                        month_name_f + 
                        s(yearn, k = 10) + # <- new, let k be automatically selected // , k = 10
                        #season +
                        OR_WA_waters +
                        #WA_pot_reduction +     #not relevant in winter
                        z_SST_avg +
                        z_wind_avg +
                        z_depth_point_mean +
                        z_depth_point_sd +
                        z_faults_km +
                        z_dist_canyon_km +
                        z_weighted_dist +
                        z_weighted_fuel_pricegal +
                        z_weighted_crab_ppp +
                        z_bottom_O2_avg +
                        z_dist_to_closed_km,
                      family = tweedie(),
                      mesh = mesh_winter,
                      spatial = "on",
                      spatiotemporal = "iid",
                      data = winter,
                      time = "yearf")
toc() #7.8min 
#this time fine to let k be automatically selected. doesn't matter if specify k=10
#when seed set and no polynomials: The model may not have converged: non-positive-definite Hessian matrix
#sanity(fit4_winter)
#lots of Xs
#sanity(fit4_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still lots of Xs
AIC(fit4_winter)
#731308.3


#--------------------------------------

#change the IID spatiotemporal fields in model 2 to “AR1” to test the autoregressive structure
# Is there support for AR1 spatiotemporal fields?
# Using the best model (fit2)

tic()
fit5_winter <- sdmTMB(tottraps ~ 0 + 
                        season +
                        month_name_f + 
                        OR_WA_waters +
                        #WA_pot_reduction +    #not relevant in winter
                        z_SST_avg +
                        z_wind_avg +
                        z_depth_point_mean +
                        z_depth_point_sd +
                        z_faults_km +
                        z_dist_canyon_km +
                        z_weighted_dist +
                        z_weighted_fuel_pricegal +
                        z_weighted_crab_ppp +
                        z_bottom_O2_avg +
                        z_dist_to_closed_km,
                      family = tweedie(),
                      mesh = mesh_winter,
                      spatial = "on",
                      spatiotemporal = "ar1", # <- new
                      data = winter,
                      time = "yearf")
toc() #37min

#when seed set and no polynomials: The model may not have converged. Maximum final gradient: 0.0397814681147731
#sanity(fit5_winter)
#some red Xs b_js, ln_tau, thetaf, ar1_phi
#sanity(fit5_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still Xs for b_js, ln_tau, thetaf
AIC(fit5_winter)
#731164.3
#summary(fit5_winter)
#Spatiotemporal AR1 correlation (rho): 0.51


#--------------------------------------

#Adding spatial and spatiotemporal fields (months)

# switch indexing of spatiotemporal fields to “month_name” and again can try the spatiotemporal fields as IID or AR1.

tic()
fit6_winter <- update(fit2_winter,
                      time = "month_n")
toc()  #7.3min

#when seed set and no polynomials: The model may not have converged. Maximum final gradient: 0.211205146064904
#sanity(fit6_winter)
#Some red Xs: b_js, ln_tau, thetaf, ln_phi
#sanity(fit6_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, ln_tau, thetaf, ln_phi
AIC(fit6_winter)
#737417.6


###as fit2 had issues, but fit2b didn't, try that one as well
tic()
fit6b_winter <- update(fit2b_winter,
                      time = "month_n")
toc()  #0.8min
#when set seed and no polynomials: no warnings
#sanity(fit6b_winter)
#only 1 X: thetaf
#sanity(fit6b_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still X for thetaf
AIC(fit6b_winter)
#738918.1
###but it's not an improvement on fit6



tic()
fit7_winter <- update(fit2_winter,
                      time = "month_n",
                      spatiotemporal = "ar1")
toc()  #14min

#when seed set and no polynomials: The model may not have converged. Maximum final gradient: 0.255937961202847
#sanity(fit7_winter)
#some red Xs (b_js, ln_tau, ln_kappa)
#sanity(fit7_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, ln_kappa
AIC(fit7_winter)
#737194.7
#summary(fit7_winter) 
#Spatiotemporal AR1 correlation (rho): 0.99


#--------------------------------------

#index by half_month step
winter$half_month_n <- 1
winter$half_month_n[which(winter$half_month=="December_2")] = 2
winter$half_month_n[which(winter$half_month=="January_1")] = 3
winter$half_month_n[which(winter$half_month=="January_2")] = 4
winter$half_month_n[which(winter$half_month=="February_1")] = 5
winter$half_month_n[which(winter$half_month=="February_2")] = 6
winter$half_month_n[which(winter$half_month=="March_1")] = 7
winter$half_month_n[which(winter$half_month=="March_2")] = 8
winter$half_month_n[which(winter$half_month=="April_1")] = 9
winter$half_month_n[which(winter$half_month=="April_2")] = 10


tic()
fit8_winter <- update(fit2_winter,
                      time = "half_month_n") #as numeric
toc()  #8min

#when seed set and no poynomials: The model may not have converged. Maximum final gradient: 0.270807483364804
#sanity(fit8_winter)
#Red Xs: b_js, ln_tau, ln_kappa
#sanity(fit8_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#only b_j on second run
AIC(fit8_winter)
#737296



#--------------------------------------

tic()
fit9_winter <- sdmTMB(tottraps ~ 0 + 
                          s(half_month_n) + # <- new, let k be automatically selected
                          season +
                          #month_name_f + 
                          OR_WA_waters +
                          #WA_pot_reduction +
                        z_SST_avg +
                        z_wind_avg +
                        z_depth_point_mean +
                        z_depth_point_sd +
                        z_faults_km +
                        z_dist_canyon_km +
                        z_weighted_dist +
                        z_weighted_fuel_pricegal +
                        z_weighted_crab_ppp +
                        z_bottom_O2_avg +
                        z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh_winter,
                        spatial = "on",
                        spatiotemporal = "iid",
                        data = winter,
                        time = "yearf")
toc() #8min 

#when seed set and no polynomials: The model may not have converged. Maximum final gradient: 0.0425400878098907
#sanity(fit9_winter)
#Red Xs: b_js, ln_tau, ln_kappa, ln_phi
#sanity(fit9_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, ln_tau, ln_phi
AIC(fit9_winter)
#731242 


#--------------------------------------

#add chosen polynomial terms

tic()
fit10a_winter <- sdmTMB(tottraps ~ 0 + 
                        season +
                        month_name_f + 
                        OR_WA_waters +
                        #WA_pot_reduction +    #not relevant in winter
                        z_SST_avg +
                        z_wind_avg +
                        poly(z_depth_point_mean,2) +
                        z_depth_point_sd +
                        z_faults_km +
                        z_dist_canyon_km +
                        z_weighted_dist +
                        z_weighted_fuel_pricegal +
                        z_weighted_crab_ppp +
                        z_bottom_O2_avg +
                        z_dist_to_closed_km,
                      family = tweedie(),
                      mesh = mesh_winter,
                      spatial = "on",
                      spatiotemporal = "ar1", # <- new
                      data = winter,
                      time = "yearf")
toc() #28min

#when seed set and depth is poly: The model may not have converged. Maximum final gradient: 0.0304755794721263
#sanity(fit10a_winter)
#some red Xs: b_js, ln_tau
#sanity(fit10a_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still Xs for b_js only
AIC(fit10a_winter)
#728925.9
#summary(fit10a_winter)
#Spatiotemporal AR1 correlation (rho): 0.48



tic()
fit10b_winter <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_name_f + 
                          OR_WA_waters +
                          #WA_pot_reduction +    #not relevant in winter
                          z_SST_avg +
                          z_wind_avg +
                          z_depth_point_mean +
                          z_depth_point_sd +
                          z_faults_km +
                          z_dist_canyon_km +
                          z_weighted_dist +
                          z_weighted_fuel_pricegal +
                          z_weighted_crab_ppp +
                          poly(z_bottom_O2_avg,2) +
                          z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh_winter,
                        spatial = "on",
                        spatiotemporal = "ar1", 
                        data = winter,
                        time = "yearf")
toc() #17min

#when seed set and bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.261323822906672
#sanity(fit10b_winter)
#some red Xs b_js, ln_tau, thetaf, ln_phi
#sanity(fit10b_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still Xs for b_js, ln_tau, thetaf, ln_phi
AIC(fit10b_winter)
#731162.1
#summary(fit10b_winter)
#Spatiotemporal AR1 correlation (rho): 0.51


tic()
fit10c_winter <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_name_f + 
                          OR_WA_waters +
                          #WA_pot_reduction +    #not relevant in winter
                          z_SST_avg +
                          z_wind_avg +
                          poly(z_depth_point_mean,2) +
                          z_depth_point_sd +
                          z_faults_km +
                          z_dist_canyon_km +
                          z_weighted_dist +
                          z_weighted_fuel_pricegal +
                          z_weighted_crab_ppp +
                          poly(z_bottom_O2_avg,2) +
                          z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh_winter,
                        spatial = "on",
                        spatiotemporal = "ar1", 
                        data = winter,
                        time = "yearf")
toc() #28min

#when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.0342582126387878
#on second run no warnings
#sanity(fit10c_winter)
#some red Xs b_j only
#sanity(fit10c_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still Xs for b_j only
AIC(fit10c_winter)
#728923.4
#summary(fit10c_winter)
#Spatiotemporal AR1 correlation (rho): 0.48

#plots <- plot_diag(fit10c_winter)

#some plots

plot_log = function(object, term) {
  g <- ggeffect(object, term, back.transform = FALSE)
  g$conf.low <- log(g$conf.low)
  g$conf.high <- log(g$conf.high)
  g$predicted <- log(g$predicted)
  plot(g)
}


p1 <- plot_log(fit10c_winter, "season [all]")
p2 <- plot_log(fit10c_winter, "month_name_f [all]")
p3 <- plot_log(fit10c_winter, "OR_WA_waters [all]")
p4 <- plot_log(fit10c_winter, "z_SST_avg [all]")
p5 <- plot_log(fit10c_winter, "z_wind_avg [all]")
p6 <- plot_log(fit10c_winter, "z_depth_point_mean [all]")
p7 <- plot_log(fit10c_winter, "z_depth_point_sd [all]")
p8 <- plot_log(fit10c_winter, "z_faults_km [all]")
p9 <- plot_log(fit10c_winter, "z_dist_canyon_km [all]")
p10 <- plot_log(fit10c_winter, "z_weighted_dist [all]")
p11 <- plot_log(fit10c_winter, "z_weighted_fuel_pricegal [all]")
p12 <- plot_log(fit10c_winter, "z_weighted_crab_ppp [all]")
p13 <- plot_log(fit10c_winter, "z_bottom_O2_avg [all]")
p14 <- plot_log(fit10c_winter, "z_dist_to_closed_km [all]")

gridExtra::grid.arrange(p1,p2,p3,ncol=2)

gridExtra::grid.arrange(p4,p5,p6,p7,ncol=2)
                        
gridExtra::grid.arrange(p8,p9,p13,p14,ncol=2)

gridExtra::grid.arrange(p10,p11,p12,ncol=2)



#------------

tic()
fit10d_winter <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_name_f + 
                          OR_WA_waters +
                          #WA_pot_reduction +    #not relevant in winter
                          z_SST_avg +
                          z_wind_avg +
                          poly(z_depth_point_mean,2) +
                          z_depth_point_sd +
                          z_faults_km +
                          z_dist_canyon_km +
                          z_weighted_dist +
                          z_weighted_fuel_pricegal +
                          z_weighted_crab_ppp +
                          poly(z_bottom_O2_avg,2) +
                          z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh_winter,
                        spatial = "on",
                        spatiotemporal = "iid", 
                        data = winter,
                        time = "yearf")
toc() #8.4min

#when seed set and depth is poly: The model may not have converged. Maximum final gradient: 0.0102656707984882
#sanity(fit10d_winter)
#some red Xs b_js, ln_phi
#sanity(fit10d_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still Xs for b_j, ln_phi
AIC(fit10d_winter)
#729059.7

#plots <- plot_diag(fit10d_winter)







#---------------------------------------



tic()
fit11a_winter <- sdmTMB(tottraps ~ 0 + 
                          season +
                          #month_name_f + 
                          OR_WA_waters +
                          #WA_pot_reduction +    #not relevant in winter
                          z_SST_avg +
                          z_wind_avg +
                          poly(z_depth_point_mean,2) +
                          z_depth_point_sd +
                          z_faults_km +
                          z_dist_canyon_km +
                          z_weighted_dist +
                          z_weighted_fuel_pricegal +
                          z_weighted_crab_ppp +
                          poly(z_bottom_O2_avg,2) +
                          z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh_winter,
                        spatial = "on",
                        spatiotemporal = "ar1", 
                        data = winter,
                        time = "month_name_f")
toc() #20min

#when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.0435730378857211
#sanity(fit11a_winter)
#some red Xs: b_js, thetaf, ln_phi, ar1_phi
#sanity(fit11a_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still Xs for b_js, thetaf, ln_phi
AIC(fit11a_winter)
#735413.4
#summary(fit11a_winter)
#Spatiotemporal AR1 correlation (rho): 0.98




tic()
fit11b_winter <- sdmTMB(tottraps ~ 0 + 
                          season +
                          #month_name_f + 
                          OR_WA_waters +
                          #WA_pot_reduction +    #not relevant in winter
                          z_SST_avg +
                          z_wind_avg +
                          poly(z_depth_point_mean,2) +
                          z_depth_point_sd +
                          z_faults_km +
                          z_dist_canyon_km +
                          z_weighted_dist +
                          z_weighted_fuel_pricegal +
                          z_weighted_crab_ppp +
                          poly(z_bottom_O2_avg,2) +
                          z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh_winter,
                        spatial = "on",
                        spatiotemporal = "ar1", 
                        data = winter,
                        time = "month_of_seasonf")
toc() #16min

#when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.0225393166430363
#sanity(fit11b_winter)
#some red Xs: b_js, ln_tau, ln_kappa
#sanity(fit11b_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still Xs for b_js, ln_tau_O
AIC(fit11b_winter)
#735088.6
#summary(fit11b_winter)
#Spatiotemporal AR1 correlation (rho): 0.98




tic()
fit11c_winter <- sdmTMB(tottraps ~ 0 + 
                          season +
                          #month_name_f + 
                          OR_WA_waters +
                          #WA_pot_reduction +    #not relevant in winter
                          z_SST_avg +
                          z_wind_avg +
                          poly(z_depth_point_mean,2) +
                          z_depth_point_sd +
                          z_faults_km +
                          z_dist_canyon_km +
                          z_weighted_dist +
                          z_weighted_fuel_pricegal +
                          z_weighted_crab_ppp +
                          poly(z_bottom_O2_avg,2) +
                          z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh_winter,
                        spatial = "on",
                        spatiotemporal = "ar1", 
                        data = winter,
                        time = "half_month_of_seasonf")
toc() #22min

#when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.0416619424295455
#sanity(fit11c_winter)
#some red Xs: b_js, thetaf, ln_phi
#sanity(fit11c_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still Xs for b_js, thetaf, ln_phi
AIC(fit11c_winter)
#733429.8
#summary(fit11c_winter)
#Spatiotemporal AR1 correlation (rho): 0.99



#---------------------------------------





tic()
fit12a_winter <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_name_f + 
                          OR_WA_waters +
                          #WA_pot_reduction +    #not relevant in winter
                          z_SST_avg +
                          z_wind_avg +
                          poly(z_depth_point_mean,2) +
                          z_depth_point_sd +
                          z_faults_km +
                          z_dist_canyon_km +
                          z_weighted_dist +
                          z_weighted_fuel_pricegal * z_weighted_crab_ppp +  #interaction
                          #z_weighted_crab_ppp +
                          poly(z_bottom_O2_avg,2) +
                          z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh_winter,
                        spatial = "on",
                        spatiotemporal = "ar1", 
                        data = winter,
                        time = "yearf")
toc() #27min

#when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.0284631870580405
#sanity(fit12a_winter)
#some red Xs b_js, ln_tau, ar1_phi
#sanity(fit12a_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still Xs for b_js only
AIC(fit12a_winter)
#728925.3
#summary(fit12a_winter)
#Spatiotemporal AR1 correlation (rho): 0.48

#plots <- plot_diag(fit12a_winter)





tic()
fit12b_winter <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_name_f + 
                          OR_WA_waters +
                          #WA_pot_reduction +    #not relevant in winter
                          z_SST_avg +
                          #z_wind_avg +
                          poly(z_depth_point_mean,2) +
                          z_depth_point_sd +
                          z_faults_km +
                          z_dist_canyon_km +
                          z_weighted_dist +
                          z_weighted_fuel_pricegal * z_wind_avg + #interaction 
                          z_weighted_crab_ppp +
                          poly(z_bottom_O2_avg,2) +
                          z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh_winter,
                        spatial = "on",
                        spatiotemporal = "ar1", 
                        data = winter,
                        time = "yearf")
toc() #22min

#when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.0342187361546555
#sanity(fit12b_winter)
#some red Xs b_js, ln_tau, ar1_phi
#sanity(fit12b_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still Xs for b_js, ln_tau
AIC(fit12b_winter)
#728907.2
#summary(fit12b_winter)
#Spatiotemporal AR1 correlation (rho): 0.48

#plots <- plot_diag(fit12b_winter)






tic()
fit12c_winter <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_name_f + 
                          #OR_WA_waters +
                          #WA_pot_reduction +    #not relevant in winter
                          z_SST_avg +
                          z_wind_avg +
                          poly(z_depth_point_mean,2) +
                          z_depth_point_sd +
                          z_faults_km +
                          z_dist_canyon_km +
                          z_weighted_dist +
                          z_weighted_fuel_pricegal +
                          z_weighted_crab_ppp +
                          poly(z_bottom_O2_avg,2) +
                          z_dist_to_closed_km * OR_WA_waters,
                        family = tweedie(),
                        mesh = mesh_winter,
                        spatial = "on",
                        spatiotemporal = "ar1", 
                        data = winter,
                        time = "yearf")
toc() #15min

#when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.0556398593034331
#sanity(fit12c_winter)
#some red Xs b_js, ln_tau, thetaf, ln_phi
#sanity(fit12c_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still Xs for b_js, ln_tau, thetaf, ln_phi
AIC(fit12c_winter)
#728911.9
#summary(fit12c_winter)
#Spatiotemporal AR1 correlation (rho): 0.48




tic()
fit12d_winter <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_name_f + 
                          OR_WA_waters +
                          #WA_pot_reduction +    #not relevant in winter
                          z_SST_avg +
                          z_wind_avg +
                          poly(z_depth_point_mean,2) +
                          z_depth_point_sd +
                          z_faults_km +
                          z_dist_canyon_km +
                          #z_weighted_dist +
                          z_weighted_fuel_pricegal * z_weighted_dist + #interaction 
                          z_weighted_crab_ppp +
                          poly(z_bottom_O2_avg,2) +
                          z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh_winter,
                        spatial = "on",
                        spatiotemporal = "ar1", 
                        data = winter,
                        time = "yearf")
toc() #31min

#when seed set and depth & bottom O2 is poly: no warnings
#sanity(fit12d_winter)
#some red Xs b_js, thetaf, ln_phi
#sanity(fit12d_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still Xs for b_j, ln_phi
AIC(fit12d_winter)
#728923.7
#summary(fit12d_winter)
#Spatiotemporal AR1 correlation (rho): 0.48

#plots <- plot_diag(fit12d_winter)





tic()
fit12e_winter <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_name_f + 
                          OR_WA_waters +
                          #WA_pot_reduction +    #not relevant in winter
                          z_SST_avg * z_wind_avg + #interaction
                          #z_wind_avg +
                          poly(z_depth_point_mean,2) +
                          z_depth_point_sd +
                          z_faults_km +
                          z_dist_canyon_km +
                          z_weighted_dist +
                          z_weighted_fuel_pricegal  +  
                          z_weighted_crab_ppp +
                          poly(z_bottom_O2_avg,2) +
                          z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh_winter,
                        spatial = "on",
                        spatiotemporal = "ar1", 
                        data = winter,
                        time = "yearf")
toc() #29min

#when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.0122815415720083
#sanity(fit12e_winter)
#some red Xs b_js, ln_tau
#sanity(fit12e_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still Xs for b_js only
AIC(fit12e_winter)
#728915.8
#summary(fit12e_winter)
#Spatiotemporal AR1 correlation (rho): 0.48

#plots <- plot_diag(fit12e_winter)
#---------------------------------






#---------------------------------






#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------





#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------

#test some basic lm -- try this on the original z-scored columns, not the 2*sd versions

#summer

tic()
mod0_summer <- lm(tottraps ~ yearn + 
                    month_n +
                    OR_WA_waters +
                    WA_pot_reduction +
                    z_SST_avg  +
                    z_wind_avg +
                    z_depth_point_mean +
                    z_depth_point_sd +
                    z_faults_km +
                    z_dist_canyon_km +
                    z_weighted_dist +
                    z_weighted_fuel_pricegal +
                    z_weighted_crab_ppp +
                    z_bottom_O2_avg  +
                    z_dist_to_closed_km, 
                  data=summer)
toc() 

summary(mod0_summer)

##same result as with the 2*sd version - only non-significant variables are year, dist to canyon and dist_to_closed...



# WINTER

tic()
mod0_winter <- lm(tottraps ~ yearn + 
                    month_n +
                    OR_WA_waters +
                    #WA_pot_reduction +
                    z_SST_avg  +
                    z_wind_avg +
                    z_depth_point_mean +
                    z_depth_point_sd +
                    z_faults_km +
                    z_dist_canyon_km +
                    z_weighted_dist +
                    z_weighted_fuel_pricegal +
                    z_weighted_crab_ppp +
                    z_bottom_O2_avg  +
                    z_dist_to_closed_km, 
                  data=winter)
toc() 

summary(mod0_winter)
##same result as with the 2*sd version - all predictors are significant



#ALL DATA

tic()
mod0_all_data <- lm(tottraps ~ yearn + 
                      month_n +
                      OR_WA_waters +
                      WA_pot_reduction +
                      z_SST_avg  +
                      z_wind_avg +
                      z_depth_point_mean +
                      z_depth_point_sd +
                      z_faults_km +
                      z_dist_canyon_km +
                      z_weighted_dist +
                      z_weighted_fuel_pricegal +
                      z_weighted_crab_ppp +
                      z_bottom_O2_avg  +
                      z_dist_to_closed_km, 
                    data=d)
toc() 

summary(mod0_all_data)
##same result as with the 2*sd version - all predictors are significant




