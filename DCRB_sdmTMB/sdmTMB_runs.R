#sdmTMB runs


#here are some of the sort of questions we'd like to be able to answer:

#Which predictors have the biggest effect on crab fishing effort distribution in space? 
#Are static variables better predictors than dynamic variables? Are environmental variables better predictors than economic variables?
  
#How static/variable is the overall fleet footprint. How predictable is footprint from year to year?
  
#What parts of the study area are most variable or consistent between time steps/years?
#Which months are most variable or most consistent?

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






tic()
fit10d_all_data <- sdmTMB(tottraps ~ 0 + 
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
                          spatiotemporal = "ar1",
                          data = d_all_data,
                          time = "month_n")
toc() #19min

#The model may not have converged. Maximum final gradient: 0.143095847409452
#sanity(fit10d_all_data)
#Red Xs: b_js, thetaf, ln_phi, ar1_phi
#sanity(fit10d_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, thetaf, ln_phi
AIC(fit10d_all_data)
# 1012992
#summary(fit10d_all_data) #rho 0.97




tic()
fit10e_all_data <- sdmTMB(tottraps ~ 0 + 
                            season +
                            month_of_seasonf +
                            #month_name_f + 
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
                          spatiotemporal = "ar1",
                          data = d_all_data,
                          time = "month_n")
toc() #30min

#The model may not have converged. Maximum final gradient: 0.0343650626988037
#sanity(fit10e_all_data)
#Red Xs: b_js, thetaf ---- on a later run "Non-linear minimizer did not converge: do not trust this model!"
#sanity(fit10e_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js only
AIC(fit10e_all_data)
# 1012439
#summary(fit10e_all_data) #rho = 0.97


tic()
fit10ex_all_data <- sdmTMB(tottraps ~ 0 + 
                            season +
                            month_of_seasonf +
                            #month_name_f + 
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

#The model may not have converged. Maximum final gradient: 0.0459241196520672
#sanity(fit10ex_all_data)
#Red Xs: b_js, ln_tau_O, thetaf, ln_phi
#sanity(fit10ex_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, thetaf, ln_phi
AIC(fit10ex_all_data)
# 1013164
#summary(fit10ex_all_data) 




#
tic()
fit10f_all_data <- sdmTMB(tottraps ~ 0 + 
                            season +
                            half_month_of_seasonf +
                            #month_name_f + 
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
                          spatiotemporal = "ar1",
                          data = d_all_data,
                          time = "month_n")
toc() #41min

#The model may not have converged. Maximum final gradient: 0.333289180781437
#sanity(fit10f_all_data)
#Red Xs: b_js, thetaf, ln_phi
#sanity(fit10f_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, thetaf, ln_phi
AIC(fit10f_all_data)
# 1011435
#summary(fit10f_all_data) #rho = 0.97


tic()
fit10fx_all_data <- sdmTMB(tottraps ~ 0 + 
                             season +
                             half_month_of_seasonf +
                             #month_name_f + 
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
toc() #17min

#The model may not have converged. Maximum final gradient: 0.0880313718006308
#sanity(fit10fx_all_data)
#Red Xs: b_js, ln_tau_E, ln_phi
#sanity(fit10fx_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, ln_phi
AIC(fit10fx_all_data)
# 1012161
#summary(fit10fx_all_data)



#
tic()
fit10fy_all_data <- sdmTMB(tottraps ~ 0 + 
                            season +
                            half_month_of_seasonf +
                            #month_name_f + 
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
                          spatiotemporal = "ar1",
                          data = d_all_data,
                          time = "yearf")
toc() #34min

#The model may not have converged. Maximum final gradient: 0.0175571849288332
#sanity(fit10fy_all_data)
#Red Xs: b_js, ln_phi
#sanity(fit10fy_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, ln_phi
AIC(fit10fy_all_data)
# 1014073
#summary(fit10fy_all_data) #rho = 0.52

#-------------------------------------


tic()
fit11a_all_data <- sdmTMB(tottraps ~ 0 + 
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
                        spatiotemporal = "ar1", # <- new
                        data = d_all_data,
                        time = "month_name_f")
toc() #19min

#when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.143095847409452
#sanity(fit11a_all_data)
#red Xs: b_js, thetaf, ln_phi, ar1_phi
#sanity(fit11a_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, thetaf, ln_phi
AIC(fit11a_all_data)
#1012992
#summary(fit11a_all_data)
#Spatiotemporal AR1 correlation (rho): 0.97


tic()
fit11b_all_data <- sdmTMB(tottraps ~ 0 + 
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
                          spatiotemporal = "ar1", # <- new
                          data = d_all_data,
                          time = "month_of_seasonf")
toc() #48min

#when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 8.24611402637197
#sanity(fit11b_all_data)
#red Xs: Non-linear minimizer did not converge: do not trust this model!
#b_js, ar1_phi
#sanity(fit11b_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#still same as above
AIC(fit11b_all_data)
#1012992
#summary(fit11b_all_data)
#Spatiotemporal AR1 correlation (rho): 0.97



tic()
fit11c_all_data <- sdmTMB(tottraps ~ 0 + 
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
                          spatiotemporal = "ar1", # <- new
                          data = d_all_data,
                          time = "half_month_of_seasonf")
toc() #64min

#when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.137235516486863
#sanity(fit11c_all_data)
#red Xs: lots
#sanity(fit11c_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#still 
AIC(fit11c_all_data)
#1010287
#summary(fit11c_all_data)
#Spatiotemporal AR1 correlation (rho): 0.99



tic()
fit11cx_all_data <- sdmTMB(tottraps ~ 0 + 
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
                          spatiotemporal = "iid", # <- new
                          data = d_all_data,
                          time = "half_month_of_seasonf")
toc() #18min

#The model may not have converged. Maximum final gradient: 0.0604548171008208
#sanity(fit11cx_all_data)
#red Xs: b_js, ln_tau_E, thetaf
#sanity(fit11cx_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, thetaf
AIC(fit11cx_all_data)
#1012012
#summary(fit11cx_all_data)









#-------------------------------------

tic()
fit12a_all_data <- sdmTMB(tottraps ~ 0 + 
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
                            z_weighted_fuel_pricegal * z_weighted_crab_ppp +  #interaction
                            #z_weighted_crab_ppp +
                            poly(z_bottom_O2_avg,2) +
                            z_dist_to_closed_km,
                          family = tweedie(),
                          mesh = mesh,
                          spatial = "on",
                          spatiotemporal = "ar1",
                          data = d_all_data,
                          time = "month_n")
toc() #18min

#The model may not have converged. Maximum final gradient: 0.0737998978392138
#sanity(fit12a_all_data)
#Red Xs: b_js, thetaf, ar1_phi
#sanity(fit12a_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, thetaf
AIC(fit12a_all_data)
# 1012994
#summary(fit12a_all_data) #ar1/rho = 0.97




tic()
fit12b_all_data <- sdmTMB(tottraps ~ 0 + 
                            season +
                            month_name_f + 
                            OR_WA_waters +
                            WA_pot_reduction +
                            z_SST_avg +
                            #z_wind_avg +
                            poly(z_depth_point_mean,2) +
                            z_depth_point_sd +
                            z_faults_km +
                            z_dist_canyon_km +
                            z_weighted_dist +
                            z_weighted_fuel_pricegal * z_wind_avg +  #interaction
                            z_weighted_crab_ppp +
                            poly(z_bottom_O2_avg,2) +
                            z_dist_to_closed_km,
                          family = tweedie(),
                          mesh = mesh,
                          spatial = "on",
                          spatiotemporal = "ar1",
                          data = d_all_data,
                          time = "month_n")
toc() #26min

#The model may not have converged. Maximum final gradient: 0.0623170825449637
#sanity(fit12b_all_data)
#Red Xs: b_js, sigma_O
#sanity(fit12b_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, sigma_O
AIC(fit12b_all_data)
# 1012970
#summary(fit12b_all_data) #ar1/rho = 0.97




tic()
fit12c_all_data <- sdmTMB(tottraps ~ 0 + 
                            season +
                            month_name_f + 
                            #OR_WA_waters +
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
                            z_dist_to_closed_km * OR_WA_waters,  #interaction
                          family = tweedie(),
                          mesh = mesh,
                          spatial = "on",
                          spatiotemporal = "ar1",
                          data = d_all_data,
                          time = "month_n")
toc() #23min

#The model may not have converged. Maximum final gradient: 0.0374019703844618
#sanity(fit12c_all_data)
#Red Xs: b_js, thetaf, ln_phi, ar1_phi
#sanity(fit12c_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, thetaf
AIC(fit12c_all_data)
# 1012994
#summary(fit12c_all_data) #ar1/rho = 0.97




tic()
fit12d_all_data <- sdmTMB(tottraps ~ 0 + 
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
                            #z_weighted_dist +
                            z_weighted_fuel_pricegal * z_weighted_dist +  #interaction
                            z_weighted_crab_ppp +
                            poly(z_bottom_O2_avg,2) +
                            z_dist_to_closed_km,  
                          family = tweedie(),
                          mesh = mesh,
                          spatial = "on",
                          spatiotemporal = "ar1",
                          data = d_all_data,
                          time = "month_n")
toc() #34min

#The model may not have converged. Maximum final gradient: 0.0261275307314213
#sanity(fit12d_all_data)
#Red Xs: b_js only
#sanity(fit12d_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js only
AIC(fit12d_all_data)
# 1012918
#summary(fit12d_all_data)

#exported model - where b_js only
#write_rds(fit12d_all_data,here::here('DCRB_sdmTMB', 'exported model objects',"fit12d_all_data_v1.rds"))
#fit12d_all_data <- read_rds(here::here('DCRB_sdmTMB', 'exported model objects','fit12d_all_data_v1.rds')) 


#plots <- plot_diag(fit12d_all_data)

#some plots

plot_log = function(object, term) {
  g <- ggeffect(object, term, back.transform = FALSE)
  g$conf.low <- log(g$conf.low)
  g$conf.high <- log(g$conf.high)
  g$predicted <- log(g$predicted)
  plot(g)
}


p1 <- plot_log(fit12d_all_data, "season [all]")
p2 <- plot_log(fit12d_all_data, "month_name_f [all]")
p3 <- plot_log(fit12d_all_data, "OR_WA_waters [all]")
p35 <- plot_log(fit12d_all_data, "WA_pot_reduction [all]")
p4 <- plot_log(fit12d_all_data, "z_SST_avg [all]")
p5 <- plot_log(fit12d_all_data, "z_wind_avg [all]")
p6 <- plot_log(fit12d_all_data, "z_depth_point_mean [all]")
p7 <- plot_log(fit12d_all_data, "z_depth_point_sd [all]")
p8 <- plot_log(fit12d_all_data, "z_faults_km [all]")
p9 <- plot_log(fit12d_all_data, "z_dist_canyon_km [all]")
p10 <- plot_log(fit12d_all_data, "z_weighted_dist [all]")
p11 <- plot_log(fit12d_all_data, "z_weighted_fuel_pricegal [all]")
p12 <- plot_log(fit12d_all_data, "z_weighted_crab_ppp [all]")
p13 <- plot_log(fit12d_all_data, "z_bottom_O2_avg [all]")
p14 <- plot_log(fit12d_all_data, "z_dist_to_closed_km [all]")

gridExtra::grid.arrange(p1,p2,p3,p35,ncol=2)

gridExtra::grid.arrange(p4,p5,p6,p7,ncol=2)

gridExtra::grid.arrange(p8,p9,p13,p14,ncol=2)

gridExtra::grid.arrange(p10,p11,p12,ncol=2)

res <- residuals(fit12d_all_data)
qqnorm(res,ylim=c(-5,5))
qqline(res)


##

tic()
fit12e_all_data <- sdmTMB(tottraps ~ 0 + 
                            season +
                            month_name_f + 
                            OR_WA_waters +
                            WA_pot_reduction +
                            z_SST_avg * z_wind_avg +  #interaction
                            #z_wind_avg +
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
                          spatiotemporal = "ar1",
                          data = d_all_data,
                          time = "month_n")
toc() #31min

#The model may not have converged. Maximum final gradient: 0.027558725238138
#sanity(fit12e_all_data)
#Red Xs: b_js, ln_tau, ln_kappa, thetaf, ln_phi, are1_phi
#sanity(fit12e_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, ln_phi
AIC(fit12e_all_data)
# 1012953
#summary(fit12e_all_data)

#-------------------------------------


#
tic()
fit13a_all_data <- sdmTMB(tottraps ~ 0 + 
                            season +
                            half_month_of_seasonf +
                            #month_name_f + 
                            OR_WA_waters +
                            WA_pot_reduction +
                            z_SST_avg +
                            z_wind_avg +
                            poly(z_depth_point_mean,2) +
                            z_depth_point_sd +
                            z_faults_km +
                            z_dist_canyon_km +
                            z_weighted_dist +
                            z_weighted_fuel_pricegal * z_weighted_crab_ppp +   #interaction
                            #z_weighted_crab_ppp +
                            poly(z_bottom_O2_avg,2) +
                            z_dist_to_closed_km,
                          family = tweedie(),
                          mesh = mesh,
                          spatial = "on",
                          spatiotemporal = "ar1",
                          data = d_all_data,
                          time = "month_n")
toc() #42min

#The model may not have converged. Maximum final gradient: 0.0155186841043213
#sanity(fit13a_all_data)
#Red Xs: b_js, thetaf, ln_phi
#sanity(fit13a_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, ln_phi
AIC(fit13a_all_data)
# 1011426
#summary(fit13a_all_data) #rho = 0.97

#write_rds(fit13a_all_data,here::here('DCRB_sdmTMB', 'exported model objects','some all data models',"fit13a_all_data_v1.rds"))



tic()
fit13ax_all_data <- sdmTMB(tottraps ~ 0 + 
                            season +
                            half_month_of_seasonf +
                            #month_name_f + 
                            OR_WA_waters +
                            WA_pot_reduction +
                            z_SST_avg +
                            z_wind_avg +
                            poly(z_depth_point_mean,2) +
                            z_depth_point_sd +
                            z_faults_km +
                            z_dist_canyon_km +
                            z_weighted_dist +
                            z_weighted_fuel_pricegal * z_weighted_crab_ppp +   #interaction
                            #z_weighted_crab_ppp +
                            poly(z_bottom_O2_avg,2) +
                            z_dist_to_closed_km,
                          family = tweedie(),
                          mesh = mesh,
                          spatial = "on",
                          spatiotemporal = "iid", # 
                          data = d_all_data,
                          time = "month_n")
toc() #18min

#The model may not have converged. Maximum final gradient: 0.0670894026901191
#sanity(fit13ax_all_data)
#Red Xs: b_js, ln_tau_E
#sanity(fit13ax_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, ln_tau_E
AIC(fit13ax_all_data)
# 1012152







#
tic()
fit13b_all_data <- sdmTMB(tottraps ~ 0 + 
                            season +
                            half_month_of_seasonf +
                            #month_name_f + 
                            OR_WA_waters +
                            WA_pot_reduction +
                            z_SST_avg +
                            #z_wind_avg +
                            poly(z_depth_point_mean,2) +
                            z_depth_point_sd +
                            z_faults_km +
                            z_dist_canyon_km +
                            z_weighted_dist +
                            z_weighted_fuel_pricegal * z_wind_avg +   #interaction
                            z_weighted_crab_ppp +
                            poly(z_bottom_O2_avg,2) +
                            z_dist_to_closed_km,
                          family = tweedie(),
                          mesh = mesh,
                          spatial = "on",
                          spatiotemporal = "ar1",
                          data = d_all_data,
                          time = "month_n")
toc() #41min

#The model may not have converged. Maximum final gradient: 0.0823073221643504
#sanity(fit13b_all_data)
#Red Xs: b_js, ln_phi
#sanity(fit13b_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs:  b_js, ln_phi
AIC(fit13b_all_data)
# 1011437
#summary(fit13b_all_data) #rho = 0.97

#write_rds(fit13b_all_data,here::here('DCRB_sdmTMB', 'exported model objects','some all data models',"fit13b_all_data_v1.rds"))



#
tic()
fit13c_all_data <- sdmTMB(tottraps ~ 0 + 
                            season +
                            half_month_of_seasonf +
                            #month_name_f + 
                            OR_WA_waters +
                            WA_pot_reduction +
                            z_SST_avg +
                            z_wind_avg +
                            poly(z_depth_point_mean,2) +
                            z_depth_point_sd +
                            z_faults_km +
                            z_dist_canyon_km +
                            #z_weighted_dist +
                            z_weighted_fuel_pricegal * z_weighted_dist +   #interaction
                            z_weighted_crab_ppp +
                            poly(z_bottom_O2_avg,2) +
                            z_dist_to_closed_km,
                          family = tweedie(),
                          mesh = mesh,
                          spatial = "on",
                          spatiotemporal = "ar1",
                          data = d_all_data,
                          time = "month_n")
toc() #35min

#The model may not have converged. Maximum final gradient: 0.0299214364144973
#sanity(fit13c_all_data)
#Red Xs: b_js, ln_tau_E, ar1_phi
#sanity(fit13c_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs:  b_js only
AIC(fit13c_all_data)
# 1011416
#summary(fit13c_all_data) #rho = 0.97

#write_rds(fit13c_all_data,here::here('DCRB_sdmTMB', 'exported model objects','some all data models',"fit13c_all_data_v1.rds"))
#fit13c_all_data <- read_rds(here::here('DCRB_sdmTMB', 'exported model objects','some all data models',"fit13c_all_data_v1.rds")) 


#plots <- plot_diag(fit13c_all_data)

#some plots

plot_log = function(object, term) {
  g <- ggeffect(object, term, back.transform = FALSE)
  g$conf.low <- log(g$conf.low)
  g$conf.high <- log(g$conf.high)
  g$predicted <- log(g$predicted)
  plot(g)
}


p1 <- plot_log(fit13c_all_data, "season [all]")
p2 <- plot_log(fit13c_all_data, "half_month_of_seasonf [all]")
p3 <- plot_log(fit13c_all_data, "OR_WA_waters [all]")
p35 <- plot_log(fit13c_all_data, "WA_pot_reduction [all]")
p4 <- plot_log(fit13c_all_data, "z_SST_avg [all]")
p5 <- plot_log(fit13c_all_data, "z_wind_avg [all]")
p6 <- plot_log(fit13c_all_data, "z_depth_point_mean [all]")
p7 <- plot_log(fit13c_all_data, "z_depth_point_sd [all]")
p8 <- plot_log(fit13c_all_data, "z_faults_km [all]")
p9 <- plot_log(fit13c_all_data, "z_dist_canyon_km [all]")
p10 <- plot_log(fit13c_all_data, "z_weighted_dist [all]")
p11 <- plot_log(fit13c_all_data, "z_weighted_fuel_pricegal [all]")
p12 <- plot_log(fit13c_all_data, "z_weighted_crab_ppp [all]")
p13 <- plot_log(fit13c_all_data, "z_bottom_O2_avg [all]")
p14 <- plot_log(fit13c_all_data, "z_dist_to_closed_km [all]")

gridExtra::grid.arrange(p1,p2,p3,p35,ncol=2)

gridExtra::grid.arrange(p4,p5,p6,p7,ncol=2)

gridExtra::grid.arrange(p8,p9,p13,p14,ncol=2)

gridExtra::grid.arrange(p10,p11,p12,ncol=2)

res <- residuals(fit13c_all_data)
qqnorm(res,ylim=c(-5,5))
qqline(res)


#####
#
tic()
fit13cx_all_data <- sdmTMB(tottraps ~ 0 + 
                            season +
                            half_month_of_seasonf +
                            #month_name_f + 
                            OR_WA_waters +
                            WA_pot_reduction +
                            z_SST_avg +
                            z_wind_avg +
                            poly(z_depth_point_mean,2) +
                            z_depth_point_sd +
                            z_faults_km +
                            z_dist_canyon_km +
                            #z_weighted_dist +
                            z_weighted_fuel_pricegal * z_weighted_dist +   #interaction
                            z_weighted_crab_ppp +
                            poly(z_bottom_O2_avg,2) +
                            z_dist_to_closed_km,
                          family = tweedie(),
                          mesh = mesh,
                          spatial = "on",
                          spatiotemporal = "iid",
                          data = d_all_data,
                          time = "month_n")
toc() #19min

#The model may not have converged. Maximum final gradient: 0.0754619393038636
#sanity(fit13cx_all_data)
#Red Xs: b_js, ln_tau_E, thetaf
#sanity(fit13cx_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, ln_tau_E, thetaf
AIC(fit13cx_all_data)
# 1012141









#
tic()
fit13d_all_data <- sdmTMB(tottraps ~ 0 + 
                            season +
                            half_month_of_seasonf +
                            #month_name_f + 
                            #OR_WA_waters +
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
                            z_dist_to_closed_km * OR_WA_waters,   #interaction
                          family = tweedie(),
                          mesh = mesh,
                          spatial = "on",
                          spatiotemporal = "ar1",
                          data = d_all_data,
                          time = "month_n")
toc() #43min

#The model may not have converged. Maximum final gradient: 45.501090250555
#sanity(fit13d_all_data)
#Red Xs: Non-linear minimizer did not converge: do not trust this model!
#and others
#sanity(fit13d_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: same as above
AIC(fit13d_all_data)
# 1012119
#summary(fit13d_all_data) #rho = 0.97





#
tic()
fit13e_all_data <- sdmTMB(tottraps ~ 0 + 
                            season +
                            half_month_of_seasonf +
                            #month_name_f + 
                            OR_WA_waters +
                            WA_pot_reduction +
                            z_SST_avg * z_wind_avg +  #interaction
                            #z_wind_avg +
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
                          spatiotemporal = "ar1",
                          data = d_all_data,
                          time = "month_n")
toc() #43min

#The model may not have converged. Maximum final gradient: 0.0517809199961596
#sanity(fit13e_all_data)
#Red Xs: b_js, ln_tau, ln_phi, ar1_phi
#sanity(fit13e_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, ln_tau_E
AIC(fit13e_all_data)
# 1011376
#summary(fit13e_all_data) #rho = 0.97

#write_rds(fit13e_all_data,here::here('DCRB_sdmTMB', 'exported model objects','some all data models',"fit13e_all_data_v1.rds"))


#--------------------------
#add more polynomial terms to fit10e

#
tic()
fit14a_all_data <- sdmTMB(tottraps ~ 0 + 
                            season +
                            month_of_seasonf +
                            #month_name_f + 
                            OR_WA_waters +
                            WA_pot_reduction +
                            z_SST_avg +
                            z_wind_avg +
                            poly(z_depth_point_mean,2) +
                            z_depth_point_sd +
                            z_faults_km +
                            z_dist_canyon_km +
                            poly(z_weighted_dist,2)  +
                            z_weighted_fuel_pricegal +
                            z_weighted_crab_ppp +
                            poly(z_bottom_O2_avg,2) +
                            poly(z_dist_to_closed_km, 2),
                          family = tweedie(),
                          mesh = mesh,
                          spatial = "on",
                          spatiotemporal = "ar1",
                          data = d_all_data,
                          time = "month_n")
toc() #35min

#The model may not have converged. Maximum final gradient: 0.0349989867712761
#sanity(fit14a_all_data)
#Red Xs: b_js, thetaf, ln_phi, ar1_phi
#sanity(fit14a_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, thetaf, ln_phi
AIC(fit14a_all_data)
# 1012196
#summary(fit14a_all_data) #rho = 0.97


tic()
fit14b_all_data <- sdmTMB(tottraps ~ 0 + 
                            season +
                            month_of_seasonf +
                            #month_name_f + 
                            OR_WA_waters +
                            WA_pot_reduction +
                            poly(z_SST_avg, 2) +
                            z_wind_avg +
                            poly(z_depth_point_mean, 2) +
                            z_depth_point_sd +
                            z_faults_km +
                            z_dist_canyon_km +
                            poly(z_weighted_dist, 2)  +
                            z_weighted_fuel_pricegal +
                            poly(z_weighted_crab_ppp,2 ) +
                            poly(z_bottom_O2_avg, 2) +
                            poly(z_dist_to_closed_km, 2),
                          family = tweedie(),
                          mesh = mesh,
                          spatial = "on",
                          spatiotemporal = "ar1",
                          data = d_all_data,
                          time = "month_n")
toc() #42min

#The model may not have converged. Maximum final gradient: 25.6403851961114
#sanity(fit14b_all_data)
#Non-linear minimizer did not converge: do not trust this model!
#Red Xs: b_js, ln_tauE, ar1_phi
#sanity(fit14b_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: same as above
AIC(fit14b_all_data)
# 1013957
#summary(fit14b_all_data) #rho = 0.98


tic()
fit14c_all_data <- sdmTMB(tottraps ~ 0 + 
                            season +
                            month_of_seasonf +
                            #month_name_f + 
                            OR_WA_waters +
                            WA_pot_reduction +
                            poly(z_SST_avg, 2) +
                            poly(z_wind_avg, 2) +
                            poly(z_depth_point_mean, 2) +
                            poly(z_depth_point_sd,2 ) +
                            poly(z_faults_km,2 ) +
                            poly(z_dist_canyon_km, 2) +
                            poly(z_weighted_dist, 2)  +
                            poly(z_weighted_fuel_pricegal,2 ) +
                            poly(z_weighted_crab_ppp,2 ) +
                            poly(z_bottom_O2_avg, 2) +
                            poly(z_dist_to_closed_km, 2),
                          family = tweedie(),
                          mesh = mesh,
                          spatial = "on",
                          spatiotemporal = "ar1",
                          data = d_all_data,
                          time = "month_n")
toc() #43min

#The model may not have converged. Maximum final gradient: 12.2625383569855
#sanity(fit14c_all_data)
#Non-linear minimizer did not converge: do not trust this model!
#Red Xs: b_js, ln_tauE, ar1_phi
#sanity(fit14c_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: same as above
AIC(fit14c_all_data)
# 1013401
#summary(fit14c_all_data) #rho = 0.97

#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
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

#add chosen polynomial terms


tic()
fit10a_summer <- sdmTMB(tottraps ~ 0 + 
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
                      mesh = mesh_summer,
                      spatial = "on",
                      spatiotemporal = "ar1", # <- new
                      data = summer,
                      time = "yearf")
toc() #12min

#when seed set and depth is poly: The model may not have converged. Maximum final gradient: 0.0201316143244483
#sanity(fit10a_summer)
#red Xs: b_js, ln_tau, thetaf
#sanity(fit10a_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, thetaf
AIC(fit10a_summer)
#266794
#summary(fit10a_summer)
#Spatiotemporal AR1 correlation (rho): 0.48




tic()
fit10b_summer <- sdmTMB(tottraps ~ 0 + 
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
                        mesh = mesh_summer,
                        spatial = "on",
                        spatiotemporal = "ar1", # <- new
                        data = summer,
                        time = "yearf")
toc() #12min

#when seed set and bottom O2 is poly: no warnings
#sanity(fit10b_summer)
#red Xs: b_js only
#sanity(fit10b_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js only
AIC(fit10b_summer)
#267626.3
#summary(fit10b_summer)
#Spatiotemporal AR1 correlation (rho): 0.47




tic()
fit10c_summer <- sdmTMB(tottraps ~ 0 + 
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
                        mesh = mesh_summer,
                        spatial = "on",
                        spatiotemporal = "ar1", # <- new
                        data = summer,
                        time = "yearf")
toc() #9min

#when seed set and depth & bottom O2 is poly: no warnings
#sanity(fit10c_summer)
#red Xs: b_js, ln_tau, thetaf
#sanity(fit10c_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_j only
AIC(fit10c_summer)
#266778.5
#summary(fit10c_summer)
#Spatiotemporal AR1 correlation (rho): 0.48

#plots <- plot_diag(fit10c_summer)


#some plots

plot_log = function(object, term) {
  g <- ggeffect(object, term, back.transform = FALSE)
  g$conf.low <- log(g$conf.low)
  g$conf.high <- log(g$conf.high)
  g$predicted <- log(g$predicted)
  plot(g)
}


p1 <- plot_log(fit10c_summer, "season [all]")
p2 <- plot_log(fit10c_summer, "month_name_f [all]")
p3 <- plot_log(fit10c_summer, "OR_WA_waters [all]")
p35 <- plot_log(fit10c_summer, "WA_pot_reduction [all]")
p4 <- plot_log(fit10c_summer, "z_SST_avg [all]")
p5 <- plot_log(fit10c_summer, "z_wind_avg [all]")
p6 <- plot_log(fit10c_summer, "z_depth_point_mean [all]")
p7 <- plot_log(fit10c_summer, "z_depth_point_sd [all]")
p8 <- plot_log(fit10c_summer, "z_faults_km [all]")
p9 <- plot_log(fit10c_summer, "z_dist_canyon_km [all]")
p10 <- plot_log(fit10c_summer, "z_weighted_dist [all]")
p11 <- plot_log(fit10c_summer, "z_weighted_fuel_pricegal [all]")
p12 <- plot_log(fit10c_summer, "z_weighted_crab_ppp [all]")
p13 <- plot_log(fit10c_summer, "z_bottom_O2_avg [all]")
p14 <- plot_log(fit10c_summer, "z_dist_to_closed_km [all]")

gridExtra::grid.arrange(p1,p2,p3,p35,ncol=2)

gridExtra::grid.arrange(p4,p5,p6,p7,ncol=2)

gridExtra::grid.arrange(p8,p9,p13,p14,ncol=2)

gridExtra::grid.arrange(p10,p11,p12,ncol=2)

res <- residuals(fit10c_summer)
qqnorm(res)
qqline(res)



tic()
fit10d_summer <- sdmTMB(tottraps ~ 0 + 
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
                        mesh = mesh_summer,
                        spatial = "on",
                        spatiotemporal = "iid", # <- new
                        data = summer,
                        time = "yearf")
toc() #5min

#when seed set and depth & bottom O2 is poly: no warnings
#sanity(fit10d_summer)
#red Xs: b_js, ln_tau
#sanity(fit10d_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, ln_tau
AIC(fit10d_summer)
#266859.2




tic()
fit10e_summer <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_of_seasonf +  #new
                          #month_name_f + 
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
                        mesh = mesh_summer,
                        spatial = "on",
                        spatiotemporal = "ar1", 
                        data = summer,
                        time = "yearf")
toc() #14min

#when seed set and depth & bottom O2 is poly: no warnings
#sanity(fit10e_summer)
#red Xs: b_js, ln_tau, thetaf, ln_phi, ar1_phi
#sanity(fit10e_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, ln_phi 
AIC(fit10e_summer)
#266726.8
#summary(fit10e_summer)
#Spatiotemporal AR1 correlation (rho): 0.48



tic()
fit10f_summer <- sdmTMB(tottraps ~ 0 + 
                          season +
                          half_month_of_seasonf +  #new
                          #month_name_f + 
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
                        mesh = mesh_summer,
                        spatial = "on",
                        spatiotemporal = "ar1", 
                        data = summer,
                        time = "yearf")
toc() #12min

#when seed set and depth & bottom O2 is poly: no warnings
#sanity(fit10f_summer)
#red Xs: b_js, thetaf, ln_phi (old model run: b_js, ln_tau, ln_kappa, thetaf, ln_phi)
#sanity(fit10f_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, thetaf (old model run: b_j only)
AIC(fit10f_summer)
#266479.6
#summary(fit10f_summer)
#Spatiotemporal AR1 correlation (rho): 0.48


#exported model
#write_rds(fit10f_summer,here::here('DCRB_sdmTMB', 'exported model objects',"fit10f_summer.rds"))
#testrds <- read_rds(here::here('DCRB_sdmTMB', 'exported model objects','fit10f_summer.rds')) 

#plots for exported model
#plots <- plot_diag(fit10f_summer)



#some plots

plot_log = function(object, term) {
  g <- ggeffect(object, term, back.transform = FALSE)
  g$conf.low <- log(g$conf.low)
  g$conf.high <- log(g$conf.high)
  g$predicted <- log(g$predicted)
  plot(g)
}


p1 <- plot_log(fit10f_summer, "season [all]")
p2 <- plot_log(fit10f_summer, "half_month_of_seasonf [all]")
p3 <- plot_log(fit10f_summer, "OR_WA_waters [all]")
p35 <- plot_log(fit10f_summer, "WA_pot_reduction [all]")
p4 <- plot_log(fit10f_summer, "z_SST_avg [all]")
p5 <- plot_log(fit10f_summer, "z_wind_avg [all]")
p6 <- plot_log(fit10f_summer, "z_depth_point_mean [all]")
p7 <- plot_log(fit10f_summer, "z_depth_point_sd [all]")
p8 <- plot_log(fit10f_summer, "z_faults_km [all]")
p9 <- plot_log(fit10f_summer, "z_dist_canyon_km [all]")
p10 <- plot_log(fit10f_summer, "z_weighted_dist [all]")
p11 <- plot_log(fit10f_summer, "z_weighted_fuel_pricegal [all]")
p12 <- plot_log(fit10f_summer, "z_weighted_crab_ppp [all]")
p13 <- plot_log(fit10f_summer, "z_bottom_O2_avg [all]")
p14 <- plot_log(fit10f_summer, "z_dist_to_closed_km [all]")

gridExtra::grid.arrange(p1,p2,p3,p35,ncol=2)

gridExtra::grid.arrange(p4,p5,p6,p7,ncol=2)

gridExtra::grid.arrange(p8,p9,p13,p14,ncol=2)

gridExtra::grid.arrange(p10,p11,p12,ncol=2)

res <- residuals(fit10f_summer)
qqnorm(res)
qqline(res)



#-------------------------------------


tic()
fit11a_summer <- sdmTMB(tottraps ~ 0 + 
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
                        mesh = mesh_summer,
                        spatial = "on",
                        spatiotemporal = "ar1", # <- new
                        data = summer,
                        time = "month_name_f")
toc() #7min

#when seed set and depth & bottom O2 is poly: no warnings
#sanity(fit11a_summer)
#red Xs: b_js, thetaf, ln_tau, sigma_O
#sanity(fit11a_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still thetaf, ln_tau, sigma_O
AIC(fit11a_summer)
#271257.9
#summary(fit11a_summer)
#Spatiotemporal AR1 correlation (rho): 0.96




tic()
fit11b_summer <- sdmTMB(tottraps ~ 0 + 
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
                        mesh = mesh_summer,
                        spatial = "on",
                        spatiotemporal = "ar1", # <- new
                        data = summer,
                        time = "month_of_seasonf")
toc() #14min

#when seed set and depth & bottom O2 is poly: no warnings
#sanity(fit11a_summer)
#red Xs: b_js, thetaf, ln_tau, sigma_O
#sanity(fit11a_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still thetaf, ln_tau, sigma_O
AIC(fit11a_summer)
#271257.9
#summary(fit11a_summer)
#Spatiotemporal AR1 correlation (rho): 0.96




tic()
fit11c_summer <- sdmTMB(tottraps ~ 0 + 
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
                        mesh = mesh_summer,
                        spatial = "on",
                        spatiotemporal = "ar1", # <- new
                        data = summer,
                        time = "half_month_of_seasonf")
toc() #59min

#when seed set and depth & bottom O2 is poly: no warnings
#sanity(fit11c_summer)
#red Xs: b_js, thetaf, ln_phi, ln_tau, sigma_O
#sanity(fit11c_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still ln_tau, sigma_O
AIC(fit11c_summer)
#270355.7
#summary(fit11c_summer)
#Spatiotemporal AR1 correlation (rho): 0.98


#-------------------------------------

#INTERACTIONS

tic()
fit12a_summer <- sdmTMB(tottraps ~ 0 + 
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
                          z_weighted_fuel_pricegal * z_weighted_crab_ppp + #interaction
                          #z_weighted_crab_ppp +
                          poly(z_bottom_O2_avg,2) +
                          z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh_summer,
                        spatial = "on",
                        spatiotemporal = "ar1", # <- new
                        data = summer,
                        time = "yearf")
toc() #10min

#when seed set and depth & bottom O2 is poly: no warnings
#sanity(fit12a_summer)
#red Xs: b_js, ln_tau_E
#sanity(fit12a_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#no red Xs 
AIC(fit12a_summer)
#266775.4
#summary(fit12a_summer)
#Spatiotemporal AR1 correlation (rho): 0.48

#plots <- plot_diag(fit12a_summer)

plot_log = function(object, term) {
  g <- ggeffect(object, term, back.transform = FALSE)
  g$conf.low <- log(g$conf.low)
  g$conf.high <- log(g$conf.high)
  g$predicted <- log(g$predicted)
  plot(g)
}


p1 <- plot_log(fit12a_summer, "season [all]")
p2 <- plot_log(fit12a_summer, "month_name_f [all]")
p3 <- plot_log(fit12a_summer, "OR_WA_waters [all]")
p35 <- plot_log(fit12a_summer, "WA_pot_reduction [all]")
p4 <- plot_log(fit12a_summer, "z_SST_avg [all]")
p5 <- plot_log(fit12a_summer, "z_wind_avg [all]")
p6 <- plot_log(fit12a_summer, "z_depth_point_mean [all]")
p7 <- plot_log(fit12a_summer, "z_depth_point_sd [all]")
p8 <- plot_log(fit12a_summer, "z_faults_km [all]")
p9 <- plot_log(fit12a_summer, "z_dist_canyon_km [all]")
p10 <- plot_log(fit12a_summer, "z_weighted_dist [all]")
p11 <- plot_log(fit12a_summer, "z_weighted_fuel_pricegal [all]")
p12 <- plot_log(fit12a_summer, "z_weighted_crab_ppp [all]")
p13 <- plot_log(fit12a_summer, "z_bottom_O2_avg [all]")
p14 <- plot_log(fit12a_summer, "z_dist_to_closed_km [all]")

gridExtra::grid.arrange(p1,p2,p3,p35,ncol=2)

gridExtra::grid.arrange(p4,p5,p6,p7,ncol=2)

gridExtra::grid.arrange(p8,p9,p13,p14,ncol=2)

gridExtra::grid.arrange(p10,p11,p12,ncol=2)

res <- residuals(fit12a_summer)
qqnorm(res)
qqline(res)







tic()
fit12b_summer <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_name_f + 
                          OR_WA_waters +
                          WA_pot_reduction +
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
                        mesh = mesh_summer,
                        spatial = "on",
                        spatiotemporal = "ar1", # <- new
                        data = summer,
                        time = "yearf")
toc() #9min

#when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.0222395155980664
#sanity(fit12a_summer)
#red Xs: b_js, ln_tau_E
#sanity(fit12a_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
# none
AIC(fit12a_summer)
#266775.4
#summary(fit12a_summer)
#Spatiotemporal AR1 correlation (rho): 0.48






tic()
fit12c_summer <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_name_f + 
                          #OR_WA_waters +
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
                          z_dist_to_closed_km * OR_WA_waters, #interaction
                        family = tweedie(),
                        mesh = mesh_summer,
                        spatial = "on",
                        spatiotemporal = "ar1", # <- new
                        data = summer,
                        time = "yearf")
toc() #9min

#when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.0158429278868581
#sanity(fit12c_summer)
#red Xs: b_js, ln_tau_O
#sanity(fit12c_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, ln_tau_O
AIC(fit12c_summer)
#266780.5
#summary(fit12c_summer)
#Spatiotemporal AR1 correlation (rho): 0.48



tic()
fit12d_summer <- sdmTMB(tottraps ~ 0 + 
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
                          #z_weighted_dist +
                          z_weighted_fuel_pricegal * z_weighted_dist + #interaction
                          z_weighted_crab_ppp +
                          poly(z_bottom_O2_avg,2) +
                          z_dist_to_closed_km, 
                        family = tweedie(),
                        mesh = mesh_summer,
                        spatial = "on",
                        spatiotemporal = "ar1", # <- new
                        data = summer,
                        time = "yearf")
toc() #18min

#when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.0113732064805987
#sanity(fit12d_summer)
#red Xs: b_js, ln_tau, ln_kappa, ar1_phi
#sanity(fit12d_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_j, ln_tau, ln_kappa
AIC(fit12d_summer)
#266778.4 
#summary(fit12d_summer)
#Spatiotemporal AR1 correlation (rho): 0.48



tic()
fit12e_summer <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_name_f + 
                          OR_WA_waters +
                          WA_pot_reduction +
                          z_SST_avg * z_wind_avg + #interaction
                          #z_wind_avg +
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
                        mesh = mesh_summer,
                        spatial = "on",
                        spatiotemporal = "ar1", # <- new
                        data = summer,
                        time = "yearf")
toc() #9min

#when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.0146633443045658
#sanity(fit12e_summer)
#red Xs: b_js only
#sanity(fit12e_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js only 
AIC(fit12e_summer)
# 266780.2
#summary(fit12e_summer)
#Spatiotemporal AR1 correlation (rho): 0.48




#-------------------------------




tic()
fit13a_summer <- sdmTMB(tottraps ~ 0 + 
                          season +
                          half_month_of_seasonf +  #new
                          #month_name_f + 
                          OR_WA_waters +
                          WA_pot_reduction +
                          z_SST_avg +
                          z_wind_avg +
                          poly(z_depth_point_mean,2) +
                          z_depth_point_sd +
                          z_faults_km +
                          z_dist_canyon_km +
                          z_weighted_dist +
                          z_weighted_fuel_pricegal * z_weighted_crab_ppp + #interaction
                          #z_weighted_crab_ppp +
                          poly(z_bottom_O2_avg,2) +
                          z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh_summer,
                        spatial = "on",
                        spatiotemporal = "ar1", 
                        data = summer,
                        time = "yearf")
toc() #12min

#when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.0282404987638287
#sanity(fit13a_summer)
#red Xs: b_js, ln_tau, thetaf, ln_phi
#sanity(fit13a_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, thetaf 
AIC(fit13a_summer)
#266474.6
#summary(fit13a_summer)
#Spatiotemporal AR1 correlation (rho): 0.48




tic()
fit13b_summer <- sdmTMB(tottraps ~ 0 + 
                          season +
                          half_month_of_seasonf +  #new
                          #month_name_f + 
                          OR_WA_waters +
                          WA_pot_reduction +
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
                        mesh = mesh_summer,
                        spatial = "on",
                        spatiotemporal = "ar1", 
                        data = summer,
                        time = "yearf")
toc() #14min

#when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.0208514122309253
#sanity(fit13b_summer)
#red Xs: b_js, ln_tau, ln_kappa, thetaf, ar1_phi
#sanity(fit13b_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, ln_kappa
AIC(fit13b_summer)
#266473.2
#summary(fit13b_summer)
#Spatiotemporal AR1 correlation (rho): 0.48




tic()
fit13c_summer <- sdmTMB(tottraps ~ 0 + 
                          season +
                          half_month_of_seasonf +  #new
                          #month_name_f + 
                          OR_WA_waters +
                          WA_pot_reduction +
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
                        mesh = mesh_summer,
                        spatial = "on",
                        spatiotemporal = "ar1", 
                        data = summer,
                        time = "yearf")
toc() #12min

#when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.027385052428599
#sanity(fit13c_summer)
#red Xs: b_js, ln_tau, ln_kappa, thetaf, ar1_phi
#sanity(fit13c_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, ln_tau, ln_kappa, thetaf
AIC(fit13c_summer)
#266478.2
#summary(fit13c_summer)
#Spatiotemporal AR1 correlation (rho): 0.48





tic()
fit13d_summer <- sdmTMB(tottraps ~ 0 + 
                          season +
                          half_month_of_seasonf +  #new
                          #month_name_f + 
                          #OR_WA_waters +
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
                          z_dist_to_closed_km * OR_WA_waters, #interaction
                        family = tweedie(),
                        mesh = mesh_summer,
                        spatial = "on",
                        spatiotemporal = "ar1", 
                        data = summer,
                        time = "yearf")
toc() #14min

#when seed set and depth & bottom O2 is poly: no warnings
#sanity(fit13c_summer)
#red Xs: b_js, ln_tau, ln_kappa, thetaf, ar1_phi
#sanity(fit13c_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, ln_tau, ln_kappa, thetaf
AIC(fit13c_summer)
#266478.2
#summary(fit13c_summer)
#Spatiotemporal AR1 correlation (rho): 0.48



tic()
fit13e_summer <- sdmTMB(tottraps ~ 0 + 
                          season +
                          half_month_of_seasonf +  #new
                          #month_name_f + 
                          OR_WA_waters +
                          WA_pot_reduction +
                          z_SST_avg * z_wind_avg +  #interaction
                          #z_wind_avg +
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
                        mesh = mesh_summer,
                        spatial = "on",
                        spatiotemporal = "ar1", 
                        data = summer,
                        time = "yearf")
toc() #11min

#when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.0328257078239034
#sanity(fit13c_summer)
#red Xs: b_js, ln_tau, ln_kappa, thetaf, ar1_phi
#sanity(fit13c_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, ln_tau, ln_kappa, thetaf
AIC(fit13c_summer)
#266478.2
#summary(fit13c_summer)
#Spatiotemporal AR1 correlation (rho): 0.48

#-------------------------------



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

res <- residuals(fit2c_winter)
qqnorm(res,ylim=c(-5,5))
qqline(res)

#when seed set at no polys / the exported model: no warnings
#sanity(fit2c_winter)
#Red Xs: None
#sanity(fit2c_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
AIC(fit2c_winter)
#734728.5
summary(fit2c_winter)
#AR1 correlation (rho): 0.51

#plots <- plot_diag(fit2c_winter)


#exported model
#write_rds(fit2c_winter,here::here('DCRB_sdmTMB', 'exported model objects',"fit2c_winter.rds"))
#testrds <- read_rds(here::here('DCRB_sdmTMB', 'exported model objects','fit2c_winter.rds')) 

res <- residuals(fit2c_winter)
qqnorm(res)
qqline(res)







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

res <- residuals(fit10c_winter)
qqnorm(res,ylim=c(-5,5))
qqline(res)

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



tic()
fit10e_winter <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_of_seasonf +  #new
                          #month_name_f + 
                          OR_WA_waters +
                          #WA_pot_reduction +  #not relevant in winter
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
toc() #25min

#when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.0100518956904025
#old run of model: no warnings
#sanity(fit10e_winter)
#red Xs: b_js, thetaf (old run of model: b_js only)
#sanity(fit10e_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, thetaf (old run of model: one b_j)
AIC(fit10e_winter)
#728209.7
#summary(fit10e_winter)
#Spatiotemporal AR1 correlation (rho): 0.48


#exported model
#write_rds(fit10e_winter,here::here('DCRB_sdmTMB', 'exported model objects',"fit10e_winter_v1.rds"))
#testrds <- read_rds(here::here('DCRB_sdmTMB', 'exported model objects','fit10e_winter_v1.rds')) 


#plots <- plot_diag(fit10e_winter)

#some plots

plot_log = function(object, term) {
  g <- ggeffect(object, term, back.transform = FALSE)
  g$conf.low <- log(g$conf.low)
  g$conf.high <- log(g$conf.high)
  g$predicted <- log(g$predicted)
  plot(g)
}


p1 <- plot_log(fit10e_winter, "season [all]")
p2 <- plot_log(fit10e_winter, "month_of_seasonf [all]")
p3 <- plot_log(fit10e_winter, "OR_WA_waters [all]")
p4 <- plot_log(fit10e_winter, "z_SST_avg [all]")
p5 <- plot_log(fit10e_winter, "z_wind_avg [all]")
p6 <- plot_log(fit10e_winter, "z_depth_point_mean [all]")
p7 <- plot_log(fit10e_winter, "z_depth_point_sd [all]")
p8 <- plot_log(fit10e_winter, "z_faults_km [all]")
p9 <- plot_log(fit10e_winter, "z_dist_canyon_km [all]")
p10 <- plot_log(fit10e_winter, "z_weighted_dist [all]")
p11 <- plot_log(fit10e_winter, "z_weighted_fuel_pricegal [all]")
p12 <- plot_log(fit10e_winter, "z_weighted_crab_ppp [all]")
p13 <- plot_log(fit10e_winter, "z_bottom_O2_avg [all]")
p14 <- plot_log(fit10e_winter, "z_dist_to_closed_km [all]")

gridExtra::grid.arrange(p1,p2,p3,ncol=2)

gridExtra::grid.arrange(p4,p5,p6,p7,ncol=2)

gridExtra::grid.arrange(p8,p9,p13,p14,ncol=2)

gridExtra::grid.arrange(p10,p11,p12,ncol=2)

res <- residuals(fit10e_winter)
qqnorm(res,ylim=c(-5,5))
qqline(res)





tic()
fit10f_winter <- sdmTMB(tottraps ~ 0 + 
                          season +
                          half_month_of_seasonf +  #new
                          #month_name_f + 
                          OR_WA_waters +
                          #WA_pot_reduction + #note relevant in winter
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
toc() #23min

#when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.0268384876646905
#old model run: The model may not have converged. Maximum final gradient: 0.102345627924144
#sanity(fit10f_winter)
#red Xs: b_js, thetaf (old model run: b_js, ln_kappa, thetaf)
#sanity(fit10f_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, thetaf (old model run: b_js, ln_kappa, thetaf)
AIC(fit10f_winter)
#727323.8
#summary(fit10f_winter)
#Spatiotemporal AR1 correlation (rho): 0.48

#exported model
#write_rds(fit10f_winter,here::here('DCRB_sdmTMB', 'exported model objects',"fit10f_winter_v1.rds"))
#testrds <- read_rds(here::here('DCRB_sdmTMB', 'exported model objects','fit10f_winter_v1.rds')) 


#some plots of exported model

#plots <- plot_diag(fit10f_winter)

plot_log = function(object, term) {
  g <- ggeffect(object, term, back.transform = FALSE)
  g$conf.low <- log(g$conf.low)
  g$conf.high <- log(g$conf.high)
  g$predicted <- log(g$predicted)
  plot(g)
}


p1 <- plot_log(fit10f_winter, "season [all]")
p2 <- plot_log(fit10f_winter, "half_month_of_seasonf [all]")
p3 <- plot_log(fit10f_winter, "OR_WA_waters [all]")
p4 <- plot_log(fit10f_winter, "z_SST_avg [all]")
p5 <- plot_log(fit10f_winter, "z_wind_avg [all]")
p6 <- plot_log(fit10f_winter, "z_depth_point_mean [all]")
p7 <- plot_log(fit10f_winter, "z_depth_point_sd [all]")
p8 <- plot_log(fit10f_winter, "z_faults_km [all]")
p9 <- plot_log(fit10f_winter, "z_dist_canyon_km [all]")
p10 <- plot_log(fit10f_winter, "z_weighted_dist [all]")
p11 <- plot_log(fit10f_winter, "z_weighted_fuel_pricegal [all]")
p12 <- plot_log(fit10f_winter, "z_weighted_crab_ppp [all]")
p13 <- plot_log(fit10f_winter, "z_bottom_O2_avg [all]")
p14 <- plot_log(fit10f_winter, "z_dist_to_closed_km [all]")

gridExtra::grid.arrange(p1,p2,p3,ncol=2)

gridExtra::grid.arrange(p4,p5,p6,p7,ncol=2)

gridExtra::grid.arrange(p8,p9,p13,p14,ncol=2)

gridExtra::grid.arrange(p10,p11,p12,ncol=2)

res <- residuals(fit10f_winter)
qqnorm(res,ylim=c(-5,5))
qqline(res)



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

#some plots

plot_log = function(object, term) {
  g <- ggeffect(object, term, back.transform = FALSE)
  g$conf.low <- log(g$conf.low)
  g$conf.high <- log(g$conf.high)
  g$predicted <- log(g$predicted)
  plot(g)
}


p1 <- plot_log(fit12b_winter, "season [all]")
p2 <- plot_log(fit12b_winter, "month_name_f [all]")
p3 <- plot_log(fit12b_winter, "OR_WA_waters [all]")
p4 <- plot_log(fit12b_winter, "z_SST_avg [all]")
p5 <- plot_log(fit12b_winter, "z_wind_avg [all]")
p6 <- plot_log(fit12b_winter, "z_depth_point_mean [all]")
p7 <- plot_log(fit12b_winter, "z_depth_point_sd [all]")
p8 <- plot_log(fit12b_winter, "z_faults_km [all]")
p9 <- plot_log(fit12b_winter, "z_dist_canyon_km [all]")
p10 <- plot_log(fit12b_winter, "z_weighted_dist [all]")
p11 <- plot_log(fit12b_winter, "z_weighted_fuel_pricegal [all]")
p12 <- plot_log(fit12b_winter, "z_weighted_crab_ppp [all]")
p13 <- plot_log(fit12b_winter, "z_bottom_O2_avg [all]")
p14 <- plot_log(fit12b_winter, "z_dist_to_closed_km [all]")

gridExtra::grid.arrange(p1,p2,p3,ncol=2)

gridExtra::grid.arrange(p4,p5,p6,p7,ncol=2)

gridExtra::grid.arrange(p8,p9,p13,p14,ncol=2)

gridExtra::grid.arrange(p10,p11,p12,ncol=2)

res <- residuals(fit12b_winter)
qqnorm(res,ylim=c(-5,5))
qqline(res)






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



plot_log = function(object, term) {
  g <- ggeffect(object, term, back.transform = FALSE)
  g$conf.low <- log(g$conf.low)
  g$conf.high <- log(g$conf.high)
  g$predicted <- log(g$predicted)
  plot(g)
}


p1 <- plot_log(fit12e_winter, "season [all]")
p2 <- plot_log(fit12e_winter, "month_name_f [all]")
p3 <- plot_log(fit12e_winter, "OR_WA_waters [all]")
p4 <- plot_log(fit12e_winter, "z_SST_avg [all]")
p5 <- plot_log(fit12e_winter, "z_wind_avg [all]")
p6 <- plot_log(fit12e_winter, "z_depth_point_mean [all]")
p7 <- plot_log(fit12e_winter, "z_depth_point_sd [all]")
p8 <- plot_log(fit12e_winter, "z_faults_km [all]")
p9 <- plot_log(fit12e_winter, "z_dist_canyon_km [all]")
p10 <- plot_log(fit12e_winter, "z_weighted_dist [all]")
p11 <- plot_log(fit12e_winter, "z_weighted_fuel_pricegal [all]")
p12 <- plot_log(fit12e_winter, "z_weighted_crab_ppp [all]")
p13 <- plot_log(fit12e_winter, "z_bottom_O2_avg [all]")
p14 <- plot_log(fit12e_winter, "z_dist_to_closed_km [all]")

gridExtra::grid.arrange(p1,p2,p3,ncol=2)

gridExtra::grid.arrange(p4,p5,p6,p7,ncol=2)

gridExtra::grid.arrange(p8,p9,p13,p14,ncol=2)

gridExtra::grid.arrange(p10,p11,p12,ncol=2)

res <- residuals(fit12e_winter)
qqnorm(res,ylim=c(-5,5))
qqline(res)


#---------------------------------

#the two versions of each model compared month_of_season and half_month_of_season fixed effect
#half-month_of_season seemed to have better AIC every time

# tic()
# fit13a_winter <- sdmTMB(tottraps ~ 0 + 
#                           season +
#                           month_of_seasonf +  #new
#                           #month_name_f + 
#                           OR_WA_waters +
#                           #WA_pot_reduction +  #not relevant in winter
#                           z_SST_avg +
#                           z_wind_avg +
#                           poly(z_depth_point_mean,2) +
#                           z_depth_point_sd +
#                           z_faults_km +
#                           z_dist_canyon_km +
#                           z_weighted_dist +
#                           z_weighted_fuel_pricegal * z_weighted_crab_ppp +  #interaction
#                           #z_weighted_crab_ppp +
#                           poly(z_bottom_O2_avg,2) +
#                           z_dist_to_closed_km,
#                         family = tweedie(),
#                         mesh = mesh_winter,
#                         spatial = "on",
#                         spatiotemporal = "ar1", 
#                         data = winter,
#                         time = "yearf")
# toc() #16min
# 
# #when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.0935398218437418
# #sanity(fit13a_winter)
# #red Xs: b_js, ar1_phi
# #sanity(fit13a_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
# #still b_js, ar1_phi 
# AIC(fit13a_winter)
# #728137
# #summary(fit13a_winter)
# #Spatiotemporal AR1 correlation (rho): 0.48



tic()
fit13a_winter <- sdmTMB(tottraps ~ 0 + 
                          season +
                           half_month_of_seasonf +  #new
                          #month_name_f + 
                          OR_WA_waters +
                          #WA_pot_reduction +  #not relevant in winter
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
toc() #14min

#when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.0282729428644547
#sanity(fit13a_winter)
#red Xs: b_js, ln_tau, thetaf
#sanity(fit13a_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, thetaf
AIC(fit13a_winter)
#727282
#summary(fit13a_winter)
#Spatiotemporal AR1 correlation (rho): 0.48





# tic()
# fit13b_winter <- sdmTMB(tottraps ~ 0 + 
#                           season +
#                           month_of_seasonf +  #new
#                           #month_name_f + 
#                           OR_WA_waters +
#                           #WA_pot_reduction +  #not relevant in winter
#                           z_SST_avg +
#                           #z_wind_avg +
#                           poly(z_depth_point_mean,2) +
#                           z_depth_point_sd +
#                           z_faults_km +
#                           z_dist_canyon_km +
#                           z_weighted_dist +
#                           z_weighted_fuel_pricegal * z_wind_avg +  #interaction
#                           z_weighted_crab_ppp +
#                           poly(z_bottom_O2_avg,2) +
#                           z_dist_to_closed_km,
#                         family = tweedie(),
#                         mesh = mesh_winter,
#                         spatial = "on",
#                         spatiotemporal = "ar1", 
#                         data = winter,
#                         time = "yearf")
# toc() #16min
# 
# #when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.238168966595684 
# #sanity(fit13b_winter)
# #red Xs: b_js, ln_tau
# #sanity(fit13b_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
# #still b_js, ln_tau 
# AIC(fit13b_winter)
# #728210.9
# #summary(fit13b_winter)
# #Spatiotemporal AR1 correlation (rho): 0.48



tic()
fit13bx_winter <- sdmTMB(tottraps ~ 0 + 
                          season +
                          half_month_of_seasonf +  #new
                          #month_name_f + 
                          OR_WA_waters +
                          #WA_pot_reduction +  #not relevant in winter
                          z_SST_avg +
                          #z_wind_avg +
                          poly(z_depth_point_mean,2) +
                          z_depth_point_sd +
                          z_faults_km +
                          z_dist_canyon_km +
                          z_weighted_dist +
                          z_weighted_fuel_pricegal * z_wind_avg +  #interaction
                          z_weighted_crab_ppp +
                          poly(z_bottom_O2_avg,2) +
                          z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh_winter,
                        spatial = "on",
                        spatiotemporal = "ar1", 
                        data = winter,
                        time = "yearf")
toc() #20min

#when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.0331277866248669 
#sanity(fit13bx_winter)
#red Xs: b_js, ln_tau, ln_phi
#sanity(fit13bx_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still  b_js, ln_phi
AIC(fit13bx_winter)
#727325.8
#summary(fit13bx_winter)
#Spatiotemporal AR1 correlation (rho): 0.48









# tic()
# fit13c_winter <- sdmTMB(tottraps ~ 0 + 
#                           season +
#                           month_of_seasonf +  #new
#                           #month_name_f + 
#                           OR_WA_waters +
#                           #WA_pot_reduction +  #not relevant in winter
#                           z_SST_avg +
#                           z_wind_avg +
#                           poly(z_depth_point_mean,2) +
#                           z_depth_point_sd +
#                           z_faults_km +
#                           z_dist_canyon_km +
#                           #z_weighted_dist +
#                           z_weighted_fuel_pricegal * z_weighted_dist +  #interaction
#                           z_weighted_crab_ppp +
#                           poly(z_bottom_O2_avg,2) +
#                           z_dist_to_closed_km,
#                         family = tweedie(),
#                         mesh = mesh_winter,
#                         spatial = "on",
#                         spatiotemporal = "ar1", 
#                         data = winter,
#                         time = "yearf")
# toc() #13min
# 
# #when seed set and depth & bottom O2 is poly:  The model may not have converged. Maximum final gradient: 0.0314707981093747
# #sanity(fit13c_winter)
# #red Xs: b_js, ln_tau, ln_kappa, thetaf, ln_phi
# #sanity(fit13c_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
# #still b_js, thetaf, ln_phi
# AIC(fit13c_winter)
# #728210.9
# #summary(fit13c_winter)
# #Spatiotemporal AR1 correlation (rho): 0.48



tic()
fit13cx_winter <- sdmTMB(tottraps ~ 0 + 
                          season +
                          half_month_of_seasonf +  #new
                          #month_name_f + 
                          OR_WA_waters +
                          #WA_pot_reduction +  #not relevant in winter
                          z_SST_avg +
                          z_wind_avg +
                          poly(z_depth_point_mean,2) +
                          z_depth_point_sd +
                          z_faults_km +
                          z_dist_canyon_km +
                          #z_weighted_dist +
                          z_weighted_fuel_pricegal * z_weighted_dist +  #interaction
                          z_weighted_crab_ppp +
                          poly(z_bottom_O2_avg,2) +
                          z_dist_to_closed_km,
                        family = tweedie(),
                        mesh = mesh_winter,
                        spatial = "on",
                        spatiotemporal = "ar1", 
                        data = winter,
                        time = "yearf")
toc() #18min

#when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.0230820652126944 
#sanity(fit13cx_winter)
#red Xs: b_js, ln_tau, ln_kappa, ar1_phi
#sanity(fit13cx_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, ln_tau, ln_kappa
AIC(fit13cx_winter)
#727322.8
#summary(fit13cx_winter)
#Spatiotemporal AR1 correlation (rho): 0.48






# tic()
# fit13d_winter <- sdmTMB(tottraps ~ 0 + 
#                           season +
#                           month_of_seasonf +  #new
#                           #month_name_f + 
#                           #OR_WA_waters +
#                           #WA_pot_reduction +  #not relevant in winter
#                           z_SST_avg +
#                           z_wind_avg +
#                           poly(z_depth_point_mean,2) +
#                           z_depth_point_sd +
#                           z_faults_km +
#                           z_dist_canyon_km +
#                           z_weighted_dist +
#                           z_weighted_fuel_pricegal  +  
#                           z_weighted_crab_ppp +
#                           poly(z_bottom_O2_avg,2) +
#                           z_dist_to_closed_km * OR_WA_waters,   #interaction
#                         family = tweedie(),
#                         mesh = mesh_winter,
#                         spatial = "on",
#                         spatiotemporal = "ar1", 
#                         data = winter,
#                         time = "yearf")
# toc() #21min
# 
# #when seed set and depth & bottom O2 is poly:  The model may not have converged. Maximum final gradient: 0.0731648099216983
# #sanity(fit13d_winter)
# #red Xs: b_js only
# #sanity(fit13d_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
# #still b_js only
# AIC(fit13d_winter)
# #728205.8
# #summary(fit13d_winter)
# #Spatiotemporal AR1 correlation (rho): 0.48




tic()
fit13d_winter <- sdmTMB(tottraps ~ 0 + 
                          season +
                          half_month_of_seasonf +  #new
                          #month_name_f + 
                          #OR_WA_waters +
                          #WA_pot_reduction +  #not relevant in winter
                          z_SST_avg +
                          z_wind_avg +
                          poly(z_depth_point_mean,2) +
                          z_depth_point_sd +
                          z_faults_km +
                          z_dist_canyon_km +
                          z_weighted_dist +
                          z_weighted_fuel_pricegal  +  
                          z_weighted_crab_ppp +
                          poly(z_bottom_O2_avg,2) +
                          z_dist_to_closed_km * OR_WA_waters,   #interaction
                        family = tweedie(),
                        mesh = mesh_winter,
                        spatial = "on",
                        spatiotemporal = "ar1", 
                        data = winter,
                        time = "yearf")
toc() #29min

#when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.0538545880054488
#old run of model: The model may not have converged. Maximum final gradient: 0.0762587022907275
#sanity(fit13d_winter)
#red Xs: b_js, ln_tau_E, ln_kappa, ar1_phi (old run of model: b_js only)
#sanity(fit13d_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, ln_kappa (old run of model: b_js only)
AIC(fit13d_winter)
#727322.1
#summary(fit13d_winter)
#Spatiotemporal AR1 correlation (rho): 0.48

#exported model
#write_rds(fit13d_winter,here::here('DCRB_sdmTMB', 'exported model objects',"fit13d_winter_v1.rds"))
#testrds <- read_rds(here::here('DCRB_sdmTMB', 'exported model objects','fit13d_winter_v1.rds')) 

#plots <- plot_diag(fit13d_winter)

#some plots

plot_log = function(object, term) {
  g <- ggeffect(object, term, back.transform = FALSE)
  g$conf.low <- log(g$conf.low)
  g$conf.high <- log(g$conf.high)
  g$predicted <- log(g$predicted)
  plot(g)
}


p1 <- plot_log(fit13d_winter, "season [all]")
p2 <- plot_log(fit13d_winter, "half_month_of_seasonf [all]")
p3 <- plot_log(fit13d_winter, "OR_WA_waters [all]")
p4 <- plot_log(fit13d_winter, "z_SST_avg [all]")
p5 <- plot_log(fit13d_winter, "z_wind_avg [all]")
p6 <- plot_log(fit13d_winter, "z_depth_point_mean [all]")
p7 <- plot_log(fit13d_winter, "z_depth_point_sd [all]")
p8 <- plot_log(fit13d_winter, "z_faults_km [all]")
p9 <- plot_log(fit13d_winter, "z_dist_canyon_km [all]")
p10 <- plot_log(fit13d_winter, "z_weighted_dist [all]")
p11 <- plot_log(fit13d_winter, "z_weighted_fuel_pricegal [all]")
p12 <- plot_log(fit13d_winter, "z_weighted_crab_ppp [all]")
p13 <- plot_log(fit13d_winter, "z_bottom_O2_avg [all]")
p14 <- plot_log(fit13d_winter, "z_dist_to_closed_km [all]")

gridExtra::grid.arrange(p1,p2,p3,ncol=2)

gridExtra::grid.arrange(p4,p5,p6,p7,ncol=2)

gridExtra::grid.arrange(p8,p9,p13,p14,ncol=2)

gridExtra::grid.arrange(p10,p11,p12,ncol=2)

res <- residuals(fit13d_winter)
qqnorm(res,ylim=c(-5,5))
qqline(res)







# 
# tic()
# fit13e_winter <- sdmTMB(tottraps ~ 0 + 
#                           season +
#                           month_of_seasonf +  #new
#                           #month_name_f + 
#                           OR_WA_waters +
#                           #WA_pot_reduction +  #not relevant in winter
#                           z_SST_avg * z_wind_avg +   #interaction
#                           #z_wind_avg +
#                           poly(z_depth_point_mean,2) +
#                           z_depth_point_sd +
#                           z_faults_km +
#                           z_dist_canyon_km +
#                           z_weighted_dist +
#                           z_weighted_fuel_pricegal  + 
#                           z_weighted_crab_ppp +
#                           poly(z_bottom_O2_avg,2) +
#                           z_dist_to_closed_km,   
#                         family = tweedie(),
#                         mesh = mesh_winter,
#                         spatial = "on",
#                         spatiotemporal = "ar1", 
#                         data = winter,
#                         time = "yearf")
# toc() #37min
# 
# #when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.158834313364429
# #sanity(fit13e_winter)
# #red Xs: b_js, ln_tau_E, ar1_phi
# #sanity(fit13e_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
# #still b_js, ln_tau_E
# AIC(fit13e_winter)
# #728186
# #summary(fit13e_winter)
# #Spatiotemporal AR1 correlation (rho): 0.48



tic()
fit13ex_winter <- sdmTMB(tottraps ~ 0 + 
                          season +
                           half_month_of_seasonf +  #new
                          #month_name_f + 
                          OR_WA_waters +
                          #WA_pot_reduction +  #not relevant in winter
                          z_SST_avg * z_wind_avg +   #interaction
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
toc() #15min

#when seed set and depth & bottom O2 is poly: The model may not have converged. Maximum final gradient: 0.0259957443801763
#sanity(fit13ex_winter)
#red Xs: b_js, ln_phi
#sanity(fit13ex_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still b_js, ln_phi
AIC(fit13ex_winter)
#727309.2
#summary(fit13ex_winter)
#Spatiotemporal AR1 correlation (rho): 0.48





#---------------------------------






#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
##split OR and WA data

WA_all_data <- d_all_data %>% 
  filter(OR_WA_waters == 1)

mesh_WA_all_data <- make_mesh(WA_all_data, xy_cols = c("X","Y"), cutoff = 10)
mesh_WA_all_data$mesh$n



OR_all_data <- d_all_data %>% 
  filter(OR_WA_waters == 0)

mesh_OR_all_data <- make_mesh(OR_all_data, xy_cols = c("X","Y"), cutoff = 10)
mesh_OR_all_data$mesh$n



tic()
fit10c_WA_all_data <- sdmTMB(tottraps ~ 0 + 
                            season +
                            month_name_f + 
                            #OR_WA_waters +
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
                          mesh = mesh_WA_all_data,
                          spatial = "on",
                          spatiotemporal = "iid",
                          data = WA_all_data,
                          time = "month_n")
toc() #4min

#The model may not have converged. Maximum final gradient: 0.0106879779276081
#sanity(fit10c_WA_all_data)
#Red Xs: b_js, thetaf, ln_phi
#sanity(fit10c_WA_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: thetaf, ln_phi
AIC(fit10c_WA_all_data)
# 365161
#summary(fit10c_WA_all_data)



tic()
fit10c_OR_all_data <- sdmTMB(tottraps ~ 0 + 
                               season +
                               month_name_f + 
                               #OR_WA_waters +
                               #WA_pot_reduction +
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
                             mesh = mesh_OR_all_data,
                             spatial = "on",
                             spatiotemporal = "iid",
                             data = OR_all_data,
                             time = "month_n")
toc() #7min

#The model may not have converged: non-positive-definite Hessian matrix
#sanity(fit10c_OR_all_data)
#Red Xs: LOTS
#sanity(fit10c_OR_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: LOTS
AIC(fit10c_OR_all_data)
# 646805.1
#summary(fit10c_OR_all_data)



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




