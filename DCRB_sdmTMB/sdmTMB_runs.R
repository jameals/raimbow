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

set.seed(1)

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

#d$month_f <- as.factor(d$month_n)

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
#When seed set: Warning message: The model may not have converged. Maximum final gradient: 0.0102533041557145 
#some red Xs in sanity check - b_js only
#sanity(fit0_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#1x b_j red X
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
#no warning about convergence issues. some red Xs in sanity check (b_j only)
#sanity(fit1_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#No red Xs
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
toc() #13.3min

#sanity(fit2_all_data)
#no warning about convergence issues. 2 red Xs in sanity check ("`ln_kappa` gradient > 0.001"; "`ln_phi` gradient > 0.001")
#sanity(fit2_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#none if change gradient
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
                         data = d,
                         time = "yearf")
toc() #1.3min

#sanity(fit2b_all_data)
#no warning about convergence issues. No red Xs in sanity check 
#sanity(fit2b_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
AIC(fit2b_all_data)
#1047840

#-------------------------------------------------------------------------------------------------

#Just as a test, we can see if changing month to a smooth improves the fit

tic()
fit3_all_data <- sdmTMB(tottraps ~ 0 + 
                          s(month_n) + # <- new, let k be automatically selected
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
toc() #12.6min 

#sanity(fit3_all_data)
#No warnings. Some red Xs (b_j, ln_phi)
#sanity(fit3_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#1 red X - ln_phi
AIC(fit3_all_data)
#1014351 


#------------------------------------

# Just as a test, we can see if changing year/season to a smooth improves the fit

tic()
fit4_all_data <- sdmTMB(tottraps ~ 0 + 
                          month_name + 
                          s(yearn) + # <- new, let k be automatically selected
                          #season +
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
toc() #15.1min 

#sanity(fit4_all_data)
#Warning messages: The model may not have converged: non-positive-definite Hessian matrix
#Lots of red Xs
#sanity(fit4_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
# Still lots of red Xs
AIC(fit4_all_data)
#1014307 

#--------------------------------------
#change the IID spatiotemporal fields in model 2 to “AR1” to test the autoregressive structure
# Is there support for AR1 spatiotemporal fields?
# Using the best model (fit2)

tic()
fit5_all_data <- sdmTMB(tottraps ~ 0 + 
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
                        spatiotemporal = "ar1", # <- new
                        data = d,
                        time = "yearf")
toc() #30.8min

#sanity(fit5_all_data)
#Warning message: The model may not have converged. Maximum final gradient: 0.0267688760472424 
#Some red Xs (b_j, ln_tau, ln_kappa, thetaf, ln_phi,ar1_phi)
#sanity(fit5_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Some red Xs (ln_tau, ln_kappa, thetaf, ln_phi)
AIC(fit5_all_data)
#1014141
#summary(fit5_all_data)
#Spatiotemporal AR1 correlation (rho): 0.53

#--------------------------------------
#Adding spatial and spatiotemporal fields (months)

# switch indexing of spatiotemporal fields to “month_name” and again can try the spatiotemporal fields as IID or AR1.

tic()
fit6_all_data <- update(fit2_all_data,
               time = "month_n")
toc()  #13.7min

#sanity(fit6_all_data)
# no warning messages. some red Xs (ln_tau, ln_kappa, thetaf)
#sanity(fit6_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#
AIC(fit6_all_data)
#1012696






tic()
fit7_all_data <- update(fit2_all_data,
               time = "month_n",
               spatiotemporal = "ar1")
toc()  #43min

#sanity(fit7_all_data)
#Warning message:The model may not have converged. Maximum final gradient: 12.2876279413012
#some red Xs (Non-linear minimizer did not converge: do not trust this model, b_js, ln_tau, ar1_phi)
#sanity(fit7_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#some red Xs (Non-linear minimizer did not converge: do not trust this model, b_js, ln_tau, ar1_phi)
AIC(fit7_all_data)
#1012123
#summary(fit7_all_data) #what is ar1 rho value?
#Spatiotemporal AR1 correlation (rho): 0.97



#index by half_month step
d$half_month_n <- 1
d$half_month_n[which(d$half_month=="December_2")] = 2
d$half_month_n[which(d$half_month=="January_1")] = 3
d$half_month_n[which(d$half_month=="January_2")] = 4
d$half_month_n[which(d$half_month=="February_1")] = 5
d$half_month_n[which(d$half_month=="February_2")] = 6
d$half_month_n[which(d$half_month=="March_1")] = 7
d$half_month_n[which(d$half_month=="March_2")] = 8
d$half_month_n[which(d$half_month=="April_1")] = 9
d$half_month_n[which(d$half_month=="April_2")] = 10
d$half_month_n[which(d$half_month=="May_1")] = 11
d$half_month_n[which(d$half_month=="May_2")] = 12
d$half_month_n[which(d$half_month=="June_1")] = 13
d$half_month_n[which(d$half_month=="June_2")] = 14
d$half_month_n[which(d$half_month=="July_1")] = 15
d$half_month_n[which(d$half_month=="July_2")] = 16
d$half_month_n[which(d$half_month=="August_1")] = 17
d$half_month_n[which(d$half_month=="August_2")] = 18
d$half_month_n[which(d$half_month=="September_1")] = 19


tic()
fit8_all_data <- update(fit2_all_data,
                        time = "half_month_n") #as numeric
toc()  #15.5min

#sanity(fit8_all_data)
# no warnings, ! red Xs (b_js)
#sanity(fit8_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#no red Xs
AIC(fit8_all_data)
#1012728
#results are same if half_month is factor AS LONG AS LEVELS ARE ORDERED CORRECTLY

#--------------------------------------

tic()
fit9_all_data <- sdmTMB(tottraps ~ 0 + 
                          s(half_month_n) + # <- new, let k be automatically selected
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
toc() #12.7min 

#sanity(fit9_all_data)
#Warning message: The model may not have converged. Maximum final gradient: 0.0772079237766903
#Red Xs: b_j, thetaf, ln_phi, ln_smooth_sigma
#sanity(fit9_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: thetaf, ln_phi,
AIC(fit9_all_data)
# 1013962


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
#no warning message on second run (seed set)
#No red Xs when seed set
#sanity(fit0_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#No red Xs
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
#no warning about convergence issues. 2 red Xs in sanity check but only b_js 
#sanity(fit1_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#no red Xs
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
#sanity(fit2_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
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
#sanity(fit1b_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
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
#Warning message: The model may not have converged. Maximum final gradient: 0.0108471688968166
#red Xs: ln_tau, ln_kappa, ln_phi
#sanity(fit2b_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#red Xs: ln_tau, ln_phi
AIC(fit2b_summer)
#271531

#-------------------------------------

#Just as a test, we can see if changing month to a smooth improves the fit

tic()
fit3_summer <- sdmTMB(tottraps ~ 0 + 
                          s(month_n, k = 4) + # <- new,
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
                        data = summer,
                        time = "yearf")
toc() #4.8min 
#with summmer model if try to let k be automatically selected, get error:
#Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : A term has fewer unique covariate combinations than specified maximum degrees of freedom

#sanity(fit3_summer)
#no warning messages. Red Xs: thetaf, ln_phi
#sanity(fit3_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#No red Xs
AIC(fit3_summer)
#265953

#-------------------------------------

# Just as a test, we can see if changing year/season to a smooth improves the fit

tic()
fit4_summer <- sdmTMB(tottraps ~ 0 + 
                          month_name + 
                          s(yearn, k = 10) + # <- new, let k be automatically selected
                          #season +
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
toc() #5.9min 
#this time fine to let k be automatically selected
#Warning message:  The model may not have converged: extreme or very small eigen values detected
#sanity(fit4_summer)
#lots of red Xs
#sanity(fit4_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still lots
AIC(fit4_summer)
#265938

#-------------------------------------
#change the IID spatiotemporal fields in model 2 to “AR1” to test the autoregressive structure
# Is there support for AR1 spatiotemporal fields?
# Using the best model (fit2)

tic()
fit5_summer <- sdmTMB(tottraps ~ 0 + 
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
                        spatiotemporal = "ar1", # <- new
                        data = summer,
                        time = "yearf")
toc() #9.4min

#sanity(fit5_summer)
#no warnings. No red Xs
#sanity(fit5_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#
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

#sanity(fit6_summer)
#no warnings.  No red Xs
#sanity(fit6_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#
AIC(fit6_summer)
#270786






tic()
fit7_summer <- update(fit2_summer,
                        time = "month_n",
                        spatiotemporal = "ar1")
toc()  #7.5min

#sanity(fit7_summer)
#no warnings. Some red Xs (b_js, ln_kappa, ln_tau, sigma_O, ar1_phi)
#sanity(fit7_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still red Xs: sigma_O
AIC(fit7_summer)
#270702
#summary(fit7_summer) #what is ar1 rho value?
#Spatiotemporal AR1 correlation (rho): 0.96

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

#sanity(fit8_summer)
# No warnings. No red Xs
#sanity(fit8_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#
AIC(fit8_summer)
#270583


#-------------------------------------














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
#no warning about convergence issues. Some red Xs in sanity check (b_js, thetaf, ln_phi)
#sanity(fit0_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still X for ln_phi
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
toc() #6.7min

#sanity(fit1_winter)
#Warning message:The model may not have converged. Maximum final gradient: 0.0129679188577158
#Couple red Xs in sanity check (b_j, ln_tau, ln_kappa, thetaf, ln_phi)
#sanity(fit1_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still X for ln_phi and b_js
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
#Warning message: The model may not have converged. Maximum final gradient: 0.014876525269969
#Some red Xs in sanity check (b_js, ln_tau, thetaf, ln_phi)
#sanity(fit2_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still Xs for b_j, ln_tau, thetaf, ln_phi
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
#sanity(fit1b_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
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
#sanity(fit2b_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
AIC(fit2b_winter)
#734885
#--------------------------------------

#Just as a test, we can see if changing month to a smooth improves the fit

tic()
fit3_winter <- sdmTMB(tottraps ~ 0 + 
                        s(month_n, k = 4) + # <- new,
                        season +
                        #month_name + 
                        OR_WA_waters +
                        #WA_pot_reduction +   #not relevant in winter
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
toc() #7min 

#sanity(fit3_winter)
#Warning message:The model may not have converged. Maximum final gradient: 0.0136587717055114
#X for thetaf, ln_phi
#sanity(fit3_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still X for ln_phi
AIC(fit3_winter)
#728448

#--------------------------------------

# Just as a test, we can see if changing year/season to a smooth improves the fit

tic()
fit4_winter <- sdmTMB(tottraps ~ 0 + 
                        month_name + 
                        s(yearn, k = 10) + # <- new, let k be automatically selected // , k = 10
                        #season +
                        OR_WA_waters +
                        #WA_pot_reduction +     #not relevant in winter
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
toc() #7.8min 
#this time fine to let k be automatically selected. doesn't matter if specify k=10
#
#sanity(fit4_winter)
#Warning message: The model may not have converged: non-positive-definite Hessian matrix. lots of red Xs
#sanity(fit4_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still lots of Xs
AIC(fit4_winter)
#728422


#--------------------------------------

#change the IID spatiotemporal fields in model 2 to “AR1” to test the autoregressive structure
# Is there support for AR1 spatiotemporal fields?
# Using the best model (fit2)

tic()
fit5_winter <- sdmTMB(tottraps ~ 0 + 
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
                      spatiotemporal = "ar1", # <- new
                      data = winter,
                      time = "yearf")
toc() #min

#sanity(fit5_winter)
#Warning message: The model may not have converged. Maximum final gradient: 0.0102205309983354
#some red Xs b_j, ln_tau, thetaf, ln_phi, ar1_phi
#sanity(fit5_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still Xs for thetaf and ln_phi
AIC(fit5_winter)
#728291
#summary(fit5_winter)
#Spatiotemporal AR1 correlation (rho): 0.48


#--------------------------------------

#Adding spatial and spatiotemporal fields (months)

# switch indexing of spatiotemporal fields to “month_name” and again can try the spatiotemporal fields as IID or AR1.

tic()
fit6_winter <- update(fit2_winter,
                      time = "month_n")
toc()  #7.3min

#sanity(fit6_winter)
#no warnings. Some red Xs: thetaf and ln_phi
#sanity(fit6_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#No Xs
AIC(fit6_winter)
#735079


###as fit2 had issues, but fit2b didn't, try that one as well
tic()
fit6b_winter <- update(fit2b_winter,
                      time = "month_n")
toc()  #0.8min
#sanity(fit6b_winter)
#No warnings, only 1 X: thetaf
#sanity(fit6b_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still X for thetaf
AIC(fit6b_winter)
#738918.1
###but it's not an improvement on fit6



tic()
fit7_winter <- update(fit2_winter,
                      time = "month_n",
                      spatiotemporal = "ar1")
toc()  #9.2min

#sanity(fit7_winter)
#No warnings, some red Xs (b_j, thetaf, ln_phi)
#sanity(fit7_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still 1x b_j, and ln_phi
AIC(fit7_winter)
#734859
#summary(fit7_winter) #what is ar1 rho value?
#Spatiotemporal AR1 correlation (rho): 


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

#sanity(fit8_winter)
#No warnings. Red Xs: b_j and ln_phi
#sanity(fit8_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still X for ln_phi
AIC(fit8_winter)
#734868


#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------





#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------


