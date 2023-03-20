#sdmTMB model selection via AIC - all data


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

set.seed(123)

#-------------------------------------------------------------------------------------------------

#read in all data - the version where z-scoring is done across all data
all_data <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_all_data_20230315.rds'))
glimpse(all_data) 

all_data$month_name_f <- factor(all_data$month_name, levels = c("December", "January", "February", "March", "April",
                                                                "May", "June", "July", "August", "September"))

# Add UTM columns (zone 10)
all_data = add_utm_columns(all_data, ll_names = c("grd_x", "grd_y"))


mesh_all_data <- make_mesh(all_data, xy_cols = c("X","Y"), cutoff = 10)
mesh_all_data$mesh$n


#-------------------------------------------------------------------------------------------------

#covariates only


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
                      mesh = mesh_all_data,
                      spatial = "off",
                      spatiotemporal = "off",
                      data = all_data,
                      time = "yearn")
toc() #4min

# when seed set and no polynomials
#time = "yearn", month_name_f is fixed effect, and include WA_pot_reduction
#The model may not have converged. Maximum final gradient: 0.0119600989253428. 
#sanity(fit1_all_data)
#red Xs: Non-linear minimizer did not converge: do not trust this model!; b_js, thetaf
#sanity(fit1_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#still Non-linear minimizer did not converge: do not trust this model!; b_js, thetaf
AIC(fit1_all_data)
#AIC: 1075784
summary(fit1_all_data)


#EXPORT THIS MODEL
#write_rds(fit1_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit1_all_data.rds"))

#test the optimisation run
#fit1b_all_data <- run_extra_optimization(fit1_all_data, nlminb_loops = 0, newton_loops = 1)
#no message came up after. sanity checks: Non-linear minimizer did not converge: do not trust this model!
#AIC: 1075784 --  so same as before
#fit1c_all_data <- run_extra_optimization(fit1b_all_data, nlminb_loops = 0, newton_loops = 1)
##still doesn't fix it

#-------------------------------------------------------------------------------------------------

#covariates + spatial fields (no spatiotemporal fields)

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
                      mesh = mesh_all_data,
                      spatial = "on",
                      spatiotemporal = "off",
                      data = all_data,
                      time = "yearn")
toc() #12min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.156917271175464 
#sanity(fit2_all_data)
#red Xs: b_js, thetaf, ln_phi
#sanity(fit2_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#red Xs: b_js, thetaf, ln_phi
AIC(fit2_all_data)
#1030651
summary(fit2_all_data)

#EXPORT THIS MODEL
#write_rds(fit2_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit2_all_data.rds"))



#-------------------------------------------------------------------------------------------------

#covariates + spatial fields + spatiotemporal fields (iid)

tic()
fit3_all_data <- sdmTMB(tottraps ~ 0 + 
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
                      mesh = mesh_all_data,
                      spatial = "on",
                      spatiotemporal = "iid",
                      data = all_data,
                      time = "yearn")
toc() #14min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.111572028482209. 
#sanity(fit3_all_data)
#Red Xs: b_js, ln_tau_O
#sanity(fit3_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, ln_tau_O
AIC(fit3_all_data)
#1017984
summary(fit3_all_data)

#EXPORT THIS MODEL
#write_rds(fit3_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit3_all_data.rds"))

#just as a test do the optimisation run
#fit3b_all_data <- run_extra_optimization(fit3_all_data, nlminb_loops = 0, newton_loops = 1)
#no message came up after. basic sanity check no Xs (so no for the second check either)
#AIC: 1017984 - same as before

#-------------------------------------------------------------------------------------------------

#covariates + spatial fields + spatiotemporal fields (ar1)

tic()
fit4_all_data <- sdmTMB(tottraps ~ 0 + 
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
                      mesh = mesh_all_data,
                      spatial = "on",
                      spatiotemporal = "ar1",
                      data = all_data,
                      time = "yearn")
toc() #46min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.13803205506418. 
#sanity(fit4_all_data)
#Red Xs: b_js, ln_tau_E, thetaf
#sanity(fit4_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, ln_tau_E, thetaf
AIC(fit4_all_data)
#1017797
summary(fit4_all_data)

#EXPORT THIS MODEL
#write_rds(fit4_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit4_all_data.rds"))

#fit4b_all_data <- run_extra_optimization(fit4_all_data, nlminb_loops = 0, newton_loops = 1)
#no warnings
#no red Xs
#AIC: 1017797 --  same as before
#write_rds(fit4b_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit4b_all_data.rds"))



#-------------------------------------------------------------------------------------------------

#model with only spatial and spatiotemporal fields (seasons), no covariates

tic()
fit5_all_data <- sdmTMB(tottraps ~ 0,
                      family = tweedie(),
                      mesh = mesh_all_data,
                      spatial = "on",
                      spatiotemporal = "off",
                      data = all_data,
                      time = "yearn")
toc() #0.6min

# when seed set and no polynomials
# no warnings
#sanity(fit5_all_data)
#no red Xs
#sanity(fit5_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#no red Xs
AIC(fit5_all_data)
#1058927
summary(fit5_all_data)

#EXPORT THIS MODEL
#write_rds(fit5_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit5_all_data.rds"))



tic()
fit6_all_data <- sdmTMB(tottraps ~ 0,
                      family = tweedie(),
                      mesh = mesh_all_data,
                      spatial = "on",
                      spatiotemporal = "iid",
                      data = all_data,
                      time = "yearn")
toc() #1.2min

# when seed set and no polynomials
# no warnings
#sanity(fit6_all_data)
# ln_tau_O, ln_kappa
#sanity(fit6_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
# none
AIC(fit6_all_data)
#1047840
summary(fit6_all_data)

#EXPORT THIS MODEL
#write_rds(fit6_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit6_all_data.rds"))




tic()
fit7_all_data <- sdmTMB(tottraps ~ 0,
                      family = tweedie(),
                      mesh = mesh_all_data,
                      spatial = "on",
                      spatiotemporal = "ar1",
                      data = all_data,
                      time = "yearn")
toc() #4min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.17488861844256. 
#sanity(fit7_all_data)
#ln_tau_O, ln_tau_E, ln_kappa, thetaf, ln_phi, ar1_phi
#sanity(fit7_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#ln_tau_O, ln_tau_E, thetaf, ln_phi
AIC(fit7_all_data)
#1047657
summary(fit7_all_data)

#EXPORT THIS MODEL
#write_rds(fit7_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit7_all_data.rds"))

#-------------------------------------------------------------------------------------------------


#out of the first 7 models, fit4_all_data is the best
#covariates, spatial and s-t fields (ar1)
#using that as a base test different structures 
#(spatiotemporal fields indexed by year, month, etc; AR1 vs IID; different month variables as fixed effect)
# -- two test above where same model apart from ar1 vs iid --> ar1 is best, no need to test further


#-------------------------------------------------------------------------------------------------

#compare different time = "xx" indexes (fit4_summer is base model)

tic()
fit8_all_data <- sdmTMB(tottraps ~ 0 + 
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
                      mesh = mesh_all_data,
                      spatial = "on",
                      spatiotemporal = "ar1",
                      data = all_data,
                      time = "month_n")
toc() #32min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.103838705905338.
#sanity(fit8_all_data)
#red Xs: b_js, ln_kappa
#sanity(fit8_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js only
AIC(fit8_all_data)
#1015302
summary(fit8_all_data)

#EXPORT THIS MODEL
#write_rds(fit8_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit8_all_data.rds"))

#fit8b_all_data <- run_extra_optimization(fit8_all_data, nlminb_loops = 0, newton_loops = 1)
#no warning message
#`sigma_O` is smaller than 0.01
#AIC: 1015302 --  same as before
#write_rds(fit8b_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit8b_all_data.rds"))

#fit8c_all_data <- run_extra_optimization(fit8b_all_data, nlminb_loops = 0, newton_loops = 1)
#no warning message
#`sigma_O` is smaller than 0.01
#AIC: 1015302 --  same as before





tic()
fit9_all_data <- sdmTMB(tottraps ~ 0 + 
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
                      mesh = mesh_all_data,
                      spatial = "on",
                      spatiotemporal = "ar1",
                      data = all_data,
                      time = "month_of_season")
toc() #23min

# when seed set and no polynomials
# Warning messages:
#1: In sqrt(diag(cov)) : NaNs produced
#2: The model may not have converged: non-positive-definite Hessian matrix. 
#sanity(fit9_all_data)
#Red Xs: Non-positive-definite Hessian matrix: model may not have converged, b_js, ln_kappa, ln_tau_O, log_sigma_O, sigma_O
#sanity(fit9_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: Non-positive-definite Hessian matrix: model may not have converged, b_js, ln_kappa, ln_tau_O, log_sigma_O, sigma_O
AIC(fit9_all_data)
#1014766
summary(fit9_all_data)

#EXPORT THIS MODEL
#write_rds(fit9_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit9_all_data.rds"))

#fit9b_all_data <- run_extra_optimization(fit9_all_data, nlminb_loops = 0, newton_loops = 1)
#Warning messages:
#1: In sqrt(diag(cov)) : NaNs produced
#2: The model may not have converged: non-positive-definite Hessian matrix.
#Red Xs: Non-positive-definite Hessian matrix: model may not have converged, ln_tau_O, log_sigma_O, sigma_O
#AIC: 1014766 --  same as before
#write_rds(fit9b_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit9b_all_data.rds"))

#fit9c_all_data <- run_extra_optimization(fit9b_all_data, nlminb_loops = 0, newton_loops = 1)
#Warning messages:
#1: In sqrt(diag(cov)) : NaNs produced
#2: The model may not have converged: non-positive-definite Hessian matrix. 
#Red Xs: Non-positive-definite Hessian matrix: model may not have converged, ln_tau_O, log_sigma_O, sigma_O
#AIC: 1014766 --  same as before
#write_rds(fit9c_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit9c_all_data.rds"))






tic()
fit10_all_data <- sdmTMB(tottraps ~ 0 + 
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
                       mesh = mesh_all_data,
                       spatial = "on",
                       spatiotemporal = "ar1",
                       data = all_data,
                       time = "half_month_of_season")
toc() #1.5h

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 1.22653772950877.
#sanity(fit10_all_data)
#Red Xs: Non-linear minimizer did not converge: do not trust this model!; b_js, thetaf, ln_phi, ar1_phi
#sanity(fit10_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: Non-linear minimizer did not converge: do not trust this model!; b_js, thetaf, ln_phi, ar1_phi
AIC(fit10_all_data)
#1012781
summary(fit10_all_data)

#EXPORT THIS MODEL
#write_rds(fit10_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit10_all_data.rds"))

#so far the bet, but not converging so test  the optimisation run
#fit10b_all_data <- run_extra_optimization(fit10_all_data, nlminb_loops = 0, newton_loops = 1)
#Warning message: The model may not have converged. Maximum final gradient: 1.22653772950877.
#still same red Xs
#AIC: 1013634 --  has changed
#write_rds(fit10b_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit10b_all_data.rds"))

#fit10c_all_data <- run_extra_optimization(fit10b_all_data, nlminb_loops = 0, newton_loops = 1)
#Warning message: The model may not have converged. Maximum final gradient: 1.22653772950877. 
#still same red Xs
#AIC: 1013048 --  has changed
#write_rds(fit10c_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit10c_all_data.rds"))

#-------------------------------------------------------------------------------------------------

#after finding the best structure: 
#fit 10 and 9 have best AICs, but they don't converge
#so the best one is fit 8 (or 4...?)Eric says not to worry about the sigma_O


#then test different month variables as fixed effect


#-------------------------------------------------------------------------------------------------


#compare different month variables as fixed effect (fit8_all_data is base model - where fixed effect is month_name_f)

tic()
fit11_all_data <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_of_seasonf  + 
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
                        mesh = mesh_all_data,
                        spatial = "on",
                        spatiotemporal = "ar1",
                        data = all_data,
                        time = "month_n")
toc() #41min 

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.93750544410889. 
#sanity(fit11_all_data)
#Red Xs: Non-linear minimizer did not converge: do not trust this model! b_js, ln_tau_E, thetaf, ln_phi, ar1_phi
#sanity(fit11_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: Non-linear minimizer did not converge: do not trust this model! b_js, ln_tau_E, thetaf, ln_phi, ar1_phi
AIC(fit11_all_data)
#1014815
summary(fit11_all_data)

#EXPORT THIS MODEL
#write_rds(fit11_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit11_all_data.rds"))

#OPTIMIZATION
#fit11b_all_data <- run_extra_optimization(fit11_all_data, nlminb_loops = 0, newton_loops = 1)
#Warning: The model may not have converged. Maximum final gradient: 0.93750544410889.
#Non-linear minimizer did not converge: do not trust this model! b_js, ln_tau_E, thetaf, ln_phi, ar1_phi
#AIC: 1016610 -- some change
#write_rds(fit11b_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit11b_all_data.rds"))

#fit11c_all_data <- run_extra_optimization(fit11b_all_data, nlminb_loops = 0, newton_loops = 1)
#The model may not have converged. Maximum final gradient: 0.93750544410889. 
#Non-linear minimizer did not converge: do not trust this model! b_js, ln_tau_E, thetaf, ln_phi, ar1_phi
#AIC: 1016610 -- same as 11b
#write_rds(fit11c_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit11c_all_data.rds"))







tic()
fit12_all_data <- sdmTMB(tottraps ~ 0 + 
                           season +
                           half_month_of_seasonf  + 
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
                         mesh = mesh_all_data,
                         spatial = "on",
                         spatiotemporal = "ar1",
                         data = all_data,
                         time = "month_n")
toc() #42min 

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 1.66891020941317.  
#sanity(fit12_all_data)
#Red Xs: Non-linear minimizer did not converge: do not trust this model! b_js, ln_kappa, thetaf, ln_phi, ar1_phi, ln_tau_O, sigma_O
#sanity(fit12_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: Non-linear minimizer did not converge: do not trust this model! b_js, ln_kappa, thetaf, ln_phi, ar1_phi, ln_tau_O, sigma_O
AIC(fit12_all_data)
#1013826
summary(fit12_all_data)

#EXPORT THIS MODEL
#write_rds(fit12_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit12_all_data.rds"))

#OPTIMIZATION
#fit12b_all_data <- run_extra_optimization(fit12_all_data, nlminb_loops = 0, newton_loops = 1)
#The model may not have converged. Maximum final gradient: 0.209392765912892. 
#Non-linear minimizer did not converge: do not trust this model! b_js, ln_tau_E, ln_kappa, ar1_phi, `ln_tau_O` standard error may be large, sigma_O < 0.01
#AIC: 1013826 -- no change
#write_rds(fit12b_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit12b_all_data.rds"))

#fit12c_all_data <- run_extra_optimization(fit12b_all_data, nlminb_loops = 0, newton_loops = 1)
# no warning message
#Non-linear minimizer did not converge: do not trust this model! ln_tau_O, sigma_O
#AIC: 1013826 - no change
#write_rds(fit12c_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit12c_all_data.rds"))



################################################################
##building on fit8 did not work as models did not converge




#-------------------------------------------------------------------------------------------------

#Eric's suggestion: refit the top model with no spatial variation 

tic()
fit10X_all_data <- sdmTMB(tottraps ~ 0 + 
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
                         mesh = mesh_all_data,
                         spatial = "off",
                         spatiotemporal = "ar1",
                         data = all_data,
                         time = "half_month_of_season")
toc() #39min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.135915228357355. 
#sanity(fit10X_all_data)
#Red Xs: b_js, ln_tau_E, ln_kappa, ar1_phi
#sanity(fit10X_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, ln_tau_E, ln_kappa, ar1_phi
AIC(fit10X_all_data)
#1012757
summary(fit10X_all_data)

#EXPORT THIS MODEL
#write_rds(fit10X_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit10X_all_data.rds"))


#fit10Xb_all_data <- run_extra_optimization(fit10X_all_data, nlminb_loops = 0, newton_loops = 1)
#no warnings
#No red Xs in sanity check
#AIC: 1012757 -- same as before
#write_rds(fit10Xb_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit10Xb_all_data.rds"))

#-------------------------------------------------------------------------------------------------


#compare different month variables as fixed effect (fit10X_all_data is base model - where fixed effect is month_name_f)



tic()
fit11_all_data <- sdmTMB(tottraps ~ 0 + 
                            season +
                            month_of_seasonf + 
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
                          mesh = mesh_all_data,
                          spatial = "off",
                          spatiotemporal = "ar1",
                          data = all_data,
                          time = "half_month_of_season")
toc() #26min 

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.632939320765626. 
#sanity(fit11_all_data)
#Red Xs: b_js, thetaf, ln_phi
#sanity(fit11_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, thetaf, ln_phi
AIC(fit11_all_data)
#1012893
summary(fit11_all_data)

#EXPORT THIS MODEL
#write_rds(fit11_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit11_all_data.rds"))

#OPTIMIZATION
#fit11b_all_data <- run_extra_optimization(fit11_all_data, nlminb_loops = 0, newton_loops = 1)
#no warnings
#no Red Xs
#AIC: 1012893 -- still same
#write_rds(fit11b_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit11b_all_data.rds"))








tic()
fit12_all_data <- sdmTMB(tottraps ~ 0 + 
                           season +
                           half_month_of_seasonf + 
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
                         mesh = mesh_all_data,
                         spatial = "off",
                         spatiotemporal = "ar1",
                         data = all_data,
                         time = "half_month_of_season")
toc() #64min 

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 5.15304484350941.
#sanity(fit12_all_data)
#Red Xs: Non-linear minimizer did not converge: do not trust this model! b_js, thetaf, ln_phi, ar1_phi
#sanity(fit12_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: Non-linear minimizer did not converge: do not trust this model! b_js, thetaf, ln_phi, ar1_phi
AIC(fit12_all_data)
#1012955
summary(fit12_all_data)

#EXPORT THIS MODEL
#write_rds(fit12_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit12_all_data.rds"))

#OPTIMIZATION
#fit12b_all_data <- run_extra_optimization(fit12_all_data, nlminb_loops = 0, newton_loops = 1)
#The model may not have converged. Maximum final gradient: 5.15304484350941. 
#Non-linear minimizer did not converge: do not trust this model! b_js, thetaf, ln_phi, ar1_phi
#AIC: 1014363 -- slightly different
#write_rds(fit12b_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit12b_all_data.rds"))



#-------------------------------------------------------------------------------------------------


#then test polynomial terms and interactions
#for polynomials only test the chosen term: depth
#but try out all interactions

# fit10X_all_data  was best
#the base is fit10X_all_data as that had the best AIC

#-------------------------------------------------------------------------------------------------


#polynomial term

  
tic()
fit13_all_data <- sdmTMB(tottraps ~ 0 + 
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
                          mesh = mesh_all_data,
                          spatial = "off",
                          spatiotemporal = "ar1",
                          data = all_data,
                          time = "half_month_of_season")
toc() #45min

# when seed set 
# The model may not have converged. Maximum final gradient: 0.0976125960974379.
#sanity(fit13_all_data)
#b_js, ln_phi
#sanity(fit13_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js, ln_phi
AIC(fit13_all_data)
#1010030
summary(fit13_all_data)

#EXPORT THIS MODEL
#write_rds(fit13_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit13_all_data.rds"))

###CHECK OPTIMIZATION
#fit13b_all_data <- run_extra_optimization(fit13_all_data, nlminb_loops = 0, newton_loops = 1)
#no warnings
#no red Xs
#AIC: 1010030 -- same as before
#write_rds(fit13b_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit13b_all_data.rds"))

plot_log = function(object, term) {
  g <- ggeffect(object, term, back.transform = FALSE)
  g$conf.low <- log(g$conf.low)
  g$conf.high <- log(g$conf.high)
  g$predicted <- log(g$predicted)
  plot(g)
}


p1 <- plot_log(fit13b_all_data, "season [all]")
p2 <- plot_log(fit13b_all_data, "month_name_f [all]")
p3 <- plot_log(fit13b_all_data, "OR_WA_waters [all]")
p35 <- plot_log(fit13b_all_data, "WA_pot_reduction [all]")
p4 <- plot_log(fit13b_all_data, "z_SST_avg [all]")
p5 <- plot_log(fit13b_all_data, "z_wind_avg [all]")
p6 <- plot_log(fit13b_all_data, "z_depth_point_mean [all]")
p7 <- plot_log(fit13b_all_data, "z_depth_point_sd [all]")
p8 <- plot_log(fit13b_all_data, "z_faults_km [all]")
p9 <- plot_log(fit13b_all_data, "z_dist_canyon_km [all]")
p10 <- plot_log(fit13b_all_data, "z_weighted_dist [all]")
p11 <- plot_log(fit13b_all_data, "z_weighted_fuel_pricegal [all]")
p12 <- plot_log(fit13b_all_data, "z_weighted_crab_ppp [all]")
p13 <- plot_log(fit13b_all_data, "z_bottom_O2_avg [all]")
p14 <- plot_log(fit13b_all_data, "z_dist_to_closed_km [all]")

gridExtra::grid.arrange(p1,p2,p3,p35,ncol=2)

gridExtra::grid.arrange(p4,p5,p6,p7,ncol=2)

gridExtra::grid.arrange(p8,p9,p13,p14,ncol=2)

gridExtra::grid.arrange(p10,p11,p12,ncol=2)

res <- residuals(fit13b_all_data)
qqnorm(res,ylim=c(-5,5))
qqline(res)

#-------------------------------------------------------------------------------------------------

#interactions


tic()
fit14_all_data <- sdmTMB(tottraps ~ 0 + 
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
                           #z_weighted_fuel_pricegal + #part of interaction term
                           z_weighted_crab_ppp * z_weighted_fuel_pricegal +
                           z_bottom_O2_avg +
                           z_dist_to_closed_km,
                         family = tweedie(),
                         mesh = mesh_all_data,
                         spatial = "off",
                         spatiotemporal = "ar1",
                         data = all_data,
                         time = "half_month_of_season")
toc() #64min

# when seed set 
# The model may not have converged. Maximum final gradient: 28.4330017989434.
#sanity(fit14_all_data)
#Non-linear minimizer did not converge: do not trust this model! b_js, thetaf, ln_phi, ar1_phi
#sanity(fit14_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Non-linear minimizer did not converge: do not trust this model! b_js, thetaf, ln_phi, ar1_phi
AIC(fit14_all_data)
#1010267
summary(fit14_all_data)

#EXPORT THIS MODEL
#write_rds(fit14_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit14_all_data.rds"))





tic()
fit15_all_data <- sdmTMB(tottraps ~ 0 + 
                           season +
                           month_name_f + 
                           OR_WA_waters +
                           WA_pot_reduction + 
                           #z_SST_avg + #part of interaction term
                           z_wind_avg * z_SST_avg +
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
                         mesh = mesh_all_data,
                         spatial = "off",
                         spatiotemporal = "ar1",
                         data = all_data,
                         time = "half_month_of_season")
toc() #61min

# when seed set 
# The model may not have converged. Maximum final gradient: 0.324220189398519. 
#sanity(fit15_all_data)
#b_js, ln_tau_E, ln_kappa, ar1_phi
#sanity(fit15_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js, ln_tau_E, ln_kappa, ar1_phi
AIC(fit15_all_data)
#1009988
summary(fit15_all_data)

#EXPORT THIS MODEL
#write_rds(fit15_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit15_all_data.rds"))

###CHECK OPTIMIZATION
#fit15b_all_data <- run_extra_optimization(fit15_all_data, nlminb_loops = 0, newton_loops = 1)
#no warnings
#no red Xs
#AIC: 1009988 -- same as before
#write_rds(fit15b_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit15b_all_data.rds"))

#this one is the best all data model

#plots <- plot_diag(fit15b_all_data)
#Warning message:   Unknown or uninitialised column: `omega_s`. 
#--> fit15b_all_data doesn't include spatial fields (only spatio-temporal) - so no plot[[23]]

plot_log = function(object, term) {
  g <- ggeffect(object, term, back.transform = FALSE)
  g$conf.low <- log(g$conf.low)
  g$conf.high <- log(g$conf.high)
  g$predicted <- log(g$predicted)
  plot(g)
}


p1 <- plot_log(fit15b_all_data, "season [all]")
p2 <- plot_log(fit15b_all_data, "month_name_f [all]")
p3 <- plot_log(fit15b_all_data, "OR_WA_waters [all]")
p35 <- plot_log(fit15b_all_data, "WA_pot_reduction [all]")
p4 <- plot_log(fit15b_all_data, "z_SST_avg [all]")
p5 <- plot_log(fit15b_all_data, "z_wind_avg [all]")
p6 <- plot_log(fit15b_all_data, "z_depth_point_mean [all]")
p7 <- plot_log(fit15b_all_data, "z_depth_point_sd [all]")
p8 <- plot_log(fit15b_all_data, "z_faults_km [all]")
p9 <- plot_log(fit15b_all_data, "z_dist_canyon_km [all]")
p10 <- plot_log(fit15b_all_data, "z_weighted_dist [all]")
p11 <- plot_log(fit15b_all_data, "z_weighted_fuel_pricegal [all]")
p12 <- plot_log(fit15b_all_data, "z_weighted_crab_ppp [all]")
p13 <- plot_log(fit15b_all_data, "z_bottom_O2_avg [all]")
p14 <- plot_log(fit15b_all_data, "z_dist_to_closed_km [all]")

gridExtra::grid.arrange(p1,p2,p3,p35,ncol=2)

gridExtra::grid.arrange(p4,p5,p6,p7,ncol=2)

gridExtra::grid.arrange(p8,p9,p13,p14,ncol=2)

gridExtra::grid.arrange(p10,p11,p12,ncol=2)

res <- residuals(fit15b_all_data)
qqnorm(res,ylim=c(-5,5))
qqline(res)






tic()
fit16_all_data <- sdmTMB(tottraps ~ 0 + 
                           season +
                           month_name_f + 
                           #OR_WA_waters + #part of interaction term
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
                           OR_WA_waters * z_dist_to_closed_km,
                         family = tweedie(),
                         mesh = mesh_all_data,
                         spatial = "off",
                         spatiotemporal = "ar1",
                         data = all_data,
                         time = "half_month_of_season")
toc() #67min

# when seed set 
# The model may not have converged. Maximum final gradient: 20.4754653297611. 
#sanity(fit16_all_data)
#Non-linear minimizer did not converge: do not trust this model! b_js, thetaf, ln_phi, ar1_phi
#sanity(fit16_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Non-linear minimizer did not converge: do not trust this model! b_js, thetaf, ln_phi, ar1_phi
AIC(fit16_all_data)
#1011220
summary(fit16_all_data)

#EXPORT THIS MODEL
#write_rds(fit16_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit16_all_data.rds"))








tic()
fit17_all_data <- sdmTMB(tottraps ~ 0 + 
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
                           #z_weighted_dist + #part of interaction term
                           z_weighted_dist * z_weighted_fuel_pricegal + 
                           z_weighted_crab_ppp +
                           z_bottom_O2_avg +
                           z_dist_to_closed_km,
                         family = tweedie(),
                         mesh = mesh_all_data,
                         spatial = "off",
                         spatiotemporal = "ar1",
                         data = all_data,
                         time = "half_month_of_season")
toc() #65min

# when seed set 
# The model may not have converged. Maximum final gradient: 17.6396165929671.
#sanity(fit17_all_data)
#Non-linear minimizer did not converge: do not trust this model! b_js, ar1_phi
#sanity(fit17_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#Non-linear minimizer did not converge: do not trust this model! b_js, ar1_phi
AIC(fit17_all_data)
#1010061
summary(fit17_all_data)

#EXPORT THIS MODEL
#write_rds(fit17_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit17_all_data.rds"))






tic()
fit18_all_data <- sdmTMB(tottraps ~ 0 + 
                           season +
                           month_name_f + 
                           OR_WA_waters + 
                           WA_pot_reduction + 
                           z_SST_avg + 
                           #z_wind_avg + #part of interaction term
                           poly(z_depth_point_mean,2) +
                           z_depth_point_sd +
                           z_faults_km +
                           z_dist_canyon_km +
                           z_weighted_dist + 
                           z_weighted_fuel_pricegal * z_wind_avg + 
                           z_weighted_crab_ppp +
                           z_bottom_O2_avg +
                           z_dist_to_closed_km,
                         family = tweedie(),
                         mesh = mesh_all_data,
                         spatial = "off",
                         spatiotemporal = "ar1",
                         data = all_data,
                         time = "half_month_of_season")
toc() #42min

# when seed set 
# The model may not have converged. Maximum final gradient: 0.136528236875038. 
#sanity(fit18_all_data)
#b_js, ln_tau_E, ln_kappa, ar1_phi
#sanity(fit18_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js, ln_tau_E, ln_kappa, ar1_phi
AIC(fit18_all_data)
#1010032
summary(fit18_all_data)

#EXPORT THIS MODEL
#write_rds(fit18_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit18_all_data.rds"))








tic()
fit19_all_data <- sdmTMB(tottraps ~ 0 + 
                           season +
                           month_name_f + 
                           OR_WA_waters + 
                           WA_pot_reduction + 
                           z_SST_avg + 
                           z_wind_avg + 
                           poly(z_depth_point_mean,2) * z_bottom_O2_avg +
                           z_depth_point_sd +
                           z_faults_km +
                           z_dist_canyon_km +
                           z_weighted_dist + 
                           z_weighted_fuel_pricegal + 
                           z_weighted_crab_ppp +
                           #z_bottom_O2_avg + #part of interaction term
                           z_dist_to_closed_km,
                         family = tweedie(),
                         mesh = mesh_all_data,
                         spatial = "off",
                         spatiotemporal = "ar1",
                         data = all_data,
                         time = "half_month_of_season")
toc() #28min

# when seed set 
# The model may not have converged. Maximum final gradient: 0.111499584422893. 
#sanity(fit19_all_data)
#b_js, ln_kappa, ar1_phi
#sanity(fit19_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js, ln_kappa
AIC(fit19_all_data)
#1009641
summary(fit19_all_data)

#EXPORT THIS MODEL
#write_rds(fit19_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit19_all_data.rds"))

###CHECK OPTIMIZATION
#fit19b_all_data <- run_extra_optimization(fit19_all_data, nlminb_loops = 0, newton_loops = 1)
#no warnings
#no red Xs
#AIC: 1009641 -- same as before
#write_rds(fit19b_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit19b_all_data.rds"))


plot_log = function(object, term) {
  g <- ggeffect(object, term, back.transform = FALSE)
  g$conf.low <- log(g$conf.low)
  g$conf.high <- log(g$conf.high)
  g$predicted <- log(g$predicted)
  plot(g)
}


p1 <- plot_log(fit19b_all_data, "season [all]")
p2 <- plot_log(fit19b_all_data, "month_name_f [all]")
p3 <- plot_log(fit19b_all_data, "OR_WA_waters [all]")
p35 <- plot_log(fit19b_all_data, "WA_pot_reduction [all]")
p4 <- plot_log(fit19b_all_data, "z_SST_avg [all]")
p5 <- plot_log(fit19b_all_data, "z_wind_avg [all]")
p6 <- plot_log(fit19b_all_data, "z_depth_point_mean [all]")
p7 <- plot_log(fit19b_all_data, "z_depth_point_sd [all]")
p8 <- plot_log(fit19b_all_data, "z_faults_km [all]")
p9 <- plot_log(fit19b_all_data, "z_dist_canyon_km [all]")
p10 <- plot_log(fit19b_all_data, "z_weighted_dist [all]")
p11 <- plot_log(fit19b_all_data, "z_weighted_fuel_pricegal [all]")
p12 <- plot_log(fit19b_all_data, "z_weighted_crab_ppp [all]")
p13 <- plot_log(fit19b_all_data, "z_bottom_O2_avg [all]")
p14 <- plot_log(fit19b_all_data, "z_dist_to_closed_km [all]")

gridExtra::grid.arrange(p1,p2,p3,p35,ncol=2)

gridExtra::grid.arrange(p4,p5,p6,p7,ncol=2)

gridExtra::grid.arrange(p8,p9,p13,p14,ncol=2)

gridExtra::grid.arrange(p10,p11,p12,ncol=2)

res <- residuals(fit19b_all_data)
qqnorm(res,ylim=c(-5,5))
qqline(res)




