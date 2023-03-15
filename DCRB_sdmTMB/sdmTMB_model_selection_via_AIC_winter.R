#sdmTMB model selection via AIC - winter


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

#read in winter data - the version where z-scoring is done across winter only
winter <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_winter_20230315.rds'))
glimpse(winter) 

winter$month_name_f <- factor(winter$month_name, levels = c("December", "January", "February", "March", "April"))

# Add UTM columns (zone 10)
winter = add_utm_columns(winter, ll_names = c("grd_x", "grd_y"))


mesh_winter <- make_mesh(winter, xy_cols = c("X","Y"), cutoff = 10)
mesh_winter$mesh$n


#-------------------------------------------------------------------------------------------------

#covariates only

tic()
fit1_winter <- sdmTMB(tottraps ~ 0 + 
                        season +
                        month_name_f + 
                        OR_WA_waters +
                        #WA_pot_reduction +  #not relevant in winter
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
                      spatial = "off",
                      spatiotemporal = "off",
                      data = winter,
                      time = "yearn")
toc() #2.5min

# when seed set and no polynomials
#time = "yearn", month_name_f is fixed effect
#no convergence warning
#sanity(fit1_winter)
#red Xs: b_j only
#sanity(fit1_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still 2 X for b_j 
AIC(fit1_winter)
#777123.4
#no change in AIC regardless if time = "yearn" of time = "yearf"
#or if fixed effect is month_name or month_name_f
summary(fit1_winter)


#EXPORT THIS MODEL
#write_rds(fit1_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit1_winter.rds"))


#-------------------------------------------------------------------------------------------------

#covariates + spatial fields (no spatiotemporal fields)

tic()
fit2_winter <- sdmTMB(tottraps ~ 0 + 
                        season +
                        month_name_f + 
                        OR_WA_waters +
                        #WA_pot_reduction +  #not relevant in winter
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
                      time = "yearn")
toc() #6.9min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.146107840889455.
#sanity(fit2_winter)
#red Xs: b_j only
#sanity(fit2_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#red Xs: b_j only
AIC(fit2_winter)
#741503.4
summary(fit2_winter)

#EXPORT THIS MODEL
#write_rds(fit2_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit2_winter.rds"))



#-------------------------------------------------------------------------------------------------

#covariates + spatial fields + spatiotemporal fields (iid)

tic()
fit3_winter <- sdmTMB(tottraps ~ 0 + 
                        season +
                        month_name_f + 
                        OR_WA_waters +
                        #WA_pot_reduction +  #not relevant in winter
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
                      time = "yearn")
toc() #8.1min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.179440996013223.
#sanity(fit3_winter)
#Red Xs: b_js, ln_tau_E, thetaf, ln_phi
#sanity(fit3_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, ln_tau_E, thetaf, ln_phi
AIC(fit3_winter)
#731392.1
summary(fit3_winter)

#EXPORT THIS MODEL
#write_rds(fit3_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit3_winter.rds"))

#just as a test do the optimisation run
#fit3b_winter <- run_extra_optimization(fit3_winter, nlminb_loops = 0, newton_loops = 1)
#no message came up after. basic sanity check no Xs (so no for the second check either)
#AIC: 731392.1 - same as before

#-------------------------------------------------------------------------------------------------

#covariates + spatial fields + spatiotemporal fields (iid)

tic()
fit4_winter <- sdmTMB(tottraps ~ 0 + 
                        season +
                        month_name_f + 
                        OR_WA_waters +
                        #WA_pot_reduction +  #not relevant in winter
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
                      spatiotemporal = "ar1",
                      data = winter,
                      time = "yearn")
toc() #17min

# when seed set and no polynomials
#The model may not have converged. Maximum final gradient: 0.216568184626503. 
#sanity(fit4_winter)
#Red Xs: b_js, ln_phi
#sanity(fit4_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, ln_phi
AIC(fit4_winter)
#731237
summary(fit4_winter)

#EXPORT THIS MODEL
#write_rds(fit4_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit4_winter.rds"))

#this one is the best of the first 7 model so test  the optimisation run
#fit4b_winter <- run_extra_optimization(fit4_winter, nlminb_loops = 0, newton_loops = 1)
#no message came up after. basic sanity check no Xs (so no for the second check either)
#AIC: 731237 --  so same as before

#EXPORT THIS MODEL
#write_rds(fit4b_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit4b_winter.rds"))


#-------------------------------------------------------------------------------------------------

#model with only spatial and spatiotemporal fields (seasons), no covariates

tic()
fit5_winter <- sdmTMB(tottraps ~ 0,
                       family = tweedie(),
                       mesh = mesh_winter,
                       spatial = "on",
                       spatiotemporal = "off",
                       data = winter,
                       time = "yearn")
toc() #0.5min

# when seed set and no polynomials
# no warnings
#sanity(fit5_winter)
#no red Xs
#sanity(fit5_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#no red Xs
AIC(fit5_winter)
#744916.7
summary(fit5_winter)

#EXPORT THIS MODEL
#write_rds(fit5_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit5_winter.rds"))



tic()
fit6_winter <- sdmTMB(tottraps ~ 0,
                      family = tweedie(),
                      mesh = mesh_winter,
                      spatial = "on",
                      spatiotemporal = "iid",
                      data = winter,
                      time = "yearn")
toc() #1.1min

# when seed set and no polynomials
# no warnings
#sanity(fit6_winter)
#no red Xs
#sanity(fit6_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#no red Xs
AIC(fit6_winter)
#734885.2
summary(fit6_winter)

#EXPORT THIS MODEL
#write_rds(fit6_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit6_winter.rds"))




tic()
fit7_winter <- sdmTMB(tottraps ~ 0,
                      family = tweedie(),
                      mesh = mesh_winter,
                      spatial = "on",
                      spatiotemporal = "ar1",
                      data = winter,
                      time = "yearn")
toc() #2.3min

# when seed set and no polynomials
# no warnings
#sanity(fit7_winter)
#no red Xs
#sanity(fit7_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#no red Xs
AIC(fit7_winter)
#734728.5
summary(fit7_winter)

#EXPORT THIS MODEL
#write_rds(fit7_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit7_winter.rds"))

#-------------------------------------------------------------------------------------------------


#out of the first 7 models, fit4_winter is the best
#covariates, spatial and s-t fields (ar1)
#using that as a base test different structures 
#(spatiotemporal fields indexed by year, month, etc; AR1 vs IID; different month variables as fixed effect)
# -- two test above where same model apart from ar1 vs iid --> ar1 is best, no need to test further


#-------------------------------------------------------------------------------------------------

#compare different time = "xx" indexes (fit4_winter is base model)

tic()
fit8_winter <- sdmTMB(tottraps ~ 0 + 
                        season +
                        month_name_f + 
                        OR_WA_waters +
                        #WA_pot_reduction +  #not relevant in winter
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
                      spatiotemporal = "ar1",
                      data = winter,
                      time = "month_n")
toc() #16min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.112408805647971.
#sanity(fit8_winter)
#red Xs: b_js, ln_tau_E, ar1_phi
#sanity(fit8_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js only
AIC(fit8_winter)
#737249.2
summary(fit8_winter)

#EXPORT THIS MODEL
#write_rds(fit8_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit8_winter.rds"))




tic()
fit9_winter <- sdmTMB(tottraps ~ 0 + 
                        season +
                        month_name_f + 
                        OR_WA_waters +
                        #WA_pot_reduction +  #not relevant in winter
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
                      spatiotemporal = "ar1",
                      data = winter,
                      time = "month_of_season")
toc() #13min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.167211172907528. 
#sanity(fit9_winter)
#Red Xs: b_js, ln_kappa, thetaf, ar1_phi
#sanity(fit9_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, ln_kappa, thetaf
AIC(fit9_winter)
#736816
summary(fit9_winter)

#EXPORT THIS MODEL
#write_rds(fit9_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit9_winter.rds"))





tic()
fit10_winter <- sdmTMB(tottraps ~ 0 + 
                        season +
                        month_name_f + 
                        OR_WA_waters +
                        #WA_pot_reduction +  #not relevant in winter
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
                      spatiotemporal = "ar1",
                      data = winter,
                      time = "half_month_of_season")
toc() #16min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.272645924517795.
#sanity(fit10_winter)
#Red Xs: b_js, ln_kappa, ln_phi, ar1_phi, ln_tau_O, sigma_O
#sanity(fit10_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, ln_kappa, ln_phi, ln_tau_O, sigma_O
AIC(fit10_winter)
#735228
summary(fit10_winter)

#EXPORT THIS MODEL
#write_rds(fit10_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit10_winter.rds"))


#-------------------------------------------------------------------------------------------------


#after finding the best structure: still fit4_winter with
#ar1, time indexed by year
#then test different month variables as fixed effect


#-------------------------------------------------------------------------------------------------

#compare different month variables as fixed effect (fit4_winter is base model - where fixed effect is month_name_f)

tic()
fit11_winter <- sdmTMB(tottraps ~ 0 + 
                         season +
                         month_of_seasonf + 
                         OR_WA_waters +
                         #WA_pot_reduction +  #not relevant in winter
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
                       spatiotemporal = "ar1",
                       data = winter,
                       time = "yearn")
toc() #36min

# when seed set and no polynomials
#  The model may not have converged. Maximum final gradient: 0.0800049470813269. 
#sanity(fit11_winter)
#Red Xs: b_js, ln_tau_E, thetaf, ln_phi
#sanity(fit11_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, ln_tau_E, thetaf, ln_phi
AIC(fit11_winter)
#730548.9 -- so far the bet
summary(fit11_winter)

#EXPORT THIS MODEL
#write_rds(fit11_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit11_winter.rds"))

#test  the optimisation run
#fit11b_winter <- run_extra_optimization(fit11_winter, nlminb_loops = 0, newton_loops = 1)
#no message came up after. basic sanity check no Xs (so no for the second check either)
#AIC: 

#EXPORT THIS MODEL
#write_rds(fit11b_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit11b_winter.rds"))






tic()
fit12_winter <- sdmTMB(tottraps ~ 0 + 
                         season +
                         half_month_of_seasonf + #half_month_of_seasonf / z_half_month_of_season
                         OR_WA_waters +
                         #WA_pot_reduction +  #not relevant in winter
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
                       spatiotemporal = "ar1",
                       data = winter,
                       time = "yearn")
toc() #23min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.0968867569996164. 
#sanity(fit12_winter)
#Red Xs: b_js, ln_tau_O, ln_kappa, thetaf, ln_phi
#sanity(fit12_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, ln_tau_O, ln_kappa, thetaf, ln_phi
AIC(fit12_winter)
#729689.7
summary(fit12_winter)

#EXPORT THIS MODEL
#write_rds(fit12_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit12_winter.rds"))


# test  the optimisation run
#fit12b_winter <- run_extra_optimization(fit12_winter, nlminb_loops = 0, newton_loops = 1)
#no message came up after. basic sanity check no Xs (so no for the second check either)
#AIC: 729689.7 -- same as before

#EXPORT THIS MODEL
#write_rds(fit12b_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit12b_winter.rds"))



#the first fit12_winter
## if change OR_WA_waters to be character instead of continuous 
#The model may not have converged. Maximum final gradient: 0.0683821608491444.  
#sanity(fit12_winter)
#b_js, ln_tau_E
#sanity(fit12_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js, ln_tau_E
#AIC(fit12_winter)
#729689.7 -- same AIC even if OR_WA_waters variable is a character 

#if use z_half_month_of_season as fixed effect and  OR_WA_waters is character 
#The model may not have converged. Maximum final gradient: 0.0397527505670432. 
#sanity(fit12_winter)
#b_js, ln_tau_E, ln_phi, ar1_phi
#sanity(fit12_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js, ln_tau_E
#AIC(fit12_winter)
#731745.2 -- changes if half_month_of_season is z scored continuous

#-------------------------------------------------------------------------------------------------


#then test polynomial terms and interactions
#for polynomials only test the chosen term: depth
#but try out all interactions

#the base is fit12_winter as that had the best AIC

#-------------------------------------------------------------------------------------------------

#polynomial term

tic()
fit13_winter <- sdmTMB(tottraps ~ 0 + 
                         season +
                         half_month_of_seasonf + 
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
                         z_bottom_O2_avg +
                         z_dist_to_closed_km,
                       family = tweedie(),
                       mesh = mesh_winter,
                       spatial = "on",
                       spatiotemporal = "ar1",
                       data = winter,
                       time = "yearn")
toc() #39min

# when seed set 
# The model may not have converged. Maximum final gradient: 0.0291937496603865.
#sanity(fit13_winter)
#b_js, ln_phi
#sanity(fit13_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js, ln_phi
AIC(fit13_winter)
#727058
summary(fit13_winter)

#EXPORT THIS MODEL
#write_rds(fit13_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit13_winter.rds"))


#test  the optimisation run
#fit13b_winter <- run_extra_optimization(fit13_winter, nlminb_loops = 0, newton_loops = 1)
#no message came up after. basic sanity check no Xs (so no for the second check either)
#AIC: 727058 -- same as before

#EXPORT THIS MODEL
#write_rds(fit13b_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit13b_winter.rds"))


#-------------------------------------------------------------------------------------------------

#interactions

tic()
fit14_winter <- sdmTMB(tottraps ~ 0 + 
                         season +
                         half_month_of_seasonf + 
                         OR_WA_waters +
                         #WA_pot_reduction +  #not relevant in winter
                         z_SST_avg +
                         z_wind_avg +
                         poly(z_depth_point_mean,2) +
                         z_depth_point_sd +
                         z_faults_km +
                         z_dist_canyon_km +
                         z_weighted_dist +
                         # z_weighted_fuel_pricegal + #part of interaction term
                         z_weighted_crab_ppp * z_weighted_fuel_pricegal +
                         z_bottom_O2_avg +
                         z_dist_to_closed_km,
                       family = tweedie(),
                       mesh = mesh_winter,
                       spatial = "on",
                       spatiotemporal = "ar1",
                       data = winter,
                       time = "yearn")
toc() #40min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.0361484867733388. 
#sanity(fit14_winter)
#b_js, thetaf, ln_phi
#sanity(fit14_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js, thetaf, ln_phi
AIC(fit14_winter)
#727018.4
summary(fit14_winter)

#EXPORT THIS MODEL
#write_rds(fit14_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit14_winter.rds"))




tic()
fit15_winter <- sdmTMB(tottraps ~ 0 + 
                         season +
                         half_month_of_seasonf + 
                         OR_WA_waters +
                         #WA_pot_reduction +  #not relevant in winter
                         #z_SST_avg + #part of interaction term
                         z_wind_avg * z_SST_avg +
                         poly(z_depth_point_mean,2) +
                         z_depth_point_sd +
                         z_faults_km +
                         z_dist_canyon_km +
                         z_weighted_dist +
                         z_weighted_fuel_pricegal + 
                         z_weighted_crab_ppp  +
                         z_bottom_O2_avg +
                         z_dist_to_closed_km,
                       family = tweedie(),
                       mesh = mesh_winter,
                       spatial = "on",
                       spatiotemporal = "ar1",
                       data = winter,
                       time = "yearn")
toc() #40min

# when seed set and no polynomials
# Warning messages:
#1: In sqrt(diag(cov)) : NaNs produced
#2: The model may not have converged: non-positive-definite Hessian matrix. 
#sanity(fit15_winter)
#lots
#sanity(fit15_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#lots
AIC(fit15_winter)
#727172.8
summary(fit15_winter)

#EXPORT THIS MODEL
#write_rds(fit15_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit15_winter.rds"))





tic()
fit16_winter <- sdmTMB(tottraps ~ 0 + 
                         season +
                         half_month_of_seasonf + 
                         #OR_WA_waters + #part of interaction term
                         #WA_pot_reduction +  #not relevant in winter
                         z_SST_avg + 
                         z_wind_avg +
                         poly(z_depth_point_mean,2) +
                         z_depth_point_sd +
                         z_faults_km +
                         z_dist_canyon_km +
                         z_weighted_dist +
                         z_weighted_fuel_pricegal + 
                         z_weighted_crab_ppp  +
                         z_bottom_O2_avg +
                         OR_WA_waters * z_dist_to_closed_km,
                       family = tweedie(),
                       mesh = mesh_winter,
                       spatial = "on",
                       spatiotemporal = "ar1",
                       data = winter,
                       time = "yearn")
toc() #22min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.0330852711023812. 
#sanity(fit16_winter)
#b_js only
#sanity(fit16_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js only
AIC(fit16_winter)
#727056.8
summary(fit16_winter)

#EXPORT THIS MODEL
#write_rds(fit16_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit16_winter.rds"))





tic()
fit17_winter <- sdmTMB(tottraps ~ 0 + 
                         season +
                         half_month_of_seasonf + 
                         OR_WA_waters + 
                         #WA_pot_reduction +  #not relevant in winter
                         z_SST_avg + 
                         z_wind_avg +
                         poly(z_depth_point_mean,2) +
                         z_depth_point_sd +
                         z_faults_km +
                         z_dist_canyon_km +
                         #z_weighted_dist + #part of interaction term
                         z_weighted_dist * z_weighted_fuel_pricegal + 
                         z_weighted_crab_ppp  +
                         z_bottom_O2_avg +
                         z_dist_to_closed_km,
                       family = tweedie(),
                       mesh = mesh_winter,
                       spatial = "on",
                       spatiotemporal = "ar1",
                       data = winter,
                       time = "yearn")
toc() #34min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.0819660675468272.
#sanity(fit17_winter)
#b_js, thetaf
#sanity(fit17_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js, thetaf
AIC(fit17_winter)
#727057
summary(fit17_winter)

#EXPORT THIS MODEL
#write_rds(fit17_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit17_winter.rds"))




tic()
fit18_winter <- sdmTMB(tottraps ~ 0 + 
                         season +
                         half_month_of_seasonf + 
                         OR_WA_waters + 
                         #WA_pot_reduction +  #not relevant in winter
                         z_SST_avg + 
                         #z_wind_avg + #part of interaction term
                         poly(z_depth_point_mean,2) +
                         z_depth_point_sd +
                         z_faults_km +
                         z_dist_canyon_km +
                         z_weighted_dist + 
                         z_weighted_fuel_pricegal * z_wind_avg + 
                         z_weighted_crab_ppp  +
                         z_bottom_O2_avg +
                         z_dist_to_closed_km,
                       family = tweedie(),
                       mesh = mesh_winter,
                       spatial = "on",
                       spatiotemporal = "ar1",
                       data = winter,
                       time = "yearn")
toc() #32min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.0483960498666045. 
#sanity(fit18_winter)
#b_js, ln_tau_E, ln_kappa, thetaf
#sanity(fit18_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js, thetaf
AIC(fit18_winter)
#727060
summary(fit18_winter)

#EXPORT THIS MODEL
#write_rds(fit18_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit18_winter.rds"))






#-------------------------------------------------------------------------------------------------





#-------------------------------------------------------------------------------------------------







