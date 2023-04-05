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
winter <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_winter_20230324.rds'))
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
#775060.3 (was 777123.4 before wonky bathy fix)
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
toc() #7min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.48311427299012
#sanity(fit2_winter)
#red Xs: b_js, ln_tau_O, ln_kappa
#sanity(fit2_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#red Xs: b_js, ln_tau_O
AIC(fit2_winter)
#739926.9
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
toc() #8min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.089623522998167
#sanity(fit3_winter)
#Red Xs: b_js, ln_kappa, thetaf, ln_phi
#sanity(fit3_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, thetaf, ln_phi
AIC(fit3_winter)
#729701.8
summary(fit3_winter)

#EXPORT THIS MODEL
#write_rds(fit3_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit3_winter.rds"))

#just as a test do the optimisation run
#fit3b_winter <- run_extra_optimization(fit3_winter, nlminb_loops = 0, newton_loops = 1)
#no message came up after. basic sanity check no Xs (so no for the second check either)
#AIC: 731392.1 - same as before

#-------------------------------------------------------------------------------------------------

#covariates + spatial fields + spatiotemporal fields (ar1)

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
#The model may not have converged. Maximum final gradient: 0.0584167117420975 
#sanity(fit4_winter)
#Red Xs: b_js, ln_tau_O, ln_phi, ar1_phi
#sanity(fit4_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, ln_phi, ar1_phi
AIC(fit4_winter)
#729548.9
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
# The model may not have converged. Maximum final gradient: 0.0689342149141479
#sanity(fit8_winter)
#red Xs: b_js, ln_tau_E, ln_phi
#sanity(fit8_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js, ln_tau_E, ln_phi
AIC(fit8_winter)
#735593.9
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
toc() #9min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.116619430702939 
#sanity(fit9_winter)
#Red Xs: b_js, ln_tau_E, ln_kappa, thetaf, ar1_phi, `ln_tau_O` standard error may be large
#sanity(fit9_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, ln_tau_E, thetaf, ar1_phi, `ln_tau_O` standard error may be large
AIC(fit9_winter)
#735144
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
toc() #14min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.3589499224902
#sanity(fit10_winter)
#Red Xs: b_js, ln_tau_E, ln_kappa, ln_phi, ar1_phi, `ln_tau_O` standard error may be large, `sigma_O` is smaller than 0.01
#sanity(fit10_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, ln_tau_E, ln_kappa, ln_phi, ar1_phi, `ln_tau_O` standard error may be large, `sigma_O` is smaller than 0.01
AIC(fit10_winter)
#733522.9
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
toc() #25min

# when seed set and no polynomials
#  The model may not have converged. Maximum final gradient: 0.484813431975992 
#sanity(fit11_winter)
#Red Xs: b_js, ln_tau_O, ln_tau_E, ln_kappa, thetaf, ln_phi
#sanity(fit11_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, ln_tau_O, ln_tau_E, ln_kappa, thetaf, ln_phi
AIC(fit11_winter)
#728846.9 
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
# The model may not have converged. Maximum final gradient: 0.0351828101826014 
#sanity(fit12_winter)
#Red Xs: b_js, ln_tau_O, ar1_phi
#sanity(fit12_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, ln_tau_O
AIC(fit12_winter)
#727971.4
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
toc() #17min

# when seed set 
# The model may not have converged. Maximum final gradient: 0.0982702344726238
#sanity(fit13_winter)
#b_js, ln_tau_O
#sanity(fit13_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js only
AIC(fit13_winter)
#725833.7 -- when changed fuel and crab ppp
summary(fit13_winter)

#EXPORT THIS MODEL
#write_rds(fit13_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit13_winter.rds"))


#test  the optimisation run
#fit13b_winter <- run_extra_optimization(fit13_winter, nlminb_loops = 0, newton_loops = 1)
#no message came up after. basic sanity check no Xs (so no for the second check either)
#AIC: 725673.5 -- same as before

#EXPORT THIS MODEL
#write_rds(fit13b_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit13b_winter.rds"))

plot_log = function(object, term) {
  g <- ggeffect(object, term, back.transform = FALSE)
  g$conf.low <- log(g$conf.low)
  g$conf.high <- log(g$conf.high)
  g$predicted <- log(g$predicted)
  plot(g)
}


p1 <- plot_log(fit13b_winter, "season [all]")
p2 <- plot_log(fit13b_winter, "half_month_of_seasonf [all]")
p3 <- plot_log(fit13b_winter, "OR_WA_waters [all]")
p4 <- plot_log(fit13b_winter, "z_SST_avg [all]")
p5 <- plot_log(fit13b_winter, "z_wind_avg [all]")
p6 <- plot_log(fit13b_winter, "z_depth_point_mean [all]")
p7 <- plot_log(fit13b_winter, "z_depth_point_sd [all]")
p8 <- plot_log(fit13b_winter, "z_faults_km [all]")
p9 <- plot_log(fit13b_winter, "z_dist_canyon_km [all]")
p10 <- plot_log(fit13b_winter, "z_weighted_dist [all]")
p11 <- plot_log(fit13b_winter, "z_weighted_fuel_pricegal [all]")
p12 <- plot_log(fit13b_winter, "z_weighted_crab_ppp [all]")
p13 <- plot_log(fit13b_winter, "z_bottom_O2_avg [all]")
p14 <- plot_log(fit13b_winter, "z_dist_to_closed_km [all]")

gridExtra::grid.arrange(p1,p2,p3,ncol=2)

gridExtra::grid.arrange(p4,p5,p6,p7,ncol=2)

gridExtra::grid.arrange(p8,p9,p13,p14,ncol=2)

gridExtra::grid.arrange(p10,p11,p12,ncol=2)

res <- residuals(fit13b_winter)
qqnorm(res,ylim=c(-5,5))
qqline(res)

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
toc() #20min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.0152104066960312 
#sanity(fit14_winter)
#b_js, thetaf, ln_phi
#sanity(fit14_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js, thetaf, ln_phi
AIC(fit14_winter)
#725634.3
summary(fit14_winter)

#EXPORT THIS MODEL
#write_rds(fit14_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit14_winter.rds"))

#test  the optimisation run
#fit14b_winter <- run_extra_optimization(fit14_winter, nlminb_loops = 0, newton_loops = 1)
#no message came up after. basic sanity check no Xs (so no for the second check either)
#AIC: 727018.4 -- same as before

#EXPORT THIS MODEL
#write_rds(fit14b_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit14b_winter.rds"))


#plots <- plot_diag(fit14b_winter)

plot_log = function(object, term) {
  g <- ggeffect(object, term, back.transform = FALSE)
  g$conf.low <- log(g$conf.low)
  g$conf.high <- log(g$conf.high)
  g$predicted <- log(g$predicted)
  plot(g)
}


p1 <- plot_log(fit14b_winter, "season [all]")
p2 <- plot_log(fit14b_winter, "half_month_of_seasonf [all]")
p3 <- plot_log(fit14b_winter, "OR_WA_waters [all]")
p4 <- plot_log(fit14b_winter, "z_SST_avg [all]")
p5 <- plot_log(fit14b_winter, "z_wind_avg [all]")
p6 <- plot_log(fit14b_winter, "z_depth_point_mean [all]")
p7 <- plot_log(fit14b_winter, "z_depth_point_sd [all]")
p8 <- plot_log(fit14b_winter, "z_faults_km [all]")
p9 <- plot_log(fit14b_winter, "z_dist_canyon_km [all]")
p10 <- plot_log(fit14b_winter, "z_weighted_dist [all]")
p11 <- plot_log(fit14b_winter, "z_weighted_fuel_pricegal [all]")
p12 <- plot_log(fit14b_winter, "z_weighted_crab_ppp [all]")
p13 <- plot_log(fit14b_winter, "z_bottom_O2_avg [all]")
p14 <- plot_log(fit14b_winter, "z_dist_to_closed_km [all]")

gridExtra::grid.arrange(p1,p2,p3,ncol=2)

gridExtra::grid.arrange(p4,p5,p6,p7,ncol=2)

gridExtra::grid.arrange(p8,p9,p13,p14,ncol=2)

gridExtra::grid.arrange(p10,p11,p12,ncol=2)

res <- residuals(fit14b_winter)
qqnorm(res,ylim=c(-5,5))
qqline(res)






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
toc() #30min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.0378678986062653.
#sanity(fit15_winter)
#b_js, ln_tau_E, ln_kappa, ln_phi, ar1_phi
#sanity(fit15_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js, ln_tau_E, ln_phi
AIC(fit15_winter)
#725658.2
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
toc() #40min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.0434193807743952 
#sanity(fit16_winter)
#b_js, ln_phi, ar1_phi
#sanity(fit16_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js only
AIC(fit16_winter)
#725673
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
toc() #39min

# when seed set and no polynomials
# warnings:
#1: In sqrt(diag(cov)) : NaNs produced
#2: The model may not have converged: non-positive-definite Hessian matrix.
#sanity(fit17_winter)
#lots
#sanity(fit17_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#lots
AIC(fit17_winter)
#725911.3
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
# The model may not have converged. Maximum final gradient: 0.0477071006063969 
#sanity(fit18_winter)
#b_js, thetaf, ln_phi
#sanity(fit18_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js, thetaf,ln_phi
AIC(fit18_winter)
#725675.5
summary(fit18_winter)

#EXPORT THIS MODEL
#write_rds(fit18_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit18_winter.rds"))





tic()
fit19_winter <- sdmTMB(tottraps ~ 0 + 
                         season +
                         half_month_of_seasonf + 
                         OR_WA_waters +
                         #WA_pot_reduction +  #not relevant in winter
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
                       mesh = mesh_winter,
                       spatial = "on",
                       spatiotemporal = "ar1",
                       data = winter,
                       time = "yearn")
toc() #32min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.015548926467952
#sanity(fit19_winter)
#b_js, ln_phi
#sanity(fit19_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js, ln_phi
AIC(fit19_winter)
#725570.3
summary(fit19_winter)

#EXPORT THIS MODEL
#write_rds(fit19_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit19_winter.rds"))

#test  the optimisation run
#fit19b_winter <- run_extra_optimization(fit19_winter, nlminb_loops = 0, newton_loops = 1)
#no warnings
#no red Xs
#AIC: 725413.5 -- same as before

#EXPORT THIS MODEL
#write_rds(fit19b_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit19b_winter.rds"))

plot_log = function(object, term) {
  g <- ggeffect(object, term, back.transform = FALSE)
  g$conf.low <- log(g$conf.low)
  g$conf.high <- log(g$conf.high)
  g$predicted <- log(g$predicted)
  plot(g)
}


p1 <- plot_log(fit19b_winter, "season [all]")
p2 <- plot_log(fit19b_winter, "half_month_of_seasonf [all]")
p3 <- plot_log(fit19b_winter, "OR_WA_waters [all]")
p4 <- plot_log(fit19b_winter, "z_SST_avg [all]")
p5 <- plot_log(fit19b_winter, "z_wind_avg [all]")
p6 <- plot_log(fit19b_winter, "z_depth_point_mean [all]")
p7 <- plot_log(fit19b_winter, "z_depth_point_sd [all]")
p8 <- plot_log(fit19b_winter, "z_faults_km [all]")
p9 <- plot_log(fit19b_winter, "z_dist_canyon_km [all]")
p10 <- plot_log(fit19b_winter, "z_weighted_dist [all]")
p11 <- plot_log(fit19b_winter, "z_weighted_fuel_pricegal [all]")
p12 <- plot_log(fit19b_winter, "z_weighted_crab_ppp [all]")
p13 <- plot_log(fit19b_winter, "z_bottom_O2_avg [all]")
p14 <- plot_log(fit19b_winter, "z_dist_to_closed_km [all]")

gridExtra::grid.arrange(p1,p2,p3,ncol=2)

gridExtra::grid.arrange(p4,p5,p6,p7,ncol=2)

gridExtra::grid.arrange(p8,p9,p13,p14,ncol=2)

gridExtra::grid.arrange(p10,p11,p12,ncol=2)

res <- residuals(fit19b_winter)
qqnorm(res,ylim=c(-5,5))
qqline(res)

#-------------------------------------------------------------------------------------------------





#-------------------------------------------------------------------------------------------------







