#sdmTMB model selection via AIC - summer


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

#read in summer data - the version where z-scoring is done across summer only
summer <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_summer_20230324.rds'))
glimpse(summer) 

summer$month_name_f <- factor(summer$month_name, levels = c("May", "June", "July", "August", "September"))

# Add UTM columns (zone 10)
summer = add_utm_columns(summer, ll_names = c("grd_x", "grd_y"))


mesh_summer <- make_mesh(summer, xy_cols = c("X","Y"), cutoff = 10)
mesh_summer$mesh$n


#-------------------------------------------------------------------------------------------------

#covariates only


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
                      spatial = "off",
                      spatiotemporal = "off",
                      data = summer,
                      time = "yearn")
toc() #0.7min

# when seed set and no polynomials
#time = "yearn", month_name_f is fixed effect
#no convergence warning
#sanity(fit1_summer)
#red Xs: b_js, thetaf, ln_phi
#sanity(fit1_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#still thetaf and ln_phi
AIC(fit1_summer)
#287572.4 # don't use this
#no change in AIC if season is a factor, and all seasons are still shown in summary table
#AIC is 287572.2 if include WA_pot_reduction
summary(fit1_summer)


#EXPORT THIS MODEL
#write_rds(fit1_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit1_summer.rds"))



#-------------------------------------------------------------------------------------------------

#covariates + spatial fields (no spatiotemporal fields)

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
                      spatiotemporal = "off",
                      data = summer,
                      time = "yearn")
toc() #3min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.0203702492387459. 
#sanity(fit2_summer)
#red Xs: b_js only
#sanity(fit2_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#red Xs: b_js only
AIC(fit2_summer)
#273605.8
summary(fit2_summer)

#EXPORT THIS MODEL
#write_rds(fit2_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit2_summer.rds"))



#-------------------------------------------------------------------------------------------------

#covariates + spatial fields + spatiotemporal fields (iid)

tic()
fit3_summer <- sdmTMB(tottraps ~ 0 + 
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
                      time = "yearn")
toc() #4min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.0479833961530653. 
#sanity(fit3_summer)
#Red Xs: b_js, ln_tau_O, ln_kappa, ln_phi
#sanity(fit3_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, ln_phi
AIC(fit3_summer)
#267780
summary(fit3_summer)

#EXPORT THIS MODEL
#write_rds(fit3_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit3_summer.rds"))

#just as a test do the optimisation run
#fit3b_summer <- run_extra_optimization(fit3_summer, nlminb_loops = 0, newton_loops = 1)
#no message came up after. basic sanity check no Xs (so no for the second check either)
#AIC: 267780 - same as before

#-------------------------------------------------------------------------------------------------

#covariates + spatial fields + spatiotemporal fields (iid)

tic()
fit4_summer <- sdmTMB(tottraps ~ 0 + 
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
                      spatiotemporal = "ar1",
                      data = summer,
                      time = "yearn")
toc() #8min

# when seed set and no polynomials
#The model may not have converged. Maximum final gradient: 0.0390135581909945. 
#sanity(fit4_summer)
#Red Xs: b_js, ln_tau_E, ln_kappa
#sanity(fit4_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, ln_tau_E, ln_kappa
AIC(fit4_summer)
#267703.6
summary(fit4_summer)

#EXPORT THIS MODEL
#write_rds(fit4_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit4_summer.rds"))

#this one is the best of the first 7 model so test  the optimisation run
#fit4b_summer <- run_extra_optimization(fit4_summer, nlminb_loops = 0, newton_loops = 1)
#no message came up after. basic sanity check no Xs (so no for the second check either)
#AIC: 267703.6 --  so same as before



#-------------------------------------------------------------------------------------------------

#model with only spatial and spatiotemporal fields (seasons), no covariates

tic()
fit5_summer <- sdmTMB(tottraps ~ 0,
                      family = tweedie(),
                      mesh = mesh_summer,
                      spatial = "on",
                      spatiotemporal = "off",
                      data = summer,
                      time = "yearn")
toc() #0.2min

# when seed set and no polynomials
# no warnings
#sanity(fit5_summer)
#no red Xs
#sanity(fit5_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#no red Xs
AIC(fit5_summer)
#277485.1
summary(fit5_summer)

#EXPORT THIS MODEL
#write_rds(fit5_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit5_summer.rds"))



tic()
fit6_summer <- sdmTMB(tottraps ~ 0,
                      family = tweedie(),
                      mesh = mesh_summer,
                      spatial = "on",
                      spatiotemporal = "iid",
                      data = summer,
                      time = "yearn")
toc() #0.7min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.0108526672392912.
#sanity(fit6_summer)
#ln_tau_E, ln_kappa, ln_phi
#sanity(fit6_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#ln_tau_E, ln_phi
AIC(fit6_summer)
#271531.5
summary(fit6_summer)

#EXPORT THIS MODEL
#write_rds(fit6_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit6_summer.rds"))




tic()
fit7_summer <- sdmTMB(tottraps ~ 0,
                      family = tweedie(),
                      mesh = mesh_summer,
                      spatial = "on",
                      spatiotemporal = "ar1",
                      data = summer,
                      time = "yearn")
toc() #1.4min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.0723032723128227. 
#sanity(fit7_summer)
#ln_tau_O, ln_tau_E, thetaf, ln_phi
#sanity(fit7_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#ln_tau_O, ln_tau_E, ln_phi
AIC(fit7_summer)
#271460.9
summary(fit7_summer)

#EXPORT THIS MODEL
#write_rds(fit7_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit7_summer.rds"))

#-------------------------------------------------------------------------------------------------


#out of the first 7 models, fit4_summer is the best
#covariates, spatial and s-t fields (ar1)
#using that as a base test different structures 
#(spatiotemporal fields indexed by year, month, etc; AR1 vs IID; different month variables as fixed effect)
# -- two test above where same model apart from ar1 vs iid --> ar1 is best, no need to test further


#-------------------------------------------------------------------------------------------------

#compare different time = "xx" indexes (fit4_summer is base model)

tic()
fit8_summer <- sdmTMB(tottraps ~ 0 + 
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
                      spatiotemporal = "ar1",
                      data = summer,
                      time = "month_n")
toc() #4.3min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.173254410786804
#sanity(fit8_summer)
#red Xs: b_js, ln_tau_E, thetaf, ln_phi
#sanity(fit8_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js, thetaf, ln_phi
AIC(fit8_summer)
#271848.2
summary(fit8_summer)

#EXPORT THIS MODEL
#write_rds(fit8_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit8_summer.rds"))



tic()
fit9_summer <- sdmTMB(tottraps ~ 0 + 
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
                      spatiotemporal = "ar1",
                      data = summer,
                      time = "month_of_season")
toc() #8min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.0318550443392515.
#sanity(fit9_summer)
#Red Xs: b_js, ln_kappa
#sanity(fit9_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, ln_kappa
AIC(fit9_summer)
#271493.4
summary(fit9_summer)

#EXPORT THIS MODEL
#write_rds(fit9_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit9_summer.rds"))




tic()
fit10_summer <- sdmTMB(tottraps ~ 0 + 
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
                       spatiotemporal = "ar1",
                       data = summer,
                       time = "half_month_of_season")
toc() #43min

# when seed set and no polynomials
# no warnings
#sanity(fit10_summer)
#Red Xs: b_js, ln_kappa, sigma_O
#sanity(fit10_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js, sigma_O
AIC(fit10_summer)
#271024.2
summary(fit10_summer)

#EXPORT THIS MODEL
#write_rds(fit10_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit10_summer.rds"))


#-------------------------------------------------------------------------------------------------


#after finding the best structure: still fit4_summer with
#ar1, time indexed by year
#then test different month variables as fixed effect


#-------------------------------------------------------------------------------------------------

#compare different month variables as fixed effect (fit4_summer is base model - where fixed effect is month_name_f)

tic()
fit11_summer <- sdmTMB(tottraps ~ 0 + 
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
                       mesh = mesh_summer,
                       spatial = "on",
                       spatiotemporal = "ar1",
                       data = summer,
                       time = "yearn")
toc() #11min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.0192772667985537.
#sanity(fit11_summer)
#Red Xs: b_js only
#sanity(fit11_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js only
AIC(fit11_summer)
#267683.4
summary(fit11_summer)

#EXPORT THIS MODEL
#write_rds(fit11_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit11_summer.rds"))





tic()
fit12_summer <- sdmTMB(tottraps ~ 0 + 
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
                       mesh = mesh_summer,
                       spatial = "on",
                       spatiotemporal = "ar1",
                       data = summer,
                       time = "yearn")
toc() #19min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.0488905788395559. 
#sanity(fit12_summer)
#Red Xs: b_js only
#sanity(fit12_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#Red Xs: b_js only
AIC(fit12_summer)
#267440.6
summary(fit12_summer)

#EXPORT THIS MODEL
#write_rds(fit12_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit12_summer.rds"))


#-------------------------------------------------------------------------------------------------


#then test polynomial terms and interactions
#for polynomials only test the chosen term: depth
#but try out all interactions

#the base is fit12_summer as that had the best AIC

#-------------------------------------------------------------------------------------------------


#polynomial term

tic()
fit13_summer <- sdmTMB(tottraps ~ 0 + 
                         season +
                         half_month_of_seasonf + 
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
                       spatiotemporal = "ar1",
                       data = summer,
                       time = "yearn")
toc() #12min

# when seed set 
# The model may not have converged. Maximum final gradient: 0.0127912393276404.
#sanity(fit13_summer)
#b_js, ln_tau_O
#sanity(fit13_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js only
AIC(fit13_summer)
#266439.9
summary(fit13_summer)

#EXPORT THIS MODEL
#write_rds(fit13_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit13_summer.rds"))

###CHECK OPTIMIZATION
#fit13b_summer <- run_extra_optimization(fit13_summer, nlminb_loops = 0, newton_loops = 1)
#no warnings
#no red Xs
#AIC: 266439.9 -- same as before
#write_rds(fit13b_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit13b_summer.rds"))

plot_log = function(object, term) {
  g <- ggeffect(object, term, back.transform = FALSE)
  g$conf.low <- log(g$conf.low)
  g$conf.high <- log(g$conf.high)
  g$predicted <- log(g$predicted)
  plot(g)
}


p1 <- plot_log(fit13b_summer, "season [all]")
p2 <- plot_log(fit13b_summer, "half_month_of_seasonf [all]")
p3 <- plot_log(fit13b_summer, "OR_WA_waters [all]")
p35 <- plot_log(fit13b_summer, "WA_pot_reduction [all]")
p4 <- plot_log(fit13b_summer, "z_SST_avg [all]")
p5 <- plot_log(fit13b_summer, "z_wind_avg [all]")
p6 <- plot_log(fit13b_summer, "z_depth_point_mean [all]")
p7 <- plot_log(fit13b_summer, "z_depth_point_sd [all]")
p8 <- plot_log(fit13b_summer, "z_faults_km [all]")
p9 <- plot_log(fit13b_summer, "z_dist_canyon_km [all]")
p10 <- plot_log(fit13b_summer, "z_weighted_dist [all]")
p11 <- plot_log(fit13b_summer, "z_weighted_fuel_pricegal [all]")
p12 <- plot_log(fit13b_summer, "z_weighted_crab_ppp [all]")
p13 <- plot_log(fit13b_summer, "z_bottom_O2_avg [all]")
p14 <- plot_log(fit13b_summer, "z_dist_to_closed_km [all]")

gridExtra::grid.arrange(p1,p2,p3,p35,ncol=2)

gridExtra::grid.arrange(p4,p5,p6,p7,ncol=2)

gridExtra::grid.arrange(p8,p9,p13,p14,ncol=2)

gridExtra::grid.arrange(p10,p11,p12,ncol=2)

res <- residuals(fit13b_summer)
qqnorm(res)
qqline(res)


#-------------------------------------------------------------------------------------------------

#interactions

tic()
fit14_summer <- sdmTMB(tottraps ~ 0 + 
                         season +
                         half_month_of_seasonf + 
                         OR_WA_waters +
                         WA_pot_reduction +  
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
                       mesh = mesh_summer,
                       spatial = "on",
                       spatiotemporal = "ar1",
                       data = summer,
                       time = "yearn")
toc() #22min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.0223181413949343.  
#sanity(fit14_summer)
#b_js, ln_phi
#sanity(fit14_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js, ln_phi
AIC(fit14_summer)
#266434.1
summary(fit14_summer)

#EXPORT THIS MODEL
#write_rds(fit14_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit14_summer.rds"))





tic()
fit15_summer <- sdmTMB(tottraps ~ 0 + 
                         season +
                         half_month_of_seasonf + 
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
                         z_weighted_crab_ppp  +
                         z_bottom_O2_avg +
                         z_dist_to_closed_km,
                       family = tweedie(),
                       mesh = mesh_summer,
                       spatial = "on",
                       spatiotemporal = "ar1",
                       data = summer,
                       time = "yearn")
toc() #20min

# when seed set and no polynomials
#The model may not have converged. Maximum final gradient: 0.0161929819959852. 
#sanity(fit15_summer)
#b_js, ln_tau_E, thetaf, ln_phi
#sanity(fit15_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js, thetaf, ln_phi
AIC(fit15_summer)
#266431.4
summary(fit15_summer)

#EXPORT THIS MODEL
#write_rds(fit15_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit15_summer.rds"))


#test  the optimisation run
#fit15b_summer <- run_extra_optimization(fit15_summer, nlminb_loops = 0, newton_loops = 1)
#no message came up after. basic sanity check no Xs (so no for the second check either)
#AIC: 266431.4 -- same as before

#EXPORT THIS MODEL
#write_rds(fit15b_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit15b_summer.rds"))

#plots <- plot_diag(fit15b_summer)

plot_log = function(object, term) {
  g <- ggeffect(object, term, back.transform = FALSE)
  g$conf.low <- log(g$conf.low)
  g$conf.high <- log(g$conf.high)
  g$predicted <- log(g$predicted)
  plot(g)
}


p1 <- plot_log(fit15b_summer, "season [all]")
p2 <- plot_log(fit15b_summer, "half_month_of_seasonf [all]")
p3 <- plot_log(fit15b_summer, "OR_WA_waters [all]")
p35 <- plot_log(fit15b_summer, "WA_pot_reduction [all]")
p4 <- plot_log(fit15b_summer, "z_SST_avg [all]")
p5 <- plot_log(fit15b_summer, "z_wind_avg [all]")
p6 <- plot_log(fit15b_summer, "z_depth_point_mean [all]")
p7 <- plot_log(fit15b_summer, "z_depth_point_sd [all]")
p8 <- plot_log(fit15b_summer, "z_faults_km [all]")
p9 <- plot_log(fit15b_summer, "z_dist_canyon_km [all]")
p10 <- plot_log(fit15b_summer, "z_weighted_dist [all]")
p11 <- plot_log(fit15b_summer, "z_weighted_fuel_pricegal [all]")
p12 <- plot_log(fit15b_summer, "z_weighted_crab_ppp [all]")
p13 <- plot_log(fit15b_summer, "z_bottom_O2_avg [all]")
p14 <- plot_log(fit15b_summer, "z_dist_to_closed_km [all]")

gridExtra::grid.arrange(p1,p2,p3,p35,ncol=2)

gridExtra::grid.arrange(p4,p5,p6,p7,ncol=2)

gridExtra::grid.arrange(p8,p9,p13,p14,ncol=2)

gridExtra::grid.arrange(p10,p11,p12,ncol=2)

res <- residuals(fit15b_summer)
qqnorm(res,ylim=c(-5,5))
qqline(res)





tic()
fit16_summer <- sdmTMB(tottraps ~ 0 + 
                         season +
                         half_month_of_seasonf + 
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
                         z_weighted_crab_ppp  +
                         z_bottom_O2_avg +
                         OR_WA_waters * z_dist_to_closed_km,
                       family = tweedie(),
                       mesh = mesh_summer,
                       spatial = "on",
                       spatiotemporal = "ar1",
                       data = summer,
                       time = "yearn")
toc() #17min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.0233357238448519.
#sanity(fit16_summer)
#b_js, ln_phi
#sanity(fit16_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js, ln_phi
AIC(fit16_summer)
#266441.8
summary(fit16_summer)

#EXPORT THIS MODEL
#write_rds(fit16_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit16_summer.rds"))





tic()
fit17_summer <- sdmTMB(tottraps ~ 0 + 
                         season +
                         half_month_of_seasonf + 
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
                         z_weighted_crab_ppp  +
                         z_bottom_O2_avg +
                         z_dist_to_closed_km,
                       family = tweedie(),
                       mesh = mesh_summer,
                       spatial = "on",
                       spatiotemporal = "ar1",
                       data = summer,
                       time = "yearn")
toc() #11min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.0192603716117148. 
#sanity(fit17_summer)
#b_js, ln_tau_E, ln_kappa, thetaf, ln_phi, ar1_phi
#sanity(fit17_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js, ln_tau_E, thetaf, ln_phi
AIC(fit17_summer)
#266437.4
summary(fit17_summer)

#EXPORT THIS MODEL
#write_rds(fit17_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit17_summer.rds"))






tic()
fit18_summer <- sdmTMB(tottraps ~ 0 + 
                         season +
                         half_month_of_seasonf + 
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
                         z_weighted_crab_ppp  +
                         z_bottom_O2_avg +
                         z_dist_to_closed_km,
                       family = tweedie(),
                       mesh = mesh_summer,
                       spatial = "on",
                       spatiotemporal = "ar1",
                       data = summer,
                       time = "yearn")
toc() #11min

# when seed set and no polynomials
# The model may not have converged. Maximum final gradient: 0.0582685262179083. 
#sanity(fit18_summer)
#b_js, ln_tau_O
#sanity(fit18_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js only
AIC(fit18_summer)
#266432.8
summary(fit18_summer)

#EXPORT THIS MODEL
#write_rds(fit18_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit18_summer.rds"))






###THIS MODEL IS NOT ANY BETTER OR USEFUL

tic()
fit19_summer <- sdmTMB(tottraps ~ 0 + 
                         season +
                         half_month_of_seasonf + 
                         OR_WA_waters * WA_pot_reduction+ 
                         #WA_pot_reduction +  #part of interaction term
                         z_SST_avg + 
                         z_wind_avg + 
                         poly(z_depth_point_mean,2) +
                         z_depth_point_sd +
                         z_faults_km +
                         z_dist_canyon_km +
                         z_weighted_dist + 
                         z_weighted_fuel_pricegal  + 
                         z_weighted_crab_ppp  +
                         z_bottom_O2_avg +
                         z_dist_to_closed_km,
                       family = tweedie(),
                       mesh = mesh_summer,
                       spatial = "on",
                       spatiotemporal = "ar1",
                       data = summer,
                       time = "yearn")
toc() #19min

# when seed set and no polynomials
#  The model may not have converged. Maximum final gradient: 0.062860262338603. 
#sanity(fit19_summer)
#b_js, thetaf, `b_j` standard error may be large
#sanity(fit19_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_js, thetaf, `b_j` standard error may be large
AIC(fit19_summer)
#266441.9 - not the best AIC out of interactions
summary(fit19_summer)
##NOT EXPORTED

###############

tic()
fit19_summer <- sdmTMB(tottraps ~ 0 + 
                         season +
                         half_month_of_seasonf + 
                         OR_WA_waters +
                         WA_pot_reduction +  
                         z_SST_avg + 
                         z_wind_avg +
                         poly(z_depth_point_mean,2) * z_bottom_O2_avg  +
                         z_depth_point_sd +
                         z_faults_km +
                         z_dist_canyon_km +
                         z_weighted_dist +
                         z_weighted_fuel_pricegal + 
                         z_weighted_crab_ppp  +
                         #z_bottom_O2_avg + #part of interaction term
                         z_dist_to_closed_km,
                       family = tweedie(),
                       mesh = mesh_summer,
                       spatial = "on",
                       spatiotemporal = "ar1",
                       data = summer,
                       time = "yearn")
toc() #15min

# when seed set and no polynomials
#The model may not have converged. Maximum final gradient: 0.0227277651881579. 
#sanity(fit19_summer)
#b_js, thetaf, ln_phi
#sanity(fit19_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#b_j, thetaf, ln_phi
AIC(fit19_summer)
#266082.3
summary(fit19_summer)

#EXPORT THIS MODEL
#write_rds(fit19_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit19_summer.rds"))


#test  the optimisation run
#fit19b_summer <- run_extra_optimization(fit19_summer, nlminb_loops = 0, newton_loops = 1)
#no warnings
#no red Xs
#AIC: 266082.3 -- same as before

#EXPORT THIS MODEL
#write_rds(fit19b_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC',"fit19b_summer.rds"))


plot_log = function(object, term) {
  g <- ggeffect(object, term, back.transform = FALSE)
  g$conf.low <- log(g$conf.low)
  g$conf.high <- log(g$conf.high)
  g$predicted <- log(g$predicted)
  plot(g)
}


p1 <- plot_log(fit19b_summer, "season [all]")
p2 <- plot_log(fit19b_summer, "half_month_of_seasonf [all]")
p3 <- plot_log(fit19b_summer, "OR_WA_waters [all]")
p35 <- plot_log(fit19b_summer, "WA_pot_reduction [all]")
p4 <- plot_log(fit19b_summer, "z_SST_avg [all]")
p5 <- plot_log(fit19b_summer, "z_wind_avg [all]")
p6 <- plot_log(fit19b_summer, "z_depth_point_mean [all]")
p7 <- plot_log(fit19b_summer, "z_depth_point_sd [all]")
p8 <- plot_log(fit19b_summer, "z_faults_km [all]")
p9 <- plot_log(fit19b_summer, "z_dist_canyon_km [all]")
p10 <- plot_log(fit19b_summer, "z_weighted_dist [all]")
p11 <- plot_log(fit19b_summer, "z_weighted_fuel_pricegal [all]")
p12 <- plot_log(fit19b_summer, "z_weighted_crab_ppp [all]")
p13 <- plot_log(fit19b_summer, "z_bottom_O2_avg [all]")
p14 <- plot_log(fit19b_summer, "z_dist_to_closed_km [all]")

gridExtra::grid.arrange(p1,p2,p3,p35,ncol=2)

gridExtra::grid.arrange(p4,p5,p6,p7,ncol=2)

gridExtra::grid.arrange(p8,p9,p13,p14,ncol=2)

gridExtra::grid.arrange(p10,p11,p12,ncol=2)

res <- residuals(fit19b_summer)
qqnorm(res)
qqline(res)


#-----------------------------------------

#given that depth is so overwhelming in model estimates, what if that was the only predictor 
#(with spatial and spatio-temporal fields)?

tic()
fit20_summer <- sdmTMB(tottraps ~ 0 + 
                       poly(z_depth_point_mean,2),
                       family = tweedie(),
                       mesh = mesh_summer,
                       spatial = "on",
                       spatiotemporal = "ar1",
                       data = summer,
                       time = "yearn")
toc() #min

# when seed set 
# no warnings
#sanity(fit20_summer)
#no red Xs
#sanity(fit20_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#
AIC(fit20_summer)
#271112.3 -- not better than e.g. fit13
summary(fit20_summer)



