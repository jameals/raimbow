#sdmTMB model selection via cross validation - winter

#---------------------------------------------

#use Eric's code from here: https://github.com/jameals/raimbow/blob/master/DCRB_sdmTMB/cv_example_winter.qmd


#---------------------------------------------


library(here)
library(tidyverse)
library(tictoc)
library(viridis)

library(sdmTMB)
library(mgcv)
library(ggeffects)
library(ggplot2)

#this was needed for sdmTMB to work
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
#---------------------------------------------
#-------------------------------------------------------------------------------------------------

set.seed(123)

#-------------------------------------------------------------------------------------------------

## Data loading and cleaning


#d = readRDS("data/df_full_final_tidy_all_data.rds")

#if instead read in this one, can skip couple of the next steps:
#but note that we need May in the data set as well
d <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_all_data_20230324.rds')) 
d$month_name_f <- factor(d$month_name, levels = c("December", "January", "February", "March", "April", 
                                                  "May", "June", "July", "August", "September"))

# # Filter out NAs
# d$yearn <- as.numeric(substr(d$season,1,4))
# d$yearf <- as.factor(d$yearn)

winter <- dplyr::filter(d, month_name %in% c("December", "January", "February",
                                             "March", "April", "May"))

# # try smooth over months
# winter$month_n <- 1
# winter$month_n[which(winter$month_name=="January")] = 2
# winter$month_n[which(winter$month_name=="February")] = 3
# winter$month_n[which(winter$month_name=="March")] = 4
# winter$month_n[which(winter$month_name=="April")] = 5
# winter$month_n[which(winter$month_name=="May")] = 6


# Add UTM columns (zone 10)
winter = add_utm_columns(winter, ll_names = c("grd_x", "grd_y"))


#---------------------------------------------

#test 1
#covariates only, no polynomials, month_name_f is fixed effect, no s s-t fields, tie='yearn'


tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  sub <- dplyr::filter(winter, yearn <= yr)
  # assign folds using april_2 in this year as test/validation set
  sub$fold_id <- 1
  sub$fold_id[which(sub$yearn == yr & sub$month_name == "May")] <- 2
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  sub$month_name_f <- as.factor(as.character(sub$month_name_f))
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
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
                               fold_ids = sub$fold_id,
                               mesh = mesh,
                               spatial = "off",
                               spatiotemporal = "off",
                               data = sub,
                               time = "yearn")
  #cv_fits[[1]] is now a list of 2 models. We want the second of each of these, 
  model_selection$elpd[indx] <- cv_fits[[indx]]$fold_elpd[2]
  model_selection$loglik[indx] <- cv_fits[[indx]]$fold_loglik[2]
}
# total the log lik or ELPD now across years
tot_elpd <- sum(model_selection$elpd)
tot_loglik <- sum(model_selection$loglik)
toc()

# 15min
#tot_elpd = -3.487335
#tot_loglik = -27448.2

#Warning messages:
#  1: The model may not have converged: non-positive-definite Hessian matrix. 
#2: The model may not have converged. Maximum final gradient: 0.0129883306598302. 

#MODEL DID CONVERGE
#[[5]]$converged
#[1] FALSE

#[[5]]$pdHess
#[1] FALSE  TRUE


cv_test1_winter <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test1_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test1_winter.rds"))

#---------------------------------------------

#---------------------------------------------

#test 2
#covariates, no polynomials, month_name_f is fixed effect, 
#yes s but no s-t fields, time='yearn'


tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  sub <- dplyr::filter(winter, yearn <= yr)
  # assign folds using april_2 in this year as test/validation set
  sub$fold_id <- 1
  sub$fold_id[which(sub$yearn == yr & sub$month_name == "May")] <- 2
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  sub$month_name_f <- as.factor(as.character(sub$month_name_f))
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
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
                               fold_ids = sub$fold_id,
                               mesh = mesh,
                               spatial = "on",
                               spatiotemporal = "off",
                               data = sub,
                               time = "yearn")
  #cv_fits[[1]] is now a list of 2 models. We want the second of each of these, 
  model_selection$elpd[indx] <- cv_fits[[indx]]$fold_elpd[2]
  model_selection$loglik[indx] <- cv_fits[[indx]]$fold_loglik[2]
}
# total the log lik or ELPD now across years
tot_elpd <- sum(model_selection$elpd)
tot_loglik <- sum(model_selection$loglik)
toc()

# 42min
#tot_elpd = -3.32651
#tot_loglik = -26621.72

#Warning messages:
#  1: The model may not have converged: non-positive-definite Hessian matrix. 
#2: The model may not have converged. Maximum final gradient: 0.0782281376894849. 

#MODEL DIDN'T CONVERGE
#[[5]]$converged
#[1] FALSE
#
#[[5]]$pdHess
#[1] FALSE FALSE


cv_test2_winter <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test2_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test2_winter.rds"))

#---------------------------------------------

#test 3
#covariates, no polynomials, month_name_f is fixed effect, 
# s and  s-t fields (iid), time='yearn'


tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  sub <- dplyr::filter(winter, yearn <= yr)
  # assign folds using april_2 in this year as test/validation set
  sub$fold_id <- 1
  sub$fold_id[which(sub$yearn == yr & sub$month_name == "May")] <- 2
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  sub$month_name_f <- as.factor(as.character(sub$month_name_f))
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
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
                               fold_ids = sub$fold_id,
                               mesh = mesh,
                               spatial = "on",
                               spatiotemporal = "iid",
                               data = sub,
                               time = "yearn")
  #cv_fits[[1]] is now a list of 2 models. We want the second of each of these, 
  model_selection$elpd[indx] <- cv_fits[[indx]]$fold_elpd[2]
  model_selection$loglik[indx] <- cv_fits[[indx]]$fold_loglik[2]
}
# total the log lik or ELPD now across years
tot_elpd <- sum(model_selection$elpd)
tot_loglik <- sum(model_selection$loglik)
toc()

# 49min
#tot_elpd = -3.293958
#tot_loglik = -26426.45

#Warning messages:
#  1: The model may not have converged: non-positive-definite Hessian matrix. 

#MODEL DIDN'T CONVERGE
#[[5]]$converged
#[1] FALSE
#
#[[5]]$pdHess
#[1] FALSE FALSE


cv_test3_winter <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test3_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test3_winter.rds"))

#---------------------------------------------

#test 4
#covariates, no polynomials, month_name_f is fixed effect, 
# s and  s-t fields (ar1), time='yearn'


tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  sub <- dplyr::filter(winter, yearn <= yr)
  # assign folds using april_2 in this year as test/validation set
  sub$fold_id <- 1
  sub$fold_id[which(sub$yearn == yr & sub$month_name == "May")] <- 2
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  sub$month_name_f <- as.factor(as.character(sub$month_name_f))
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
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
                               fold_ids = sub$fold_id,
                               mesh = mesh,
                               spatial = "on",
                               spatiotemporal = "ar1",
                               data = sub,
                               time = "yearn")
  #cv_fits[[1]] is now a list of 2 models. We want the second of each of these, 
  model_selection$elpd[indx] <- cv_fits[[indx]]$fold_elpd[2]
  model_selection$loglik[indx] <- cv_fits[[indx]]$fold_loglik[2]
}
# total the log lik or ELPD now across years
tot_elpd <- sum(model_selection$elpd)
tot_loglik <- sum(model_selection$loglik)
toc()

# 49min
#tot_elpd = -3.293958
#tot_loglik = -26426.45

#Warning messages:
#  1: The model may not have converged: non-positive-definite Hessian matrix. 

#MODEL DIDN'T CONVERGE
#[[5]]$converged
#[1] FALSE
#
#[[5]]$pdHess
#[1] FALSE FALSE


cv_test3_winter <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test3_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test3_winter.rds"))

#---------------------------------------------















#---------------------------------------------







































#----------------------------------
#just a test to see if a model that used to work still works - yes


#try this on test 3


tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  sub <- dplyr::filter(winter, yearn <= yr)
  # assign folds using april_2 in this year as test/validation set
  sub$fold_id <- 1
  sub$fold_id[which(sub$yearn == yr & sub$month_name == "May")] <- 2
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  sub$month_name_f <- as.factor(as.character(sub$month_name_f))
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season +
                                 month_name_f +  #new
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
                               fold_ids = sub$fold_id,
                               mesh = mesh,
                               spatial = "on",
                               spatiotemporal = "ar1",
                               data = sub,
                               time = "yearn")
  #cv_fits[[1]] is now a list of 2 models. We want the second of each of these, 
  model_selection$elpd[indx] <- cv_fits[[indx]]$fold_elpd[2]
  model_selection$loglik[indx] <- cv_fits[[indx]]$fold_loglik[2]
}
# total the log lik or ELPD now across years
tot_elpd <- sum(model_selection$elpd)
tot_loglik <- sum(model_selection$loglik)
toc()

#1.3h
#tot_elpd = -3.262216
#tot_loglik = -26346.47
#1: The model may not have converged: non-positive-definite Hessian matrix. 
#2: The model may not have converged. Maximum final gradient: 0.0661242184450632. 


#models[[1]] coef.se are NaN, but models[[2]] has real values
# $converged
# [1] FALSE
# $pdHess
# [1] FALSE  TRUE

cv_test3_winter <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test3_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test3_winter.rds"))

#---------------------------------------------


























