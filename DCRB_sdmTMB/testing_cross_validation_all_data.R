## testing cross validation - all data (winter + summer) 

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


#---------------------------------------------


#d = readRDS("data/df_full_final_tidy_all_data.rds")
#if instead read in this one, can skip couple of the next steps:
d <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_all_data_20230209.rds')) 

d$month_name_f <- factor(d$month_name, levels = c("December", "January", "February", "March", "April", 
                                                  "May", "June", "July", "August", "September"))



# Filter out NAs
#d$yearn <- as.numeric(substr(d$season,1,4))
#d$yearf <- as.factor(d$yearn)


# try smooth over months
# d$month_n <- 1
# d$month_n[which(d$month_name=="January")] = 2
# d$month_n[which(d$month_name=="February")] = 3
# d$month_n[which(d$month_name=="March")] = 4
# d$month_n[which(d$month_name=="April")] = 5
# d$month_n[which(d$month_name=="May")] = 6
# d$month_n[which(d$month_name=="June")] = 7
# d$month_n[which(d$month_name=="July")] = 8
# d$month_n[which(d$month_name=="August")] = 9
# d$month_n[which(d$month_name=="September")] = 10


# Add UTM columns (zone 10)
d = add_utm_columns(d, ll_names = c("grd_x", "grd_y"))


#---------------------------------------------

#follow the order of models tested with winter data
#test 1:

tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  train <- dplyr::filter(d, yearn < yr)
  # add in data for this current year -- up to May
  train2 <- dplyr::filter(d, yearn == yr, month_name %in% c("December","January","February","March","April"))
  train$fold_id <- 1
  train2$fold_id <- 1
  test <- dplyr::filter(d, yearn == yr, month_name %in% c("May","June","July"))
  test$fold_id <- 2
  sub <- rbind(test, train, train2)
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + season +
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


#took about  1.9hours
#tot_elpd = -2.353104
#tot_loglik = -58,887.09
# 2: The model may not have converged: non-positive-definite Hessian matrix.
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 14: The model may not have converged. Maximum final gradient: 0.12929153157847.



#---------------------------------------------

#same as above but with iid
#test 2:

tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  train <- dplyr::filter(d, yearn < yr)
  # add in data for this current year -- up to May
  train2 <- dplyr::filter(d, yearn == yr, month_name %in% c("December","January","February","March","April"))
  train$fold_id <- 1
  train2$fold_id <- 1
  test <- dplyr::filter(d, yearn == yr, month_name %in% c("May","June","July"))
  test$fold_id <- 2
  sub <- rbind(test, train, train2)
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + season +
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


#took about  52min
#tot_elpd = -2.354905
#tot_loglik = -58,878.34
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 14: The model may not have converged. Maximum final gradient: 0.218359563143295.



#---------------------------------------------

#test 3:

tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  train <- dplyr::filter(d, yearn < yr)
  # add in data for this current year -- up to May
  train2 <- dplyr::filter(d, yearn == yr, month_name %in% c("December","January","February","March","April"))
  train$fold_id <- 1
  train2$fold_id <- 1
  test <- dplyr::filter(d, yearn == yr, month_name %in% c("May","June","July"))
  test$fold_id <- 2
  sub <- rbind(test, train, train2)
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + season +
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


#took about 2.4hours 
#tot_elpd = -2.28502
#tot_loglik = -58786.01

# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 4: In stats::nlminb(start = tmb_obj$par, objective = tmb_obj$fn,  ... : NA/NaN function evaluation
# 15: The model may not have converged. Maximum final gradient: 0.260063049009489.



#---------------------------------------------




#---------------------------------------------















