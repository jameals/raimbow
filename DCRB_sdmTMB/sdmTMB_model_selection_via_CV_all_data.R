#sdmTMB model selection via cross validation - all data

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
#---------------------------------------------


#d = readRDS("data/df_full_final_tidy_all_data.rds")
#if instead read in this one, can skip couple of the next steps:
d <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_all_data_20230324.rds')) 

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

#test 1:
#covariates only, no polynomials, month_name_f is fixed effect, no s s-t fields, time='yearn'

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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_name_f + 
                                 OR_WA_waters + 
                                 #WA_pot_reduction + #eric thinks this is the root of converging issues
                                 z_SST_avg  + 
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


# 16min
#tot_elpd = -2.35237
#tot_loglik = -60384.15

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The model may not have converged. Maximum final gradient: 0.0157588012743389.
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.

#MODEL DID CONVERGE
# [[5]]$converged
# [1] FALSE
# 
# [[5]]$pdHess
# [1] FALSE  TRUE



cv_test1_all_data <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test1_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test1_all_data.rds"))

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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_name_f + 
                                 OR_WA_waters + 
                                 #WA_pot_reduction + #eric thinks this is the root of converging issues
                                 z_SST_avg  + 
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


# 45min
#tot_elpd = -2.299429
#tot_loglik = -58524.59

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The model may not have converged. Maximum final gradient: 0.0157588012743389.
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.

#MODEL DID CONVERGE
# [[5]]$converged
# [1] FALSE
# 
# [[5]]$pdHess
# [1] FALSE  TRUE



cv_test2_all_data <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test2_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test2_all_data.rds"))

#---------------------------------------------

#test 2b
#covariates, no polynomials, month_name_f is fixed effect, 
#no s but yes s-t fields (iid), time='yearn'

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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_name_f + 
                                 OR_WA_waters + 
                                 #WA_pot_reduction + #eric thinks this is the root of converging issues
                                 z_SST_avg  + 
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


# 45min
#tot_elpd = -2.303744
#tot_loglik = -58770.55

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The model may not have converged. Maximum final gradient: 0.13379478212002
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.

#MODEL DID CONVERGE
# [[5]]$converged
# [1] FALSE
# 
# [[5]]$pdHess
# [1] FALSE  TRUE



cv_test2b_all_data <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test2b_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test2b_all_data.rds"))

#---------------------------------------------


#test 2c
#covariates, no polynomials, month_name_f is fixed effect, 
#yes s but no s-t fields, time='month_n'

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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_name_f + 
                                 OR_WA_waters + 
                                 #WA_pot_reduction + #eric thinks this is the root of converging issues
                                 z_SST_avg  + 
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
                               time = "month_n")
  #cv_fits[[1]] is now a list of 2 models. We want the second of each of these, 
  model_selection$elpd[indx] <- cv_fits[[indx]]$fold_elpd[2]
  model_selection$loglik[indx] <- cv_fits[[indx]]$fold_loglik[2]
}
# total the log lik or ELPD now across years
tot_elpd <- sum(model_selection$elpd)
tot_loglik <- sum(model_selection$loglik)
toc()


# 46min
#tot_elpd = -2.299434
#tot_loglik = -58524.58

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The model may not have converged. Maximum final gradient: 0.145534323638294
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.

#MODEL DID CONVERGE
# [[5]]$converged
# [1] FALSE
# 
# [[5]]$pdHess
# [1] FALSE  TRUE



cv_test2c_all_data <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test2c_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test2c_all_data.rds"))

#---------------------------------------------

#test 2d
#covariates, no polynomials, month_name_f is fixed effect, 
#no s but yes s-t fields (ar1), time='half-month-of-season'

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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_name_f + 
                                 OR_WA_waters + 
                                 #WA_pot_reduction + #eric thinks this is the root of converging issues
                                 z_SST_avg  + 
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
                               spatiotemporal = "ar1",
                               data = sub,
                               time = "half_month_of_season")
  #cv_fits[[1]] is now a list of 2 models. We want the second of each of these, 
  model_selection$elpd[indx] <- cv_fits[[indx]]$fold_elpd[2]
  model_selection$loglik[indx] <- cv_fits[[indx]]$fold_loglik[2]
}
# total the log lik or ELPD now across years
tot_elpd <- sum(model_selection$elpd)
tot_loglik <- sum(model_selection$loglik)
toc()


# 3.9h
#tot_elpd = -2.074545
#tot_loglik = -59908.58

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The model may not have converged. Maximum final gradient: 0.145534323638294
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
#5: In stats::nlminb(start = tmb_obj$par, objective = tmb_obj$fn,  ... :  NA/NaN function evaluation

#MODEL DID CONVERGE
# [[5]]$converged
# [1] FALSE
# 
# [[5]]$pdHess
# [1] FALSE  TRUE



cv_test2d_all_data <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test2d_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test2d_all_data.rds"))

#---------------------------------------------

#test 3
#covariates, no polynomials, month_name_f is fixed effect, 
#yes s and s-t fields (iid), time='yearn'

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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_name_f + 
                                 OR_WA_waters + 
                                 #WA_pot_reduction + #eric thinks this is the root of converging issues
                                 z_SST_avg  + 
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


# 51min
#tot_elpd = -2.299588
#tot_loglik = -58609.67

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The model may not have converged. Maximum final gradient: 0.135093975809749.
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 13: In stats::nlminb(start = tmb_obj$par, objective = tmb_obj$fn,  ... : NA/NaN function evaluation

#MODEL DID CONVERGE
# [[5]]$converged
# [1] FALSE
# 
# [[5]]$pdHess
# [1] FALSE  TRUE



cv_test3_all_data <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test3_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test3_all_data.rds"))

#---------------------------------------------

#test 4
#covariates, no polynomials, month_name_f is fixed effect, 
#yes s and s-t fields (ar1), time='yearn'

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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_name_f + 
                                 OR_WA_waters + 
                                 #WA_pot_reduction + #eric thinks this is the root of converging issues
                                 z_SST_avg  + 
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


# 1.6h
#tot_elpd = -2.29816
#tot_loglik = -58621.33

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The model may not have converged. Maximum final gradient: 0.135093975809749.
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 13: In stats::nlminb(start = tmb_obj$par, objective = tmb_obj$fn,  ... : NA/NaN function evaluation

#MODEL DID CONVERGE
# [[5]]$converged
# [1] FALSE
# 
# [[5]]$pdHess
# [1] FALSE  TRUE


cv_test4_all_data <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test4_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test4_all_data.rds"))

#---------------------------------------------

#test 5
#No covariates
#yes s but no s-t fields, time='yearn'

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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0, 
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


# 4min
#tot_elpd = -2.735127
#tot_loglik = -60752.86

#Warning messages:
# 1: The model may not have converged. Maximum final gradient: 0.0160903480331502. 
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.

#MODEL DID CONVERGE
# [[1]]$converged
# [1] TRUE
# 
# [[1]]$pdHess
# [1] TRUE TRUE


cv_test5_all_data <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test5_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test5_all_data.rds"))

#---------------------------------------------

#test 6
#No covariates
#yes s and s-t fields (iid), time='yearn'

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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0, 
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


# 8min
#tot_elpd = -3.01028
#tot_loglik = -62876.93

#Warning messages:
# 1: The model may not have converged. Maximum final gradient: 0.0160903480331502. 
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.

#MODEL DID CONVERGE
# [[3]]$converged
# [1] FALSE
# 
# [[3]]$pdHess
# [1] FALSE  TRUE


cv_test6_all_data <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test6_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test6_all_data.rds"))

#---------------------------------------------


#test 7
#No covariates
#yes s and s-t fields (ar1, time='yearn'

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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0, 
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

warnings()

# 17min
#tot_elpd = -3.007238
#tot_loglik = -62870.88

#Warning messages:
# 2: In stats::nlminb(start = tmb_obj$par, objective = tmb_obj$fn,  ... : NA/NaN function evaluation
# 3: In sqrt(diag(cov)) : NaNs produced
# 4: The model may not have converged: non-positive-definite Hessian matrix.
# 5: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.

#MODEL DID CONVERGE
# [[1]]$converged
# [1] FALSE
# 
# [[1]]$pdHess
# [1] FALSE  TRUE


cv_test7_all_data <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test7_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test7_all_data.rds"))

#---------------------------------------------

#test 8
#covariates, no polynomials, month_name_f is fixed effect, 
#yes s and s-t fields (ar1), time='month_n'

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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_name_f + 
                                 OR_WA_waters + 
                                 #WA_pot_reduction + #eric thinks this is the root of converging issues
                                 z_SST_avg  + 
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


# 1.7h
#tot_elpd = -2.298181
#tot_loglik = -58621.44

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The model may not have converged. Maximum final gradient: 0.073879206986283.
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 7: In stats::nlminb(start = tmb_obj$par, objective = tmb_obj$fn,  ... :     NA/NaN function evaluation

#MODEL DID CONVERGE
# [[5]]$converged
# [1] FALSE
# 
# [[5]]$pdHess
# [1] FALSE  TRUE


cv_test8_all_data <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test8_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test8_all_data.rds"))

#---------------------------------------------

#test 9
#covariates, no polynomials, month_name_f is fixed effect, 
#yes s and s-t fields (ar1), time='month_of_season'

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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_name_f + 
                                 OR_WA_waters + 
                                 #WA_pot_reduction + #eric thinks this is the root of converging issues
                                 z_SST_avg  + 
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
                               time = "month_of_season")
  #cv_fits[[1]] is now a list of 2 models. We want the second of each of these, 
  model_selection$elpd[indx] <- cv_fits[[indx]]$fold_elpd[2]
  model_selection$loglik[indx] <- cv_fits[[indx]]$fold_loglik[2]
}
# total the log lik or ELPD now across years
tot_elpd <- sum(model_selection$elpd)
tot_loglik <- sum(model_selection$loglik)
toc()


# h
#tot_elpd = -2.083177
#tot_loglik = -60673.67

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The model may not have converged. Maximum final gradient: 0.248144635757374.
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.

#MODEL DID CONVERGE
# [[5]]$converged
# [1] FALSE
# 
# [[5]]$pdHess
# [1] FALSE  TRUE


cv_test9_all_data <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test9_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test9_all_data.rds"))

#---------------------------------------------

#test 10
#covariates, no polynomials, month_name_f is fixed effect, 
#yes s and s-t fields (ar1), time='month_of_season'

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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_name_f + 
                                 OR_WA_waters + 
                                 #WA_pot_reduction + #eric thinks this is the root of converging issues
                                 z_SST_avg  + 
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
                               time = "half_month_of_season")
  #cv_fits[[1]] is now a list of 2 models. We want the second of each of these, 
  model_selection$elpd[indx] <- cv_fits[[indx]]$fold_elpd[2]
  model_selection$loglik[indx] <- cv_fits[[indx]]$fold_loglik[2]
}
# total the log lik or ELPD now across years
tot_elpd <- sum(model_selection$elpd)
tot_loglik <- sum(model_selection$loglik)
toc()


# 5h
#tot_elpd = -2.074554
#tot_loglik = -59910.92

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The model may not have converged. Maximum final gradient: 0.343190260309679.
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 7: In stats::nlminb(start = tmb_obj$par, objective = tmb_obj$fn,  ... : NA/NaN function evaluation
# 12: In sqrt(diag(cov)) : NaNs produced

#MODEL DID CONVERGE
# [[5]]$converged
# [1] FALSE
# 
# [[5]]$pdHess
# [1] FALSE  TRUE


cv_test10_all_data <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test10_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test10_all_data.rds"))

#---------------------------------------------

#test 11
#covariates, no polynomials, month_of_season_f is fixed effect, 
#yes s and s-t fields (ar1), time='yearn'

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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_of_seasonf + 
                                 OR_WA_waters + 
                                 #WA_pot_reduction + #eric thinks this is the root of converging issues
                                 z_SST_avg  + 
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


# 1.9h
#tot_elpd = -2.360101
#tot_loglik = -58714.99

#Warning messages:
# 1: In stats::nlminb(start = tmb_obj$par, objective = tmb_obj$fn,  ... : NA/NaN function evaluation
# 2: The model may not have converged: non-positive-definite Hessian matrix.
# 3: The model may not have converged. Maximum final gradient: 0.0211982542444815.
# 4: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.

#MODEL DID CONVERGE
# [[5]]$converged
# [1] FALSE
# 
# [[5]]$pdHess
# [1] FALSE  TRUE


cv_test11_all_data <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test11_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test11_all_data.rds"))

#---------------------------------------------

#test 12
#covariates, no polynomials, half_month_of_season_f is fixed effect, 
#yes s and s-t fields (ar1), time='yearn'

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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_of_seasonf + 
                                 OR_WA_waters + 
                                 #WA_pot_reduction + #eric thinks this is the root of converging issues
                                 z_SST_avg  + 
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


# 2h
#tot_elpd = -2.3601
#tot_loglik = -58715

#Warning messages:
# 1: In stats::nlminb(start = tmb_obj$par, objective = tmb_obj$fn,  ... : NA/NaN function evaluation
# 2: The model may not have converged: non-positive-definite Hessian matrix.
# 3: The model may not have converged. Maximum final gradient: 0.0211982542444815.
# 4: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.

#MODEL DID CONVERGE
# [[5]]$converged
# [1] FALSE
# 
# [[5]]$pdHess
# [1] FALSE  TRUE


cv_test12_all_data <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test12_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test12_all_data.rds"))

#---------------------------------------------
# so far best one if test2_all_data. use that to build further. add polynomial term
#---------------------------------------------

#test 13
#covariates, polynomial for depth, month_name_f is fixed effect, 
#yes s but no s-t fields, time='yearn'

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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_name_f + 
                                 OR_WA_waters + 
                                 #WA_pot_reduction + #eric thinks this is the root of converging issues
                                 z_SST_avg  + 
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


# min
#tot_elpd = -
#tot_loglik = -

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The model may not have converged. Maximum final gradient: 0.0157588012743389.
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.

#MODEL DID CONVERGE
# [[5]]$converged
# [1] FALSE
# 
# [[5]]$pdHess
# [1] FALSE  TRUE



cv_test13_all_data <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test13_all_data, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test13_all_data.rds"))

#---------------------------------------------















