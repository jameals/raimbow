## testing cross validation - summer 

#---------------------------------------------

#use Eric's code from here: https://github.com/jameals/raimbow/blob/master/DCRB_sdmTMB/cv_example_summer.qmd


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


## Data loading and cleaning


#d = readRDS("data/df_full_final_tidy_all_data.rds")

#if instead read in this one, can skip couple of the next steps:
summer <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_summer_20230209.rds')) 
summer$month_name_f <- factor(summer$month_name, levels = c("May", "June", "July", "August", "September"))

# # Filter out NAs
# d$yearn <- as.numeric(substr(d$season,1,4))
# d$yearf <- as.factor(d$yearn)
# summer <- dplyr::filter(d, month_name %in% c("May","June","July","August","September"))
# # try smooth over months
# summer$month_n <- 1
# summer$month_n[which(summer$month_name=="June")] = 2
# summer$month_n[which(summer$month_name=="July")] = 3
# summer$month_n[which(summer$month_name=="August")] = 4
# summer$month_n[which(summer$month_name=="September")] = 5


# Add UTM columns (zone 10)
summer = add_utm_columns(summer, ll_names = c("grd_x", "grd_y"))




d <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_all_data_20230209.rds')) 

d$month_name_f <- factor(d$month_name, levels = c("December", "January", "February", "March", "April", 
                                                  "May", "June", "July", "August", "September"))

# Filter out NAs
#d$yearn <- as.numeric(substr(d$season,1,4))
#d$yearf <- as.factor(d$yearn)

summer <- dplyr::filter(d, month_name %in% c("May","June","July","August","September"))

# # try smooth over months
# summer$month_n <- 1
# summer$month_n[which(summer$month_name=="June")] = 2
# summer$month_n[which(summer$month_name=="July")] = 3
# summer$month_n[which(summer$month_name=="August")] = 4
# summer$month_n[which(summer$month_name=="September")] = 5

# Add UTM columns (zone 10)
summer = add_utm_columns(summer, ll_names = c("grd_x", "grd_y"))














#---------------------------------------------

## Workflow

#-   identify a small number of models to do the cross validation for. below is code for 1 model, and you'd want to repeat this for several
#-   identify time period used for validation. below, it's the second half-month of April. For the May/summer models, this filtering will have to be slightly different.
#-   to make sure our predictions are robust, we'll want to test each of these models against 5-10 years of data. These are forward looking, so all future years are not included in fits
#-   sdmTMB_cv replaces sdmTMB, where we include the fold IDs
#-   several metrics exist for model selection, and we want to total these across our validation years. The highest ELPD or loglik will correspond to the model with highest predictive accuracy.

#---------------------------------------------

## Example of a model

# first test: fit10f_summer -- except can't add in polynomials without getting an error - so do fit10f without polynomials
#now try this with new input file and polynomials should work --no still doesn't work...


tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  train <- dplyr::filter(summer, yearn < yr)
  train$fold_id <- 1
  test <- dplyr::filter(summer, yearn == yr, month_name == "May")
  test$fold_id <- 2
  sub <- rbind(test, train)
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 half_month_of_seasonf + 
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

#took about 40-45mins (when input df that is already prefiltered to be summer and then z-scored)
#tot_elpd = -9.716058
#tot_loglik = -1,148,336

#exported model
#write_rds(cv_fits[[indx]],here::here('DCRB_sdmTMB', 'exported model objects', 'cross validation',"fit10f_summer_noPolys.rds"))

#when re-run with the new input file (all data, and then filter) but no polynomials
#46min
#1: The model may not have converged: non-positive-definite Hessian matrix.
#2: The time elements in `newdata` are not identical to those in the original dataset.
#This is normally fine, but may create problems for index standardization.
#tot_elpd = -9.690052
#tot_loglik = -1,128,565


#---------------------------------------------

#same as fit10f_summer above (with no polys), but now replace ar1 with iid

tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  train <- dplyr::filter(summer, yearn < yr)
  train$fold_id <- 1
  test <- dplyr::filter(summer, yearn == yr, month_name == "May")
  test$fold_id <- 2
  sub <- rbind(test, train)
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 half_month_of_seasonf + 
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

#took about 20mins
#tot_elpd = -9.805504
#tot_loglik = -1,214,130 -- more negative than with ar1, so ar1 has better predictive ability?


#---------------------------------------------

#now try version of fit11c_summer (but no polys) -- this one can be run with polys

tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  train <- dplyr::filter(summer, yearn < yr)
  train$fold_id <- 1
  test <- dplyr::filter(summer, yearn == yr, month_name == "May")
  test$fold_id <- 2
  sub <- rbind(test, train)
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_name_f  + 
                                 OR_WA_waters +
                                 WA_pot_reduction +
                                 z_SST_avg  + 
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
                               fold_ids = sub$fold_id,
                               mesh = mesh,
                               spatial = "on",
                               spatiotemporal = "ar1",
                               data = sub,
                               time = "half_month_of_season") #factorial or numeric version?
  #cv_fits[[1]] is now a list of 2 models. We want the second of each of these, 
  model_selection$elpd[indx] <- cv_fits[[indx]]$fold_elpd[2]
  model_selection$loglik[indx] <- cv_fits[[indx]]$fold_loglik[2]
}
# total the log lik or ELPD now across years
tot_elpd <- sum(model_selection$elpd)
tot_loglik <- sum(model_selection$loglik)
toc()

#took definitely more than an hour to run, prob closer to 2h (using time = half_month_of_seasonf)
#tot_elpd = -9.541089
#tot_loglik = -367,838.3
#2.4h to run, if use time = half_month_of_season (numerical) Almost the same elpd and loglik 
#tot_elpd = -9.54091
#tot_loglik = -367,848.2

#if add polynomial terms
#3.3 h
#1: The model may not have converged: non-positive-definite Hessian matrix.
#3: The time elements in `newdata` are not identical to those in the original dataset.
#This is normally fine, but may create problems for index standardization.
#tot_elpd = -3.415496
#tot_loglik = -33,997.32

#---------------------------------------------

# fit10d_summer with no polys

tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  train <- dplyr::filter(summer, yearn < yr)
  train$fold_id <- 1
  test <- dplyr::filter(summer, yearn == yr, month_name == "May")
  test$fold_id <- 2
  sub <- rbind(test, train)
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_name_f + 
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

#took about 16mins
#tot_elpd = -7.516844
#tot_loglik = -347,764



#---------------------------------------------


# same as above (fit10d_summer with no polys) but use ar1

tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  train <- dplyr::filter(summer, yearn < yr)
  train$fold_id <- 1
  test <- dplyr::filter(summer, yearn == yr, month_name == "May")
  test$fold_id <- 2
  sub <- rbind(test, train)
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_name_f + 
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

#took about 39mins
#tot_elpd = -7.347511
#tot_loglik = -320,347.2


#---------------------------------------------


# same as above (fit10d_summer with no polys) but use ar1 and MOS is ifex effect

tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  train <- dplyr::filter(summer, yearn < yr)
  train$fold_id <- 1
  test <- dplyr::filter(summer, yearn == yr, month_name == "May")
  test$fold_id <- 2
  sub <- rbind(test, train)
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_of_seasonf + #new
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

#took about 41mins
#tot_elpd = -8.919929
#tot_loglik = -739,651.8




#---------------------------------------------


# test a version where index by month - so far year was better than HMOS in time= argument

tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  train <- dplyr::filter(summer, yearn < yr)
  train$fold_id <- 1
  test <- dplyr::filter(summer, yearn == yr, month_name == "May")
  test$fold_id <- 2
  sub <- rbind(test, train)
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_name_f + #take out as month is in time= argument
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
                                 z_bottom_O2_avg +
                                 z_dist_to_closed_km, 
                               family = tweedie(),
                               fold_ids = sub$fold_id,
                               mesh = mesh,
                               spatial = "on",
                               spatiotemporal = "ar1",
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

#this is when no month argument as fixed effect:
#took about 22mins
#tot_elpd = -12.18581
#tot_loglik = -974,221.8

#if in addition to time = "month_n" also have month_name_f as fixed effect:
#took about 20mins
#tot_elpd = -10.31455
#tot_loglik = -397,440.1


#---------------------------------------------


# test one interaction with the best cv test
#didn't run it, but looks like this should work with polynomials

tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  train <- dplyr::filter(summer, yearn < yr)
  train$fold_id <- 1
  test <- dplyr::filter(summer, yearn == yr, month_name == "May")
  test$fold_id <- 2
  sub <- rbind(test, train)
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_name_f + 
                                 OR_WA_waters +
                                 #WA_pot_reduction +
                                 z_SST_avg  + 
                                 z_wind_avg +
                                 poly(z_depth_point_mean,2) +
                                 z_depth_point_sd +
                                 z_faults_km +
                                 z_dist_canyon_km +
                                 z_weighted_dist +
                                 z_weighted_fuel_pricegal +
                                 z_weighted_crab_ppp +
                                 poly(z_bottom_O2_avg,2) +
                                 z_dist_to_closed_km * WA_pot_reduction, 
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

#took about 41mins
#tot_elpd = -7.089913
#tot_loglik = -297,753


#---------------------------------------------


# test polynomial terms on new input data

tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  train <- dplyr::filter(summer, yearn < yr)
  train$fold_id <- 1
  test <- dplyr::filter(summer, yearn == yr, month_name == "May")
  test$fold_id <- 2
  sub <- rbind(test, train)
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_name_f + 
                                 OR_WA_waters +
                                 WA_pot_reduction +
                                 z_SST_avg  + 
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

#1: The model may not have converged: non-positive-definite Hessian matrix.
#2: The time elements in `newdata` are not identical to those in the original dataset.
#This is normally fine, but may create problems for index standardization.
#9: In stats::nlminb(start = tmb_obj$par, objective = tmb_obj$fn,  ... :  NA/NaN function evaluation
#the first 1-2 round were super quick, extra warnings?

#took about 39mins
#tot_elpd = -2.802408
#tot_loglik = -42865.04


#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------

#run the same test set ups as per winter and all data for easier comparison / follow the same order of tests
#use the all data that is then filtered

#test 1

tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  train <- dplyr::filter(summer, yearn < yr)
  train$fold_id <- 1
  test <- dplyr::filter(summer, yearn == yr, month_name == "May")
  test$fold_id <- 2
  sub <- rbind(test, train)
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_of_seasonf + #new
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

#took about 36mins
#tot_elpd = -8.81714
#tot_loglik = -704,210.6
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.


#--------------------------------------------------

#test 2 - same as test 1 but with iid

tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  train <- dplyr::filter(summer, yearn < yr)
  train$fold_id <- 1
  test <- dplyr::filter(summer, yearn == yr, month_name == "May")
  test$fold_id <- 2
  sub <- rbind(test, train)
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_of_seasonf + #new
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

#took about 17mins
#tot_elpd = -8.955214
#tot_loglik = -746,894.9
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.


#--------------------------------------------------

#test 3

tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  train <- dplyr::filter(summer, yearn < yr)
  train$fold_id <- 1
  test <- dplyr::filter(summer, yearn == yr, month_name == "May")
  test$fold_id <- 2
  sub <- rbind(test, train)
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_name_f + #new
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

#took about 34mins
#tot_elpd = -3.207158
#tot_loglik = -370,48.42
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 5: In stats::nlminb(start = tmb_obj$par, objective = tmb_obj$fn,  ... : NA/NaN function evaluation


#--------------------------------------------------

#test 4 - same as test 3 but with iid

tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  train <- dplyr::filter(summer, yearn < yr)
  train$fold_id <- 1
  test <- dplyr::filter(summer, yearn == yr, month_name == "May")
  test$fold_id <- 2
  sub <- rbind(test, train)
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_name_f + #new
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

#took about 16mins
#tot_elpd = -3.242822
#tot_loglik = -36846.72

# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.


#--------------------------------------------------

#test 5

tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  train <- dplyr::filter(summer, yearn < yr)
  train$fold_id <- 1
  test <- dplyr::filter(summer, yearn == yr, month_name == "May")
  test$fold_id <- 2
  sub <- rbind(test, train)
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 half_month_of_seasonf + #new
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

#took about 50mins
#tot_elpd = -9.690321
#tot_loglik = -1128718
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.

#--------------------------------------------------

#test 6

tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  train <- dplyr::filter(summer, yearn < yr)
  train$fold_id <- 1
  test <- dplyr::filter(summer, yearn == yr, month_name == "May")
  test$fold_id <- 2
  sub <- rbind(test, train)
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 half_month_of_seasonf + #new
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

#took about 21mins
#tot_elpd = -9.778816
#tot_loglik = -1,194,605
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.


#--------------------------------------------------

#test 7

tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  train <- dplyr::filter(summer, yearn < yr)
  train$fold_id <- 1
  test <- dplyr::filter(summer, yearn == yr, month_name == "May")
  test$fold_id <- 2
  sub <- rbind(test, train)
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 +
                                 season +
                                 month_name_f + #new
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

#took about 2.7hours
#tot_elpd = -4.038753
#tot_loglik = -37979.35

# 1: Parameter ar1_phi is very close or equal to its lower bound.
# Consider changing your model configuration or bounds.
# 3: The model may not have converged: non-positive-definite Hessian matrix.
# 4: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.


#--------------------------------------------------

#test 8

tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  train <- dplyr::filter(summer, yearn < yr)
  train$fold_id <- 1
  test <- dplyr::filter(summer, yearn == yr, month_name == "May")
  test$fold_id <- 2
  sub <- rbind(test, train)
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 +
                                 season +
                                 month_name_f + #new
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
                                 z_bottom_O2_avg +
                                 z_dist_to_closed_km,
                               family = tweedie(),
                               fold_ids = sub$fold_id,
                               mesh = mesh,
                               spatial = "on",
                               spatiotemporal = "ar1",
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

#took about 20mins
#tot_elpd = -4.912929
#tot_loglik = -46,189.21

# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 7: In stats::nlminb(start = tmb_obj$par, objective = tmb_obj$fn,  ... : NA/NaN function evaluation

#--------------------------------------------------

#test 9

tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  train <- dplyr::filter(summer, yearn < yr)
  train$fold_id <- 1
  test <- dplyr::filter(summer, yearn == yr, month_name == "May")
  test$fold_id <- 2
  sub <- rbind(test, train)
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 +
                                 season +
                                 month_name_f + #new
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

#took about 57mins
#tot_elpd = -4.660863
#tot_loglik = -45053.38

# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 7: Parameter ar1_phi is very close or equal to its lower bound.
# Consider changing your model configuration or bounds.


#--------------------------------------------------

#test 10 to 10d -- no covariates included - test different terms in time = xx?

tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  train <- dplyr::filter(summer, yearn < yr)
  train$fold_id <- 1
  test <- dplyr::filter(summer, yearn == yr, month_name == "May")
  test$fold_id <- 2
  sub <- rbind(test, train)
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
                               time = "month_n")
  #cv_fits[[1]] is now a list of 2 models. We want the second of each of these,
  model_selection$elpd[indx] <- cv_fits[[indx]]$fold_elpd[2]
  model_selection$loglik[indx] <- cv_fits[[indx]]$fold_loglik[2]
}
# total the log lik or ELPD now across years
tot_elpd <- sum(model_selection$elpd)
tot_loglik <- sum(model_selection$loglik)
toc()


#time = "yearn"
#took about 8 mins
#tot_elpd = -2.136352
#tot_loglik = -31958.28
# 1: In sqrt(diag(cov)) : NaNs produced
# 2: The model may not have converged: non-positive-definite Hessian matrix.
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 6: The model may not have converged. Maximum final gradient: 0.0494452411295043.
# 19: The model may not have converged. Maximum final gradient: 0.136542894124339.
# $converged
# [1] FALSE
# $pdHess
# [1] FALSE  TRUE

#time = "month_n"
#took about 4mins
#tot_elpd = -2.537671
#tot_loglik = -27630.81
# 1: In sqrt(diag(cov)) : NaNs produced
# 2: The model may not have converged: non-positive-definite Hessian matrix.
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 12: The model may not have converged: extreme or very small eigen values detected.
# $converged
# [1] TRUE
# $pdHess
# [1] TRUE TRUE
#EXPORT THIS MODEL
#write_rds(cv_fits, here::here('DCRB_sdmTMB', 'exported model objects', 'cross validation',"cv_summer_test_10b_ar1.rds"))
#and export the data for plotting: cv_fits_5_data <- cv_fits[[5]]$data
#write_rds(cv_fits_5_data, here::here('DCRB_sdmTMB', 'exported model objects', 'cross validation',"summer_cv_fits_5_data.rds"))


#time = "month_of_season"
#took about 15mins
#tot_elpd = -2.542227
#tot_loglik = -28139.36
# 1: In sqrt(diag(cov)) : NaNs produced
# 2: The model may not have converged: non-positive-definite Hessian matrix. 
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization. 
# $converged
# [1] TRUE
# $pdHess
# [1] TRUE TRUE

#time = "half_month_of_season"
#took about 49mins
#tot_elpd = -2.501227
#tot_loglik = -28073.07
# 1: Parameter ar1_phi is very close or equal to its lower bound.
# Consider changing your model configuration or bounds. 
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization. 
# $converged
# [1] TRUE
# $pdHess
# [1] TRUE TRUE

#--------------------------------------------------

#test 11 = test 3 but with polynomial terms

tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  train <- dplyr::filter(summer, yearn < yr)
  train$fold_id <- 1
  test <- dplyr::filter(summer, yearn == yr, month_name == "May")
  test$fold_id <- 2
  sub <- rbind(test, train)
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_name_f + #new
                                 OR_WA_waters +
                                 WA_pot_reduction +
                                 z_SST_avg  + 
                                 z_wind_avg +
                                 poly(z_depth_point_mean, 2) +
                                 z_depth_point_sd +
                                 z_faults_km +
                                 z_dist_canyon_km +
                                 z_weighted_dist +
                                 z_weighted_fuel_pricegal +
                                 z_weighted_crab_ppp +
                                 poly(z_bottom_O2_avg, 2) +
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

#took about 39mins
#tot_elpd = -2.802409
#tot_loglik = -42864.93

# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.

#both models[[1]] and models[[2]]  coef.se are NaN
# $converged
# [1] FALSE
# $pdHess
# [1] FALSE FALSE
#--------------------------------------------------

#test 12 = test 11 but with interaction

tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  train <- dplyr::filter(summer, yearn < yr)
  train$fold_id <- 1
  test <- dplyr::filter(summer, yearn == yr, month_name == "May")
  test$fold_id <- 2
  sub <- rbind(test, train)
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_name_f + #new
                                 #OR_WA_waters +
                                 WA_pot_reduction +
                                 z_SST_avg  + 
                                 z_wind_avg +
                                 poly(z_depth_point_mean, 2) +
                                 z_depth_point_sd +
                                 z_faults_km +
                                 z_dist_canyon_km +
                                 z_weighted_dist +
                                 z_weighted_fuel_pricegal +
                                 z_weighted_crab_ppp +
                                 poly(z_bottom_O2_avg, 2) +
                                 OR_WA_waters * z_dist_to_closed_km, 
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

#took about 49mins
#tot_elpd = -3.181442
#tot_loglik = -56287.83

# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 17: In stats::nlminb(start = tmb_obj$par, objective = tmb_obj$fn,  ... : NA/NaN function evaluation

#coef.se are Nan for both models
#--------------------------------------------------



#--------------------------------------------------
#---------------------------------------------

#test Eric's suggested fix to solve convergence issues
# "If you inspect the fitted model's 'sdreport', e.g. fit$sdreport, you'll see a number of the b_j elements exactly 0.
# This gives a clue that something's amiss. What's happening was that the model is trying to estimate coefficients 
# for all months / years, even those that had been filtered out of the original datsaset. 
# So the fix is to create the factors from the 'sub' dataframe right before fitting. 
# For example, you could add the line
# > sub$month_name_f <- as.factor(as.character(sub$month_name_f))
# right before the sdmTMB_cv() line

#try this on test 11 - code shared in cf fine tuning script 

# fix test 1

tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  train <- dplyr::filter(summer, yearn < yr)
  train$fold_id <- 1
  test <- dplyr::filter(summer, yearn == yr, month_name == "May")
  test$fold_id <- 2
  sub <- rbind(test, train)
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  sub$month_name_f <- as.factor(as.character(sub$month_name_f))
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season + 
                                 month_name_f + 
                                 #OR_WA_waters + #part of interaction term
                                 #WA_pot_reduction + #eric thinks this is the root of converging issues
                                 z_SST_avg  + 
                                 z_wind_avg +
                                 poly(z_depth_point_mean, 2) +
                                 z_depth_point_sd +
                                 z_faults_km +
                                 z_dist_canyon_km +
                                 z_weighted_dist +
                                 z_weighted_fuel_pricegal +
                                 z_weighted_crab_ppp +
                                 z_bottom_O2_avg +
                                 OR_WA_waters * z_dist_to_closed_km, 
                               family = tweedie(),
                               fold_ids = sub$fold_id,
                               mesh = mesh,
                               spatial = "on",
                               spatiotemporal = "iid", #this could be changed to iid / ar1
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

#when use ar1
#took about 46min
#tot_elpd = -6.53703
#tot_loglik = -526931
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The time elements in `newdata` are not identical to those in the original
# dataset.
# This is normally fine, but may create problems for index standardization.
# 17: In stats::nlminb(start = tmb_obj$par, objective = tmb_obj$fn,  ... : NA/NaN function evaluation

#both models[[1]] and models[[2]]  coef.se are NaN
# $converged
# [1] FALSE
# $pdHess
# [1] FALSE FALSE


#fix test 2
#when use iid
#took about 19min
#tot_elpd = -6.695907
#tot_loglik = -730072.6
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The time elements in `newdata` are not identical to those in the original
# dataset.
# This is normally fine, but may create problems for index standardization.

#both models[[1]] and models[[2]]  coef.se are NaN
# $converged
# [1] FALSE
# $pdHess
# [1] FALSE FALSE


#if test iid and skip the "fix" code line, still not converging


#eric's suggestion: remove WA_pot_reduction variable
#test that here -- still didn't converge
#when use iid
#took about 19min
#tot_elpd = -6.713508
#tot_loglik = -731612.3
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
#both models[[1]] and models[[2]]  coef.se are NaN
# $converged
# [1] FALSE
# $pdHess
# [1] FALSE FALSE




#eric's model that apparently works:

tic()
validation_years <- 2015:2019 # I'd make this no fewer than 5, no more than 10
cv_fits <- list()
model_selection <- data.frame(validation_years = validation_years,
                              elpd = NA,
                              loglik = NA)
for(yr in validation_years) {
  # remove data in future years
  train <- dplyr::filter(summer, yearn < yr)
  train$fold_id <- 1
  test <- dplyr::filter(summer, yearn == yr, month_name == "May")
  test$fold_id <- 2
  sub <- rbind(test, train)
  # make mesh for this dataset
  mesh <- make_mesh(sub, xy_cols = c("X","Y"), cutoff = 10)
  # fit model with sdmTMB_cv
  indx <- yr - min(validation_years) + 1
  sub$month_name_f <- as.factor(as.character(sub$month_name_f))
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 +
            season +
            month_name_f +
            #OR_WA_waters + #part of interaction term
            #WA_pot_reduction +
            z_SST_avg  +
            z_wind_avg +
            poly(z_depth_point_mean, 2) +
            z_depth_point_sd +
            z_faults_km +
            z_dist_canyon_km +
            z_weighted_dist +
            z_weighted_fuel_pricegal +
            z_weighted_crab_ppp +
            z_bottom_O2_avg +
            OR_WA_waters * z_dist_to_closed_km,
          family = tweedie(),
          fold_ids = sub$fold_id,
          mesh = mesh,
          spatial = "off",
          spatiotemporal = "iid",
          data = sub,
          control = sdmTMBcontrol(newton_loops = 1),
          time = "yearn"
          )
  #cv_fits[[1]] is now a list of 2 models. We want the second of each of these, 
  model_selection$elpd[indx] <- cv_fits[[indx]]$fold_elpd[2]
  model_selection$loglik[indx] <- cv_fits[[indx]]$fold_loglik[2]
}
# total the log lik or ELPD now across years
tot_elpd <- sum(model_selection$elpd)
tot_loglik <- sum(model_selection$loglik)
toc()


#adding control argument gave this
#Error in solve.default(h, g) : Lapack routine dgesv: system is exactly singular: U[1,1] = 0







