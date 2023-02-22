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

#took about 40-45mins
#tot_elpd = -9.716058
#tot_loglik = -1,148,336

#exported model
#write_rds(cv_fits[[indx]],here::here('DCRB_sdmTMB', 'exported model objects', 'cross validation',"fit10f_summer_noPolys.rds"))

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

#now try version of fit11c_summer (but no polys)

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
                                 z_depth_point_mean +
                                 z_depth_point_sd +
                                 z_faults_km +
                                 z_dist_canyon_km +
                                 z_weighted_dist +
                                 z_weighted_fuel_pricegal +
                                 z_weighted_crab_ppp +
                                 z_bottom_O2_avg +
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











