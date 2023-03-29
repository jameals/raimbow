#sdmTMB model selection via cross validation - summer

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



d <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_all_data_20230324.rds')) 

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

# 3min
#tot_elpd = -2.116793
#tot_loglik = -31618.71

#Warning messages:
#1: The model may not have converged: non-positive-definite Hessian matrix.
#2: The time elements in `newdata` are not identical to those in the original dataset.
#This is normally fine, but may create problems for index standardization.

#MODEL DID NOT CONVERGE
#[[5]]$converged
#[1] FALSE

#[[5]]$pdHess
#[1] FALSE  FALSE


cv_test1_summer <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test1_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test1_summer.rds"))

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

# 10min
#tot_elpd = -14.59454
#tot_loglik = -2356202

#Warning messages:
#1: The model may not have converged: non-positive-definite Hessian matrix.
#2: The time elements in `newdata` are not identical to those in the original dataset.
#This is normally fine, but may create problems for index standardization.

#MODEL DID NOT CONVERGE
#[[5]]$converged
#[1] FALSE

#[[5]]$pdHess
#[1] FALSE  FALSE


cv_test2_summer <- cv_fits
#print(tidy(cv_test2_summer[[5]]$models[[2]]), n=30)


#EXPORT THIS MODEL
#write_rds(cv_test2_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test2_summer.rds"))

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

# 17min
#tot_elpd = -7.958591
#tot_loglik = -409288

#Warning messages:
#1: The model may not have converged: non-positive-definite Hessian matrix.
#2: The time elements in `newdata` are not identical to those in the original dataset.
#This is normally fine, but may create problems for index standardization.

#MODEL DID NOT CONVERGE
#[[5]]$converged
#[1] FALSE

#[[5]]$pdHess
#[1] FALSE  FALSE


cv_test3_summer <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test3_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test3_summer.rds"))

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

# 40min
#tot_elpd = -7.792366
#tot_loglik = -376210.2

#Warning messages:
#1: The model may not have converged: non-positive-definite Hessian matrix.
#2: The time elements in `newdata` are not identical to those in the original dataset.
#This is normally fine, but may create problems for index standardization.

#MODEL DID NOT CONVERGE
#[[5]]$converged
#[1] FALSE

#[[5]]$pdHess
#[1] FALSE  FALSE


cv_test4_summer <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test4_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test4_summer.rds"))

#---------------------------------------------


#test 5
#no covariates, spatial fields but no s-t fields



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

# 1.2min
#tot_elpd = -2.258108
#tot_loglik = -28303.96

#Warning messages:
#1: The model may not have converged: non-positive-definite Hessian matrix.
#2: The time elements in `newdata` are not identical to those in the original dataset.
#This is normally fine, but may create problems for index standardization.

#MODEL DID NOT CONVERGE
# [[5]]$converged
# [1] TRUE
# 
# [[5]]$pdHess
# [1] TRUE TRUE


cv_test5_summer <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test5_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test5_summer.rds"))

#---------------------------------------------


#test 6
#model with only spatial and spatiotemporal fields (seasons), no covariates



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

# 4min
#tot_elpd = -2.13266
#tot_loglik = -32291.75

#Warning messages:
#1: The model may not have converged: non-positive-definite Hessian matrix.
#2: The time elements in `newdata` are not identical to those in the original dataset.
#This is normally fine, but may create problems for index standardization.

#MODEL DID CONVERGE
# [[5]]$converged
# [1] TRUE
# 
# [[5]]$pdHess
# [1] TRUE TRUE


cv_test6_summer <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test6_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test6_summer.rds"))

#---------------------------------------------


#test 7
#no covariates, spatial fields but no s-t fields



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

# 9min
#tot_elpd = -2.136352
#tot_loglik = -31958.28

#Warning messages:
# Warning messages:
#   1: In sqrt(diag(object$cov.fixed)) : NaNs produced
# 2: In sqrt(as.numeric(object$diag.cov.random)) : NaNs produced

#MODEL DID CONVERGE
#[[5]]$converged
#[1] FALSE

#[[5]]$pdHess
#[1] FALSE  TRUE


cv_test7_summer <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test7_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test7_summer.rds"))

#---------------------------------------------



















































