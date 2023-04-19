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
#have this step up here already
summer$month_name_f <- factor(summer$month_name_f, levels = c("May", "June", "July", "August", "September"))

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
  #sub$month_name_f <- as.factor(as.character(sub$month_name_f))
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 #season +  #season is the variable causing problems
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
#tot_elpd = -2.5578
#tot_loglik = -27877.3

#Warning messages:
#1: The model may not have converged: non-positive-definite Hessian matrix.
#2: The time elements in `newdata` are not identical to those in the original dataset.
#This is normally fine, but may create problems for index standardization.

#MODEL DID CONVERGE
#[[5]]$converged
#[1] FALSE

#[[5]]$pdHess
#[1] FALSE  TRUE


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
                                 #season +  #season is the variable causing problems
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

# 9min
#tot_elpd = -2.291745
#tot_loglik = -28971.01

#Warning messages:
#1: The model may not have converged: non-positive-definite Hessian matrix.
#2: The time elements in `newdata` are not identical to those in the original dataset.
#This is normally fine, but may create problems for index standardization.

#MODEL DID CONVERGE
#[[5]]$converged
#[1] FALSE

#[[5]]$pdHess
#[1] FALSE  TRUE


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
                                 #season +  #season is the variable causing problems
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

# 15min
#tot_elpd = -2.094359
#tot_loglik = -35737.56

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 3: The model may not have converged. Maximum final gradient: 0.025883370061841.

#MODEL DID CONVERGE
#[[5]]$converged
#[1] FALSE

#[[5]]$pdHess
#[1] FALSE  TRUE


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
                                 #season + #season is the variable causing problems
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

# 37min
#tot_elpd = -2.099761
#tot_loglik = -35161.09

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 3: The model may not have converged. Maximum final gradient: 0.0526072364840893.

#MODEL DID CONVERGE
#[[5]]$converged
#[1] FALSE

#[[5]]$pdHess
#[1] FALSE  TRUE


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

#MODEL DID CONVERGE
# [[5]]$converged
# [1] TRUE
# 
# [[5]]$pdHess
# [1] TRUE TRUE


cv_test5_summer <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test5_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test5_summer.rds"))



#test 5b
#no covariates, spatial fields but no s-t fields, time=month

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
                               time = "month_n")
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
#tot_loglik = -28303.96 - doesn't matter if time = month_n, prob as no s-t field anyway

#Warning messages:
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization. 
# 4: The model may not have converged. Maximum final gradient: 0.0439054271480988. 

#MODEL DID CONVERGE
# [[5]]$converged
# [1] TRUE
# 
# [[5]]$pdHess
# [1] TRUE TRUE


cv_test5b_summer <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test5b_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test5b_summer.rds"))

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



#7b - test 7 but tim=month
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
                               time = "month_n")
  #cv_fits[[1]] is now a list of 2 models. We want the second of each of these, 
  model_selection$elpd[indx] <- cv_fits[[indx]]$fold_elpd[2]
  model_selection$loglik[indx] <- cv_fits[[indx]]$fold_loglik[2]
}
# total the log lik or ELPD now across years
tot_elpd <- sum(model_selection$elpd)
tot_loglik <- sum(model_selection$loglik)
toc()

# 4min
#tot_elpd = -2.537671
#tot_loglik = -27630.81

#Warning messages:
# Warning messages:
#   1: In sqrt(diag(object$cov.fixed)) : NaNs produced
# 2: In sqrt(as.numeric(object$diag.cov.random)) : NaNs produced

#MODEL DID CONVERGE
#[[5]]$converged
#[1] FALSE

#[[5]]$pdHess
#[1] FALSE  TRUE
#or all TRUES for model 5

cv_test7b_summer <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test7b_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test7b_summer.rds"))


##7c --7b but with iid
# 3min
#tot_elpd = -2.536224
#tot_loglik = -27621.68

#EXPORT THIS MODEL
#write_rds(cv_test7c_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test7c_summer.rds"))


#---------------------------------------------


#test 8
#covariates, no polynomials, month_name_f is fixed effect, 
# s and  s-t fields (ar1), time='month_n'



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
                                 #season +  #season is the variable causing problems
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
                               time = "month_n")
  #cv_fits[[1]] is now a list of 2 models. We want the second of each of these, 
  model_selection$elpd[indx] <- cv_fits[[indx]]$fold_elpd[2]
  model_selection$loglik[indx] <- cv_fits[[indx]]$fold_loglik[2]
}
# total the log lik or ELPD now across years
tot_elpd <- sum(model_selection$elpd)
tot_loglik <- sum(model_selection$loglik)
toc()

# 15min
#tot_elpd = -2.411552
#tot_loglik = -27800.26

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 4: The model may not have converged. Maximum final gradient: 0.0429437861194231.

#MODEL DID CONVERGE
#[[5]]$converged
#[1] FALSE

#[[5]]$pdHess
#[1] FALSE  TRUE


cv_test8_summer <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test8_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test8_summer.rds"))

###########################
###extra test, use yearn (numeric) instead of season (categorical)
#model does partly converge, except for month fixed effect where coef.se are NaN (at least first model), also much poorer LL -33558.4 

## if bot yearn and month_n are numeric:
#first model
#                             coef.est coef.se
# yearn                       -0.16     NaN
# month_n                     -0.49    0.03
# OR_WA_watersOR             333.91     NaN
# OR_WA_watersWA             333.41     NaN
#later ones seem fine, but LL still poor: -32915.96


#---------------------------------------------

#test 9
#covariates, no polynomials, month_name_f is fixed effect, 
# s and  s-t fields (ar1), time='month_of_season'



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
                                 #season +  #season is the variable causing problems
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

# 48min
#tot_elpd = -2.346226
#tot_loglik = -29323.28

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The model may not have converged. Maximum final gradient: 0.03809487064404.
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 4: The model may not have converged: non-positive-definite Hessian matrix.

#MODEL DID CONVERGE
#[[5]]$converged
#[1] FALSE

#[[5]]$pdHess
#[1] FALSE  TRUE


cv_test9_summer <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test9_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test9_summer.rds"))

#---------------------------------------------

#test 10
#covariates, no polynomials, month_name_f is fixed effect, 
# s and  s-t fields (ar1), time='half_month_of_season'



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
                                 #season +  #season is the variable causing problems
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

# 2.4h
#tot_elpd = -2.334728
#tot_loglik = -29726.54

#Warning messages:
# 1: Parameter ar1_phi is very close or equal to its lower bound.
# Consider changing your model configuration or bounds.
# 2: The model may not have converged: non-positive-definite Hessian matrix.
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 5: The model may not have converged. Maximum final gradient: 0.0103733706538955.

#MODEL DID CONVERGE
#[[5]]$converged
#[1] FALSE

#[[5]]$pdHess
#[1] FALSE  TRUE


cv_test10_summer <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test10_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test10_summer.rds"))

#---------------------------------------------

#test 11
#covariates, no polynomials, month_of_season_f  is fixed effect, 
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
                                 #season +  #season is the variable causing problems
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

# 57min
#tot_elpd = -2.100704
#tot_loglik = -37795.78

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 3: The model may not have converged. Maximum final gradient: 0.0161544073505304.

#MODEL DID NOT CONVERGE
#[[5]]$converged
#[1] FALSE

#[[5]]$pdHess
#[1] FALSE  TRUE


cv_test11_summer <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test11_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test11_summer.rds"))

#---------------------------------------------

#test 12
#covariates, no polynomials, half_month_of_season_f  is fixed effect, 
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
                                 #season +  #season is the variable causing problems
                                 half_month_of_seasonf + 
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

# 1.2h
#tot_elpd = -2.104816
#tot_loglik = -44244.14

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 3: The model may not have converged. Maximum final gradient: 0.0378212869772874.

#MODEL DID CONVERGE
#[[5]]$converged
#[1] FALSE

#[[5]]$pdHess
#[1] FALSE  TRUE


cv_test12_summer <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test12_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test12_summer.rds"))

#---------------------------------------------

#test 13
#covariates, polynomial term for depth, month_name_f is fixed effect, 
# s and  s-t fields (iid), time='month_n'



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
                                 #season +  #season is the variable causing problems
                                 month_name_f + 
                                 OR_WA_waters + 
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
                                 z_dist_to_closed_km, 
                               family = tweedie(),
                               fold_ids = sub$fold_id,
                               mesh = mesh,
                               spatial = "on",
                               spatiotemporal = "iid", 
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

# 13min
#tot_elpd = -2.402891
#tot_loglik = -32090.34

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 8: The model may not have converged. Maximum final gradient: 0.0430607237786607

#MODEL DID CONVERGE
#[[5]]$converged
#[1] FALSE

#[[5]]$pdHess
#[1] FALSE  TRUE


cv_test13_summer <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test13_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test13_summer.rds"))


##13b -- same but with ar1
#26min
#tot_elpd = -2.403407
#tot_loglik = -32196.49

# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 6: In sqrt(diag(cov)) : NaNs produced
# 10: The model may not have converged. Maximum final gradient: 0.0205153585749116.
# 12: In stats::nlminb(start = tmb_obj$par, objective = tmb_obj$fn,  ... : NA/NaN function evaluation
                     
#---------------------------------------------

#test 14
#covariates, polynomial term for depth, month_name_f is fixed effect, interaction: crab price * fuel price
# s and  s-t fields (iid), time='month_n'


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
                                 #season +  #season is the variable causing problems
                                 month_name_f + 
                                 OR_WA_waters + 
                                 #WA_pot_reduction + #eric thinks this is the root of converging issues
                                 z_SST_avg  + 
                                 z_wind_avg +
                                 poly(z_depth_point_mean, 2) +
                                 z_depth_point_sd +
                                 z_faults_km +
                                 z_dist_canyon_km +
                                 z_weighted_dist +
                                 z_weighted_fuel_pricegal * z_weighted_crab_ppp +
                                 #z_weighted_crab_ppp +
                                 z_bottom_O2_avg +
                                 z_dist_to_closed_km, 
                               family = tweedie(),
                               fold_ids = sub$fold_id,
                               mesh = mesh,
                               spatial = "on",
                               spatiotemporal = "iid", 
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

# 14min
#tot_elpd = -2.403375
#tot_loglik = -32344.1

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 4: The model may not have converged. Maximum final gradient: 0.0380703810846104.

#MODEL DID CONVERGE
#[[5]]$converged
#[1] FALSE

#[[5]]$pdHess
#[1] FALSE  TRUE


cv_test14_summer <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test14_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test14_summer.rds"))


#---------------------------------------------

#test 15
#covariates, polynomial term for depth, month_name_f is fixed effect, interaction: SST * wind
# s and  s-t fields (iid), time='month_n'


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
                                 #season +  #season is the variable causing problems
                                 month_name_f + 
                                 OR_WA_waters + 
                                 #WA_pot_reduction + #eric thinks this is the root of converging issues
                                 z_SST_avg * z_wind_avg + 
                                 #z_wind_avg +
                                 poly(z_depth_point_mean, 2) +
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
                               time = "month_n")
  #cv_fits[[1]] is now a list of 2 models. We want the second of each of these, 
  model_selection$elpd[indx] <- cv_fits[[indx]]$fold_elpd[2]
  model_selection$loglik[indx] <- cv_fits[[indx]]$fold_loglik[2]
}
# total the log lik or ELPD now across years
tot_elpd <- sum(model_selection$elpd)
tot_loglik <- sum(model_selection$loglik)
toc()

# 15min
#tot_elpd = -2.397991
#tot_loglik = -32231.68

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 4: The model may not have converged. Maximum final gradient: 0.0118775320531181

#MODEL DID CONVERGE
#[[5]]$converged
#[1] FALSE

#[[5]]$pdHess
#[1] FALSE  TRUE


cv_test15_summer <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test15_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test15_summer.rds"))


#---------------------------------------------

#test 16
#covariates, polynomial term for depth, month_name_f is fixed effect, interaction: OR/WA waters * closed area
# s and  s-t fields (iid), time='month_n'


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
                                 #season +  #season is the variable causing problems
                                 month_name_f + 
                                 #OR_WA_waters + 
                                 #WA_pot_reduction + #eric thinks this is the root of converging issues
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
                               fold_ids = sub$fold_id,
                               mesh = mesh,
                               spatial = "on",
                               spatiotemporal = "iid", 
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

# 13min
#tot_elpd = -2.607136
#tot_loglik = -35267.37

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.

#MODEL DID CONVERGE
#[[5]]$converged
#[1] FALSE

#[[5]]$pdHess
#[1] FALSE  TRUE


cv_test16_summer <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test16_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test16_summer.rds"))


#---------------------------------------------

#test 17
#covariates, polynomial term for depth, month_name_f is fixed effect, interaction: fuel price * dist to port
# s and  s-t fields (iid), time='month_n'


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
                                 #season +  #season is the variable causing problems
                                 month_name_f + 
                                 OR_WA_waters + 
                                 #WA_pot_reduction + #eric thinks this is the root of converging issues
                                 z_SST_avg + 
                                 z_wind_avg +
                                 poly(z_depth_point_mean, 2) +
                                 z_depth_point_sd +
                                 z_faults_km +
                                 z_dist_canyon_km +
                                 z_weighted_dist * z_weighted_fuel_pricegal +
                                 #z_weighted_fuel_pricegal +
                                 z_weighted_crab_ppp +
                                 z_bottom_O2_avg +
                                 z_dist_to_closed_km, 
                               family = tweedie(),
                               fold_ids = sub$fold_id,
                               mesh = mesh,
                               spatial = "on",
                               spatiotemporal = "iid", 
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

# 14min
#tot_elpd = -2.387324
#tot_loglik = -33149.93

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 8: The model may not have converged. Maximum final gradient: 0.0110806192675672.

#MODEL DID CONVERGE
#[[5]]$converged
#[1] FALSE

#[[5]]$pdHess
#[1] FALSE  TRUE


cv_test17_summer <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test17_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test17_summer.rds"))


#---------------------------------------------

#test 18
#covariates, polynomial term for depth, month_name_f is fixed effect, interaction: fuel price * wind
# s and  s-t fields (iid), time='month_n'


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
                                 #season +  #season is the variable causing problems
                                 month_name_f + 
                                 OR_WA_waters + 
                                 #WA_pot_reduction + #eric thinks this is the root of converging issues
                                 z_SST_avg + 
                                 z_wind_avg * z_weighted_fuel_pricegal +
                                 poly(z_depth_point_mean, 2) +
                                 z_depth_point_sd +
                                 z_faults_km +
                                 z_dist_canyon_km +
                                 z_weighted_dist +
                                 #z_weighted_fuel_pricegal +
                                 z_weighted_crab_ppp +
                                 z_bottom_O2_avg +
                                 z_dist_to_closed_km, 
                               family = tweedie(),
                               fold_ids = sub$fold_id,
                               mesh = mesh,
                               spatial = "on",
                               spatiotemporal = "iid", 
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

# 14min
#tot_elpd = -2.412389
#tot_loglik = -32041.35

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.

#MODEL DID CONVERGE
#[[5]]$converged
#[1] FALSE

#[[5]]$pdHess
#[1] FALSE  TRUE


cv_test18_summer <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test18_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test18_summer.rds"))



#for summer model this was the best interaction, but not as good than just model with no polynomial term
##test fit 18 with no polynomial
#12min
#tot_elpd = -2.421361
#tot_loglik = -27719.07
#fine convergence
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The model may not have converged. Maximum final gradient: 0.010446257248006.
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
#EXPORT THIS MODEL
#write_rds(cv_test18_no_poly_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test18_no_poly_summer.rds"))


##then test with ar1
#17min
#tot_elpd = -2.421159
#tot_loglik = -27750.8
#fine convergence
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The model may not have converged. Maximum final gradient: 0.0428902622589149.
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 6: In stats::nlminb(start = tmb_obj$par, objective = tmb_obj$fn,  ... : NA/NaN function evaluation

##and with time==year (iid)
#19min
#tot_elpd = -2.093703
#tot_loglik = -35676.75
#fine convergence

#---------------------------------------------

#test 19
#covariates, polynomial term for depth, month_name_f is fixed effect, interaction: bottom O2 * depth
# s and  s-t fields (iid), time='month_n'


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
                                 #season +  #season is the variable causing problems
                                 month_name_f + 
                                 OR_WA_waters + 
                                 #WA_pot_reduction + #eric thinks this is the root of converging issues
                                 z_SST_avg + 
                                 z_wind_avg +
                                 z_bottom_O2_avg * poly(z_depth_point_mean, 2) +
                                 z_depth_point_sd +
                                 z_faults_km +
                                 z_dist_canyon_km +
                                 z_weighted_dist +
                                 z_weighted_fuel_pricegal +
                                 z_weighted_crab_ppp +
                                 #z_bottom_O2_avg +
                                 z_dist_to_closed_km, 
                               family = tweedie(),
                               fold_ids = sub$fold_id,
                               mesh = mesh,
                               spatial = "on",
                               spatiotemporal = "iid", 
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

# 15min
#tot_elpd = -2.401378
#tot_loglik = -32734.66

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.

#MODEL DID CONVERGE
#[[5]]$converged
#[1] FALSE

#[[5]]$pdHess
#[1] FALSE  TRUE


cv_test19_summer <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test19_summer, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test19_summer.rds"))


#---------------------------------------------


































