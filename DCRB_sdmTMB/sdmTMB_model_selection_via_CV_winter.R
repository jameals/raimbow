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
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 4: The model may not have converged. Maximum final gradient: 0.013992997718373.

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

# 33min
#tot_elpd = -3.326518
#tot_loglik = -26621.73

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The model may not have converged. Maximum final gradient: 0.0137257711133432.
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.

#MODEL DID CONVERGE
#[[5]]$converged
#[1] FALSE

#[[5]]$pdHess
#[1] FALSE  TRUE


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

# 39min
#tot_elpd = -3.270455
#tot_loglik = -26410

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The model may not have converged. Maximum final gradient: 0.0384819939249539.
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.

#MODEL DID CONVERGE
# [[1]]$converged
# [1] FALSE
# 
# [[1]]$pdHess
# [1] FALSE  TRUE


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

# 73min
#tot_elpd = -3.268641
#tot_loglik = -26413.44

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The model may not have converged. Maximum final gradient: 0.096839048103142.
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization. 

#MODEL DID CONVERGE
# [[2]]$converged
# [1] FALSE
# 
# [[2]]$pdHess
# [1] FALSE  TRUE


cv_test4_winter <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test4_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test4_winter.rds"))

#---------------------------------------------

#test 5
#No covariates, no polynomials
# spatial but no  s-t fields, time='yearn'


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

# 3min
#tot_elpd = -3.785159
#tot_loglik = -27277.99

#Warning messages:
# 4: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization. 
# 5: The model may not have converged. Maximum final gradient: 0.0287616318709073. 

#MODEL DID CONVERGE
# [[1]]$converged
# [1] TRUE
# 
# [[1]]$pdHess
# [1] TRUE TRUE


cv_test5_winter <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test5_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test5_winter.rds"))

#---------------------------------------------


#test 6
#No covariates, no polynomials
# spatial and  s-t fields, iid, time='yearn'


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

# 6min
#tot_elpd = -3.777363
#tot_loglik = -27236.47

#Warning messages:
# 1: The model may not have converged. Maximum final gradient: 0.0214664789250421. 
# 2: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.

#MODEL DID CONVERGE
# [[4]]$converged
# [1] TRUE
# 
# [[4]]$pdHess
# [1] TRUE TRUE

cv_test6_winter <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test6_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test6_winter.rds"))

#---------------------------------------------

#test 7
#No covariates, no polynomials
# spatial and  s-t fields, iid, time='yearn'


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

# 13min
#tot_elpd = -3.774164
#tot_loglik = -27236.09

#Warning messages:

#MODEL DID CONVERGE
# [[2]]$converged
# [1] FALSE
# 
# [[2]]$pdHess
# [1] FALSE  TRUE


cv_test7_winter <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test7_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test7_winter.rds"))

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
                               time = "month_n")
  #cv_fits[[1]] is now a list of 2 models. We want the second of each of these, 
  model_selection$elpd[indx] <- cv_fits[[indx]]$fold_elpd[2]
  model_selection$loglik[indx] <- cv_fits[[indx]]$fold_loglik[2]
}
# total the log lik or ELPD now across years
tot_elpd <- sum(model_selection$elpd)
tot_loglik <- sum(model_selection$loglik)
toc()

# 58min
#tot_elpd = -3.038956
#tot_loglik = -26791.74

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The model may not have converged. Maximum final gradient: 0.0481542966005422.
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 7: In stats::nlminb(start = tmb_obj$par, objective = tmb_obj$fn,  ... :   NA/NaN function evaluation

#MODEL DID CONVERGE
# [[2]]$converged
# [1] FALSE
# 
# [[2]]$pdHess
# [1] FALSE  TRUE


cv_test8_winter <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test8_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test8_winter.rds"))


#test8b iid instead of ar1
#37min
# tot_elpd = -3.042702
# tot_loglik = -26832.66
#model did converge,but it isn't the best model 
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
                               time = "month_of_season")
  #cv_fits[[1]] is now a list of 2 models. We want the second of each of these, 
  model_selection$elpd[indx] <- cv_fits[[indx]]$fold_elpd[2]
  model_selection$loglik[indx] <- cv_fits[[indx]]$fold_loglik[2]
}
# total the log lik or ELPD now across years
tot_elpd <- sum(model_selection$elpd)
tot_loglik <- sum(model_selection$loglik)
toc()

# 59min
#tot_elpd = -3.19837
#tot_loglik = -26793.03

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The model may not have converged. Maximum final gradient: 0.0763801297044484
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.
# 7: In stats::nlminb(start = tmb_obj$par, objective = tmb_obj$fn,  ... :   NA/NaN function evaluation

#MODEL DID CONVERGE
# [[2]]$converged
# [1] FALSE
# 
# [[2]]$pdHess
# [1] FALSE  TRUE


cv_test9_winter <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test9_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test9_winter.rds"))


#---------------------------------------------

#test 10
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
                               time = "half_month_of_season")
  #cv_fits[[1]] is now a list of 2 models. We want the second of each of these, 
  model_selection$elpd[indx] <- cv_fits[[indx]]$fold_elpd[2]
  model_selection$loglik[indx] <- cv_fits[[indx]]$fold_loglik[2]
}
# total the log lik or ELPD now across years
tot_elpd <- sum(model_selection$elpd)
tot_loglik <- sum(model_selection$loglik)
toc()

# 1.9h
#tot_elpd = -3.102217
#tot_loglik = -26634.45

#Warning messages:
# 1: Parameter ar1_phi is very close or equal to its lower bound.
# Consider changing your model configuration or bounds.
# 2: The model may not have converged: non-positive-definite Hessian matrix.
# 3: The model may not have converged. Maximum final gradient: 0.123697669406631.
# 4: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.

#MODEL DID CONVERGE
# [[2]]$converged
# [1] FALSE
# 
# [[2]]$pdHess
# [1] FALSE  TRUE


cv_test10_winter <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test10_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test10_winter.rds"))


#---------------------------------------------

#test 11
#covariates, no polynomials, month_of_seasonf is fixed effect, 
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

# 1.4h
#tot_elpd = -3.291668
#tot_loglik = -26428.36

#Warning messages:
# 2: The model may not have converged: non-positive-definite Hessian matrix.
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.

#MODEL DID NOT CONVERGE
# [[1]]$converged
# [1] FALSE
# 
# [[1]]$pdHess
# [1] FALSE FALSE


cv_test11_winter <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test11_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test11_winter.rds"))


#---------------------------------------------

#test 12
#covariates, no polynomials, half_month_of_seasonf is fixed effect, 
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
                                 half_month_of_seasonf +  
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

# 1.6h
#tot_elpd = -3.304559
#tot_loglik = -26454.48

#Warning messages:
# 2: The model may not have converged: non-positive-definite Hessian matrix.
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.

#MODEL DID NOT CONVERGE
# [[1]]$converged
# [1] FALSE
# 
# [[1]]$pdHess
# [1] FALSE FALSE


cv_test12_winter <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test12_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test12_winter.rds"))


#---------------------------------------------

#test 13
#covariates, polynomial term for depth, month_name_f is fixed effect, 
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
#tot_elpd = -3.263548
#tot_loglik = -26341.25

#Warning messages:
# 1: The model may not have converged: non-positive-definite Hessian matrix.
# 2: The model may not have converged. Maximum final gradient: 0.0318359416653351
# 3: The time elements in `newdata` are not identical to those in the original dataset.
# This is normally fine, but may create problems for index standardization.

#MODEL DID CONVERGE
# [[1]]$converged
# [1] FALSE
# 
# [[1]]$pdHess
# [1] FALSE  TRUE


cv_test13_winter <- cv_fits

#EXPORT THIS MODEL
#write_rds(cv_test13_winter, here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via CV',"cv_test13_winter.rds"))

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


























