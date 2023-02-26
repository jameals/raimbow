## testing cross validation - winter 

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


## Data loading and cleaning


#d = readRDS("data/df_full_final_tidy_all_data.rds")

#if instead read in this one, can skip couple of the next steps:
#but note that we need May in the data set as well
d <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_all_data_20230209.rds')) 
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

## Workflow

#-   identify a small number of models to do the cross validation for. below is code for 1 model, and you'd want to repeat this for several
#-   identify time period used for validation. below, it's the second half-month of April. For the May/summer models, this filtering will have to be slightly different.
#-   to make sure our predictions are robust, we'll want to test each of these models against 5-10 years of data. These are forward looking, so all future years are not included in fits
#-   sdmTMB_cv replaces sdmTMB, where we include the fold IDs
#-   several metrics exist for model selection, and we want to total these across our validation years. The highest ELPD or loglik will correspond to the model with highest predictive accuracy.

#---------------------------------------------

## Example of a model

#based on fit10e_winter but no polynomial terms

#this doesn't work with polys
#Error in stats::nlminb(start = tmb_obj$par, objective = tmb_obj$fn, gradient = tmb_obj$gr,  :  NA/NaN gradient evaluation
#In addition: Warning message:
#In stats::nlminb(start = tmb_obj$par, objective = tmb_obj$fn, gradient = tmb_obj$gr,  :  NA/NaN function evaluation


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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season +
                                 month_of_seasonf +  #new
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

#took about 1.3 hours
#tot_elpd = -3.280038
#tot_loglik = -26529.34


#---------------------------------------------

#same as above but with iid
#polys don't work in this one either

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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season +
                                 month_of_seasonf +  #new
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
                                 poly(z_bottom_O2_avg,2) +
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


#took about 40mins
#tot_elpd = -3.28263
#tot_loglik = -26,528.2


#---------------------------------------------

#month_name_f as fixed effect
#didn't run it, but polys seem to work in this model

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


#took about 1.6h
#tot_elpd = -3.248421
#tot_loglik = -26498.39


#---------------------------------------------

#month_name_f as fixed effect - now with iid

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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season +
                                 month_name_f +  #new
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


#took about 39min
#tot_elpd = -3.250456
#tot_loglik = -26495.87

#---------------------------------------------


#based on fit10f_winter but no polynomial terms - test HMOS as fixed effect 


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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season +
                                 half_month_of_seasonf +  #new
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


#took about 1.4h
#tot_elpd = -3.293931
#tot_loglik = -26546.88


#---------------------------------------------

#based on fit10f_winter but no polynomial terms - test HMOS as fixed effect WITH IID


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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season +
                                 half_month_of_seasonf +  #new
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


#took about 46min
#tot_elpd = -3.296629
#tot_loglik = -26545.78



#---------------------------------------------

#based on fit2_winter but no polynomial terms - time = "half_month_of_seasonf" 


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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season +
                                 month_name_f +  #new
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
                               time = "half_month_of_season") #The time column is a factor and there are extra factor levels
  #cv_fits[[1]] is now a list of 2 models. We want the second of each of these, 
  model_selection$elpd[indx] <- cv_fits[[indx]]$fold_elpd[2]
  model_selection$loglik[indx] <- cv_fits[[indx]]$fold_loglik[2]
}
# total the log lik or ELPD now across years
tot_elpd <- sum(model_selection$elpd)
tot_loglik <- sum(model_selection$loglik)
toc()

##had some wraning ar1 close to ro at its lowest end. and the 'may not have convergerd - hessina matrix stuff

#took about 2.2h
#tot_elpd = -3.094883
#tot_loglik = -26756


#---------------------------------------------

#same as above but with iid

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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season +
                                 month_name_f +  #new
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
                               time = "half_month_of_season") #The time column is a factor and there are extra factor levels
  #cv_fits[[1]] is now a list of 2 models. We want the second of each of these, 
  model_selection$elpd[indx] <- cv_fits[[indx]]$fold_elpd[2]
  model_selection$loglik[indx] <- cv_fits[[indx]]$fold_loglik[2]
}
# total the log lik or ELPD now across years
tot_elpd <- sum(model_selection$elpd)
tot_loglik <- sum(model_selection$loglik)
toc()

#1: The model may not have converged: non-positive-definite Hessian matrix.
#3: The time elements in `newdata` are not identical to those in the original dataset.
#This is normally fine, but may create problems for index standardization.

#took about 44min
#tot_elpd = -3.104165
#tot_loglik = -26773.54


#---------------------------------------------



#based on  fit7_winter but no polynomial terms - time = "month_n" 


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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season +
                                 month_name_f +  #new
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
                               time = "month_n") #
  #cv_fits[[1]] is now a list of 2 models. We want the second of each of these, 
  model_selection$elpd[indx] <- cv_fits[[indx]]$fold_elpd[2]
  model_selection$loglik[indx] <- cv_fits[[indx]]$fold_loglik[2]
}
# total the log lik or ELPD now across years
tot_elpd <- sum(model_selection$elpd)
tot_loglik <- sum(model_selection$loglik)
toc()

#5: The model may not have converged: non-positive-definite Hessian matrix.
#7: In stats::nlminb(start = tmb_obj$par, objective = tmb_obj$fn,  ... : NA/NaN function evaluation

#took about 1.2h
#tot_elpd = -3.025076
#tot_loglik = -26906.81




#---------------------------------------------

#same as above but now with iid

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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season +
                                 month_name_f +  #new
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
                               time = "month_n") #
  #cv_fits[[1]] is now a list of 2 models. We want the second of each of these, 
  model_selection$elpd[indx] <- cv_fits[[indx]]$fold_elpd[2]
  model_selection$loglik[indx] <- cv_fits[[indx]]$fold_loglik[2]
}
# total the log lik or ELPD now across years
tot_elpd <- sum(model_selection$elpd)
tot_loglik <- sum(model_selection$loglik)
toc()

#1: The model may not have converged: non-positive-definite Hessian matrix.
#3: The time elements in `newdata` are not identical to those in the original dataset.
#This is normally fine, but may create problems for index standardization.

#took about 38min
#tot_elpd = -3.028857
#tot_loglik = -26947.56




#---------------------------------------------


#based on  fit11b_winter but no polynomial terms - time = "month_of_season" 


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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
                                 season +
                                 month_name_f +  #new
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
                               time = "month_of_season") #
  #cv_fits[[1]] is now a list of 2 models. We want the second of each of these, 
  model_selection$elpd[indx] <- cv_fits[[indx]]$fold_elpd[2]
  model_selection$loglik[indx] <- cv_fits[[indx]]$fold_loglik[2]
}
# total the log lik or ELPD now across years
tot_elpd <- sum(model_selection$elpd)
tot_loglik <- sum(model_selection$loglik)
toc()

#1: The model may not have converged: non-positive-definite Hessian matrix.
#3: The time elements in `newdata` are not identical to those in the original dataset.
#This is normally fine, but may create problems for index standardization.

#took about 62min
#tot_elpd = -3.184797
#tot_loglik = -26932.85



#---------------------------------------------

#model based of fit2c_winter - no covariates - test different terms in time = xx?


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
  cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0, 
                               family = tweedie(),
                               fold_ids = sub$fold_id,
                               mesh = mesh,
                               spatial = "on",
                               spatiotemporal = "ar1",
                               data = sub,
                               time = "half_month_of_season") #year / month / MOS/ HMOS
  #cv_fits[[1]] is now a list of 2 models. We want the second of each of these, 
  model_selection$elpd[indx] <- cv_fits[[indx]]$fold_elpd[2]
  model_selection$loglik[indx] <- cv_fits[[indx]]$fold_loglik[2]
}
# total the log lik or ELPD now across years
tot_elpd <- sum(model_selection$elpd)
tot_loglik <- sum(model_selection$loglik)
toc()


#time = "yearn"
#1: The time elements in `newdata` are not identical to those in the original dataset.
#This is normally fine, but may create problems for index standardization. 
#2: The model may not have converged. Maximum final gradient: 0.015803517873854. 
#4: In sqrt(diag(cov)) : NaNs produced
#5: The model may not have converged: non-positive-definite Hessian matrix. 
#took about 12min
#tot_elpd = -3.774164
#tot_loglik = -27236.09


#time = "month_n"
#1: The time elements in `newdata` are not identical to those in the original dataset.
#This is normally fine, but may create problems for index standardization. 
#2: The time elements in `newdata` are not identical to those in the original dataset.
#This is normally fine, but may create problems for index standardization. 
#3: The model may not have converged: non-positive-definite Hessian matrix. 
#took about 13min
#tot_elpd = -3.008716
#tot_loglik = -26900.54


#time = "month_of_season"
#1: In sqrt(diag(cov)) : NaNs produced
#2: The model may not have converged: non-positive-definite Hessian matrix.
#3: The time elements in `newdata` are not identical to those in the original dataset.
#This is normally fine, but may create problems for index standardization.
#8: The model may not have converged. Maximum final gradient: 0.0161154175954374.
#10: The model may not have converged. Maximum final gradient: 0.0234743564858193.
#took about 10min
#tot_elpd = -3.198134
#tot_loglik = -26757.84


#time = "half_month_of_season"
#1: Parameter ar1_phi is very close or equal to its lower bound.
#Consider changing your model configuration or bounds. 
#2: The model may not have converged. Maximum final gradient: 0.0107073975749472. 
#3: The time elements in `newdata` are not identical to those in the original dataset.
#This is normally fine, but may create problems for index standardization. 
#took about 26min
#tot_elpd = -3.149612
#tot_loglik = -26682.6


#---------------------------------------------

#realised polynomial terms work here
#month_name_f as fixed effect (test 3) but now with polynomials

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
#3: The time elements in `newdata` are not identical to those in the original dataset.
#This is normally fine, but may create problems for index standardization.

#took about 1.7h
#tot_elpd = -
#tot_loglik = -


#---------------------------------------------

#---------------------------------------------

#---------------------------------------------

#---------------------------------------------

#---------------------------------------------


















