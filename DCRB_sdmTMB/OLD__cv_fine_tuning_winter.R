# fine tuning cross validation - winter data

# this script has example code for the best performing CV model so far 

#------------------------------------------------------------------------------------------


library(here)
library(tidyverse)
library(tictoc)
library(viridis)

library(sdmTMB)
library(mgcv)
library(ggeffects)
library(ggplot2)

#------------------------------------------------------------------------------------------

#---------------------------------------------

              ## WINTER DATA ##


# _20230209.rds is the most up to date df
d <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_all_data_20230209.rds')) 
#d = readRDS("data/df_full_final_tidy_all_data_20230209.rds")

d$month_name_f <- factor(d$month_name, levels = c("December", "January", "February", "March", "April", 
                                                  "May", "June", "July", "August", "September"))


#there is already a column for year as a numeric (yearn)
# and year as a factor (yearf)

winter <- dplyr::filter(d, month_name %in% c("December", "January", "February",
                                             "March", "April", "May"))

#there is already a column for month as a numeric (month_n) where e.g. December = 1, January = 2...


# Add UTM columns (zone 10)
winter = add_utm_columns(winter, ll_names = c("grd_x", "grd_y"))


## best cv model

# this is effectively test 11 from model comparison data sheet, 
# except that the polynomial term is removed from bottom O2, and one interaction is added
# test 11 took about 1.7hours to run, so this might take a while
#tot_elpd for test 11 was -3.24055
#tot_loglik for test 11 was -26431.91
#test 11 had these unique warnings
#1: The model may not have converged: non-positive-definite Hessian matrix.
#3: The time elements in `newdata` are not identical to those in the original dataset.
#This is normally fine, but may create problems for index standardization.


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
                                 month_name_f +  
                                 #OR_WA_waters + #part of interaction term
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
                                 OR_WA_waters * z_dist_to_closed_km, 
                               family = tweedie(),
                               fold_ids = sub$fold_id,
                               mesh = mesh,
                               spatial = "on",
                               spatiotemporal = "ar1", #this could be changed to iid
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





















