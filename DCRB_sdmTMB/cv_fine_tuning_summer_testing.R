# fine tuning cross validation - summer data

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

              ## SUMMER DATA ##


# _20230209.rds is the most up to date df
d <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_all_data_20230209.rds')) 
#d = readRDS("data/df_full_final_tidy_all_data_20230209.rds")

d$month_name_f <- factor(d$month_name, levels = c("December", "January", "February", "March", "April", 
                                                  "May", "June", "July", "August", "September"))


#there is already a column for year as a numeric (yearn)
# and year as a factor (yearf)

summer <- dplyr::filter(d, month_name %in% c("May","June","July","August","September"))
summer$month_name_f <- as.factor(summer$month_name)
#there is already a column for month as a numeric (month_n) where e.g. May = 6, June = 7... to match winter and all data


# Add UTM columns (zone 10)
summer = add_utm_columns(summer, ll_names = c("grd_x", "grd_y"))



## best cv model

# this is effectively test 11 from model comparison data sheet, 
# except that the polynomial term is removed from bottom O2, and one interaction is added
## note thought that with summer data, a model with no polynomials (test 3 and 4) was better than test 11
# test 11 took about 40 mins to run
#tot_elpd for test 11 was -2.802409
#tot_loglik for test 11 was -42864.93
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
  train <- dplyr::filter(summer, yearn < yr)
  train$fold_id <- 1
  test <- dplyr::filter(summer, yearn == yr, month_name == "May")
  test$fold_id <- 2
  sub <- rbind(test, train)
  # make mesh for this dataset
  mesh <- make_mesh(train, xy_cols = c("X","Y"), cutoff = 10)
  indx <- yr - min(validation_years) + 1
  
  fit <- sdmTMB(formula = tottraps ~ 0 + 
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
            mesh = mesh,
            spatial = "off",
            spatiotemporal = "iid",
            data = train,
            control = sdmTMBcontrol(newton_loops = 1),
            time = "yearn")
  sanity(fit)
  # cv_fits[[indx]] <- sdmTMB_cv(formula = tottraps ~ 0 + 
  #                                season + 
  #                                month_name_f + 
  #                                #OR_WA_waters + #part of interaction term
  #                                z_SST_avg  + 
  #                                z_wind_avg +
  #                                poly(z_depth_point_mean, 2) +
  #                                z_depth_point_sd +
  #                                z_faults_km +
  #                                z_dist_canyon_km +
  #                                z_weighted_dist +
  #                                z_weighted_fuel_pricegal +
  #                                z_weighted_crab_ppp +
  #                                z_bottom_O2_avg +
  #                                OR_WA_waters * z_dist_to_closed_km, 
  #                              family = tweedie(),
  #                              fold_ids = sub$fold_id,
  #                              mesh = mesh,
  #                              spatial = "on",
  #                              spatiotemporal = "iid", #this could be changed to iid
  #                              data = sub,
  #                              time = "yearn")
  # #cv_fits[[1]] is now a list of 2 models. We want the second of each of these, 
  # model_selection$elpd[indx] <- cv_fits[[indx]]$fold_elpd[2]
  # model_selection$loglik[indx] <- cv_fits[[indx]]$fold_loglik[2]
}
# total the log lik or ELPD now across years
# tot_elpd <- sum(model_selection$elpd)
# tot_loglik <- sum(model_selection$loglik)
# toc()


















