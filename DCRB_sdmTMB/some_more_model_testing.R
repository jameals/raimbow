##some more model testing

#-------------------------------------------------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(sdmTMB)
library(sf)
library(ggcorrplot)
library(mgcv)
library(ggeffects)
library(tictoc)
library(car)
library(arm)

#-------------------------------------------------------------------------------------------------

#read in files where did a new scaling - center and divide by 2sd

d <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_all_data_20230203.rds'))
#z-scoring has been done across all data (winter and summer)
glimpse(d) 


#summer
summer <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_summer_20230203.rds')) 
#z-scoring has been done across summer only
glimpse(summer)



#winter
winter <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_winter_20230203.rds'))
#z-scoring has been done across summer only
glimpse(winter) 


#-------------------------------------------------------------------------------------------------

#1. fit a full linear model for each period (probably without and spatial or spatiotemporal fields). 
#2. drop coefficients that aren't significant at P < 0.05
#3. plot residuals from that model against each of the remaining coefficients, 
#and for anything looking quadratic, add the poly(...,2) in 

set.seed(1)
#-------------------------------------------------------------------------------------------------

#summer

tic()
mod0_summer <- lm(tottraps ~ z2sd_yearn + 
                    z2sd_month_n +
                    z2sd_OR_WA_waters +
                    z2sd_WA_pot_reduction +
                    z2sd_SST_avg +
                    z2sd_wind_avg +
                    z2sd_depth_point_mean +
                    z2sd_depth_point_sd +
                    z2sd_faults_km +
                    z2sd_dist_canyon_km +
                    z2sd_weighted_dist +
                    z2sd_weighted_fuel_pricegal +
                    z2sd_weighted_crab_ppp +
                    z2sd_bottom_O2_avg +
                    z2sd_dist_to_closed_km, 
                  data=summer)
toc() 
AIC(mod0_summer) #additional test done later: one at a time test polynomial term on each variable and comapre AICs

summary(mod0_summer) #no polynomials

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-153.01  -53.10  -22.30   11.24 3018.64 

#Coefficients:
#                             Estimate    Std. Error  t value Pr(>|t|)    
#  (Intercept)                  3.769e+01  4.212e-01  89.482  < 2e-16 ***
#  z2sd_yearn                   2.916e-01  1.835e-01   1.589   0.1120    
#  z2sd_month_n                -9.313e+00  9.452e-01  -9.852  < 2e-16 ***
#  z2sd_OR_WA_waters           -1.650e+01  6.654e+00  -2.479   0.0132 *  
#  z2sd_WA_pot_reduction       -5.394e+01  1.699e+01  -3.175   0.0015 ** 
#  z2sd_SST_avg                -6.138e-01  9.963e-02  -6.161 7.28e-10 ***
#  z2sd_wind_avg               -3.665e+00  1.448e+00  -2.530   0.0114 *  
#  z2sd_depth_point_mean        2.548e-02  4.872e-04  52.294  < 2e-16 ***
#  z2sd_depth_point_sd          1.661e-02  2.855e-03   5.816 6.04e-09 ***
#  z2sd_faults_km               2.345e-01  9.181e-02   2.554   0.0107 *  
#  z2sd_dist_canyon_km          1.073e-02  6.407e-03   1.675   0.0940 .  
#  z2sd_weighted_dist          -8.709e-03  6.536e-04 -13.324  < 2e-16 ***
#  z2sd_weighted_fuel_pricegal  1.305e+01  2.250e+00   5.799 6.70e-09 ***
#  z2sd_weighted_crab_ppp       4.991e+00  1.006e+00   4.961 7.01e-07 ***
#  z2sd_bottom_O2_avg           5.913e-03  6.437e-04   9.186  < 2e-16 ***
#  z2sd_dist_to_closed_km      -2.300e-05  2.689e-05  -0.855   0.3924    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 132.4 on 98760 degrees of freedom
#Multiple R-squared:  0.09408,	Adjusted R-squared:  0.09395 
#F-statistic: 683.8 on 15 and 98760 DF,  p-value: < 2.2e-16



residualPlots(mod0_summer)

#                               Test stat Pr(>|Test stat|)    
#  z2sd_yearn                     7.5450        4.562e-14 ***
#  z2sd_month_n                  14.4822        < 2.2e-16 ***
#  z2sd_OR_WA_waters              0.7165         0.473702    
#  z2sd_WA_pot_reduction         -0.3268         0.743797    
#  z2sd_SST_avg                  -1.6782         0.093312 .  
#  z2sd_wind_avg                 -1.4026         0.160748    
#  z2sd_depth_point_mean         44.5300        < 2.2e-16 ***
#  z2sd_depth_point_sd           -2.0276         0.042606 *  
#  z2sd_faults_km                 1.6335         0.102358    
#  z2sd_dist_canyon_km           14.3559        < 2.2e-16 ***
#  z2sd_weighted_dist            -2.7880         0.005305 ** 
#  z2sd_weighted_fuel_pricegal   11.2860        < 2.2e-16 ***
#  z2sd_weighted_crab_ppp         8.5259        < 2.2e-16 ***
#  z2sd_bottom_O2_avg            13.6348        < 2.2e-16 ***
#  z2sd_dist_to_closed_km         1.7167         0.086041 .  
#Tukey test                    52.2195        < 2.2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


plotmo(mod0_summer, caption="Prediction Sensitivity Plot")

arm::display(standardize(mod0_summer))


#first build lm with non-standardised variables?
mod0_summer_raw <- lm(tottraps ~ yearn + 
                        month_n +
                        OR_WA_waters +
                        WA_pot_reduction +
                        SST_avg +
                        wind_avg +
                        depth_point_mean +
                        depth_point_sd +
                        faults_km +
                        dist_canyon_km +
                        weighted_dist +
                        weighted_fuel_pricegal +
                        weighted_crab_ppp +
                        bottom_O2_avg +
                        dist_to_closed_km, 
                      data=summer)
#then use arm::standardize() on the lm object?
mod0_summer_raw_standardized <- standardize(mod0_summer_raw)
display(mod0_summer_raw_standardized )
summary(mod0_summer_raw_standardized)

#-------------------------------------------------------------------------------------------------

# WINTER

tic()
mod0_winter <- lm(tottraps ~ z2sd_yearn + 
                    z2sd_month_n +
                    z2sd_OR_WA_waters +
                    #z2sd_WA_pot_reduction + #not relevant in winter
                    z2sd_SST_avg +
                    z2sd_wind_avg +
                    z2sd_depth_point_mean +
                    z2sd_depth_point_sd +
                    z2sd_faults_km +
                    z2sd_dist_canyon_km +
                    z2sd_weighted_dist +
                    z2sd_weighted_fuel_pricegal +
                    z2sd_weighted_crab_ppp +
                    z2sd_bottom_O2_avg +
                    z2sd_dist_to_closed_km, 
                  data=winter)
toc() 

summary(mod0_winter)

#Residuals:
#  Min     1Q Median     3Q    Max 
#-382.3 -114.9  -39.4   42.5 3736.6 

#Coefficients:
#                             Estimate  Std. Error    t value Pr(>|t|)    
#(Intercept)                  1.210e+02  7.573e-01 159.807  < 2e-16 ***
#  z2sd_yearn                   6.857e-01  2.155e-01   3.182 0.001463 ** 
#  z2sd_month_n                -2.321e+01  1.243e+00 -18.677  < 2e-16 ***
#  z2sd_OR_WA_waters            3.650e+01  1.358e+01   2.687 0.007202 ** 
#  z2sd_SST_avg                -1.489e+00  3.345e-01  -4.453 8.50e-06 ***
#  z2sd_wind_avg               -6.129e+00  7.005e-01  -8.750  < 2e-16 ***
#  z2sd_depth_point_mean        6.298e-02  9.488e-04  66.379  < 2e-16 ***
#  z2sd_depth_point_sd          5.500e-02  4.642e-03  11.850  < 2e-16 ***
#  z2sd_faults_km               6.836e-01  1.847e-01   3.701 0.000215 ***
#  z2sd_dist_canyon_km         -9.004e-02  1.162e-02  -7.750 9.29e-15 ***
#  z2sd_weighted_dist          -2.427e-02  1.315e-03 -18.457  < 2e-16 ***
#  z2sd_weighted_fuel_pricegal  1.360e+01  3.653e+00   3.723 0.000197 ***
#  z2sd_weighted_crab_ppp      -7.573e+00  8.145e-01  -9.298  < 2e-16 ***
#  z2sd_bottom_O2_avg           1.125e-02  5.294e-04  21.258  < 2e-16 ***
# z2sd_dist_to_closed_km       2.555e-04  8.932e-05   2.860 0.004236 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 247.9 on 107130 degrees of freedom
#Multiple R-squared:  0.1609,	Adjusted R-squared:  0.1608 
#F-statistic:  1467 on 14 and 107130 DF,  p-value: < 2.2e-16


residualPlots(mod0_winter)

#                               Test stat   Pr(>|Test stat|)    
#  z2sd_yearn                    -3.7843        0.0001542 ***
#  z2sd_month_n                 -11.6253        < 2.2e-16 ***
#  z2sd_OR_WA_waters             -1.2284        0.2192886    
#  z2sd_SST_avg                  -9.4347        < 2.2e-16 ***
#  z2sd_wind_avg                 -5.4592        4.792e-08 ***
#  z2sd_depth_point_mean         25.9452        < 2.2e-16 ***
#  z2sd_depth_point_sd           -6.6571        2.805e-11 ***
#  z2sd_faults_km                 8.9520        < 2.2e-16 ***
#  z2sd_dist_canyon_km          -14.0639        < 2.2e-16 ***
#  z2sd_weighted_dist             4.8029        1.566e-06 ***
#  z2sd_weighted_fuel_pricegal    9.5785        < 2.2e-16 ***
#  z2sd_weighted_crab_ppp        -0.2638        0.7919079    
#  z2sd_bottom_O2_avg            17.2061        < 2.2e-16 ***
#  z2sd_dist_to_closed_km       -11.8667        < 2.2e-16 ***
#  Tukey test                    38.7944        < 2.2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1




plotmo(mod0_winter, caption="Prediction Sensitivity Plot")

#-------------------------------------------------------------------------------------------------

#ALL DATA

tic()
mod0_all_data <- lm(tottraps ~ z2sd_yearn + 
                    z2sd_month_n +
                    z2sd_OR_WA_waters +
                    z2sd_WA_pot_reduction +
                    z2sd_SST_avg +
                    z2sd_wind_avg +
                    z2sd_depth_point_mean +
                    z2sd_depth_point_sd +
                    z2sd_faults_km +
                    z2sd_dist_canyon_km +
                    z2sd_weighted_dist +
                    z2sd_weighted_fuel_pricegal +
                    z2sd_weighted_crab_ppp +
                    z2sd_bottom_O2_avg +
                    z2sd_dist_to_closed_km, 
                  data=d)
toc() 

summary(mod0_all_data)

# Residuals:
#   Min     1Q Median     3Q    Max 
# -347.4  -89.7  -31.2   29.7 3756.3 
# 
# Coefficients:
#                               Estimate  Std. Error  t value Pr(>|t|)    
# (Intercept)                  8.105e+01  4.476e-01 181.077  < 2e-16 ***
#   z2sd_yearn                   1.480e+00  1.422e-01  10.410  < 2e-16 ***
#   z2sd_month_n                -1.076e+01  2.871e-01 -37.468  < 2e-16 ***
#   z2sd_OR_WA_waters           -2.094e+01  7.179e+00  -2.917  0.00353 ** 
#   z2sd_WA_pot_reduction       -1.516e+02  3.411e+01  -4.444 8.86e-06 ***
#   z2sd_SST_avg                -6.146e-01  8.141e-02  -7.550 4.38e-14 ***
#   z2sd_wind_avg               -3.777e+00  4.518e-01  -8.360  < 2e-16 ***
#   z2sd_depth_point_mean        4.157e-02  5.299e-04  78.454  < 2e-16 ***
#   z2sd_depth_point_sd          3.793e-02  2.874e-03  13.197  < 2e-16 ***
#   z2sd_faults_km               4.760e-01  1.030e-01   4.621 3.82e-06 ***
#   z2sd_dist_canyon_km         -4.578e-02  6.712e-03  -6.821 9.10e-12 ***
#   z2sd_weighted_dist          -1.849e-02  7.296e-04 -25.340  < 2e-16 ***
#   z2sd_weighted_fuel_pricegal  2.295e+01  2.168e+00  10.588  < 2e-16 ***
#   z2sd_weighted_crab_ppp      -8.474e+00  5.367e-01 -15.788  < 2e-16 ***
#   z2sd_bottom_O2_avg           1.523e-02  3.691e-04  41.271  < 2e-16 ***
#   z2sd_dist_to_closed_km       8.639e-05  3.589e-05   2.407  0.01609 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 203.1 on 205905 degrees of freedom
# Multiple R-squared:   0.16,	Adjusted R-squared:  0.1599 
# F-statistic:  2614 on 15 and 205905 DF,  p-value: < 2.2e-16


residualPlots(mod0_all_data)

#                                 Test stat Pr(>|Test stat|)    
# z2sd_yearn                    -1.0192        0.3080893    
# z2sd_month_n                  10.9924        < 2.2e-16 ***
# z2sd_OR_WA_waters              0.7780        0.4365840    
# z2sd_WA_pot_reduction          1.1365        0.2557667    
# z2sd_SST_avg                  -1.8100        0.0702919 .  
# z2sd_wind_avg                -12.3544        < 2.2e-16 ***
# z2sd_depth_point_mean         39.6543        < 2.2e-16 ***
# z2sd_depth_point_sd           -8.2619        < 2.2e-16 ***
# z2sd_faults_km                 7.9761        1.518e-15 ***
# z2sd_dist_canyon_km           -0.5100        0.6100434    
# z2sd_weighted_dist             4.4524        8.497e-06 ***
# z2sd_weighted_fuel_pricegal   13.1534        < 2.2e-16 ***
# z2sd_weighted_crab_ppp         3.5861        0.0003358 ***
# z2sd_bottom_O2_avg            37.2106        < 2.2e-16 ***
# z2sd_dist_to_closed_km        -8.9504        < 2.2e-16 ***
#   Tukey test                    83.7010        < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


plotmo(mod0_all_data, caption="Prediction Sensitivity Plot")



#first build lm with non-standardised variables?
mod0_alldata_raw <- lm(tottraps ~ yearn + 
                        month_n +
                        OR_WA_waters +
                        WA_pot_reduction +
                        SST_avg +
                        wind_avg +
                        depth_point_mean +
                        depth_point_sd +
                        faults_km +
                        dist_canyon_km +
                        weighted_dist +
                        weighted_fuel_pricegal +
                        weighted_crab_ppp +
                        bottom_O2_avg +
                        dist_to_closed_km, 
                      data=d)
#then use arm::standardize() on the lm object?
mod0_alldata_raw_standardized <- standardize(mod0_alldata_raw)
display(mod0_alldata_raw_standardized )
summary(mod0_alldata_raw_standardized)



#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------

#test the new columns in one of the good models
##SUMMER MODEL

#best summer model is fit5_summer

# Add UTM columns (zone 10)
summer = add_utm_columns(summer, ll_names = c("grd_x", "grd_y"))

mesh <- make_mesh(summer, xy_cols = c("X","Y"), cutoff = 10)
mesh$mesh$n


tic()
fit5_summer_new_cols <- sdmTMB(tottraps ~ 0 + 
                        z2sd_yearn + 
                        z2sd_month_n +
                        z2sd_OR_WA_waters +
                        z2sd_WA_pot_reduction +
                        z2sd_SST_avg +
                        z2sd_wind_avg +
                        z2sd_depth_point_mean +
                        z2sd_depth_point_sd +
                        z2sd_faults_km +
                        z2sd_dist_canyon_km +
                        z2sd_weighted_dist +
                        z2sd_weighted_fuel_pricegal +
                        z2sd_weighted_crab_ppp +
                        z2sd_bottom_O2_avg +
                        z2sd_dist_to_closed_km,
                      family = tweedie(),
                      mesh = mesh,
                      spatial = "on",
                      spatiotemporal = "ar1", # <- new
                      data = summer,
                      time = "yearf")
toc() #30 min

#Warning messages:
#1: In stats::nlminb(start = tmb_obj$par, objective = tmb_obj$fn, gradient = tmb_obj$gr,  :   NA/NaN function evaluation
#2: In stats::nlminb(start = tmb_obj$par, objective = tmb_obj$fn, gradient = tmb_obj$gr,  :   NA/NaN function evaluation
#3: In stats::nlminb(start = tmb_obj$par, objective = tmb_obj$fn, gradient = tmb_obj$gr,  :   NA/NaN function evaluation
#4: The model may not have converged: non-positive-definite Hessian matrix

#sanity(fit5_summer_new_cols)
#lots of red Xs
#sanity(fit5_summer_new_cols, big_sd_log10 = 3, gradient_thresh = 0.005)
#lots of red Xs
AIC(fit5_summer_new_cols)
#284834
#summary(fit5_summer_new_cols)
#all coef.se are NaN
#Spatiotemporal AR1 correlation (rho): 0.01








