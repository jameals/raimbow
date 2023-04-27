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
library(plotmo)

#-------------------------------------------------------------------------------------------------

#read in files where did a new scaling - center and divide by 2sd

d <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_all_data_20230209.rds'))
#z-scoring has been done across all data (winter and summer)
glimpse(d) 


#summer
summer <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_summer_20230209.rds')) 
#z-scoring has been done across summer only
glimpse(summer)



#winter
winter <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_winter_20230209.rds'))
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
                    #z2sd_month_n +
                    #z2sd_month_of_season +
                    z2sd_half_month_of_season +
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
AIC(mod0_summer) #additional test done later: one at a time test polynomial term on each variable and compare AICs

summary(mod0_summer) #no polynomials

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-153.01  -53.10  -22.30   11.24 3018.64 

#Coefficients:
#                             Estimate    Std. Error  t value Pr(>|t|)    
# (Intercept)                    37.6875     0.4212  89.482  < 2e-16 ***
#   z2sd_yearn                    2.9167     1.8353   1.589   0.1120    
#   z2sd_month_n                -13.1388     1.3336  -9.852  < 2e-16 ***
#   z2sd_OR_WA_waters            -4.0546     1.6353  -2.479   0.0132 *  
#   z2sd_WA_pot_reduction        -3.1370     0.9881  -3.175   0.0015 ** 
#   z2sd_SST_avg                 -7.1452     1.1598  -6.161 7.28e-10 ***
#   z2sd_wind_avg                -2.5285     0.9993  -2.530   0.0114 *  
#   z2sd_depth_point_mean        72.2535     1.3817  52.294  < 2e-16 ***
#   z2sd_depth_point_sd           4.9554     0.8520   5.816 6.04e-09 ***
#   z2sd_faults_km                2.2938     0.8981   2.554   0.0107 *  
#   z2sd_dist_canyon_km           2.0589     1.2295   1.675   0.0940 .  
#   z2sd_weighted_dist          -14.6379     1.0986 -13.324  < 2e-16 ***
#   z2sd_weighted_fuel_pricegal   8.8644     1.5286   5.799 6.70e-09 ***
#   z2sd_weighted_crab_ppp        5.9083     1.1909   4.961 7.01e-07 ***
#   z2sd_bottom_O2_avg           12.3380     1.3431   9.186  < 2e-16 ***
#   z2sd_dist_to_closed_km       -1.0228     1.1960  -0.855   0.3924    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 132.4 on 98760 degrees of freedom
#Multiple R-squared:  0.09408,	Adjusted R-squared:  0.09395 
#F-statistic: 683.8 on 15 and 98760 DF,  p-value: < 2.2e-16



residualPlots(mod0_summer)

#                               Test stat Pr(>|Test stat|)    
#z2sd_yearn                     7.5450        4.562e-14 ***
#z2sd_month_n                  14.4822        < 2.2e-16 ***
#z2sd_OR_WA_waters             -0.6989         0.484610    
#z2sd_WA_pot_reduction          0.0705         0.943791    
#z2sd_SST_avg                  -1.6782         0.093312 .  
# z2sd_wind_avg                 -1.4026         0.160748    
#z2sd_depth_point_mean         44.5300        < 2.2e-16 ***
#z2sd_depth_point_sd           -2.0276         0.042606 *  
#z2sd_faults_km                 1.6335         0.102358    
#z2sd_dist_canyon_km           14.3559        < 2.2e-16 ***
#z2sd_weighted_dist            -2.7880         0.005305 ** 
#z2sd_weighted_fuel_pricegal   11.2860        < 2.2e-16 ***
#z2sd_weighted_crab_ppp         8.5259        < 2.2e-16 ***
#z2sd_bottom_O2_avg            13.6348        < 2.2e-16 ***
#z2sd_dist_to_closed_km         1.7167         0.086041 .  
# Tukey test                    52.2195        < 2.2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


plotmo(mod0_summer, caption="Prediction Sensitivity Plot")

arm::display(standardize(mod0_summer))


# #first build lm with non-standardised variables?
# mod0_summer_raw <- lm(tottraps ~ yearn + 
#                         month_n +
#                         OR_WA_waters +
#                         WA_pot_reduction +
#                         SST_avg +
#                         wind_avg +
#                         depth_point_mean +
#                         depth_point_sd +
#                         faults_km +
#                         dist_canyon_km +
#                         weighted_dist +
#                         weighted_fuel_pricegal +
#                         weighted_crab_ppp +
#                         bottom_O2_avg +
#                         dist_to_closed_km, 
#                       data=summer)
# #then use arm::standardize() on the lm object?
# mod0_summer_raw_standardized <- standardize(mod0_summer_raw)
# display(mod0_summer_raw_standardized )
# summary(mod0_summer_raw_standardized)

#-------------------------------------------------------------------------------------------------

# WINTER

tic()
mod0_winter <- lm(tottraps ~ z2sd_yearn + 
                    z2sd_month_n +
                    #z2sd_month_of_season +
                    z2sd_half_month_of_season +
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
AIC(mod0_winter)
summary(mod0_winter)

#Residuals:
#  Min     1Q Median     3Q    Max 
#-382.3 -114.9  -39.4   42.5 3736.6 

#Coefficients:
#                             Estimate  Std. Error    t value Pr(>|t|)    
# (Intercept)                 121.0276     0.7573 159.807  < 2e-16 ***
#   z2sd_yearn                    6.9097     2.1715   3.182 0.001463 ** 
#   z2sd_month_n                -37.4356     2.0044 -18.677  < 2e-16 ***
#   z2sd_OR_WA_waters             8.1320     3.0260   2.687 0.007202 ** 
#   z2sd_SST_avg                 -8.8431     1.9861  -4.453 8.50e-06 ***
#   z2sd_wind_avg               -18.1742     2.0771  -8.750  < 2e-16 ***
#   z2sd_depth_point_mean       178.0153     2.6818  66.379  < 2e-16 ***
#   z2sd_depth_point_sd          18.1695     1.5333  11.850  < 2e-16 ***
#   z2sd_faults_km                6.0211     1.6271   3.701 0.000215 ***
#   z2sd_dist_canyon_km         -17.1959     2.2189  -7.750 9.29e-15 ***
#   z2sd_weighted_dist          -36.2062     1.9617 -18.457  < 2e-16 ***
#   z2sd_weighted_fuel_pricegal   8.5147     2.2870   3.723 0.000197 ***
#   z2sd_weighted_crab_ppp      -18.2186     1.9595  -9.298  < 2e-16 ***
#   z2sd_bottom_O2_avg           48.2610     2.2702  21.258  < 2e-16 ***
#   z2sd_dist_to_closed_km        6.9682     2.4363   2.860 0.004236 **
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
                    #z2sd_month_n +
                    #z2sd_month_of_season +
                    z2sd_half_month_of_season +
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
AIC(mod0_all_data)
summary(mod0_all_data)

# Residuals:
#   Min     1Q Median     3Q    Max 
# -347.4  -89.7  -31.2   29.7 3756.3 
# 
# Coefficients:
#                               Estimate  Std. Error  t value Pr(>|t|)    
#   (Intercept)                  81.0511     0.4476 181.077  < 2e-16 ***
#   z2sd_yearn                   14.8795     1.4293  10.410  < 2e-16 ***
#   z2sd_month_n                -63.1206     1.6847 -37.468  < 2e-16 ***
#   z2sd_OR_WA_waters            -4.9490     1.6964  -2.917  0.00353 ** 
#   z2sd_WA_pot_reduction        -4.3733     0.9842  -4.444 8.86e-06 ***
#   z2sd_SST_avg                -12.3862     1.6406  -7.550 4.38e-14 ***
#   z2sd_wind_avg               -10.9801     1.3133  -8.360  < 2e-16 ***
#   z2sd_depth_point_mean       117.7516     1.5009  78.454  < 2e-16 ***
#   z2sd_depth_point_sd          11.9503     0.9055  13.197  < 2e-16 ***
#   z2sd_faults_km                4.4196     0.9563   4.621 3.82e-06 ***
#   z2sd_dist_canyon_km          -8.7665     1.2853  -6.821 9.10e-12 ***
#   z2sd_weighted_dist          -29.4025     1.1603 -25.340  < 2e-16 ***
#   z2sd_weighted_fuel_pricegal  14.9575     1.4127  10.588  < 2e-16 ***
#   z2sd_weighted_crab_ppp      -15.6003     0.9881 -15.788  < 2e-16 ***
#   z2sd_bottom_O2_avg           56.8239     1.3768  41.271  < 2e-16 ***
#   z2sd_dist_to_closed_km        3.0913     1.2843   2.407  0.01609 * 
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
# z2sd_OR_WA_waters             -0.8126        0.4164277    
# z2sd_WA_pot_reduction         -1.3075        0.1910322    
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
# Tukey test                    83.7010        < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


plotmo(mod0_all_data, caption="Prediction Sensitivity Plot")



# #first build lm with non-standardised variables?
# mod0_alldata_raw <- lm(tottraps ~ yearn + 
#                         month_n +
#                         OR_WA_waters +
#                         WA_pot_reduction +
#                         SST_avg +
#                         wind_avg +
#                         depth_point_mean +
#                         depth_point_sd +
#                         faults_km +
#                         dist_canyon_km +
#                         weighted_dist +
#                         weighted_fuel_pricegal +
#                         weighted_crab_ppp +
#                         bottom_O2_avg +
#                         dist_to_closed_km, 
#                       data=d)
# #then use arm::standardize() on the lm object?
# mod0_alldata_raw_standardized <- standardize(mod0_alldata_raw)
# display(mod0_alldata_raw_standardized )
# summary(mod0_alldata_raw_standardized)



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
toc() #8.5 min

#After fixing 2*sd
#Warning messages:
#The model may not have converged. Maximum final gradient: 0.208164203222275

#sanity(fit5_summer_new_cols)
#few b_j, ln_tau, thetaf, ln_phi
#sanity(fit5_summer_new_cols, big_sd_log10 = 3, gradient_thresh = 0.005)
#2x b_j, thetaf, ln_phi
AIC(fit5_summer_new_cols)
#267862
#summary(fit5_summer_new_cols)
#all coef.se are NaN
#Spatiotemporal AR1 correlation (rho): 0.01



#ALL DATA

#best all data model is fit6_all_data

# Add UTM columns (zone 10)
d = add_utm_columns(d, ll_names = c("grd_x", "grd_y"))

mesh <- make_mesh(d, xy_cols = c("X","Y"), cutoff = 10)
mesh$mesh$n


tic()
fit6_all_data_new_cols <- sdmTMB(tottraps ~ 0 + 
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
                          spatiotemporal = "iid", 
                          data = d,
                          time = "month_n")
toc() #7.5min

# Warning messages:
# 1: In doTryCatch(return(expr), name, parentenv, handler) :  display list redraw incomplete
# 2: In doTryCatch(return(expr), name, parentenv, handler) :  invalid graphics state
# 3: In doTryCatch(return(expr), name, parentenv, handler) :  invalid graphics state
# 4: The model may not have converged. Maximum final gradient: 0.124553691012341. 
#sanity(fit6_all_data_new_cols)
# bunch
#sanity(fit6_all_data_new_cols, big_sd_log10 = 3, gradient_thresh = 0.005)
#still a bunch
AIC(fit6_all_data_new_cols)
#1016582




