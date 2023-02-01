#sdmTMB runs part 2 - work on best models


#-------------------------------------------------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(sdmTMB)
library(sf)
library(ggcorrplot)
library(mgcv)
library(ggeffects)
library(tictoc)

#this was needed for sdmTMB to work
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)

#-------------------------------------------------------------------------------------------------

set.seed(1)

#-------------------------------------------------------------------------------------------------
# run models based on Eric's "examples" document in repo

#read in df
#no need to filter, that has been done
#but make sure set UTM zone

##NOTE THAT NEED TO RUN ALL DATA, SUMMER, AND WINTER DATASETS


d <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_all_data.rds'))
#z-scoring has been done across all data (winter and summer)
glimpse(d) #none z-scored column have been retained

d$yearn <- as.numeric(substr(d$season,1,4))
d$yearf <- as.factor(d$yearn)

# Add UTM columns (zone 10)
d = add_utm_columns(d, ll_names = c("grd_x", "grd_y"))

# try smooth over months
d$month_n <- 1
d$month_n[which(d$month_name=="January")] = 2
d$month_n[which(d$month_name=="February")] = 3
d$month_n[which(d$month_name=="March")] = 4
d$month_n[which(d$month_name=="April")] = 5
d$month_n[which(d$month_name=="May")] = 6
d$month_n[which(d$month_name=="June")] = 7
d$month_n[which(d$month_name=="July")] = 8
d$month_n[which(d$month_name=="August")] = 9
d$month_n[which(d$month_name=="September")] = 10

#d$month_f <- as.factor(d$month_n)

#summer
summer <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_summer.rds'))
#z-scoring has been done across summer only
glimpse(summer) #none z-scored column have been retained

summer$yearn <- as.numeric(substr(summer$season,1,4))
summer$yearf <- as.factor(summer$yearn)

# try smooth over months
summer$month_n <- 5
summer$month_n[which(summer$month_name=="June")] = 6
summer$month_n[which(summer$month_name=="July")] = 7
summer$month_n[which(summer$month_name=="August")] = 8
summer$month_n[which(summer$month_name=="September")] = 9

# Add UTM columns (zone 10)
summer = add_utm_columns(summer, ll_names = c("grd_x", "grd_y"))



#winter

winter <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_final_tidy_winter.rds'))
#z-scoring has been done across summer only
glimpse(winter) #none z-scored column have been retained

winter$yearn <- as.numeric(substr(winter$season,1,4))
winter$yearf <- as.factor(winter$yearn)

# try smooth over months
winter$month_n <- 1
winter$month_n[which(winter$month_name=="January")] = 2
winter$month_n[which(winter$month_name=="February")] = 3
winter$month_n[which(winter$month_name=="March")] = 4
winter$month_n[which(winter$month_name=="April")] = 5

# Add UTM columns (zone 10)
winter = add_utm_columns(winter, ll_names = c("grd_x", "grd_y"))
#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------

#ALL DATA

#best all data model is fit6_all_data

mesh <- make_mesh(d, xy_cols = c("X","Y"), cutoff = 10)
mesh$mesh$n


tic()
fit6_all_data <- sdmTMB(tottraps ~ 0 + 
                          season +
                          month_name + 
                          OR_WA_waters +
                          WA_pot_reduction +
                          poly(z_SST_avg,2) +
                          poly(z_wind_avg,2) +
                          poly(z_depth_point_mean,2) +
                          poly(z_depth_point_sd,2) +
                          poly(z_faults_km,2) +
                          poly(z_dist_canyon_km,2) +
                          poly(z_weighted_dist,2) +
                          poly(z_weighted_fuel_pricegal,2) +
                          poly(z_weighted_crab_ppp,2) +
                          poly(z_bottom_O2_avg,2) +
                          poly(z_dist_to_closed_km ,2),
                        family = tweedie(),
                        mesh = mesh,
                        spatial = "on",
                        spatiotemporal = "iid",
                        data = d,
                        time = "month_n")
toc() #13.7min

#sanity(fit6_all_data)
# no warning messages. some red Xs (ln_tau, ln_kappa, thetaf)
#sanity(fit6_all_data, big_sd_log10 = 3, gradient_thresh = 0.005)
#
AIC(fit6_all_data)
#1012696



## do estimated relationships make sense?

#  This little function is just for helping to display the effects on log scale

plot_log = function(object, term) {
  g <- ggeffect(object, term, back.transform = FALSE)
  g$conf.low <- log(g$conf.low)
  g$conf.high <- log(g$conf.high)
  g$predicted <- log(g$predicted)
  plot(g)
}


p1 <- plot_log(fit6_all_data, "z_SST_avg [all]")
p2 <- plot_log(fit6_all_data, "z_wind_avg [all]")
p3 <- plot_log(fit6_all_data, "z_depth_point_mean [all]")
p4 <- plot_log(fit6_all_data, "z_depth_point_sd [all]")
p5 <- plot_log(fit6_all_data, "z_faults_km [all]")
p6 <- plot_log(fit6_all_data, "z_dist_canyon_km [all]")
p7 <- plot_log(fit6_all_data, "z_weighted_dist [all]")
p8 <- plot_log(fit6_all_data, "z_weighted_fuel_pricegal [all]")
p9 <- plot_log(fit6_all_data, "z_weighted_crab_ppp [all]")
p10 <- plot_log(fit6_all_data, "z_bottom_O2_avg [all]")
p11 <- plot_log(fit6_all_data, "z_dist_to_closed_km  [all]")
gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,nrow=4)


#Residuals – do qqplots look ok? #Error in plot.window(...) : need finite 'ylim' values
res_fit6_all_data <- residuals(fit6_all_data)
qqnorm(res_fit6_all_data,ylim=c(-5,5))
qqline(res_fit6_all_data)

#The slower and more robust check is:
tic()
mcmc_res_fit6_all_data <- residuals(fit6_all_data, type = "mle-mcmc", mcmc_iter = 101, mcmc_warmup = 100)
toc() #
qqnorm(mcmc_res_fit6_all_data)
qqline(mcmc_res_fit6_all_data)


#Spatial predictions – make sense?
pred <- d
pred$OR_WA_waters <- summer$OR_WA_waters[1]
pred$season <- "2018-2019"
pred$z_SST_avg <- mean(summer$z_SST_avg)
pred$z_wind_avg <- mean(summer$z_wind_avg)
pred$z_depth_point_mean <- mean(summer$z_depth_point_mean)
pred$z_depth_point_sd <- mean(summer$z_depth_point_sd)
pred$z_faults_km <- mean(summer$z_faults_km)
pred$z_dist_canyon_km <- mean(summer$z_dist_canyon_km)
pred$z_weighted_dist <- mean(summer$z_weighted_dist)
pred$z_weighted_fuel_pricegal <- mean(summer$z_weighted_fuel_pricegal)
pred$z_weighted_crab_ppp <- mean(summer$z_weighted_crab_ppp)
pred$z_bottom_O2_avg <- mean(summer$z_bottom_O2_avg)
pred$z_dist_to_closed_km <- mean(summer$z_dist_to_closed_km)

pred <- predict(fit6_all_data, pred)
#as(<dgCMatrix>, "dgTMatrix") is deprecated since Matrix 1.5-0; do as(., "TsparseMatrix") instead

p <- ggplot(dplyr::filter(pred,yearf=="2019"), aes(X,Y, col = est)) +
  geom_point(size=1.8,alpha=1.8) +
  scale_color_gradient2()
p


#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------

##SUMMER MODEL

#best summer model is fit5_summer

mesh <- make_mesh(summer, xy_cols = c("X","Y"), cutoff = 10)
mesh$mesh$n


tic()
fit5_summer <- sdmTMB(tottraps ~ 0 + 
                        season +
                        month_name + 
                        OR_WA_waters +
                        WA_pot_reduction +
                        poly(z_SST_avg,2) +
                        poly(z_wind_avg,2) +
                        poly(z_depth_point_mean,2) +
                        poly(z_depth_point_sd,2) +
                        poly(z_faults_km,2) +
                        poly(z_dist_canyon_km,2) +
                        poly(z_weighted_dist,2) +
                        poly(z_weighted_fuel_pricegal,2) +
                        poly(z_weighted_crab_ppp,2) +
                        poly(z_bottom_O2_avg,2) +
                        poly(z_dist_to_closed_km ,2),
                      family = tweedie(),
                      mesh = mesh,
                      spatial = "on",
                      spatiotemporal = "ar1", # <- new
                      data = summer,
                      time = "yearf")
toc() #13min

#sanity(fit5_summer)
#no warnings. No red Xs
#sanity(fit5_summer, big_sd_log10 = 3, gradient_thresh = 0.005)
#
AIC(fit5_summer)
#265861
#summary(fit5_summer)
#Spatiotemporal AR1 correlation (rho): 0.47


## do estimated relationships make sense?

#  This little function is just for helping to display the effects on log scale

plot_log = function(object, term) {
  g <- ggeffect(object, term, back.transform = FALSE)
  g$conf.low <- log(g$conf.low)
  g$conf.high <- log(g$conf.high)
  g$predicted <- log(g$predicted)
  plot(g)
}


p1 <- plot_log(fit5_summer, "z_SST_avg [all]")
p2 <- plot_log(fit5_summer, "z_wind_avg [all]")
p3 <- plot_log(fit5_summer, "z_depth_point_mean [all]")
p4 <- plot_log(fit5_summer, "z_depth_point_sd [all]")
p5 <- plot_log(fit5_summer, "z_faults_km [all]")
p6 <- plot_log(fit5_summer, "z_dist_canyon_km [all]")
p7 <- plot_log(fit5_summer, "z_weighted_dist [all]")
p8 <- plot_log(fit5_summer, "z_weighted_fuel_pricegal [all]")
p9 <- plot_log(fit5_summer, "z_weighted_crab_ppp [all]")
p10 <- plot_log(fit5_summer, "z_bottom_O2_avg [all]")
p11 <- plot_log(fit5_summer, "z_dist_to_closed_km  [all]")
gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,nrow=4)


#Residuals – do qqplots look ok?
  res_fit5_summer <- residuals(fit5_summer)
qqnorm(res_fit5_summer)
qqline(res_fit5_summer)

#The slower and more robust check is:
tic()
  mcmc_res_fit5_summer <- residuals(fit5_summer, type = "mle-mcmc", mcmc_iter = 101, mcmc_warmup = 100)
toc() #41min
qqnorm(mcmc_res_fit5_summer)
qqline(mcmc_res_fit5_summer)


#Spatial predictions – make sense?
pred <- summer
pred$OR_WA_waters <- summer$OR_WA_waters[1]
pred$season <- "2018-2019"
pred$z_SST_avg <- mean(summer$z_SST_avg)
pred$z_wind_avg <- mean(summer$z_wind_avg)
pred$z_depth_point_mean <- mean(summer$z_depth_point_mean)
pred$z_depth_point_sd <- mean(summer$z_depth_point_sd)
pred$z_faults_km <- mean(summer$z_faults_km)
pred$z_dist_canyon_km <- mean(summer$z_dist_canyon_km)
pred$z_weighted_dist <- mean(summer$z_weighted_dist)
pred$z_weighted_fuel_pricegal <- mean(summer$z_weighted_fuel_pricegal)
pred$z_weighted_crab_ppp <- mean(summer$z_weighted_crab_ppp)
pred$z_bottom_O2_avg <- mean(summer$z_bottom_O2_avg)
pred$z_dist_to_closed_km <- mean(summer$z_dist_to_closed_km)

pred <- predict(fit5_summer, pred)
#as(<dgCMatrix>, "dgTMatrix") is deprecated since Matrix 1.5-0; do as(., "TsparseMatrix") instead

p <- ggplot(dplyr::filter(pred,yearf=="2019"), aes(X,Y, col = est)) +
  geom_point(size=1.8,alpha=1.8) +
  scale_color_gradient2()
p


#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------

#WINTER MODELS

#best winter model is fit2b_winter -- no covariates so can't do marginal effect plots
##although now question if maybe better models are fit7_winter of fit8_winter -- maybe fit6_winter...
#if no issues on second run of same model...


mesh <- make_mesh(winter, xy_cols = c("X","Y"), cutoff = 10)
mesh$mesh$n


tic()
fit2b_winter <- sdmTMB(tottraps ~ 0,
                       family = tweedie(),
                       mesh = mesh,
                       spatial = "on",
                       spatiotemporal = "iid",
                       data = winter,
                       time = "yearf")
toc() #1.1min

#sanity(fit2b_winter)
#no warning about convergence issues. No red Xs in sanity check
#sanity(fit2b_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
AIC(fit2b_winter)
#734885





tic()
fit7_winter <- sdmTMB(tottraps ~ 0 + 
                        season +
                        month_name + 
                        OR_WA_waters +
                        #WA_pot_reduction +      #not relevant in winter
                        poly(z_SST_avg,2) +
                        poly(z_wind_avg,2) +
                        poly(z_depth_point_mean,2) +
                        poly(z_depth_point_sd,2) +
                        poly(z_faults_km,2) +
                        poly(z_dist_canyon_km,2) +
                        poly(z_weighted_dist,2) +
                        poly(z_weighted_fuel_pricegal,2) +
                        poly(z_weighted_crab_ppp,2) +
                        poly(z_bottom_O2_avg,2) +
                        poly(z_dist_to_closed_km ,2),
                      family = tweedie(),
                      mesh = mesh,
                      spatial = "on",
                      spatiotemporal = "ar1",
                      data = winter,
                      time = "month_n")
toc() #10.4min

#sanity(fit7_winter)
#No warnings, some red Xs (b_j, thetaf) set.seed(1)
#sanity(fit7_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#still 1x b_j
AIC(fit7_winter)
#734859
#summary(fit7_winter) #what is ar1 rho value?
#Spatiotemporal AR1 correlation (rho): 0.98




tic()
fit6_winter <- sdmTMB(tottraps ~ 0 + 
                        season +
                        month_name + 
                        OR_WA_waters +
                        #WA_pot_reduction +      #not relevant in winter
                        poly(z_SST_avg,2) +
                        poly(z_wind_avg,2) +
                        poly(z_depth_point_mean,2) +
                        poly(z_depth_point_sd,2) +
                        poly(z_faults_km,2) +
                        poly(z_dist_canyon_km,2) +
                        poly(z_weighted_dist,2) +
                        poly(z_weighted_fuel_pricegal,2) +
                        poly(z_weighted_crab_ppp,2) +
                        poly(z_bottom_O2_avg,2) +
                        poly(z_dist_to_closed_km ,2),
                      family = tweedie(),
                      mesh = mesh,
                      spatial = "on",
                      spatiotemporal = "iid",
                      data = winter,
                      time = "month_n")
toc() #7.3min

#sanity(fit6_winter)
#no warnings. Some red Xs: b_js only
#sanity(fit6_winter, big_sd_log10 = 3, gradient_thresh = 0.005)
#No Xs 
AIC(fit6_winter)
#735079






## do estimated relationships make sense? -- only if chosen model has covariates in it

#  This little function is just for helping to display the effects on log scale

plot_log = function(object, term) {
  g <- ggeffect(object, term, back.transform = FALSE)
  g$conf.low <- log(g$conf.low)
  g$conf.high <- log(g$conf.high)
  g$predicted <- log(g$predicted)
  plot(g)
}


p1 <- plot_log(fit6_winter, "z_SST_avg [all]")
p2 <- plot_log(fit6_winter, "z_wind_avg [all]")
p3 <- plot_log(fit6_winter, "z_depth_point_mean [all]")
p4 <- plot_log(fit6_winter, "z_depth_point_sd [all]")
p5 <- plot_log(fit6_winter, "z_faults_km [all]")
p6 <- plot_log(fit6_winter, "z_dist_canyon_km [all]")
p7 <- plot_log(fit6_winter, "z_weighted_dist [all]")
p8 <- plot_log(fit6_winter, "z_weighted_fuel_pricegal [all]")
p9 <- plot_log(fit6_winter, "z_weighted_crab_ppp [all]")
p10 <- plot_log(fit6_winter, "z_bottom_O2_avg [all]")
p11 <- plot_log(fit6_winter, "z_dist_to_closed_km  [all]")
gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,nrow=4)


#Residuals – do qqplots look ok? --- why creates infinte values??
res_fit6_winter <- residuals(fit6_winter)
qqnorm(res_fit6_winter,ylim=c(-5,5))
qqline(res_fit6_winter)
#Error in plot.window(...) : need finite 'ylim' values

res_fit2b_winter <- residuals(fit2b_winter)
qqnorm(res_fit2b_winter)
qqline(res_fit2b_winter)






#The slower and more robust check is:
tic()
mcmc_res_fit7_winter <- residuals(fit7_winter, type = "mle-mcmc", mcmc_iter = 101, mcmc_warmup = 100)
toc() 
qqnorm(mcmc_res_fit7_winter)
qqline(mcmc_res_fit7_winter)


#Spatial predictions – make sense?
pred <- winter
pred$OR_WA_waters <- winter$OR_WA_waters[1]
pred$season <- "2018-2019"
pred$z_SST_avg <- mean(winter$z_SST_avg)
pred$z_wind_avg <- mean(winter$z_wind_avg)
pred$z_depth_point_mean <- mean(winter$z_depth_point_mean)
pred$z_depth_point_sd <- mean(winter$z_depth_point_sd)
pred$z_faults_km <- mean(winter$z_faults_km)
pred$z_dist_canyon_km <- mean(winter$z_dist_canyon_km)
pred$z_weighted_dist <- mean(winter$z_weighted_dist)
pred$z_weighted_fuel_pricegal <- mean(winter$z_weighted_fuel_pricegal)
pred$z_weighted_crab_ppp <- mean(winter$z_weighted_crab_ppp)
pred$z_bottom_O2_avg <- mean(winter$z_bottom_O2_avg)
pred$z_dist_to_closed_km <- mean(winter$z_dist_to_closed_km)

pred <- predict(fit6_winter, pred)
#as(<dgCMatrix>, "dgTMatrix") is deprecated since Matrix 1.5-0; do as(., "TsparseMatrix") instead
p <- ggplot(dplyr::filter(pred,yearf=="2019"), aes(X,Y, col = est)) +
  geom_point(size=1.8,alpha=1.8) +
  scale_color_gradient2()
p

#Spatial predictions – make sense?
pred <- winter
pred$OR_WA_waters <- winter$OR_WA_waters[1]
pred$season <- "2018-2019"

pred <- predict(fit2b_winter, pred)
#as(<dgCMatrix>, "dgTMatrix") is deprecated since Matrix 1.5-0; do as(., "TsparseMatrix") instead
p <- ggplot(dplyr::filter(pred,yearf=="2019"), aes(X,Y, col = est)) +
  geom_point(size=1.8,alpha=1.8) +
  scale_color_gradient2()
p

#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
















