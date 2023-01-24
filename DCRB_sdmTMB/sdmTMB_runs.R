#sdmTMB runs


#here are some of the sort of questions we'd like to be able to answer:

#Which predictors have the biggest effect on crab fishing effort distribution in space? 
#Are static variables better predictors than dynamic variables? Are environmental variables better predictors than economic variables?
  
#How static/variable is the overall fleet footprint. How predictable is footprint from year to year?
  
#What parts of the study area are most variable or consistent between time steps/years?
#Which months are most variable or most consistent?

#-------------------------------------------------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(sdmTMB)
library(sf)
library(ggcorrplot)
library(mgcv)
library(ggeffects)

#this was needed for sdmTMB to work
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)

#-------------------------------------------------------------------------------------------------

#read in df
#note that this is not yet a finished version of df - some predictors might still get added

df_full <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_with_dist_to_closed_areas_not_final_20230123.rds')) %>% 
  ungroup()
glimpse(df_full)


#currently grid centroid location grd_x and grd_y are in lat and lon
#keep them but also create coordinates in UTM 10 zone (grd_x_UTM10 and grd_y_UTM10)
df_full_sf <- st_as_sf(df_full, 
                       coords = c("grd_x", "grd_y"),
                       crs = 4326,
                       remove=F
                      ) %>% 
  # project to UTM zone 10
  st_transform(crs = "+proj=utm +north +zone=10 +ellps=WGS84")


#for some reason this code stopped working...
# df_full_NOgeom <- df_full_sf %>% 
#   dplyr::mutate(grd_x_UTM10 = sf::st_coordinates(.)[,1],
#                 grd_y_UTM10 = sf::st_coordinates(.)[,2]) %>% 
#   st_set_geometry(NULL)

df_full_NOgeom <- df_full_sf %>% add_utm_columns(ll_names = c("grd_x", "grd_y")) %>% 
  st_set_geometry(NULL) %>% 
  rename(grd_x_UTM10 = X, grd_y_UTM10 = Y) 

#-------------------------------------------------------------------------------------------------

#remove grids that were closed (fishery not open, or WA special management area (SMA) not open for state fishers)
#i.e. these are cases of 'NA effort', instead of '0 effort'
#note that no logbook data for WA in 2007-2008 and 2008-2009 so all grids in WA waters are closed
#we also have columns denoting what % of the grid area was open and what % of the half-month time step the grid was open
#we probably won't use them but they're included just in case

df_full_NOgeom <- df_full_NOgeom %>% 
  filter(open_closed == 'open')

#this step reduces the size of the df

#also drop 2007-08 and 208-09

df_full_NOgeom <- df_full_NOgeom %>% 
  filter(!season %in% c("2007-2008","2008-2009"))

#-------------------------------------------------------------------------------------------------

#we'll model winter and summer separately

df_winter <- df_full_NOgeom %>% filter(winter_summer=="Winter")
nrow(df_winter)

df_summer <- df_full_NOgeom %>% filter(winter_summer=="Summer")
nrow(df_summer)

#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------

# check correlation between predictors

df_summer$yearn <- as.numeric(substr(df_summer$season,6,9))
df_summer <- df_summer %>% 
  mutate(monthn = case_when(
    month_name == "May" ~ 5,
    month_name == "June" ~ 6,
    month_name == "July" ~ 7,
    month_name == "August" ~ 8,
    month_name == "September"  ~ 9
  ))

df_summer_predictors_only <- df_summer %>% 
  #select(season, SST_avg, wind_avg, depth_point_mean:month_name, dist_to_closed_km, OR_WA_waters, WA_pot_reduction) 
  #different version of corrplot if year and month are numeric
  select(SST_avg, wind_avg, depth_point_mean:weighted_crab_ppp, dist_to_closed_km:WA_pot_reduction, yearn, monthn)
#note that there are multiple options for depth data: 
#those with naming convention depth_zonal_ vs depth_point_ got sourced in slightly different ways
#my current top choice to use is depth_point_mean


model.matrix(~0+., data=df_summer_predictors_only) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)



df_winter$yearn <- as.numeric(substr(df_winter$season,6,9))
df_winter <- df_winter %>% 
  mutate(monthn = case_when(
    month_name == "December" ~ 0,
    month_name == "January" ~ 1,
    month_name == "February" ~ 2,
    month_name == "March" ~ 3,
    month_name == "April"  ~ 4
  ))

df_winter_predictors_only <- df_winter %>% 
  #select(season, SST_avg, wind_avg, depth_point_mean:month_name, dist_to_closed_km, OR_WA_waters)
  #different version of corrplot if year and month are numeric
  select(SST_avg, wind_avg, depth_point_mean:weighted_crab_ppp, dist_to_closed_km:OR_WA_waters, yearn, monthn)
#note that there are multiple options for depth data: 
#those with naming convention depth_zonal_ vs depth_point_ got sourced in slightly different ways
#my current top choice to use is depth_point_mean


model.matrix(~0+., data=df_winter_predictors_only) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

#not too bad
#the depth variables are somewhat correlated, but we'd probably just choose to use one of them, lets say depth_mean
#distance to canyon features is correlated with distance to escarpments (not too surprising)
#similarly depth is somewhat correlated with distance to canyon/escarpment as those features are in deeper waters (off the shelf)


#-------------------------------------------------------------------------------------------------

# z-score continuous predictors
#that should be: SST_avg, wind_avg, our chosen depth variable (or all if we don't yet know which one to use)
#and everything between faults_km:weighted_crab_ppp



#-------------------------------------------------------------------------------------------------

#model runs
# tottraps = response variable
# present = denotes presence/absence
# extra columns that are in the df just in case but won't be included in a model: winter_summer:optional SMA name





#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------

#using Eric's code from "examples.pdf"
#test on summer data

d <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_with_dist_to_closed_areas_not_final_20230120.rds'))

# Filter out NAs
d <- dplyr::filter(d,
                   !is.na(weighted_fuel_pricegal),
                   !is.na(weighted_crab_ppp),
                   open_closed == "open",
                   season %in% c("2007-2008","2008-2009") == FALSE)
d$yearn <- as.numeric(substr(d$season,1,4))
d$yearf <- as.factor(d$yearn)
summer <- dplyr::filter(d, winter_summer == "Summer")

# try smooth over months
summer$month_n <- 5
summer$month_n[which(summer$month_name=="June")] = 6
summer$month_n[which(summer$month_name=="July")] = 7
summer$month_n[which(summer$month_name=="August")] = 8
summer$month_n[which(summer$month_name=="September")] = 9
# Add UTM columns (zone 10)
summer = add_utm_columns(summer, ll_names = c("grd_x", "grd_y"))



##Initial model: covariates only
mesh <- make_mesh(summer, xy_cols = c("X","Y"), cutoff = 10)
mesh$mesh$n
fit0 <- sdmTMB(tottraps ~ 0 + month_name + OR_WA_waters +
                 season +
                 SST_avg +
                 wind_avg +
                 poly(depth_zonal_mean,2) +
                 poly(depth_zonal_sd,2) +
                 poly(faults_km,2) +
                 poly(dist_canyon_km,2) +
                 poly(weighted_dist,2) +
                 poly(weighted_fuel_pricegal,2) +
                 poly(weighted_crab_ppp,2) +
                 poly(dist_to_closed_km,2),
               family = tweedie(),
               mesh = mesh,
               spatial = "off",
               spatiotemporal = "off",
               data = summer,
               time = "yearf")
#Warning message:
#The model may not have converged. Maximum final gradient: 0.017510364781316. 
#sanity(fit0)
AIC(fit0)
#300625.8



##Adding spatial and spatiotemporal fields (seasons)
fit1 <- sdmTMB(tottraps ~ 0 + month_name + OR_WA_waters +
                 season +
                 SST_avg +
                 wind_avg +
                 poly(depth_zonal_mean,2) +
                 poly(depth_zonal_sd,2) +
                 poly(faults_km,2) +
                 poly(dist_canyon_km,2) +
                 poly(weighted_dist,2) +
                 poly(weighted_fuel_pricegal,2) +
                 poly(weighted_crab_ppp,2) +
                 poly(dist_to_closed_km,2),
               family = tweedie(),
               mesh = mesh,
               spatial = "on",
               spatiotemporal = "off",
               data = summer,
               time = "yearf")
#sanity(fit1)

fit2 <- sdmTMB(tottraps ~ 0 + month_name + OR_WA_waters +
                 season +
                 SST_avg +
                 wind_avg +
                 poly(depth_zonal_mean,2) +
                 poly(depth_zonal_sd,2) +
                 poly(faults_km,2) +
                 poly(dist_canyon_km,2) +
                 poly(weighted_dist,2) +
                 poly(weighted_fuel_pricegal,2) +
                 poly(weighted_crab_ppp,2) +
                 poly(dist_to_closed_km,2),
               family = tweedie(),
               mesh = mesh,
               spatial = "on",
               spatiotemporal = "iid",
               data = summer,
               time = "yearf")
#sanity(fit2)

#no complaints about models not converging

AIC(fit1, fit2)
#     df    AIC
#fit1 38 278815.1
#fit2 39 272014.4


##Just as a test, we can see if changing month to a smooth improves the fit
fit3 <- sdmTMB(tottraps ~ 0 + s(month_n, k = 3) + # <- new
                 OR_WA_waters +
                 season +
                 SST_avg +
                 wind_avg +
                 poly(depth_zonal_mean,2) +
                 poly(depth_zonal_sd,2) +
                 poly(faults_km,2) +
                 poly(dist_canyon_km,2) +
                 poly(weighted_dist,2) +
                 poly(weighted_fuel_pricegal,2) +
                 poly(weighted_crab_ppp,2) +
                 poly(dist_to_closed_km,2),
               family = tweedie(),
               mesh = mesh,
               spatial = "on",
               spatiotemporal = "iid",
               data = summer,
               time = "yearf")
#sanity(fit3)
#no complaints about convergence issues
AIC(fit3)
#272020.7



## Just as a test, we can see if changing year/season to a smooth improves the fit
fit4 <- sdmTMB(tottraps ~ 0 + month_name + OR_WA_waters +
                 s(yearn) + # <- new
                 SST_avg +
                 wind_avg +
                 poly(depth_zonal_mean,2) +
                 poly(depth_zonal_sd,2) +
                 poly(faults_km,2) +
                 poly(dist_canyon_km,2) +
                 poly(weighted_dist,2) +
                 poly(weighted_fuel_pricegal,2) +
                 poly(weighted_crab_ppp,2) +
                 poly(dist_to_closed_km,2),
               family = tweedie(),
               mesh = mesh,
               spatial = "on",
               spatiotemporal = "iid",
               data = summer,
               time = "yearf")
#sanity(fit4)
#Warning message:The model may not have converged: extreme or very small eigen values detected. 
AIC(fit4)



## Similarly, we can change the IID spatiotemporal fields in model 2 to “AR1” to test the autoregressive structure
# Is there support for AR1 spatiotemporal fields?
# Using the best model (fit2)
fit5 <- sdmTMB(tottraps ~ 0 + month_name + OR_WA_waters +
                 season +
                 SST_avg +
                 wind_avg +
                 poly(depth_zonal_mean,2) +
                 poly(depth_zonal_sd,2) +
                 poly(faults_km,2) +
                 poly(dist_canyon_km,2) +
                 poly(weighted_dist,2) +
                 poly(weighted_fuel_pricegal,2) +
                 poly(weighted_crab_ppp,2) +
                 poly(dist_to_closed_km,2),
               family = tweedie(),
               mesh = mesh,
               spatial = "on",
               spatiotemporal = "ar1",# <- new
               data = summer,
               time = "yearf")
#sanity(fit5)
#takes longer to run than previous iterations
#sanity() seemed fine, but:
#Warning message: In stats::nlminb(start = tmb_obj$par, objective = tmb_obj$fn, gradient = tmb_obj$gr,  :  NA/NaN function evaluation
AIC(fit2, fit5)
#     df    AIC
#fit2 39 272014.4
#fit5 40 271946.5




##Adding spatial and spatiotemporal fields (months)
#Here we switch indexing of spatiotemporal fields to “month_name” and again can try the
#spatiotemporal fields as IID or AR1. Model 6 seems to generally converge – but model 7
#struggles -- similar to winter model

# Other questions are whether it makes more sense to switch the spatiotemporal fields to month
fit6 <- update(fit2,
               time = "month_n")
#sanity(fit6)
fit7 <- update(fit2,
               time = "month_n",
               spatiotemporal = "ar1")
#sanity(fit7)
#Warning messages:
# 1: In sqrt(diag(cov)) : NaNs produced
#2: The model may not have converged: non-positive-definite Hessian matrix. 

AIC(fit6, fit7) #no improvements
#     df   AIC
#fit6 39 276930.6
#fit7 40 277902.9




#skip models 8 and 9 - fancier smooths, had convergence issues in winter models


#Comparing delta- models to the Tweedie distribution
#Maybe should have done this first, but one thing that’s worth doing is also swapping in a delta-
# Gamma hurdle or delta-model for the Tweedie, which should increase flexibility (particularly for lots of 0s).
#This model is a lot slower than the straight Tweedie above…
# tic()
# fit10 <- update(fit5,
# family = delta_gamma())
# toc()

tic()
fit10 <- sdmTMB(tottraps ~ 0 + month_name + OR_WA_waters +
                  yearf +
                  SST_avg +
                  wind_avg +
                  poly(depth_zonal_mean,2) +
                  poly(depth_zonal_sd,2) +
                  poly(faults_km,2) +
                  poly(dist_canyon_km,2) +
                  poly(weighted_dist,2) +
                  poly(weighted_fuel_pricegal,2) +
                  poly(weighted_crab_ppp,2) +
                  poly(dist_to_closed_km,2),
                family = delta_gamma(),
                mesh = mesh,
                spatial = "on",
                spatiotemporal = "ar1",
                data = summer,
                time = "yearf")
toc()
#Eric winter model: The delta-model is quite a bit better than the Tweedie – but takes longer to run (80 minutes on my slow computer)
#summer model 83min on Leena's computer
AIC(fit5, fit10)
#      df    AIC
#fit5  40 271946.5
#fit10 77 262781.3




#Gut check: do estimated relationships make sense?
#  This little function is just for helping to display the effects on log scale
plot_log = function(object, term) {
  g <- ggeffect(object, term, back.transform = FALSE)
  g$conf.low <- log(g$conf.low)
  g$conf.high <- log(g$conf.high)
  g$predicted <- log(g$predicted)
  plot(g)
}

p1 <- plot_log(fit5, "depth_zonal_mean [all]")
p2 <- plot_log(fit5, "depth_zonal_sd [all]")
p3 <- plot_log(fit5, "faults_km [all]")
p4 <- plot_log(fit5, "dist_canyon_km [all]")
gridExtra::grid.arrange(p1,p2,p3,p4,nrow=2)

p1 <- plot_log(fit5, "weighted_dist [all]")
p2 <- plot_log(fit5, "weighted_fuel_pricegal [all]")
p3 <- plot_log(fit5, "weighted_crab_ppp [all]")
p4 <- plot_log(fit5, "dist_to_closed_km [all]")
gridExtra::grid.arrange(p1,p2,p3,p4,nrow=2)

p1 <- plot_log(fit5, "SST_avg [all]")
p2 <- plot_log(fit5, "wind_avg [all]")
gridExtra::grid.arrange(p1,p2,nrow=1)


#Residuals – do qqplots look ok?
res <- residuals(fit5)
qqnorm(res)
qqline(res)


#The slower and more robust check is:
mcmc_res <- residuals(fit5, type = "mle-mcmc", mcmc_iter = 101, mcmc_warmup = 100)
qqnorm(mcmc_res)
qqline(mcmc_res)




#Spatial predictions – make sense?
pred <- summer
pred$OR_WA_waters <- summer$OR_WA_waters[1]
pred$season <- "2018-2019"
pred$SST_avg <- mean(summer$SST_avg)
pred$wind_avg <- mean(summer$wind_avg)
pred$depth_zonal_mean <- mean(summer$depth_zonal_mean)
pred$depth_zonal_sd <- mean(summer$depth_zonal_sd)
pred$faults_km <- mean(summer$faults_km)
pred$dist_canyon_km <- mean(summer$dist_canyon_km)
pred$weighted_dist <- mean(summer$weighted_dist)
pred$weighted_fuel_pricegal <- mean(summer$weighted_fuel_pricegal)
pred$weighted_crab_ppp <- mean(summer$weighted_crab_ppp)
pred$dist_to_closed_km <- mean(summer$dist_to_closed_km)
pred <- predict(fit5, pred)
#as(<dgCMatrix>, "dgTMatrix") is deprecated since Matrix 1.5-0; do as(., "TsparseMatrix") instead
p <- ggplot(dplyr::filter(pred,yearf=="2019"), aes(X,Y, col = est)) +
  geom_point(size=1.5,alpha=0.3) +
  scale_color_gradient2()
p








































#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
#this was just Leena testing out sdmTMB for the first time ever

mesh <- make_mesh(df_summer, c("grd_x_UTM10", "grd_y_UTM10"), cutoff = 10)
#> as(<dgCMatrix>, "dgTMatrix") is deprecated since Matrix 1.5-0; do as(., "TsparseMatrix") instead
plot(mesh)

#-----------------------
#presence/absence model

m <- sdmTMB(
  data = df_summer,
  formula = present ~ SST_avg + wind_avg,
  mesh = mesh, # can be omitted for a non-spatial model
  family = binomial(link = "logit"),
  spatial = "off"
)
# Warning messages:
#   1: In checkMatrixPackageVersion() :
#   Package version inconsistency detected.
# TMB was built with Matrix version 1.5.3
# Current Matrix version is 1.5.1
# Please re-install 'TMB' from source using install.packages('TMB', type = 'source') or ask CRAN for a binary version of 'TMB' matching CRAN's 'Matrix' package
# 2: The model may not have converged. Maximum final gradient: 0.0184931985802471. 

#after updating Matrix: 
#Warning message:
#The model may not have converged. Maximum final gradient: 0.0184933781139155. 

m
AIC(m)

#with spatial fields on took 2-3min to run
m1 <- sdmTMB(
  data = df_summer,
  formula = present ~ SST_avg + wind_avg,
  mesh = mesh,
  family = binomial(link = "logit"),
  spatial = "on"
)
m1
AIC(m1) #much better AIC with spatial fields on

#-----------------------------
#presence only model

m3 <- sdmTMB(
  data = df_summer,
  formula = tottraps ~ poly(SST_avg, 2), #Error in poly(log(SST_avg), 2) : missing values are not allowed in 'poly'
  mesh = mesh,
  family = tweedie(link = "log"),
  spatial = "on",
  time = "season",
  spatiotemporal = "IID"
)
m3

##lost track on how long this takes to run, definitely 1-2 h

# Warning message:
#   The model may not have converged. Maximum final gradient: 0.0261937814485123. 
# > m3
# Spatiotemporal model fit by ML ['sdmTMB']
# Formula: tottraps ~ poly(SST_avg, 2)
# Mesh: mesh
# Time column: season
# Data: df_summer
# Family: tweedie(link = 'log')
# 
# coef.est coef.se
# (Intercept)         -11.38    1.82
# poly(SST_avg, 2)1  -195.47    3.26
# poly(SST_avg, 2)2    38.80    3.11
# 
# Dispersion parameter: 32.81
# Tweedie p: 1.39
# Matern range: 61458.11
# Spatial SD: 6.85
# Spatiotemporal SD: 3.25
# ML criterion at convergence: 149400.360
# 
# See ?tidy.sdmTMB to extract these values as a data frame.

#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------


