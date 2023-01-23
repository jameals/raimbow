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


df_summer_predictors_only <- df_summer %>% 
  select(season, SST_avg, wind_avg, depth_point_mean:month_name, dist_to_closed_km, OR_WA_waters, WA_pot_reduction) 
#note that there are multiple options for depth data: 
#those with naming convention depth_zonal_ vs depth_point_ got sourced in slightly different ways
#my current top choice to use is depth_point_mean


model.matrix(~0+., data=df_summer_predictors_only) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)


df_winter_predictors_only <- df_winter %>% 
  select(season, SST_avg, wind_avg, depth_point_mean:month_name, dist_to_closed_km, OR_WA_waters)
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


