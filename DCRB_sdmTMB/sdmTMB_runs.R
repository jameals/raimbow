#sdmTMB runs

#-------------------------------------------------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(sdmTMB)
library(sf)

#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)

#-------------------------------------------------------------------------------------------------

#read in df
#note that this may not yet be a finished version of df - closed areas not done, some predictors might be added

df_full <- read_rds(here::here('DCRB_sdmTMB', 'data','df_full_not_final.rds'))


#does X and Y need to be in a projected CRS? this is the lambert Azimuth projection
#grd <- read_sf(here::here('wdfw','data','fivekm_grid_polys_shore_lamb.shp'))
#st_crs(grd)



# df_full <- df_full %>% 
#   st_as_sf(coords=c("grd_x","grd_y"),crs=st_crs(grd),remove=F) %>% 
#   # project to UTM zone 10
#   st_transform(crs = "+proj=utm +north +zone=10 +ellps=WGS84")


df_full_sf <- st_as_sf(df_full, 
                       coords = c("grd_x", "grd_y"),
                       crs = 4326,
                       remove=F
                      ) %>% 
  # project to UTM zone 10
  st_transform(crs = "+proj=utm +north +zone=10 +ellps=WGS84")


df_full_NOgeom <- df_full_sf %>% 
  dplyr::mutate(grd_x_UTM10 = sf::st_coordinates(.)[,1],
                grd_y_UTM10 = sf::st_coordinates(.)[,2]) %>% 
  st_set_geometry(NULL)



#remove closed areas grids if they are still in df




#-------------------------------------------------------------------------------------------------

#we'll model winter and summer separately

df_winter <- df_full_NOgeom %>% filter(winter_summer=="Winter")

df_summer <- df_full_NOgeom %>% filter(winter_summer=="Summer")


#-------------------------------------------------------------------------------------------------
#don't think the mesh command works on grd_x and grd_y - use the UTM version - plot looks much better

mesh <- make_mesh(df_full_NOgeom, c("grd_x_UTM10", "grd_y_UTM10"), cutoff = 10)
#> as(<dgCMatrix>, "dgTMatrix") is deprecated since Matrix 1.5-0; do as(., "TsparseMatrix") instead
plot(mesh)
#Messge: The x or y column values are fairly large. This can cause estimation problems since
#the spatial range is dependent on the scale of the coordinates. Consider scaling the x
#and y coordinates. For example, try working in UTM km instead of UTM m by divided by 1000.


mesh <- make_mesh(df_summer, c("grd_x_UTM10", "grd_y_UTM10"), cutoff = 10)
#> as(<dgCMatrix>, "dgTMatrix") is deprecated since Matrix 1.5-0; do as(., "TsparseMatrix") instead
plot(mesh)

#-------------------------------------------------------------------------------------------------

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



#-------------------------------------------------------------------------------------------------

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




