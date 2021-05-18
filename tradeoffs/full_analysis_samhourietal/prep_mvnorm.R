# generate multivariate distributions for fishing and whales

# for fishing data (pings, revenue, landings), scenarios shift effort day by day, so we want multivariate distributions for each day and sample from those

# for blwh and hump predictions, we want to generate multivariate distributions for each yr_mth and sample from those

library(tidyverse)
library(lubridate)
library(here)


# fishing
# fdf <- con_df_daily_years_5km_CA
# get data
fdf <- readRDS("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_daily_2009-2019_all_vessels_regions_depths.RDS") 
glimpse(fdf)

# calculate mvnorm dist parameters for each day in each crab year
# means
f_means <- fdf %>%
  split(.$crab_year, .$day_of_year) %>%
  map(colMeans) %>%
  map_dfr
# sigma
f_sigma <- fdf %>%
  split(.$crab_year, .$day_of_year) %>%
  map(cov) %>%
  map_dfr

# make draws from mvnorm based on parameters above
n = 100 # number of draws for each day

# change code below to apply to each crab_year and day_of_year
# draw_one_realization
out <- MASS::mvrnorm( 
  n = n, # number of replicate draws desired for each year month
  mu = f_means, # means for each grid cell each year month
  Sigma = f_sigma # covariance across grid cells for each year month
)

# use purrr package?
# # If each element of the output is a data frame, use
# # map_dfr to row-bind them together:
# mtcars %>%
#   split(.$cyl) %>%
#   map(~ lm(mpg ~ wt, data = .x)) %>%
#   map_dfr(~ as.data.frame(t(as.matrix(coef(.)))))
# #>   (Intercept)        wt
# #> 1    39.57120 -5.647025
# #> 2    28.40884 -2.780106
# #> 3    23.86803 -2.192438
# # (if you also want to preserve the variable names see
# # the broom package)