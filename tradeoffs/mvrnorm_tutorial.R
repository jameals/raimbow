# figuring out multivariate normal distributions

#https://stackoverflow.com/questions/37294131/how-do-i-estimate-the-parameters-of-a-bivariate-normal-distribution-in-r-from-re
library(MASS)
n <- 10000
set.seed(123) #for reproducible results
dat <- MASS::mvrnorm(n=n, 
                     mu=c(5, 10), 
                     Sigma= matrix(c(1,0.5,0.5,2), byrow=T, ncol=2)
)
head(dat)

# my df will be: columns equivalent to grid cells, rows equivalent to time (days), for each yr_mth
# calculate number of grid cells
# my_cells <- ncol(dat)
# get means for each grid cell
my_means <- colMeans(dat)
# get variance for each grid cell
my_var <- apply(dat, 2, var)
# calculate sample correlation
my_sigma <- cov(dat)

# then to sample from the multivariate normal defined by these parameters
colMeans(
  MASS::mvrnorm( 
  n = n, # number of replicate draws desired for each year month
  mu = my_means, # means for each grid cell each year month
  Sigma = my_sigma # covariance across grid cells for each year month
  )
)

# once i can do the above with pings and whale model outputs, i will want to follow the steps in Slack chat with owen from 07/06/20

