remotes::install_github("fate-spatialindicators/vista")
library(vista)

plot_diag = function(obj) {
  d <- obj$data
  d$pred <- predict(obj)$est
  d$resid <- residuals(obj)
  
  # first -- make basic residual plots with vista
  # https://fate-spatialindicators.github.io/vista/
  plots <- list()
  
  # Look at all predictions over space, faceted by time (or not)
  plots[[1]] <- pred_space(df = dplyr::filter(d, is.finite(resid), is.finite(pred)), 
             time = obj$time)
  # Look at all predictions time
  plots[[2]] <- pred_time(df = dplyr::filter(d, is.finite(resid), is.finite(pred)), 
            time = obj$time)
  # Look at residuals over space, faceted by time (or not)
  plots[[3]] <- resid_space(df = dplyr::filter(d, is.finite(resid), is.finite(pred)), 
              time = obj$time)
  # Look at all residuals over time
  plots[[4]] <- resid_time(df = dplyr::filter(d, is.finite(resid), is.finite(pred)), 
             time = obj$time)
  # Look at variation in residuals through time
  plots[[5]] <- sd_resid_time(df = dplyr::filter(d, is.finite(resid), is.finite(pred)), 
                              time = obj$time)
  # Look at qq plots, faceted by time (or not)
  plots[[6]] <- qq(df = dplyr::filter(d, is.finite(resid), is.finite(pred)), 
     time = obj$time)
  # Look at qq plots over space, faceted by time (or not)
  plots[[7]] <- qq_space(df = dplyr::filter(d, is.finite(resid), is.finite(pred)), 
                         time = obj$time)
  # Look at predictions vs residuals, faceted by time (or not)
  plots[[8]] <- pred_resid(df = dplyr::filter(d, is.finite(resid), is.finite(pred)), 
                           time = obj$time)
  # add marginal plots of covariates vs residuals
  plots[[9]] <- ggplot(d, aes(OR_WA_waters, resid)) + 
    geom_boxplot(alpha=0.1) + ggtitle("OR / WA waters")  + 
    ylab("Residuals")
  plots[[10]] <- ggplot(d, aes(month_name, resid)) + 
    geom_boxplot(alpha=0.1) + 
    ylab("Residuals")
  plots[[11]] <- ggplot(d, aes(yearf, resid)) + 
    geom_boxplot(alpha=0.1) + 
    ylab("Residuals")
  
  plots[[12]] <- ggplot(d, aes(z_SST_avg, resid)) + 
    geom_point(alpha=0.1)  + 
    geom_smooth() + ylab("Residuals")
  
  plots[[13]] <- ggplot(d, aes(z_wind_avg, resid)) + 
    geom_point(alpha=0.1)  + 
    geom_smooth() + ylab("Residuals")
  
  plots[[14]] <- ggplot(d, aes(z_depth_point_mean, resid)) + 
    geom_point(alpha=0.1)  + 
    geom_smooth() + ylab("Residuals")
  
  plots[[15]] <- ggplot(d, aes(z_depth_point_sd, resid)) + 
    geom_point(alpha=0.1)  + 
    geom_smooth() + ylab("Residuals")
  
  plots[[16]] <- ggplot(d, aes(z_faults_km, resid)) + 
    geom_point(alpha=0.1)  + 
    geom_smooth() + ylab("Residuals")
  
  plots[[17]] <- ggplot(d, aes(z_dist_canyon_km, resid)) + 
    geom_point(alpha=0.1)  + 
    geom_smooth() + ylab("Residuals")
  
  plots[[18]] <- ggplot(d, aes(z_weighted_dist, resid)) + 
    geom_point(alpha=0.1)  + 
    geom_smooth() + ylab("Residuals")
  
  plots[[19]] <- ggplot(d, aes(z_weighted_fuel_pricegal, resid)) + 
    geom_point(alpha=0.1)  + 
    geom_smooth() + ylab("Residuals")
  
  plots[[20]] <- ggplot(d, aes(z_weighted_crab_ppp, resid)) + 
    geom_point(alpha=0.1)  + 
    geom_smooth() + ylab("Residuals")
  
  plots[[21]] <- ggplot(d, aes(z_bottom_O2_avg, resid)) + 
    geom_point(alpha=0.1)  + 
    geom_smooth() + ylab("Residuals")
  
  plots[[22]] <- ggplot(d, aes(z_dist_to_closed_km, resid)) + 
    geom_point(alpha=0.1)  + 
    geom_smooth() + ylab("Residuals")
  
  return(plots)
}
