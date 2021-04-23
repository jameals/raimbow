# Code to make plots for ODFW,e tc.

library(gridExtra)
library(lubridate)
library(tidyverse)

source(here::here("humpback_risk/funcs_whale_risk_timeseries.R"))
file.timeseries <- "C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Whale_risk_timeseries.Rdata"
path.plots <- "C:/SMW/RAIMBOW/raimbow-local/Plots/Whale_risk_timeseries_OR/"

load(file.timeseries)

raimbow_se <- function(x) {
  #x: numeric vector of values that were used to calculate mean
  x.mean <- mean(x, na.rm = TRUE)
  if (sum(!is.na(x)) == 0) {
    NA
  } else {
    sqrt(sum((x - x.mean)^2, na.rm = TRUE) / length(x))
  }
}


#------------------------------------------------------------------------------
### Orig plot
p1 <- raimbow_ggplot(
  all.df.summ, risk_sum_dens, plot.main = "Risk", 
  y.lab = "Whales * VMS pings / km2", wa.flag = FALSE
)
p2 <- raimbow_ggplot(
  all.df.summ, mn_sum_dens, plot.main = "Humpback whales", 
  y.lab = "Whales / km2", wa.flag = TRUE
)
p3 <- raimbow_ggplot(
  all.df.summ, vms_sum_dens, plot.main = "Fishing (non-confidential OR)", 
  y.lab = "VMS pings / km2", wa.flag = FALSE
)

p123 <- grid.arrange(p1, p2, p3, nrow = 3)
ggsave(filename = "Humpback_risk_dens_all.png", p123, path = path.plots, 
       height = 10, width = 10)


### Orig plot - filterd for only OR
or.df.summ <- all.df.summ %>% filter(region %in% "OR")
o1 <- raimbow_ggplot(
  or.df.summ, risk_sum_dens, plot.main = "Risk", 
  y.lab = "Whales * VMS pings / km2", wa.flag = FALSE
)
o2 <- raimbow_ggplot(
  or.df.summ, mn_sum_dens, plot.main = "Humpback whales", 
  y.lab = "Whales / km2", wa.flag = TRUE
)
o3 <- raimbow_ggplot(
  or.df.summ, vms_sum_dens, plot.main = "Fishing (non-confidential OR)", 
  y.lab = "VMS pings / km2", wa.flag = FALSE
)

o123 <- grid.arrange(o1, o2, o3, nrow = 3)
ggsave(filename = "Humpback_risk_dens_OR.png", o123, path = path.plots, 
       height = 10, width = 10)


#------------------------------------------------------------------------------
# Average risk by month for all regions, with interannual variability
risk.month.all <- all.df.summ %>% 
  group_by(region, mon) %>% 
  summarise(risk_mean_dens = mean(risk_sum_dens), 
            var_interannual = raimbow_se(risk_sum_dens), 
            .groups = "drop")

r1 <- risk.month.all %>% 
  filter(region != "WA") %>% 
  mutate(mon = factor(mon, levels = 1:12, labels = month(1:12, label = TRUE))) %>% 
  ggplot(aes(mon, risk_mean_dens)) + #, colour = region, group = region
  geom_point() + 
  geom_errorbar(aes(ymin = risk_mean_dens - var_interannual,
                    ymax = risk_mean_dens + var_interannual)) + 
  facet_wrap(vars(region), nrow = 3) + 
  xlab(NULL)+ 
  ylab("Risk (whales * pings / km2") + 
  ggtitle("Average monthly humpback risk of entanglement, by region")
ggsave(filename = "Monthly_risk_dens.png", r1, path = path.plots, 
       height = 7, width = 7)



#------------------------------------------------------------------------------
# Average risk for OR for pre/post, with interannual variability
risk.month <- all.df.summ %>% 
  mutate(period = ifelse(yr >= 2015 | (yr == 2014 & mon %in% c(11, 12)), 
                         "late (Dec 2014 and after)", "early (Nov 2014 and before)")) %>% 
  group_by(region, period, mon) %>% 
  summarise(risk_mean_dens = mean(risk_sum_dens), 
            var_interannual = raimbow_se(risk_sum_dens), 
            .groups = "drop")

r2 <- risk.month %>% 
  mutate(mon = factor(mon, levels = 1:12, labels = month(1:12, label = TRUE))) %>% 
  filter(region == "OR") %>% 
  ggplot(aes(mon, risk_mean_dens)) + #, colour = region, group = region
  geom_point() + 
  geom_errorbar(aes(ymin = risk_mean_dens - var_interannual,
                    ymax = risk_mean_dens + var_interannual)) + 
  facet_grid(cols = vars(period)) + 
  xlab(NULL)+ 
  ylab("Risk (whales * pings / km2") + 
  ggtitle("Average monthly humpback risk of entanglement for Oregon, by time period")
ggsave(filename = "Monthly_risk_dens_prepost_OR.png", r2, path = path.plots, 
       height = 5, width = 7)


r3 <- risk.month %>% 
  mutate(mon = factor(mon, levels = 1:12, labels = month(1:12, label = TRUE))) %>% 
  filter(region != "WA") %>%
  ggplot(aes(mon, risk_mean_dens)) + #, colour = region, group = region
  geom_point() + 
  geom_errorbar(aes(ymin = risk_mean_dens - var_interannual,
                    ymax = risk_mean_dens + var_interannual)) + 
  facet_grid(cols = vars(period), rows = vars(region)) + 
  xlab(NULL)+ 
  ylab("Risk (whales * pings / km2") + 
  ggtitle("Average monthly humpback risk of entanglement, by region and time period")
ggsave(filename = "Monthly_risk_dens_prepost.png", r3, path = path.plots, 
       height = 7, width = 7)


#------------------------------------------------------------------------------

