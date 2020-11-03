### Additional plots for KAF
# By SMW, Nov 2020

library(gridExtra)
library(lubridate)
library(sf)
library(tidyverse)


source(here::here("User_script_local.R"))

if (user == "JS") {
  
  file.grid.region <- "/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/5x5 Grid/Grid5km_key.rds"
  file.whale.data <- "/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/5x5 Grid/Grid5km_whale.rds"
  
} else if (user == "SMW") {
  flag.save <- FALSE
  
  # file.data.bmpreds <- "C:/SMW/RAIMBOW/raimbow-local/Outputs/BlueWhale_5km_long_monthly.rds"
  # file.data.mnpreds <- "C:/SMW/RAIMBOW/raimbow-local/Outputs/Humpback_5km_long_monthly.rds"
  file.grid.region <- "C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid5km_key.rds"
  file.whale.data <- "C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid5km_whale.rds"
  
} else {
  stop("User not recognized")
}


###################################################################################################

grid.key <- readRDS(file.grid.region)
x.orig <- readRDS(file.whale.data) 

x <- x.orig %>% 
  left_join(grid.key, by = "GRID5KM_ID") %>% 
  mutate(Humpback_dens = Humpback_abund_mean / area_km_lno, 
         Region = factor(Region, levels = c("NorCA", "CenCA", "OR")))

stopifnot(!anyNA(x$Region))


# Quantiles - from https://www.tidyverse.org/blog/2020/03/dplyr-1-0-0-summarise/
# Note: could swap these out to use normalized values instead
quibble2 <- function(x, q = c(0.025, 0.25, 0.50, 0.75, 0.975)) {
  tibble("{{ x }}" := quantile(x, q, na.rm = TRUE), "{{ x }}_q" := q)
}





x.max <- length(unique(x.summ$year_month))
x.lab.idx <- seq(1, to = x.max, by = 3)
x.lab <- sort(unique(x.summ$year_month))[x.lab.idx]
vert.lines <- seq(0.5, to = x.max, by = 12)
# vert.lines2 <- seq(6.5, to = x.max, by = 12)


###################################################################################################

x.month <- x %>%  
  mutate(year = as.numeric(substr(year_month, 1, 4)), 
         month = as.numeric(substr(year_month, 6, 7)), 
         ym_date = as.Date(paste(year, month, "01", sep = "-"))) #%>% 
# filter(between(ym_date, as.Date("2009-11-01"), as.Date("2014-10-15")))
range(x.month$ym_date)


mn.q.ym <- x.month %>% 
  group_by(year, month, Region) %>% 
  summarise(quibble2(Humpback_dens), 
            .groups = "drop") %>% 
  mutate(Humpback_dens = unname(Humpback_dens)) %>% 
  tidyr::pivot_wider(names_from = Humpback_dens_q, names_prefix = "mn_dens_", 
                     values_from = Humpback_dens)


x.summ.month <- x.month %>%   
  group_by(year, month, Region) %>%
  summarise(bm_occ_mean = mean(Blue_occurrence_mean, na.rm = TRUE),
            mn_dens_mean = mean(Humpback_dens, na.rm = TRUE),
            bm_normalized_med = median(normalized_blue, na.rm = TRUE),
            mn_normalized_med = median(normalized_humpback, na.rm = TRUE),
            .groups = "drop") %>%
  # left_join(bm.q.month, by = c("month", "Region")) %>%
  left_join(mn.q.ym, by = c("year", "month", "Region")) %>%
  mutate(season = ifelse(month %in% c(11, 12), paste(year, year + 1, sep = "-"), paste(year - 1, year, sep = "-")), 
         month = factor(month.abb[month], levels = month.abb))
         

table(x.summ.month$year)
season.vec <- paste(2008:2018, 2009:2019, sep = "-")

for (i in unique(x.summ.month$year)) {
  d <- raimbow_ca_ts_facets(
    x.summ.month %>% filter(year == i), month, mn_dens_0.5, "Whales / km2", paste("Humpback whale - median -", i), 
    month.flag = TRUE , y.range = c(0, 0.06)
  )
  ggsave(filename = paste0("../raimbow-local/Plots/Mn_monthly_median/Mn_monthly_median_yearly_", i, ".png"), plot = d, height = 5, width = 7)
}





         
         