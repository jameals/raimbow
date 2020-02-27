### Extract, process, and save values to CSV for JVR

library(dplyr)
library(tidyr)

path.out <- "../raimbow-local/Outputs/raimbow_timeseries_forJVR/"


###############################################################################
# Extract/spread/save risk time series and baseline values to CSV for JVR

### Prep
load("../raimbow-local/RDATA_files/Whale_risk_timeseries.Rdata")
load("../raimbow-local/RDATA_files/Whale_risk_timeseries_base.Rdata")

long_to_csv <- function(x, x.col, file.out = NULL) {
  x.col <- enquo(x.col)
  tmp <- x %>% 
    select(region, ym, yr, mon, !!x.col) %>% 
    spread(region, !!x.col)
  
  if (!is.null(file.out)) write.csv(tmp, file = file.out, row.names = FALSE)
  tmp
}

### Area
reg.areas <- all.df.summ %>% 
  select(region, area_sum) %>% 
  filter(!duplicated(paste0(region, area_sum)))
write.csv(reg.areas, file = paste0(path.out, "Region_areas.csv"), row.names = FALSE)


### Time series - original values
risk.total <- long_to_csv(all.df.summ, risk_sum_total, file.out = paste0(path.out, "Orig_total_risk.csv"))
mn.abund   <- long_to_csv(all.df.summ, mn_sum_abund, file.out = paste0(path.out, "Orig_total_mn_whales.csv"))
vms.pings  <- long_to_csv(all.df.summ, vms_sum_pings, file.out = paste0(path.out, "Orig_total_vms_pings.csv"))

risk.dens <- long_to_csv(all.df.summ, risk_sum_dens, file.out = paste0(path.out, "Orig_dens_risk.csv"))
mn.dens   <- long_to_csv(all.df.summ, mn_sum_dens, file.out = paste0(path.out, "Orig_dens_mn_whales.csv"))
vms.dens  <- long_to_csv(all.df.summ, vms_sum_dens, file.out = paste0(path.out, "Orig_dens_vms_pings.csv"))

### Time series - baseline values
base.mn.abund    <- long_to_csv(x.summ, mn_sum_abund_base, file.out = paste0(path.out, "Base_total_mn_whales.csv"))
base.vms.pings   <- long_to_csv(x.summ, vms_sum_pings_base, file.out = paste0(path.out, "Base_total_vms_pings.csv"))
basew.risk.total <- long_to_csv(x.summ, risk_sum_total_basew, file.out = paste0(path.out, "Basew_total_risk.csv"))
basef.risk.total <- long_to_csv(x.summ, risk_sum_total_basef, file.out = paste0(path.out, "Basef_total_risk.csv"))

base.mn.dens    <- long_to_csv(x.summ, mn_sum_dens_base, file.out = paste0(path.out, "Base_dens_mn_whales.csv"))
base.vms.dens   <- long_to_csv(x.summ, vms_sum_dens_base, file.out = paste0(path.out, "Base_dens_vms_pings.csv"))
basew.risk.dens <- long_to_csv(x.summ, risk_sum_dens_basew, file.out = paste0(path.out, "Basew_dens_risk.csv"))
basef.risk.dens <- long_to_csv(x.summ, risk_sum_dens_basef, file.out = paste0(path.out, "Basef_dens_risk.csv"))


###############################################################################
# Extract/spread/save risk time series and baseline values to CSV for JVR
load("../raimbow-local/RDATA_files/Whale_risk_management.RDATA")

mng_to_csv <- function(x, file.out = NULL) {
  tmp <- x %>% 
    mutate(yr = x.orig$yr) %>% 
    select(region, yr, risk_total_season, risk_dens_season) %>% 
    pivot_wider(names_from = region, values_from = c(risk_total_season, risk_dens_season))
  
  if (!is.null(file.out)) write.csv(tmp, file = file.out, row.names = FALSE)
  tmp
}

out.x.orig    <- mng_to_csv(x.orig, file.out = paste0(path.out, "Mng_all.csv"))
out.x.close05 <- mng_to_csv(x.close05, file.out = paste0(path.out, "Mng_close05.csv"))
out.x.close04 <- mng_to_csv(x.close04, file.out = paste0(path.out, "Mng_close04.csv"))
out.x.open12  <- mng_to_csv(x.open12, file.out = paste0(path.out, "Mng_open12.csv"))
out.x.open01  <- mng_to_csv(x.open01, file.out = paste0(path.out, "Mng_open01.csv"))

###############################################################################
