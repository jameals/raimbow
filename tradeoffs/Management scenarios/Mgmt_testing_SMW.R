library(dplyr)
library(lubridate)



x.hump <- readRDS("C:/SMW/RAIMBOW/raimbow-local/Outputs/Humpback_5km_long_monthly.rds") %>%
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Humpback_dens_mean, Humpback_dens_se)

x.blue <- readRDS("C:/SMW/RAIMBOW/raimbow-local/Outputs/BlueWhale_5km_long_monthly.rds") %>%
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Blue_occurrence_mean, Blue_occurrence_se)

x.orig.noinfo <- readRDS("C:/SMW/RAIMBOW/raimbow-local/Data/fishing/CA_DCRB_vms_fishing_daily_2009-2018_fishtix_humpback_blue_whales_grids.RDS") %>%
  select(-year_mo, -contains("risk"), -contains("H_Avg_Abund"), -contains("Blue_"), 
         -Region, -CA_OFFSHOR)

grid.key <- readRDS("C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid5km_key_region.rds") %>% 
  select(-region_ts)

x.orig <- x.orig.noinfo %>% 
  left_join(grid.key, by = "GRID5KM_ID") %>% 
  mutate(Region = ifelse(Region == "OR", "NorCA", Region)) #TODO: discuss these/update effort_mgmt to handle other regions
stopifnot(nrow(grid.key) == nrow(distinct(select(x.orig, GRID5KM_ID, Region, CA_OFFSHOR))))




# ##### TODO - discuss
# x.reg.key.test <- x.orig.noinfo %>% 
#   select(Region, GRID5KM_ID) %>% 
#   distinct()
# x.reg.key.test[which(duplicated(x.reg.key.test$GRID5KM_ID)), ]
# paste(sort(x.reg.key.test[which(duplicated(x.reg.key.test$GRID5KM_ID)), ][["GRID5KM_ID"]]), 
#       collapse = ", ")
# 
# # ^ Shows that several grid cells have multiple 'Region' specifications - this is a temporary fix
# x.orig.noinfo <- x.orig.noinfo %>% 
#   mutate(Region = ifelse(GRID5KM_ID %in% c(63521:63524), "CenCA", Region))
# x.reg.key <- x.orig.noinfo %>% 
#   select(Region, GRID5KM_ID) %>% 
#   distinct()
# 
# 
# # ^ Shows that several grid cells have multiple 'CAOFFSHOR' specifications
# x.off.key.test <- x.orig.noinfo %>% 
#   select(CA_OFFSHOR, GRID5KM_ID) %>% 
#   distinct()
# x.off.key.test[which(duplicated(x.off.key.test$GRID5KM_ID)), ]
# paste(sort(x.off.key.test[which(duplicated(x.off.key.test$GRID5KM_ID)), ][["GRID5KM_ID"]]), 
#       collapse = ", ")
# #####


# x.orig <- x.orig.noinfo %>%
#   left_join(x.blue, by = c("year_month", "GRID5KM_ID")) %>% 
#   left_join(x.hump, by = c("year_month", "GRID5KM_ID"))


x.whale <- full_join(x.blue, x.hump, by = c("GRID5KM_ID", "year_month"))# %>% 
# left_join(x.reg.key)
# rm(x.hump, x.blue)


### Shift/redistributeeffort as specified
source("tradeoffs/Management scenarios/Mgmt_scenarios_shift_effort.R")
d <- effort_mgmt(
  x = x.orig,
  early.data.method = "remove", 
  delay.date = as.Date("2009-12-15"),
  delay.region = "NorCA",
  delay.method = "lag",
  delay.method.fidelity = "spatial",
  closure.date = NULL,
  closure.region = "CenCA",
  closure.method = "temporal",
  closure.redist.percent = 10
)


### Calculate and summarize risk
source("tradeoffs/Management scenarios/Mgmt_scenarios_risk.R")
risk_mgmt(d.noinfo, Num_DCRB_VMS_pings, x.whale)



###############################################################################
source("tradeoffs/Management scenarios/make_scenarios_table.R")
scenario_table[1,]
source("tradeoffs/Management scenarios/Mgmt_scenarios_shift_effort.R")

# for(i in 1:nrow(scenario_table)) {
scenario.output.list <- lapply(1:nrow(scenario_table), function(i, scenario_table) {
  print(i)
  # browser()

  # i=1 # testing. breaks because "At least one of delay.date or closure.date must not be NULL"
  # i=2 # testing. breaks because when switch() is used and does not return NULL, it returns nothing
  scenario.output.df.noinfo <- effort_mgmt(
    x = x.orig.noinfo,
    
    early.data.method = scenario_table$early.data.method[i], 
    
    delay.date = if (scenario_table$delay.date[i] == "NULL") {
      NULL
    } else {
      as.Date(scenario_table$delay.date[i])
    },
    
    delay.region = if (scenario_table$delay.region[i] == "NULL") NULL else scenario_table$delay.region[i],
    
    delay.method.shift = scenario_table$delay.method.shift[i],
    
    delay.method.fidelity = scenario_table$delay.method.fidelity[i],
    
    closure.date = if (scenario_table$closure.date[i] == "NULL") NULL else as.Date(scenario_table$closure.date[i]),
    
    closure.region = if (scenario_table$closure.region[i] == "NULL") NULL else scenario_table$closure.region[i],
    
    closure.method = scenario_table$closure.method[i],
    
    closure.redist.percent = scenario_table$closure.redist.percent[i]
  )
  
}, scenario_table = scenario_table)




