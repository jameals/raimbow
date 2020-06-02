library(dplyr)
library(lubridate)
library(sf)


# x.hump <- readRDS("C:/SMW/RAIMBOW/raimbow-local/Outputs/Humpback_5km_long_monthly.rds") %>%
#   mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
#   select(GRID5KM_ID, year_month, Humpback_dens_mean, Humpback_dens_se)
# 
# x.blue <- readRDS("C:/SMW/RAIMBOW/raimbow-local/Outputs/BlueWhale_5km_long_monthly.rds") %>%
#   mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
#   select(GRID5KM_ID, year_month, Blue_occurrence_mean, Blue_occurrence_se)

x.orig.noinfo <- readRDS("C:/SMW/RAIMBOW/raimbow-local/Data/fishDataCA/CA_DCRB_vms_fishing_daily_2009-2019_all_vessels.RDS") %>%
  select(-contains("risk"), -contains("H_Avg_Abund"), -contains("Blue_"), 
         -Region, -CA_OFFSHOR)

grid.key <- readRDS("C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid5km_key.rds") %>% 
  select(-region_ts)

x.orig <- x.orig.noinfo %>% 
  left_join(grid.key, by = "GRID5KM_ID")
stopifnot(nrow(grid.key) == nrow(distinct(select(x.orig, GRID5KM_ID, Region, CA_OFFSHOR))))


x.whale <- readRDS("C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid5km_whale.rds") %>% 
  select(-area_km_lno) #area.key object takes care of this in risk_mgmt()

# d.join <- left_join(x.orig, x.whale)
# length(table(d.join$GRID5KM_ID[is.na(d.join$Blue_occurrence_mean)])) #24, as expected
# length(table(d.join$GRID5KM_ID[is.na(d.join$Humpback_abund_mean)])) #3, as expected




### Shift/redistribute effort as specified
source("tradeoffs/Management scenarios/Mgmt_scenarios_shift_effort.R")
d <- effort_mgmt(
  x = x.orig,
  early.data.method = "pile", 
  delay.date = as.Date("2009-12-15"),
  delay.region = "All",
  delay.method = "depth",
  delay.method.fidelity = "temporal",
  closure.date = as.Date("2010-04-01"),
  closure.region = c("All"),
  closure.method = "depth",
  # closure.redist.percent = 100,
  depth.min = -100,
  depth.max = -500,
  reduction.before.date = as.Date("2009-12-15"), 
  reduction.before.percent = 50, 
  reduction.before.region = "All", 
  reduction.after.date = as.Date("2010-04-01"), 
  reduction.after.percent = 50, 
  reduction.after.region = "All"
)

# source("C:/SMW/RAIMBOW/raimbow/tradeoffs/Management scenarios/Mgmt_scenarios_plot.R")
# sum(x.orig$DCRB_lbs)
# sum(d2$DCRB_lbs)
# sum(d50$DCRB_lbs)
# sum(d100$DCRB_lbs)
# 
# sum(x.orig$DCRB_lbs) - (sum(x.orig$DCRB_lbs) - sum(d50$DCRB_lbs)) * 2
# 
# effort_plot_effort(x.orig, DCRB_lbs)
# effort_plot_effort(d, DCRB_lbs)
# effort_plot_effort(d2, DCRB_lbs)


# Load and prep grid cell - area key
grid.5km.lno <- readRDS("C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid_5km_landerased.rds")
area.key <- grid.5km.lno %>% 
  st_drop_geometry() %>% 
  select(GRID5KM_ID, area_km_lno) %>% 
  distinct()


### Calculate and summarize risk
source("tradeoffs/Management scenarios/Mgmt_scenarios_risk.R")
d.risk <- risk_mgmt(
  x = d, x.col = Num_DCRB_VMS_pings, y = x.whale, # %>% select(-area_km_lno), #Don't have to remove area column
  risk.unit = "dens", area.key = area.key
)
d.risk.summ <- risk_mgmt_summ(d.risk, summary.level = "Region")



###############################################################################
source("tradeoffs/Management scenarios/make_scenarios_table.R")
scenario_table[1, ]
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




