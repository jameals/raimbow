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

x.orig.noinfo <- readRDS("C:/SMW/RAIMBOW/raimbow-local/Data/fishing/CA_DCRB_vms_fishing_daily_2009-2018_fishtix_humpback_blue_whales_grids.RDS") %>%
  select(-year_mo, -contains("risk"), -contains("H_Avg_Abund"), -contains("Blue_"), 
         -Region, -CA_OFFSHOR)

grid.key <- readRDS("C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid5km_key_region.rds") %>% 
  select(-region_ts)

grid.depth <- readRDS("C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid5km_depth.rds")

x.orig <- x.orig.noinfo %>% 
  left_join(grid.key, by = "GRID5KM_ID") %>% 
  left_join(grid.depth, by = "GRID5KM_ID") #%>% 
  # mutate(Region = ifelse(Region == "OR", "NorCA", Region)) #TODO: discuss these/update effort_mgmt to handle other regions
stopifnot(nrow(grid.key) == nrow(distinct(select(x.orig, GRID5KM_ID, Region, CA_OFFSHOR))))


x.whale <- readRDS("C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid5km_whale.rds")

# d.join <- left_join(x.orig, x.whale)
# length(table(d.join$GRID5KM_ID[is.na(d.join$Blue_occurrence_mean)])) #24, as expected
# length(table(d.join$GRID5KM_ID[is.na(d.join$Humpback_abund_mean)])) #3, as expected






### Shift/redistributeeffort as specified
source("tradeoffs/Management scenarios/Mgmt_scenarios_shift_effort.R")
d <- effort_mgmt(
  x = x.orig,
  early.data.method = "pile", 
  delay.date = NULL,
  delay.region = "All",
  delay.method = "pile",
  delay.method.fidelity = "temporal",
  closure.date = as.Date("2010-04-10"),
  closure.region = c("CenCA"),
  closure.method = "temporal",
  closure.redist.percent = 100, 
  depth.val = -200
)


# Load and prep grid cell - area key
load("C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid_5km_landerased.RDATA")
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




