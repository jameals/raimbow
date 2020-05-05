library(dplyr)
library(lubridate)

rm(list = ls())

# SW
# x.hump <- readRDS("../raimbow-local/Outputs/Humpback_5km_long_monthly.rds") %>%
#   mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
#   select(GRID5KM_ID, year_month, Humpback_dens_mean, Humpback_dens_se)
#JS
x.hump <- readRDS("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Input_Data/Humpback whale data/Forney et al./Humpback_5km_long_monthly.rds") %>%
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Humpback_dens_mean, Humpback_dens_se)

# SW
# x.blue <- readRDS("../raimbow-local/Outputs/BlueWhale_5km_long_monthly.rds") %>%
#   mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
#   select(GRID5KM_ID, year_month, Blue_occurrence_mean, Blue_occurrence_se)

# JS 
x.blue <- readRDS("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Input_Data/Blue whale data/Overlay on 5km Grid/BlueWhale_5km_long_monthly.rds") %>%
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Blue_occurrence_mean, Blue_occurrence_se)

#SW
# x.orig <- readRDS("../raimbow-local/Data/fishing/CA_DCRB_vms_fishing_daily_2009-2018_fishtix_humpback_blue_whales_grids.RDS") %>%
#   select(-year_mo, -contains("risk"), -contains("H_Avg_Abund")) %>%
#   left_join(x.hump, by = c("year_month", "GRID5KM_ID"))
# rm(x.hump)
#JS
# x.orig <- readRDS("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_daily_2009-2018_fishtix_humpback_blue_whales_grids.RDS") %>%
#   select(-year_mo, -contains("risk"), -contains("H_Avg_Abund")) %>%
#   left_join(x.hump, by = c("year_month", "GRID5KM_ID"))

x.orig.noinfo <- readRDS("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_daily_2009-2018_fishtix_humpback_blue_whales_grids.RDS") %>%
  select(-year_mo, -contains("risk"),
         -contains("H_Avg_Abund"), -contains("Blue_")) 

#rm(x.hump)

##### TODO - discuss
x.reg.key.test <- x.orig.noinfo %>% 
  select(Region, GRID5KM_ID) %>% 
  distinct()
x.reg.key.test[which(duplicated(x.reg.key.test$GRID5KM_ID)), ]

# ^ Shows that several grid cells have multiple 'Region' specifications - this is a temporary fix
x.orig.noinfo <- x.orig.noinfo %>% 
  mutate(Region = ifelse(GRID5KM_ID %in% c(63521:63524), "CenCA", Region))
x.reg.key <- x.orig.noinfo %>% 
  select(Region, GRID5KM_ID) %>% 
  distinct()

#####


x.orig <- x.orig.noinfo %>%
  left_join(x.blue, by = c("year_month", "GRID5KM_ID")) %>% 
  left_join(x.hump, by = c("year_month", "GRID5KM_ID"))


x.whale <- full_join(x.blue, x.hump, by = c("GRID5KM_ID", "year_month")) %>% 
  left_join(x.reg.key)
# rm(x.hump, x.blue)


###############################################################################

##### Loop through scenarios of interest and create a list of output df's

source("tradeoffs/Management scenarios/make_scenarios_table.R")
#scenario_table[1,]
source("tradeoffs/Management scenarios/Mgmt_scenarios_shift_effort.R")

scenario.output.list <- lapply(1:nrow(scenario_table), function(i, scenario_table) { # for testing. nrow(scenario_table[1:12,])
  print(i)
  #browser()
  
  # i=1 # testing. no longer breaks because "At least one of delay.date or closure.date must not be NULL"
  # i=2 # testing. no longer breaks because when switch() is used and does not return NULL, it returns nothing
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

#rm()

save.image(paste0("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/scenario_output_dataframes/scenario.output.df.noinfo_",today(),".RData"))
# this line gave the following warning message, not sure why:
#Warning message:
# In file.remove(outfile) :
#   cannot remove file 'scenario_output_dataframes/scenario.output.df.noinfo_2020-05-01.RDataTmp', reason 'No such file or directory'

###############################################################################


### Calculate and summarize risk for multiple scenarios

# does risk_mgmt() function differ from code in prep_data_for_scenario_df_function.R lines 182-209?

source("tradeoffs/Management scenarios/Mgmt_scenarios_risk.R")
risk_mgmt(scenario.output.df.noinfo, Num_DCRB_VMS_pings, x.whale)


###############################################################################


##### test individual scenarios

source("tradeoffs/Management scenarios/Mgmt_scenarios_shift_effort.R")
scenario.output.df.noinfo <- effort_mgmt(
  x = x.orig.noinfo,
  early.data.method = "remove", 
  delay.date = NULL,
  delay.region = NULL,
  delay.method.shift = "lag",
  delay.method.fidelity = "spatial",
  closure.date = as.Date("2010-04-01"),
  closure.region = "CenCA",
  closure.method = "temporal",
  closure.redist.percent = 100
)

scenario.output.df <- effort_mgmt(
  x = x.orig,
  early.data.method = "remove", 
  delay.date = NULL,
  delay.region = NULL,
  delay.method.shift = "lag",
  delay.method.fidelity = "spatial",
  closure.date = as.Date("2010-04-01"),
  closure.region = "BIA",
  closure.method = "temporal",
  closure.redist.percent = 100
)

tail(data.frame(scenario.output.df))[1:100]
head(data.frame(scenario.output.df))

###############################################################################

### Calculate and summarize risk for an individual scenario

source("tradeoffs/Management scenarios/Mgmt_scenarios_risk.R")
risk_out <- risk_mgmt(scenario.output.df.noinfo, Num_DCRB_VMS_pings, x.whale)


### graveyard
##### Loop through scenarios of interest and create a list of output df's
# source("tradeoffs/Management scenarios/make_scenarios_table.R")
# 
# source("tradeoffs/Management scenarios/Mgmt_scenarios_shift_effort.R")
# 
# for(i in 1:nrow(scenario_table)){
#   
#   i=1 # testing. breaks because "At least one of delay.date or closure.date must not be NULL"
#   i=2 # testing. breaks because when switch() is used and does not return NULL, it returns nothing
#   scenario.output.df.noinfo <- effort_mgmt(
#     x = x.orig.noinfo,
#     
#     early.data.method = scenario_table$early.data.method[i], 
#     
#     delay.date = switch(
#       scenario_table$delay.date[i] == "NULL", 
#       NULL,
#       as.Date(scenario_table$delay.date[i])
#       ),
#     
#     delay.region = switch(
#       scenario_table$delay.region[i] == "NULL",
#       NULL,
#       scenario_table$delay.region[i]
#     ),
#     
#     delay.method.shift = scenario_table$delay.method.shift[i],
#     
#     delay.method.fidelity = scenario_table$delay.method.fidelity[i],
#     
#     closure.date = switch(
#       scenario_table$closure.date[i] == "NULL", 
#       NULL,
#       as.Date(scenario_table$closure.date[i]),
#       ),
#     
#     closure.region = switch(
#       scenario_table$closure.region[i] == "NULL",
#       NULL,
#       scenario_table$closure.region[i]
#     ),
#     
#     closure.method = scenario_table$closure.method[i],
#     
#     closure.redist.percent = scenario_table$closure.redist.percent[i]
#   )
#   
# }
# 
# 
