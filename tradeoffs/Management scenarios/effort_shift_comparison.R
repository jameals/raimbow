library(dplyr)
library(lubridate)

rm(list = ls())

###############################################################################

##### Loop through scenarios of interest and create a list of output df's

x.orig.noinfo <- readRDS("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_daily_2009-2018_fishtix_humpback_blue_whales_grids.RDS") %>%
  select(-year_mo, -contains("risk"), -contains("H_Avg_Abund"), -contains("Blue_"), 
         -Region, -CA_OFFSHOR)

grid.key <- readRDS("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/5x5 Grid/Grid5km_key_region.rds") %>% 
  select(-region_ts)

x.orig <- x.orig.noinfo %>% 
  left_join(grid.key, by = "GRID5KM_ID") %>% 
  mutate(Region = ifelse(Region == "OR", "NorCA", Region)) #TODO: discuss these/update effort_mgmt to handle other regions
stopifnot(nrow(grid.key) == nrow(distinct(select(x.orig, GRID5KM_ID, Region, CA_OFFSHOR))))

source("tradeoffs/Management scenarios/make_scenarios_table_effort_comparison.R")
scenario_table_5[1,]
scenario_table <- scenario_table_5
source("tradeoffs/Management scenarios/Mgmt_scenarios_shift_effort.R")

scenario.output.list <- lapply(1:nrow(scenario_table), function(i, scenario_table) { # for testing. nrow(scenario_table[1:2,])
  print(i)
  #browser()
  
  # i=1 # testing. no longer breaks because "At least one of delay.date or closure.date must not be NULL"
  # i=2 # testing. no longer breaks because when switch() is used and does not return NULL, it returns nothing
  
  scenario.output.df <- effort_mgmt(
    x = x.orig,
    
    early.data.method = scenario_table$early.data.method[i], 
    
    delay.date = if (scenario_table$delay.date[i] == "NULL") {
      NULL
    } else {
      as.Date(scenario_table$delay.date[i])
    },
    
    delay.region = if (scenario_table$delay.region[i] == "NULL") NULL else scenario_table$delay.region[i],
    
    delay.method = if (scenario_table$delay.method[i] == "NULL") NULL else scenario_table$delay.method[i],
    
    delay.method.fidelity = scenario_table$delay.method.fidelity[i],
    
    closure.date = if (scenario_table$closure.date[i] == "NULL") NULL else as.Date(scenario_table$closure.date[i]),
    
    closure.region = if (scenario_table$closure.region[i] == "NULL") NULL else scenario_table$closure.region[i],
    
    closure.method = if (scenario_table$closure.method[i] == "NULL") NULL else scenario_table$closure.method[i],
    
    closure.redist.percent = scenario_table$closure.redist.percent[i]
  )
  
}, scenario_table = scenario_table)

#rm()

save.image(paste0("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/scenario_output_dataframes/effort_shift_scenariocomparisons_",today(),".RData"))


###############################################################################


### Calculate and summarize risk for multiple scenarios to evaluate different methods of shifting effort

# grab shifted effort fishing data

# JS 
load(paste0("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/scenario_output_dataframes/effort_shift_scenariocomparisons_","2020-05-18",".RData"))

# grab whale data

# JS 
x.hump <- readRDS("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Input_Data/Humpback whale data/Forney et al./Humpback_5km_long_monthly.rds") %>%
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Humpback_dens_mean, Humpback_dens_se)

x.blue <- readRDS("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Input_Data/Blue whale data/Overlay on 5km Grid/BlueWhale_5km_long_monthly.rds") %>%
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  select(GRID5KM_ID, year_month, Blue_occurrence_mean, Blue_occurrence_se)

x.whale <- full_join(x.blue, x.hump, by = c("GRID5KM_ID", "year_month"))

source("tradeoffs/Management scenarios/Mgmt_scenarios_risk.R")

start.time <- Sys.time()

risk_out_list <- lapply(1:nrow(scenario_table), function(i, scenario_table) { # for testing. nrow(scenario_table[1:3,])
  print(paste("Summarizing risk for Scenario", i))
  #browser()
  
  risk_out <- risk_mgmt(
    scenario.output.list[[i]], 
    Num_DCRB_VMS_pings, 
    x.whale
    )
  }, scenario_table = scenario_table
  )
  
Sys.time() - start.time
# Time difference of 2.249338 mins

save.image(paste0("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/scenario_output_dataframes/effort_shift_scenariocomparisons_withrisk_","2020-05-18",".RData"))

glimpse(risk_out_list[[1]])

###############################################################################

### test function to make tradeoff df's

# assign number IDs to scenarios
scenario_table$number_id <- row.names(scenario_table)

source("tradeoffs/Management scenarios/make_tradeoff_dataframes_function.R")
start.time <- Sys.time()
tradeoff_df_function(
  risk_list = risk_out_list,
  scenario_names_table = scenario_table,
  annual_statewide_df_name = "annual_statewide_df",
  df_tradeoff_name = "df_tradeoff"
  )
Sys.time() - start.time

# is risk actually greater for whales under some scenarios? yes. 
dim(df_tradeoff)
length(which(df_tradeoff$relative_hump_risk < 0)) # 48 out of 432; for effort comparison, 6 out of 171
length(which(df_tradeoff$relative_blwh_risk < 0)) # 83 out of 432; for effort comparison, 8 out of 171

# is $ or pounds actually greater for the fishery under some scenarios? yes
length(which(df_tradeoff$relative_dollars > 100)) # 0 out of 432; for effort comparison, 5 out of 171
length(which(df_tradeoff$relative_pounds > 100)) # 6 out of 432; for effort comparison, 0 out of 171

###############################################################################

