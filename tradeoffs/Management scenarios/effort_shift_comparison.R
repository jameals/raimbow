library(dplyr)
library(lubridate)

rm(list = ls())

###############################################################################

##### Loop through scenarios of interest and create a list of output df's

x.orig.noinfo <- readRDS("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_daily_2009-2018_fishtix_humpback_blue_whales_grids.RDS") %>%
  select(-year_mo, -contains("risk"),
         -contains("H_Avg_Abund"), -contains("Blue_")) 

source("tradeoffs/Management scenarios/make_scenarios_table_effort_comparison.R")
scenario_table_5[1,]
scenario_table <- scenario_table_5
source("tradeoffs/Management scenarios/Mgmt_scenarios_shift_effort.R")

scenario.output.list <- lapply(1:nrow(scenario_table[1:2,]), function(i, scenario_table) { # for testing. nrow(scenario_table[1:12,])
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
    
    delay.method = scenario_table$delay.method[i],
    
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

# does risk_mgmt() function differ from code in prep_data_for_scenario_df_function.R lines 182-209? yes

load(paste0("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/scenario_output_dataframes/scenario.output.df.noinfo_","2020-05-01",".RData"))

start.time <- Sys.time()

source("tradeoffs/Management scenarios/Mgmt_scenarios_risk.R")

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
# Time difference of 5.490882 mins

save.image(paste0("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/scenario_output_dataframes/scenario.output.df.noinfo_with.risk",today(),".RData"))


###############################################################################

### test function to make tradeoff df's
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
length(which(df_tradeoff$relative_hump_risk < 0)) # 48 out of 432
length(which(df_tradeoff$relative_blwh_risk < 0)) # 83 out of 432

# is $ or pounds actually greater for the fishery under some scenarios? yes
length(which(df_tradeoff$relative_dollars > 100)) # 0 out of 432
length(which(df_tradeoff$relative_pounds > 100)) # 6 out of 432

###############################################################################

