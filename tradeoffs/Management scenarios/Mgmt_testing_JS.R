library(dplyr)
library(lubridate)

rm(list = ls())

###############################################################################

##### Loop through scenarios of interest and create a list of output df's

x.orig.noinfo <- readRDS("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_daily_2009-2018_fishtix_humpback_blue_whales_grids.RDS") %>%
  select(-year_mo, -contains("risk"),
         -contains("H_Avg_Abund"), -contains("Blue_")) 

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


##### shift effort for individual scenarios

source("tradeoffs/Management scenarios/Mgmt_scenarios_shift_effort.R")

# STATUS QUO
scenario.output.df.noinfo.sq <- effort_mgmt(
  x = x.orig.noinfo,
  early.data.method = "remove", 
  delay.date = NULL,
  delay.region = NULL,
  delay.method = NULL,
  delay.method.fidelity = NULL,
  closure.date = NULL,
  closure.region = NULL,
  closure.method = NULL,
  closure.redist.percent = 100
)

# effort and depth restrictions statewide in spring

x.orig <- readRDS("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_daily_2009-2019_all_vessels_regions_depths.RDS") 
glimpse(x.orig)

source("tradeoffs/Management scenarios/Mgmt_scenarios_shift_effort.R")
scenario.output.df <- effort_mgmt(
  x = x.orig,
  
  early.data.method = "remove",
  delay.date = NULL,
  delay.region = NULL,
  delay.method = "lag",
  delay.method.fidelity = "spatial",
  closure.date = as.Date("2010-04-01"),
  closure.region = "All",
  closure.method = "depth",
  closure.redist.percent = 0,
  depth.val = as.numeric(-54.864),
  reduction.before.date = NULL,
  reduction.before.percent = 50,
  reduction.before.region = NULL,
  reduction.after.date = as.Date("2010-04-01"),
  reduction.after.percent = 50,
  reduction.after.region = "All"
  
  )

tail(data.frame(scenario.output.df))
head(data.frame(scenario.output.df))

#####
scenario.output.df.noinfo <- effort_mgmt(
  x = x.orig.noinfo,
  early.data.method = "remove", 
  delay.date = NULL,
  delay.region = NULL,
  delay.method = "lag",
  delay.method.fidelity = "spatial",
  closure.date = as.Date("2010-04-01"),
  closure.region = "BIA",
  closure.method = "temporal",
  closure.redist.percent = 100
)

scenario.output.df <- effort_mgmt(
  x = x.orig,
  early.data.method = "remove", 
  delay.date = NULL,
  delay.region = NULL,
  delay.method = "lag",
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
risk_out_sq <- risk_mgmt(scenario.output.df.noinfo.sq, Num_DCRB_VMS_pings, x.whale)
risk_out_sq


### Calculate and summarize risk for sq scenario with normalized outputs

source("tradeoffs/Management scenarios/Mgmt_scenarios_risk_normalize.R")
risk_out_sq <- risk_mgmt(scenario.output.df.noinfo.sq, Num_DCRB_VMS_pings, x.whale)
glimpse(risk_out_sq)

# subset to march 2015 for trial run using prioritizr
risk_out_sq_mar_2015 <- risk_out_sq %>% 
  filter(year_month == "2015_03")
glimpse(risk_out_sq_mar_2015)

risk_5km_sq <- read_rds(paste0("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/scenario_output_dataframes/status_quo_risk_","2020-05-15",".rds"))
glimpse(risk_5km_sq)

# subset to march 2015 for trial run using prioritizr
risk_5km_sq_mar_2015 <- risk_5km_sq %>% 
  filter(year_month == "2015_03")
glimpse(risk_5km_sq_mar_2015)
write_rds(risk_5km_sq_mar_2015, paste0("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/scenario_output_dataframes/status_quo_risk_mar2015_",today(),".rds"))


###############################################################################
# initial testing

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

##### TODO - discuss. resolved with Grid5km_key.R
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


x.whale <- full_join(x.blue, x.hump, by = c("GRID5KM_ID", "year_month")) #%>% 
#left_join(x.reg.key)
# rm(x.hump, x.blue)

###############################################################################

###############################################################################

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
#     delay.method = scenario_table$delay.method[i],
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
