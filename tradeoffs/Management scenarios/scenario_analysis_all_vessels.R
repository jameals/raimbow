library(tidyverse)
library(lubridate)

rm(list = ls())

### STEPS ###

### 1) Loop through scenarios of interest and create a list of output df's
### 2) Calculate and summarize risk for multiple scenarios, including status quo
### 3) make annual and tradeoff df's. 

###############################################################################

##### 1) Loop through scenarios of interest and create a list of output df's

x.orig <- readRDS("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_daily_2009-2019_all_vessels_regions_depths.RDS") 
glimpse(x.orig)

source("tradeoffs/Management scenarios/make_scenarios_table.R")
scenario_table[1,]
scenario_table_edr[1,]

# scenario_table_all <- scenario_table %>%
#   bind_rows(scenario_table_edr)


# read in season start date key
# grab helper function. https://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string
simpleCap <- function(x) {
  s <- sapply(strsplit(x, " "), function(i) i[[1]])
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}
season.st.date.key <- readRDS("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Input_Data/season_start_dates/start_dates_by_CA_region.rds") %>% 
  mutate(crab_year = gsub("-", "_", .data$crab_season), 
         Region = unname(sapply(CA_region, simpleCap))) %>% 
  select(crab_year, Region, start_of_season_oneperc)

source("tradeoffs/Management scenarios/Mgmt_scenarios_shift_effort.R")

# for some strange reason i did not think to create scenario_table_all before writing the 2 sets of code below for the effort_mgmt() function. ah, well

### 060520: THERE IS STILL A PROBLEM WITH THE effort_mgmt() function. IT CANNOT READ THE scenario_table ENTRIES AND RUN PROPERLY. I THINK IT HAS TO DO WITH season.st.date.key

### just the early and late season closure scenarios
scenario.output.list.closures <- lapply(1:nrow(scenario_table[1:2,]), function(i, scenario_table) { # for testing. nrow(scenario_table[1:2,])
  print(i)
  #browser()
  
  # i=1 # testing. no longer breaks because "At least one of delay.date or closure.date must not be NULL"
  # i=2 # testing. no longer breaks because when switch() is used and does not return NULL, it returns nothing
  
  scenario.output.df <- effort_mgmt(
    x = x.orig,
    
    season.st.key = season.st.date.key,
    
    preseason.days = scenario_table$preseason.days,
    
    season.st.backstop = if (scenario_table$season.st.backstop[i] == "NULL") {
      NULL
      }
    else {
      as.Date(scenario_table$season.st.backstop[i])
      },
    
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
    
    closure.redist.percent = scenario_table$closure.redist.percent[i],
    
    depth.shallow = if (scenario_table$depth.shallow[i] == "NULL") NULL else as.numeric(scenario_table$depth.shallow[i]), 
    
    depth.deep = if (scenario_table$depth.deep[i] == "NULL") NULL else as.numeric(scenario_table$depth.deep[i]),
    
    reduction.before.date = if (scenario_table$reduction.before.date[i] == "NULL") NULL else scenario_table$reduction.before.date[i],
    
    reduction.before.percent = 50,
    
    reduction.before.region = if (scenario_table$reduction.before.region[i] == "NULL") NULL else scenario_table$reduction.before.region[i],
    
    reduction.after.date = if (scenario_table$reduction.after.date[i] == "NULL") NULL else as.Date(scenario_table$reduction.after.date[i]),
    
    reduction.after.percent = 50,
    
    reduction.after.region = if (scenario_table$reduction.after.region[i] == "NULL") NULL else scenario_table$reduction.after.region[i]
    
  )
  
}, scenario_table = scenario_table)

#rm()

### just the late season effort and depth restriction scenarios

scenario.output.list.edr <- lapply(1:nrow(scenario_table_edr), function(i, scenario_table) { # for testing. nrow(scenario_table[1:2,])
  print(i)
  #browser()
  
  # i=1 # testing. no longer breaks because "At least one of delay.date or closure.date must not be NULL"
  # i=2 # testing. no longer breaks because when switch() is used and does not return NULL, it returns nothing
  
  scenario.output.df.edr <- effort_mgmt(
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
    
    closure.redist.percent = scenario_table$closure.redist.percent[i],
    
    depth.val = as.numeric(scenario_table$depth.val),
    
    reduction.before.date = if (scenario_table$reduction.before.date[i] == "NULL") NULL else scenario_table$reduction.before.date[i],
    
    reduction.before.percent = 50,
    
    reduction.before.region = if (scenario_table$reduction.before.region[i] == "NULL") NULL else scenario_table$reduction.before.region[i],
    
    reduction.after.date = if (scenario_table$reduction.after.date[i] == "NULL") NULL else as.Date(scenario_table$reduction.after.date[i]),
    
    reduction.after.percent = 50,
    
    reduction.after.region = if (scenario_table$reduction.after.region[i] == "NULL") NULL else scenario_table$reduction.after.region[i]
    
  )
  
}, scenario_table = scenario_table)

#glimpse(scenario.output.list.edr[[6]])

scenario.output.list <- c(scenario.output.list.closures,scenario.output.list.edr)
#length(scenario.output.list)

save.image(paste0("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/scenario_output_dataframes/scenario_output_",today(),".RData"))

###############################################################################


### 2) Calculate and summarize risk for multiple scenarios, including status quo

# grab shifted effort fishing data

# JS 
load(paste0("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/scenario_output_dataframes/scenario_output_","2020-05-28",".RData"))

# grab whale data

# JS 
x.whale <-readRDS("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/5x5 Grid/Grid5km_whale.rds")

# source("tradeoffs/Management scenarios/Mgmt_scenarios_risk.R")
# 
# start.time <- Sys.time()
# 
# risk_out_list <- lapply(1:nrow(scenario_table), function(i, scenario_table) { # for testing. nrow(scenario_table[1:3,])
#   print(paste("Summarizing risk for Scenario", i))
#   #browser()
#   
#   risk_out <- risk_mgmt(
#     scenario.output.list[[i]], 
#     Num_DCRB_VMS_pings, 
#     x.whale
#     )
#   }, scenario_table = scenario_table
#   )
#   
# Sys.time() - start.time
# # Time difference of 2.249338 mins
# 
# save.image(paste0("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/scenario_output_dataframes/effort_shift_scenariocomparisons_withrisk_","2020-05-18",".RData"))
# 
# glimpse(risk_out_list[[1]])

scenario_table_all <- scenario_table %>%
  bind_rows(scenario_table_edr)
write_rds(scenario_table_all, here::here(
  "tradeoffs",
  "Management scenarios",
  "scenario_table_all.rds")
)

# Load and prep grid cell - area key
load("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/5x5 Grid/Grid_5km_landerased.RDATA")
area.key <- grid.5km.lno %>% 
  st_drop_geometry() %>% 
  select(GRID5KM_ID, area_km_lno) %>% 
  distinct()


### Calculate and summarize risk
source("tradeoffs/Management scenarios/Mgmt_scenarios_risk.R")
# what about normalizing? added in. need to decide about normalizing within/across years and regions issue

# will need to re-run all of the above and below for sm and lg vessels

start.time <- Sys.time()

risk_out_list <- lapply(1:nrow(scenario_table_all), function(i, scenario_table) { # for testing. nrow(scenario_table[1:3,])
  print(paste("Summarizing risk for Scenario", i))
  #browser()
  
  risk_out <- risk_mgmt(
    x = scenario.output.list[[i]], 
    x.col = Num_DCRB_VMS_pings, 
    y = x.whale,
    risk.unit = "dens", 
    area.key = area.key
  )
}, scenario_table = scenario_table
)

Sys.time() - start.time
# Time difference of 19.11915 secs


### save status quo scenario as its own df ###
#scenario_table_all$scenario_df_name # can use 1 or 13

glimpse(risk_out_list[[1]])
risk_5km_yr_mth_sq_all <- risk_out_list[[1]]

write_rds(risk_5km_yr_mth_sq_all, paste0("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/scenario_output_dataframes/risk_5km_yr_mth_sq_all_",today(),".rds"))

# summarize risk by region

start.time <- Sys.time()
risk_out_summ_list <- lapply(1:nrow(scenario_table_all), function(i, scenario_table) { # for testing. nrow(scenario_table[1:3,])
  print(paste("Summarizing risk for Scenario", i))
  #browser()
  
  risk_out_summ <- risk_mgmt_summ(
    x = risk_out_list[[i]], 
    summary.level = "Region"
  )
}, scenario_table = scenario_table
)

Sys.time() - start.time
#Time difference of 0.725035 secs

glimpse(risk_out_summ_list[[1]])


# summarize risk by BIA

# start.time <- Sys.time()
# risk_out_summ_list_BIA <- lapply(1:nrow(scenario_table_all), function(i, scenario_table) { # for testing. nrow(scenario_table[1:3,])
#   print(paste("Summarizing risk for Scenario", i))
#   #browser()
#   
#   risk_out_summ <- risk_mgmt_summ(
#     x = risk_out_list[[i]], 
#     summary.level = "BIA"
#   )
# }, scenario_table = scenario_table
# )
# 
# Sys.time() - start.time
#Time difference of 0.725035 secs

save.image(paste0("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/scenario_output_dataframes/scenario_output_risk_","2020-05-29",".RData"))



###############################################################################

### 3) make annual and tradeoff df's

# if needed, load data
load(paste0("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/scenario_output_dataframes/scenario_output_risk_","2020-05-29",".RData"))

# assign number IDs to scenarios
scenario_table_all$number_id <- row.names(scenario_table_all)

# could subset or map to region for regional summaries here
source("tradeoffs/Management scenarios/make_tradeoff_dataframes_function.R")
start.time <- Sys.time()
tradeoff_df_function(
  risk_list = risk_out_summ_list,
  scenario_names_table = scenario_table_all,
  annual_statewide_df_name = "annual_statewide_df", # this ensures df is available in the globalEnv after running function
  df_tradeoff_name = "df_tradeoff" # this ensures df is available in the globalEnv after running function
  )
Sys.time() - start.time
#Time difference of 1.041596 secs

# is risk actually greater for whales under some scenarios? yes.
dim(df_tradeoff)
length(which(df_tradeoff$relative_hump_risk < 0)) # 23 out of 300
length(which(df_tradeoff$relative_blwh_risk < 0)) # 36 out of 300

# is $ or pounds actually greater for the fishery under some scenarios? yes
length(which(df_tradeoff$relative_dollars > 100)) # 0 out of 300
length(which(df_tradeoff$relative_pounds > 100)) # 5 out of 300


###############################################################################

###############################################################################

### 4) Subset to focal scenarios for main text

# drop closure.redist.percent == 10
# annual_statewide_df_focal_scenarios <- annual_statewide_df %>%
#   filter(
#     closure.redist.percent == 100) 

# drop closure.redist.percent == 10 for BIA early closure, drop closure.redist.percent == 100 for CenCa early closure. i.e., ifelse(closure.region == "BIA, drop closure.redist.percent == 10, drop closure.redist.percent == 100)
annual_statewide_df_focal_scenarios <- annual_statewide_df[-which(
  annual_statewide_df$closure.region == "BIA" & annual_statewide_df$closure.redist.percent == 10),]
annual_statewide_df_focal_scenarios <- annual_statewide_df_focal_scenarios[-which( 
    annual_statewide_df_focal_scenarios$closure.region != "BIA" & annual_statewide_df_focal_scenarios$closure.redist.percent == 100),]

write_rds(annual_statewide_df_focal_scenarios, paste0("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/annual_statewide_df_focal_scenarios_","2020-05-29",".rds"))

scenario_table_focal_scenarios <- scenario_table_all[-which(
  scenario_table_all$closure.region == "BIA" & scenario_table_all$closure.redist.percent == 10),]
scenario_table_focal_scenarios <- scenario_table_focal_scenarios[-which( 
  scenario_table_focal_scenarios$closure.region != "BIA" & scenario_table_focal_scenarios$closure.redist.percent == 100),]
write_rds(scenario_table_focal_scenarios, here::here(
  "tradeoffs",
  "Management scenarios",
  "scenario_table_focal_scenarios.rds")
)

unique(annual_statewide_df_focal_scenarios$number_id)
# which scenario number is status quo? 25
annual_statewide_df_focal_scenarios$number_id[which(annual_statewide_df_focal_scenarios$scenario_df_name == "No_Delay_No_Early_Closure_delay_method_fidelity_spatial_closure_redist_percent_10")]

df_tradeoff_focal_scenarios <- df_tradeoff[which(
  df_tradeoff$number_id %in% unique(annual_statewide_df_focal_scenarios$number_id)
),]
unique(df_tradeoff_focal_scenarios$number_id)

write_rds(df_tradeoff_focal_scenarios, paste0("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/df_tradeoff_focal_scenarios_","2020-05-29",".rds"))

###############################################################################

### 5) Make some plots of focal scenarios. Use plot_tradeoffs_function.R

