library(tidyverse)
library(viridis)
library(ggrepel)

####################################################################
####################################################################

# 052920

# run make_tradeoff_dataframes_function_effort_comparison.R first from effort_shift_comparison.R
annual_statewide_df_n <- read_rds("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/annual_statewide_effort_shift_scenariocomparisons_n_2020-05-19.rds")
glimpse(annual_statewide_df_n)
# delay_scenario,, closure_scenario, early.data.method,
# delay.date, delay.region, delay.method, delay.method.fidelity, closure.date, 
# closure.region, closure.method, closure.redist.percent, number_id    

tradeoff_df_effort_shift_scenariocomparisons_n <- read_rds("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/tradeoff_df_effort_shift_scenariocomparisons_n_2020-05-19.rds")

glimpse(tradeoff_df_effort_shift_scenariocomparisons_n)
unique(tradeoff_df_effort_shift_scenariocomparisons_n$scenario_df_name)
names(tradeoff_df_effort_shift_scenariocomparisons_n)

unique(tradeoff_df_effort_shift_scenariocomparisons_n$scenario_df_name) == unique(annual_statewide_df_n$scenario_df_name)

View (
  tradeoff_df_effort_shift_scenariocomparisons_n %>% 
  group_by(
  scenario_df_name 
  ) %>%
  summarise(
    mean_relative_hump_risk_n = mean(relative_hump_risk_n),
    sd_relative_hump_risk_n = sd(relative_hump_risk_n),
    
    mean_relative_blwh_risk_n = mean(relative_blwh_risk_n),
    sd_relative_blwh_risk_n = sd(relative_blwh_risk_n),
    
    mean_relative_dollars = mean(relative_dollars),
    sd_relative_dollars = sd(relative_dollars)
  ) %>%
  arrange(
    mean_relative_hump_risk_n,
    mean_relative_blwh_risk_n,
    mean_relative_dollars
  )
)


