# make table of scenarios
delay_scenarios <- c(
  "No_Delay",
  "Statewide_Marine_Life_Delay",
  "CenCA_Marine_Life_Delay"
)

closure_scenarios <- c(
  "No_Early_Closure",
  "Statewide_Early_Closure",
  "CenCA_Early_Closure",
  "BIA_Early_Closure"
)

scenario_table <- expand.grid(
  "delay_scenario" = delay_scenarios,
  "closure_scenario" = closure_scenarios
) 
# slice(-3, .preserve=TRUE) %>% # remove BIA delay
# slice(-8, .preserve=TRUE) # remove BIA squeeze

# scenario_table <- add_row(scenario_table,
#                           spatial="Status_Quo",
#                           temporal="Status_Quo"
#                           )

#add columns
scenario_table <- scenario_table %>%
  mutate(
    scenario_df_name = paste(delay_scenario,closure_scenario,sep="_"),
    delay_time_scenario = ifelse(delay_scenario != "No_Delay", "Dec-15", NA),
    delay_domain_scenario = ifelse(delay_scenario != "No_Delay", substr(delay_scenario,1,5), NA),
    closure_time_scenario = ifelse(closure_scenario != "No_Early_Closure", "Spring-Summer", NA),
    closure_domain_scenario =  ifelse(closure_scenario != "No_Early_Closure", substr(closure_scenario,1,3), NA)
  ) %>%
  dplyr::select(scenario_df_name, delay_scenario, closure_scenario, delay_time_scenario, delay_domain_scenario, closure_time_scenario, closure_domain_scenario)


write_rds(scenario_table, here::here(
  "tradeoffs",
  "Management scenarios",
  "scenario_table.RDS"
  )
)
# 
# scenario_table <- read_rds(here::here(
#   "tradeoffs",
#   "Management scenarios",
#   "scenario_table.RDS"
#   )
# )
