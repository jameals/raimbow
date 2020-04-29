# make table of scenarios
library(tidyverse)

# columns need to include all arguments for the function described in Mgmt_scenarios_shift_effort.R
# early.data.method: character; either "pile" or "remove". Represents what to to do with data that comes before minimum season start date. e.g. data that comes before 15 Nov in Central CA. 
# delay.date: Date; date for which the fishery will open in 2009-10 crab season. If NULL, then there is no delayed opening. 
# delay.region: character; one of NULL, "All", "CenCA", "BIA". Not BIA not yet supported
# delay.method.shift: character; if used, either "pile" or "lag"
# delay.method.fidelity: character; method of redistribution, if used, either "spatial" (fidelity) or "temporal" (fidelity)
# closure.date: Date; date for which the fishery will close in 2009-10 crab season. If NULL, then there is no early (e.g. spring) closure
# closure.region: character; one of NULL, "All", "CenCA", "BIA".
# closure.method: character; if used, either "remove" or "temporal (fidelity)
# closure.redist.percent: numeric; default is 100. If used, percentage of effort to redistribute that is kept

# 042820. settings
# early.data.method: set as remove for all scenarios unless asked to do otherwise
# delay.date: NULL or Dec 15
# delay.region: NULL, "All", "CenCA"
# delay.method.shift: set as "lag" unless asked to do otherwise
# delay.method.fidelity: develop 2 complete sets of scenarios, 1 with this setting as "spatial" and 1 as "temporal". JS preferred / main text scenario setting is "spatial"
# closure.date: NULL or Apr 1
# closure.region: NULL, "All", "CenCA", "BIA"
# closure.method: set as "temporal" unless asked to do otherwise
# closure.redist.percent: develop 2 complete sets of scenarios, 1 with this setting as 100 and 1 as 10. JS preferred / main text scenario setting is 10 for closure.region== "CenCA", 100 for closure.region=="BIA

### Make the scenario combinations 
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

# set delayed opening and early closure dates for scenarios
delayed.opening.date <- "2009-12-15"
early.closure.date <- "2010-04-01"

### Add columns to match arguments in Mgmt_scenarios_shift_effort.R

# 1st set of scenarios
# delay.method.fidelity: "spatial"
# closure.redist.percent: 100
scenario_table_1 <- scenario_table %>%
  mutate(
    scenario_df_name = paste(delay_scenario,closure_scenario,"delay_method_fidelity_spatial","closure_redist_percent_100",sep="_"),
    early.data.method = "remove",
    delay.date = ifelse(delay_scenario == "No_Delay", "NULL", delayed.opening.date),
    delay.region = ifelse(delay_scenario == "No_Delay", "NULL",
                          ifelse(substr(delay_scenario,1,5) == "State",
                                 "All",
                                 "CenCA")
                          ),
    delay.method.shift = "lag",
    delay.method.fidelity = "spatial",
    closure.date = ifelse(closure_scenario != "No_Early_Closure", early.closure.date, "NULL"),
    closure.region = ifelse(closure_scenario == "No_Early_Closure", "NULL",
                                      ifelse(substr(closure_scenario,1,3) == "Sta", 
                                             "All",
                                             ifelse(substr(closure_scenario,1,3) == "Cen",
                                                    "CenCA", "BIA"
                                                    )
                                             )
                                      ),
    closure.method = "temporal",
    closure.redist.percent = 100
  ) %>%
  dplyr::select(scenario_df_name, delay_scenario, closure_scenario, early.data.method, delay.date,
                delay.region, delay.method.shift, delay.method.fidelity, closure.date, closure.region, 
                closure.method, closure.redist.percent)

# 2nd set of scenarios
# delay.method.fidelity: "temporal"
# closure.redist.percent: 100
scenario_table_2 <- scenario_table %>%
  mutate(
    scenario_df_name = paste(delay_scenario,closure_scenario,"delay_method_fidelity_temporal","closure_redist_percent_100",sep="_"),
    early.data.method = "remove",
    delay.date = ifelse(delay_scenario == "No_Delay", "NULL", delayed.opening.date),
    delay.region = ifelse(delay_scenario == "No_Delay", "NULL",
                          ifelse(substr(delay_scenario,1,5) == "State",
                                 "All",
                                 "CenCA")
    ),
    delay.method.shift = "lag",
    delay.method.fidelity = "temporal",
    closure.date = ifelse(closure_scenario != "No_Early_Closure", early.closure.date, "NULL"),
    closure.region = ifelse(closure_scenario == "No_Early_Closure", "NULL",
                            ifelse(substr(closure_scenario,1,3) == "Sta", 
                                   "All",
                                   ifelse(substr(closure_scenario,1,3) == "Cen",
                                          "CenCA", "BIA"
                                   )
                            )
    ),
    closure.method = "temporal",
    closure.redist.percent = 100
  ) %>%
  dplyr::select(scenario_df_name, delay_scenario, closure_scenario, early.data.method, delay.date,
                delay.region, delay.method.shift, delay.method.fidelity, closure.date, closure.region, 
                closure.method, closure.redist.percent)

# 3rd set of scenarios
# delay.method.fidelity: "spatial"
# closure.redist.percent: 10
scenario_table_3 <- scenario_table %>%
  mutate(
    scenario_df_name = paste(delay_scenario,closure_scenario,"delay_method_fidelity_spatial","closure_redist_percent_10",sep="_"),
    early.data.method = "remove",
    delay.date = ifelse(delay_scenario == "No_Delay", "NULL", delayed.opening.date),
    delay.region = ifelse(delay_scenario == "No_Delay", "NULL",
                          ifelse(substr(delay_scenario,1,5) == "State",
                                 "All",
                                 "CenCA")
    ),
    delay.method.shift = "lag",
    delay.method.fidelity = "spatial",
    closure.date = ifelse(closure_scenario != "No_Early_Closure", early.closure.date, "NULL"),
    closure.region = ifelse(closure_scenario == "No_Early_Closure", "NULL",
                            ifelse(substr(closure_scenario,1,3) == "Sta", 
                                   "All",
                                   ifelse(substr(closure_scenario,1,3) == "Cen",
                                          "CenCA", "BIA"
                                   )
                            )
    ),
    closure.method = "temporal",
    closure.redist.percent = 10
  ) %>%
  dplyr::select(scenario_df_name, delay_scenario, closure_scenario, early.data.method, delay.date,
                delay.region, delay.method.shift, delay.method.fidelity, closure.date, closure.region, 
                closure.method, closure.redist.percent)

# 4th set of scenarios
# delay.method.fidelity: "temporal"
# closure.redist.percent: 10
scenario_table_4 <- scenario_table %>%
  mutate(
    scenario_df_name = paste(delay_scenario,closure_scenario,"delay_method_fidelity_temporal","closure_redist_percent_10",sep="_"),
    early.data.method = "remove",
    delay.date = ifelse(delay_scenario == "No_Delay", "NULL", delayed.opening.date),
    delay.region = ifelse(delay_scenario == "No_Delay", "NULL",
                          ifelse(substr(delay_scenario,1,5) == "State",
                                 "All",
                                 "CenCA")
    ),
    delay.method.shift = "lag",
    delay.method.fidelity = "temporal",
    closure.date = ifelse(closure_scenario != "No_Early_Closure", early.closure.date, "NULL"),
    closure.region = ifelse(closure_scenario == "No_Early_Closure", "NULL",
                            ifelse(substr(closure_scenario,1,3) == "Sta", 
                                   "All",
                                   ifelse(substr(closure_scenario,1,3) == "Cen",
                                          "CenCA", "BIA"
                                   )
                            )
    ),
    closure.method = "temporal",
    closure.redist.percent = 10
  ) %>%
  dplyr::select(scenario_df_name, delay_scenario, closure_scenario, early.data.method, delay.date,
                delay.region, delay.method.shift, delay.method.fidelity, closure.date, closure.region, 
                closure.method, closure.redist.percent)

scenario_table <- bind_rows(list(scenario_table_1,scenario_table_2, scenario_table_3, scenario_table_4))

write_rds(scenario_table, here::here(
  "tradeoffs",
  "Management scenarios",
  "scenario_table.RDS"
)
)

################################################
################################################
################################################
################################################


### code prior to Sam Woodman developing Mgmt_scenarios_shift_effort.R
# delay_scenarios <- c(
#   "No_Delay",
#   "Statewide_Marine_Life_Delay",
#   "CenCA_Marine_Life_Delay"
# )
# 
# closure_scenarios <- c(
#   "No_Early_Closure",
#   "Statewide_Early_Closure",
#   "CenCA_Early_Closure",
#   "BIA_Early_Closure"
# )
# 
# scenario_table <- expand.grid(
#   "delay_scenario" = delay_scenarios,
#   "closure_scenario" = closure_scenarios
# ) 
# # slice(-3, .preserve=TRUE) %>% # remove BIA delay
# # slice(-8, .preserve=TRUE) # remove BIA squeeze
# 
# # scenario_table <- add_row(scenario_table,
# #                           spatial="Status_Quo",
# #                           temporal="Status_Quo"
# #                           )
# 
# #add columns
# scenario_table <- scenario_table %>%
#   mutate(
#     scenario_df_name = paste(delay_scenario,closure_scenario,sep="_"),
#     delay_time_scenario = ifelse(delay_scenario != "No_Delay", "Dec-15", NA),
#     delay_domain_scenario = ifelse(delay_scenario != "No_Delay", substr(delay_scenario,1,5), NA),
#     closure_time_scenario = ifelse(closure_scenario != "No_Early_Closure", "Spring-Summer", NA),
#     closure_domain_scenario =  ifelse(closure_scenario != "No_Early_Closure", substr(closure_scenario,1,3), NA)
#   ) %>%
#   dplyr::select(scenario_df_name, delay_scenario, closure_scenario, delay_time_scenario, delay_domain_scenario, closure_time_scenario, closure_domain_scenario)
# 
# 
# write_rds(scenario_table, here::here(
#   "tradeoffs",
#   "Management scenarios",
#   "scenario_table.RDS"
#   )
# )
# 
# scenario_table <- read_rds(here::here(
#   "tradeoffs",
#   "Management scenarios",
#   "scenario_table.RDS"
#   )
# )
