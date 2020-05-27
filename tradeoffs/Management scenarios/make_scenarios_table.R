# make table of scenarios
library(tidyverse)

# columns need to include all arguments for the function described in Mgmt_scenarios_shift_effort.R
# early.data.method: character; either "pile" or "remove". Represents what to
#   to do with data that comes before minimum season start date, 
#   e.g. data that comes before 15 Nov in Central CA
# delay.date: Date; date for which the fishery will open in 2009-10 crab season.
#   If NULL, then there is no delayed opening and 
#   other delay.. arguments are ignored
# delay.region: character; one of options listed above. Ignored if delay.date is NULL
# delay.method: character; one of options listed above. Ignored if delay.date is NULL
# delay.method.fidelity: one of options listed above. Ignored (only) if delay.date is NULL 
# closure.date: Date; date for which the fishery will close in 2009-10 crab season
#   If NULL, then there is no early (e.g. spring) closure and 
#   other 'closure...' arguments are ignored
# closure.region: character; one of options listed above. Ignored if closure.date is NULL
# closure.method: character; one of options listed above. Ignored if closure.date is NULL
# closure.redist.percent: numeric; default is 100. 
#   Ignored if closure.method is not "temporal". Otherwise, 
#   values being redistributed are multiplied by (closure.redist.percent / 100) 
# depth.val: numeric; if either delay.method or closure.method is "depth", 
#   then all effort less than this value (i.e. with a higher absolute value) is removed
# reduction.before.date: Date; if not NULL, then all effort values before this date
#   are multiplied by (1 - (reduction.before.percent / 100))
# reduction.before.percent: numeric; between 0 and 100. Ignored if reduction.before.date is NULL
# reduction.before.region: character; one of options listed above. 
#   Ignored if reduction.before.date is NULL
# reduction.after.date: Date; if not NULL, then all effort values after this date
#   are multiplied by (1- (reduction.after.percent / 100))
# reduction.after.percent: numeric; between 0 and 100. Ignored if reduction.after.date is NULL
# reduction.after.region: character; one of options listed above. 
#   Ignored if reduction.after.date is NULL

# 052720. settings. 

########### PICK UP HERE 052720 ###############

# early.data.method: set as remove for all scenarios unless asked to do otherwise
# delay.date: NULL or Dec 15
# delay.region: NULL, "All", "CenCA"
# delay.method: set as "lag", but compare to "remove"
# delay.method.fidelity: develop 2 complete sets of scenarios, 1 with this setting as "spatial" and 1 as "temporal". JS preferred / main text scenario setting is "spatial"
# closure.date: NULL or Apr 1
# closure.region: NULL, "All", "CenCA", "BIA"
# closure.method: set as "temporal" but compare to "remove"; must be "remove" when closure.region is "All"
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
    delay.method = "lag",
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
    closure.method = ifelse(closure.region != "All", "temporal", "remove"),
    closure.redist.percent = 100
  ) %>%
  dplyr::select(scenario_df_name, delay_scenario, closure_scenario, early.data.method, delay.date,
                delay.region, delay.method, delay.method.fidelity, closure.date, closure.region, 
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
    delay.method = "lag",
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
                delay.region, delay.method, delay.method.fidelity, closure.date, closure.region, 
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
    delay.method = "lag",
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
                delay.region, delay.method, delay.method.fidelity, closure.date, closure.region, 
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
    delay.method = "lag",
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
                delay.region, delay.method, delay.method.fidelity, closure.date, closure.region, 
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

# 051220. settings
# early.data.method: set as remove for all scenarios unless asked to do otherwise
# delay.date: NULL or Dec 15
# delay.region: NULL, "All", "CenCA"
# delay.method: set as "lag", but compare to "remove"
# delay.method.fidelity: develop 2 complete sets of scenarios, 1 with this setting as "spatial" and 1 as "temporal". JS preferred / main text scenario setting is "spatial"
# closure.date: NULL or Apr 1
# closure.region: NULL, "All", "CenCA", "BIA"
# closure.method: set as "temporal" but compare to "remove"; must be "remove" when closure.region is "All"
# closure.redist.percent: develop 2 complete sets of scenarios, 1 with this setting as 100 and 1 as 10. JS preferred / main text scenario setting is 10 for closure.region== "CenCA", 100 for closure.region=="BIA

# ### Make the scenario combinations 
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
# 
# # set delayed opening and early closure dates for scenarios
# delayed.opening.date <- "2009-12-15"
# early.closure.date <- "2010-04-01"
# 
# ### Add columns to match arguments in Mgmt_scenarios_shift_effort.R
# 
# # 1st set of scenarios
# # delay.method.fidelity: "spatial"
# # closure.redist.percent: 100
# scenario_table_1 <- scenario_table %>%
#   mutate(
#     scenario_df_name = paste(delay_scenario,closure_scenario,"delay_method_fidelity_spatial","closure_redist_percent_100",sep="_"),
#     early.data.method = "remove",
#     delay.date = ifelse(delay_scenario == "No_Delay", "NULL", delayed.opening.date),
#     delay.region = ifelse(delay_scenario == "No_Delay", "NULL",
#                           ifelse(substr(delay_scenario,1,5) == "State",
#                                  "All",
#                                  "CenCA")
#                           ),
#     delay.method = "lag",
#     delay.method.fidelity = "spatial",
#     closure.date = ifelse(closure_scenario != "No_Early_Closure", early.closure.date, "NULL"),
#     closure.region = ifelse(closure_scenario == "No_Early_Closure", "NULL",
#                                       ifelse(substr(closure_scenario,1,3) == "Sta", 
#                                              "All",
#                                              ifelse(substr(closure_scenario,1,3) == "Cen",
#                                                     "CenCA", "BIA"
#                                                     )
#                                              )
#                                       ),
#     closure.method = ifelse(closure.region != "All", "temporal", "remove"),
#     closure.redist.percent = 100
#   ) %>%
#   dplyr::select(scenario_df_name, delay_scenario, closure_scenario, early.data.method, delay.date,
#                 delay.region, delay.method, delay.method.fidelity, closure.date, closure.region, 
#                 closure.method, closure.redist.percent)
# 
# # 2nd set of scenarios
# # delay.method.fidelity: "temporal"
# # closure.redist.percent: 100
# scenario_table_2 <- scenario_table %>%
#   mutate(
#     scenario_df_name = paste(delay_scenario,closure_scenario,"delay_method_fidelity_temporal","closure_redist_percent_100",sep="_"),
#     early.data.method = "remove",
#     delay.date = ifelse(delay_scenario == "No_Delay", "NULL", delayed.opening.date),
#     delay.region = ifelse(delay_scenario == "No_Delay", "NULL",
#                           ifelse(substr(delay_scenario,1,5) == "State",
#                                  "All",
#                                  "CenCA")
#     ),
#     delay.method = "lag",
#     delay.method.fidelity = "temporal",
#     closure.date = ifelse(closure_scenario != "No_Early_Closure", early.closure.date, "NULL"),
#     closure.region = ifelse(closure_scenario == "No_Early_Closure", "NULL",
#                             ifelse(substr(closure_scenario,1,3) == "Sta", 
#                                    "All",
#                                    ifelse(substr(closure_scenario,1,3) == "Cen",
#                                           "CenCA", "BIA"
#                                    )
#                             )
#     ),
#     closure.method = "temporal",
#     closure.redist.percent = 100
#   ) %>%
#   dplyr::select(scenario_df_name, delay_scenario, closure_scenario, early.data.method, delay.date,
#                 delay.region, delay.method, delay.method.fidelity, closure.date, closure.region, 
#                 closure.method, closure.redist.percent)
# 
# # 3rd set of scenarios
# # delay.method.fidelity: "spatial"
# # closure.redist.percent: 10
# scenario_table_3 <- scenario_table %>%
#   mutate(
#     scenario_df_name = paste(delay_scenario,closure_scenario,"delay_method_fidelity_spatial","closure_redist_percent_10",sep="_"),
#     early.data.method = "remove",
#     delay.date = ifelse(delay_scenario == "No_Delay", "NULL", delayed.opening.date),
#     delay.region = ifelse(delay_scenario == "No_Delay", "NULL",
#                           ifelse(substr(delay_scenario,1,5) == "State",
#                                  "All",
#                                  "CenCA")
#     ),
#     delay.method = "lag",
#     delay.method.fidelity = "spatial",
#     closure.date = ifelse(closure_scenario != "No_Early_Closure", early.closure.date, "NULL"),
#     closure.region = ifelse(closure_scenario == "No_Early_Closure", "NULL",
#                             ifelse(substr(closure_scenario,1,3) == "Sta", 
#                                    "All",
#                                    ifelse(substr(closure_scenario,1,3) == "Cen",
#                                           "CenCA", "BIA"
#                                    )
#                             )
#     ),
#     closure.method = "temporal",
#     closure.redist.percent = 10
#   ) %>%
#   dplyr::select(scenario_df_name, delay_scenario, closure_scenario, early.data.method, delay.date,
#                 delay.region, delay.method, delay.method.fidelity, closure.date, closure.region, 
#                 closure.method, closure.redist.percent)
# 
# # 4th set of scenarios
# # delay.method.fidelity: "temporal"
# # closure.redist.percent: 10
# scenario_table_4 <- scenario_table %>%
#   mutate(
#     scenario_df_name = paste(delay_scenario,closure_scenario,"delay_method_fidelity_temporal","closure_redist_percent_10",sep="_"),
#     early.data.method = "remove",
#     delay.date = ifelse(delay_scenario == "No_Delay", "NULL", delayed.opening.date),
#     delay.region = ifelse(delay_scenario == "No_Delay", "NULL",
#                           ifelse(substr(delay_scenario,1,5) == "State",
#                                  "All",
#                                  "CenCA")
#     ),
#     delay.method = "lag",
#     delay.method.fidelity = "temporal",
#     closure.date = ifelse(closure_scenario != "No_Early_Closure", early.closure.date, "NULL"),
#     closure.region = ifelse(closure_scenario == "No_Early_Closure", "NULL",
#                             ifelse(substr(closure_scenario,1,3) == "Sta", 
#                                    "All",
#                                    ifelse(substr(closure_scenario,1,3) == "Cen",
#                                           "CenCA", "BIA"
#                                    )
#                             )
#     ),
#     closure.method = "temporal",
#     closure.redist.percent = 10
#   ) %>%
#   dplyr::select(scenario_df_name, delay_scenario, closure_scenario, early.data.method, delay.date,
#                 delay.region, delay.method, delay.method.fidelity, closure.date, closure.region, 
#                 closure.method, closure.redist.percent)
# 
# scenario_table <- bind_rows(list(scenario_table_1,scenario_table_2, scenario_table_3, scenario_table_4))
# 
# write_rds(scenario_table, here::here(
#   "tradeoffs",
#   "Management scenarios",
#   "scenario_table.RDS"
# )
# )


# 042820. settings
# early.data.method: set as remove for all scenarios unless asked to do otherwise
# delay.date: NULL or Dec 15
# delay.region: NULL, "All", "CenCA"
# delay.method: set as "lag" unless asked to do otherwise
# delay.method.fidelity: develop 2 complete sets of scenarios, 1 with this setting as "spatial" and 1 as "temporal". JS preferred / main text scenario setting is "spatial"
# closure.date: NULL or Apr 1
# closure.region: NULL, "All", "CenCA", "BIA"
# closure.method: set as "temporal" unless asked to do otherwise, though it must be "remove" when closure.region is "All"
# closure.redist.percent: develop 2 complete sets of scenarios, 1 with this setting as 100 and 1 as 10. JS preferred / main text scenario setting is 10 for closure.region== "CenCA", 100 for closure.region=="BIA

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
