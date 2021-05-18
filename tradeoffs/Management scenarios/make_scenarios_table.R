# make table of scenarios

# 1) table for delays and early closures, without effort reduction or depth restriction
# 2) table for late season effort reduction, depth restriction, or both

library(tidyverse)

# 052720, updated 060520. settings. updated 110920.

# 1) table for delays and early closures, no effort reduction or depth restriction 
# season.st.key: set as key based on 1% accumulated landings
# preseason.days: set as 3 to account for pots in the water during 64 hour pre-soak time
# season.st.backstop = NULL,
# early.data.method: set as remove for all scenarios unless asked to do otherwise
# delay.date: NULL or Dec 15
# delay.region: NULL, "All", "CenCA"
# delay.method: set as "lag", but compare to "remove"
# delay.method.fidelity: preferred / main text scenario setting is "spatial"
# closure.date: NULL or Apr 1
# closure.region: NULL, "All", "CenCA", "BIA"
# closure.method: set as "temporal" but compare to "remove"; must be "remove" when closure.region is "All". 
# closure.redist.percent: develop 2 complete sets of scenarios, 1 with this setting as 100 and 1 as 10. JS preferred / main text scenario setting is 10 for closure.region== "CenCA", 100 for closure.region=="BIA
# depth.shallow = NULL, 
# depth.deep = NULL, 
# reduction.before.date: NULL
# reduction.before.percent= 50. this is the default but will be ignored because reduction.before.date is NULL
# reduction.before.region: NULL
# reduction.after.date: NULL
# reduction.after.percent = 50. this is the default but will be ignored because reduction.after.date is NULL
# reduction.after.region: NULL
# reduction.after.redist: FALSE
# reduction.after.redist.percent: NULL

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
    #season.st.key = "season.st.date.key",
    preseason.days = 3,
    season.st.backstop = "NULL",
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
    closure.redist.percent = 100,
    depth.shallow = "NULL", 
    depth.deep = "NULL", 
    reduction.before.date = "NULL",
    reduction.before.percent = 50,
    reduction.before.region = "NULL",
    reduction.after.date = "NULL",
    reduction.after.percent = 50,
    reduction.after.region = "NULL",
    reduction.after.redist = "FALSE",
    reduction.after.redist.percent = 0 
  ) %>%
  dplyr::select(scenario_df_name, delay_scenario, closure_scenario, #season.st.key,        
                preseason.days, season.st.backstop, early.data.method, delay.date,
                delay.region, delay.method, delay.method.fidelity, closure.date, closure.region,
                closure.method, closure.redist.percent, depth.shallow, depth.deep, reduction.before.date, reduction.before.percent, reduction.before.region, reduction.after.date, reduction.after.percent, reduction.after.region, reduction.after.redist, reduction.after.redist.percent)

# 2nd set of scenarios
# delay.method.fidelity: "spatial"
# closure.redist.percent: 10
scenario_table_2 <- scenario_table %>%
  mutate(
    scenario_df_name = paste(delay_scenario,closure_scenario,"delay_method_fidelity_spatial","closure_redist_percent_10",sep="_"),
    #season.st.key = "season.st.date.key",
    preseason.days = 3,
    season.st.backstop = "NULL",
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
    closure.redist.percent = 10,
    depth.shallow = "NULL", 
    depth.deep = "NULL", 
    reduction.before.date = "NULL",
    reduction.before.percent = 50,
    reduction.before.region = "NULL",
    reduction.after.date = "NULL",
    reduction.after.percent = 50,
    reduction.after.region = "NULL",
    reduction.after.redist = "FALSE",
    reduction.after.redist.percent =  0 
  ) %>%
  dplyr::select(scenario_df_name, delay_scenario, closure_scenario, #season.st.key,        
                preseason.days, season.st.backstop, early.data.method, delay.date,
                delay.region, delay.method, delay.method.fidelity, closure.date, closure.region,
                closure.method, closure.redist.percent, depth.shallow, depth.deep, reduction.before.date, reduction.before.percent, reduction.before.region, reduction.after.date, reduction.after.percent, reduction.after.region, reduction.after.redist, reduction.after.redist.percent)

scenario_table <- bind_rows(list(scenario_table_1,scenario_table_2))

write_rds(scenario_table, here::here(
  "tradeoffs",
  "Management scenarios",
  "scenario_table_spatial_lag_10_100.RDS"
)
)

# 2) table for late season effort reduction, depth restriction, or both
# season.st.key: set as key based on 1% accumulated landings
# preseason.days: set as 3 to account for pots in the water during 64 hour pre-soak time
# season.st.backstop = NULL,
# early.data.method: set as remove for all scenarios unless asked to do otherwise
# delay.date: NULL
# delay.region: NULL
# delay.method: NULL
# delay.method.fidelity: NULL
# closure.date: Apr 1
# closure.region: "All", "CenCA"
# closure.method: "temporal", "depth+temporal". Use 'depth+temporal' for depth restrictions, which temporally redistributes effort outside of the range specified by depth.deep and depth.shallow into the specified depth range, in the same region
# closure.redist.percent: 0
# depth.shallow = NULL, 
# depth.deep = -54.864; assuming units are meters this corresponds to 30 fathoms. with depth.shallow set to NULL this keeps all effort shallower than -54.864 m, and removes everything deeper (seaward)
# reduction.before.date: NULL
# reduction.before.percent= 50. this is the default but will be ignored because reduction.before.date is NULL
# reduction.before.region: NULL
# reduction.after.date: Apr 1
# reduction.after.percent = 50. this is the default
# reduction.after.region: "All", "CenCA"
# reduction.after.redist: TRUE or FALSE (logical) indicating if effort removed by reduction.after... argumentsis redistributed using temporal fidelity. This can only be TRUE if reduction.after.region is 'CenCA' or 'NorCA', in which case the effort is temporally redistributed to the other region
# reduction.after.redist.percent: if reduction.after.redist is TRUE, then (effort removed * (reduction.after.redist.percent / 100)) effort is redistributed to the other (open) region

### Make the scenario combinations 
delay_scenarios_edr <- c(
  "No_Delay"
)

closure_scenarios_edr <- c(
  "No_Early_Closure"
)

restriction_scenarios_edr <- c(
  "Statewide",
  "CenCA"
)

scenario_table_edr <- expand.grid(
  "delay_scenario" = delay_scenarios_edr,
  "closure_scenario" = closure_scenarios_edr,
  "restriction_scenario" = restriction_scenarios_edr
) 

# set delayed opening and early closure dates for scenarios
#delayed.opening.date <- "2009-12-15"
restriction_after_date <- "2010-04-01"

### Add columns to match arguments in Mgmt_scenarios_shift_effort.R

# 3rd set of scenarios
# reduction.after.percent: 50
# redistribute to norCA if only cenCA closed
scenario_table_3 <- scenario_table_edr %>%
  mutate(
    scenario_df_name = paste(delay_scenario, closure_scenario, restriction_scenario,"reduction_after_percent_50",sep="_"),
    #season.st.key = "season.st.date.key",
    preseason.days = 3,
    season.st.backstop = "NULL",
    early.data.method = "remove",
    delay.date = ifelse(delay_scenario == "No_Delay", "NULL", delayed.opening.date),
    delay.region = ifelse(delay_scenario == "No_Delay", "NULL",
                          ifelse(substr(delay_scenario,1,5) == "State",
                                 "All",
                                 "CenCA")
    ),
    delay.method = "lag",
    delay.method.fidelity = "spatial",
    closure.date = "NULL",
    closure.region = "NULL",
    closure.method = "remove",
    closure.redist.percent = 0,
    depth.shallow = "NULL", 
    depth.deep = "NULL",
    reduction.before.date = "NULL",
    reduction.before.percent = 50,
    reduction.before.region = "NULL",
    reduction.after.date = restriction_after_date,
    reduction.after.percent = 50,
    reduction.after.region = ifelse(substr(restriction_scenario,1,3) == "Sta", 
                                           "All",
                                           "CenCA"
                                    ),
    reduction.after.redist = ifelse(substr(restriction_scenario,1,3) == "Sta", 
                                    "FALSE",
                                    "TRUE"
    ),
    reduction.after.redist.percent = ifelse(substr(restriction_scenario,1,3) == "Sta", 
                                            0,
                                            10
    )
  ) %>%
  dplyr::select(scenario_df_name, delay_scenario, closure_scenario, #season.st.key,        
                preseason.days, season.st.backstop, early.data.method, delay.date,
                delay.region, delay.method, delay.method.fidelity, closure.date, closure.region,
                closure.method, closure.redist.percent, depth.shallow, depth.deep, reduction.before.date, reduction.before.percent, reduction.before.region, reduction.after.date, reduction.after.percent, reduction.after.region, reduction.after.redist, reduction.after.redist.percent)

# 3rd set of scenarios - alt
# reduction.after.percent: 50
# remove all effort when only cenCA closed
scenario_table_3alt <- scenario_table_edr[2,] %>%
  mutate(
    scenario_df_name = paste(delay_scenario[1], closure_scenario, restriction_scenario,"reduction_after_percent_50_remove",sep="_"),
    #season.st.key = "season.st.date.key",
    preseason.days = 3,
    season.st.backstop = "NULL",
    early.data.method = "remove",
    delay.date = ifelse(delay_scenario == "No_Delay", "NULL", delayed.opening.date),
    delay.region = ifelse(delay_scenario == "No_Delay", "NULL",
                          ifelse(substr(delay_scenario,1,5) == "State",
                                 "All",
                                 "CenCA")
    ),
    delay.method = "lag",
    delay.method.fidelity = "spatial",
    closure.date = "NULL",
    closure.region = "NULL",
    closure.method = "remove",
    closure.redist.percent = 0,
    depth.shallow = "NULL", 
    depth.deep = "NULL",
    reduction.before.date = "NULL",
    reduction.before.percent = 50,
    reduction.before.region = "NULL",
    reduction.after.date = restriction_after_date,
    reduction.after.percent = 50,
    reduction.after.region = "CenCA",
    reduction.after.redist = "FALSE",
    reduction.after.redist.percent = 0
  ) %>%
  dplyr::select(scenario_df_name, delay_scenario, closure_scenario, #season.st.key,        
                preseason.days, season.st.backstop, early.data.method, delay.date,
                delay.region, delay.method, delay.method.fidelity, closure.date, closure.region,
                closure.method, closure.redist.percent, depth.shallow, depth.deep, reduction.before.date, reduction.before.percent, reduction.before.region, reduction.after.date, reduction.after.percent, reduction.after.region, reduction.after.redist, reduction.after.redist.percent)


# 4th set of scenarios
# depth.val = -54.864
# redistribute offshore effort onshore by region
scenario_table_4 <- scenario_table_edr %>%
  mutate(
    scenario_df_name = paste(delay_scenario,closure_scenario,restriction_scenario,"depth_val_30fathom",sep="_"),
    #season.st.key = "season.st.date.key",
    preseason.days = 3,
    season.st.backstop = "NULL",
    early.data.method = "remove",
    delay.date = ifelse(delay_scenario == "No_Delay", "NULL", delayed.opening.date),
    delay.region = ifelse(delay_scenario == "No_Delay", "NULL",
                          ifelse(substr(delay_scenario,1,5) == "State",
                                 "All",
                                 "CenCA")
    ),
    delay.method = "lag",
    delay.method.fidelity = "spatial",
    closure.date = ifelse(restriction_scenario != "No_Early_Closure", restriction_after_date, "NULL"),
    closure.region = ifelse(restriction_scenario == "No_Early_Closure", "NULL",
                            ifelse(substr(restriction_scenario,1,3) == "Sta", 
                                   "All",
                                   "CenCA"
                                   )
                            ),
    closure.method = "depth+temporal",
    closure.redist.percent = 100,
    depth.shallow = "NULL", 
    depth.deep = as.character(-54.864),
    reduction.before.date = "NULL",
    reduction.before.percent = 50,
    reduction.before.region = "NULL",
    reduction.after.date = "NULL",
    reduction.after.percent = 50,
    reduction.after.region = "NULL",
    reduction.after.redist = "FALSE",
    reduction.after.redist.percent = 0
    ) %>%
  dplyr::select(scenario_df_name, delay_scenario, closure_scenario, #season.st.key,        
                preseason.days, season.st.backstop, early.data.method, delay.date,
                delay.region, delay.method, delay.method.fidelity, closure.date, closure.region,
                closure.method, closure.redist.percent, depth.shallow, depth.deep, reduction.before.date, reduction.before.percent, reduction.before.region, reduction.after.date, reduction.after.percent, reduction.after.region, reduction.after.redist, reduction.after.redist.percent)

# 4th set of scenarios - alt
# depth.val = -54.864
# remove offshore effort completely
scenario_table_4alt <- scenario_table_edr %>%
  mutate(
    scenario_df_name = paste(delay_scenario,closure_scenario,restriction_scenario,"depth_val_30fathom_remove",sep="_"),
    #season.st.key = "season.st.date.key",
    preseason.days = 3,
    season.st.backstop = "NULL",
    early.data.method = "remove",
    delay.date = ifelse(delay_scenario == "No_Delay", "NULL", delayed.opening.date),
    delay.region = ifelse(delay_scenario == "No_Delay", "NULL",
                          ifelse(substr(delay_scenario,1,5) == "State",
                                 "All",
                                 "CenCA")
    ),
    delay.method = "lag",
    delay.method.fidelity = "spatial",
    closure.date = ifelse(restriction_scenario != "No_Early_Closure", restriction_after_date, "NULL"),
    closure.region = ifelse(restriction_scenario == "No_Early_Closure", "NULL",
                            ifelse(substr(restriction_scenario,1,3) == "Sta", 
                                   "All",
                                   "CenCA"
                            )
    ),
    closure.method = "depth",
    closure.redist.percent = 0,
    depth.shallow = "NULL", 
    depth.deep = as.character(-54.864),
    reduction.before.date = "NULL",
    reduction.before.percent = 50,
    reduction.before.region = "NULL",
    reduction.after.date = "NULL",
    reduction.after.percent = 50,
    reduction.after.region = "NULL",
    reduction.after.redist = "FALSE",
    reduction.after.redist.percent = 0
  ) %>%
  dplyr::select(scenario_df_name, delay_scenario, closure_scenario, #season.st.key,        
                preseason.days, season.st.backstop, early.data.method, delay.date,
                delay.region, delay.method, delay.method.fidelity, closure.date, closure.region,
                closure.method, closure.redist.percent, depth.shallow, depth.deep, reduction.before.date, reduction.before.percent, reduction.before.region, reduction.after.date, reduction.after.percent, reduction.after.region, reduction.after.redist, reduction.after.redist.percent)


# 5th set of scenarios
# reduction.after.percent: 50
# redistribute to norCA if only cenCA closed
# depth.val = -54.864
# redistribute offshore effort onshore by region
scenario_table_5 <- scenario_table_edr %>%
  mutate(
    scenario_df_name = paste(delay_scenario,closure_scenario,restriction_scenario,"reduction_after_percent_50","depth_val_30fathom",sep="_"),
    #season.st.key = "season.st.date.key",
    preseason.days = 3,
    season.st.backstop = "NULL",
    early.data.method = "remove",
    delay.date = ifelse(delay_scenario == "No_Delay", "NULL", delayed.opening.date),
    delay.region = ifelse(delay_scenario == "No_Delay", "NULL",
                          ifelse(substr(delay_scenario,1,5) == "State",
                                 "All",
                                 "CenCA")
    ),
    delay.method = "lag",
    delay.method.fidelity = "spatial",
    closure.date = ifelse(restriction_scenario != "No_Early_Closure", restriction_after_date, "NULL"),
    closure.region = ifelse(restriction_scenario == "No_Early_Closure", "NULL",
                            ifelse(substr(restriction_scenario,1,3) == "Sta", 
                                   "All",
                                   "CenCA"
                            )
    ),
    closure.method = "depth+temporal",
    closure.redist.percent = 100,
    depth.shallow = "NULL", 
    depth.deep = as.character(-54.864),
    reduction.before.date = "NULL",
    reduction.before.percent = 50,
    reduction.before.region = "NULL",
    reduction.after.date = restriction_after_date,
    reduction.after.percent = 50,
    reduction.after.region = ifelse(substr(restriction_scenario,1,3) == "Sta", 
                                    "All",
                                    "CenCA"
    ),
    reduction.after.redist = ifelse(substr(restriction_scenario,1,3) == "Sta", 
                                    "FALSE",
                                    "TRUE"
    ),
    reduction.after.redist.percent = ifelse(substr(restriction_scenario,1,3) == "Sta", 
                                            0,
                                            10
    )
  ) %>%
  dplyr::select(scenario_df_name, delay_scenario, closure_scenario, #season.st.key,        
                preseason.days, season.st.backstop, early.data.method, delay.date,
                delay.region, delay.method, delay.method.fidelity, closure.date, closure.region,
                closure.method, closure.redist.percent, depth.shallow, depth.deep, reduction.before.date, reduction.before.percent, reduction.before.region, reduction.after.date, reduction.after.percent, reduction.after.region, reduction.after.redist, reduction.after.redist.percent)

# 5th set of scenarios - alt
# reduction.after.percent: 50
# remove all effort
# depth.val = -54.864
# remove all offshore effort
scenario_table_5alt <- scenario_table_edr %>%
  mutate(
    scenario_df_name = paste(delay_scenario,closure_scenario,restriction_scenario,"reduction_after_percent_50","depth_val_30fathom_remove",sep="_"),
    #season.st.key = "season.st.date.key",
    preseason.days = 3,
    season.st.backstop = "NULL",
    early.data.method = "remove",
    delay.date = ifelse(delay_scenario == "No_Delay", "NULL", delayed.opening.date),
    delay.region = ifelse(delay_scenario == "No_Delay", "NULL",
                          ifelse(substr(delay_scenario,1,5) == "State",
                                 "All",
                                 "CenCA")
    ),
    delay.method = "lag",
    delay.method.fidelity = "spatial",
    closure.date = ifelse(restriction_scenario != "No_Early_Closure", restriction_after_date, "NULL"),
    closure.region = ifelse(restriction_scenario == "No_Early_Closure", "NULL",
                            ifelse(substr(restriction_scenario,1,3) == "Sta", 
                                   "All",
                                   "CenCA"
                            )
    ),
    closure.method = "depth",
    closure.redist.percent = 0,
    depth.shallow = "NULL", 
    depth.deep = as.character(-54.864),
    reduction.before.date = "NULL",
    reduction.before.percent = 50,
    reduction.before.region = "NULL",
    reduction.after.date = restriction_after_date,
    reduction.after.percent = 50,
    reduction.after.region = ifelse(substr(restriction_scenario,1,3) == "Sta", 
                                    "All",
                                    "CenCA"
    ),
    reduction.after.redist = "FALSE",
    reduction.after.redist.percent = 0
  ) %>%
  dplyr::select(scenario_df_name, delay_scenario, closure_scenario, #season.st.key,        
                preseason.days, season.st.backstop, early.data.method, delay.date,
                delay.region, delay.method, delay.method.fidelity, closure.date, closure.region,
                closure.method, closure.redist.percent, depth.shallow, depth.deep, reduction.before.date, reduction.before.percent, reduction.before.region, reduction.after.date, reduction.after.percent, reduction.after.region, reduction.after.redist, reduction.after.redist.percent)

# scenario 18
# reduction.after.percent: 50
# redistribute to norCA when only cenCA closed
# depth.val = -54.864
# redistribute offshore effort onshore by region
scenario_18 <- scenario_table_edr[2,] %>%
  mutate(
    scenario_df_name = paste(delay_scenario, closure_scenario, restriction_scenario,"reduction_after_percent_50","Statewide","depth_val_30fathom",sep="_"),
    #season.st.key = "season.st.date.key",
    preseason.days = 3,
    season.st.backstop = "NULL",
    early.data.method = "remove",
    delay.date = "NULL",
    delay.region = "NULL",
    delay.method = "lag",
    delay.method.fidelity = "spatial",
    closure.date = restriction_after_date,
    closure.region = "All",
    closure.method = "depth+temporal",
    closure.redist.percent = 100,
    depth.shallow = "NULL", 
    depth.deep = as.character(-54.864),
    reduction.before.date = "NULL",
    reduction.before.percent = 50,
    reduction.before.region = "NULL",
    reduction.after.date = restriction_after_date,
    reduction.after.percent = 50,
    reduction.after.region = "CenCA",
    reduction.after.redist = "TRUE",
    reduction.after.redist.percent = 10
    ) %>%
  dplyr::select(scenario_df_name, delay_scenario, closure_scenario, #season.st.key,        
                preseason.days, season.st.backstop, early.data.method, delay.date,
                delay.region, delay.method, delay.method.fidelity, closure.date, closure.region,
                closure.method, closure.redist.percent, depth.shallow, depth.deep, reduction.before.date, reduction.before.percent, reduction.before.region, reduction.after.date, reduction.after.percent, reduction.after.region, reduction.after.redist, reduction.after.redist.percent)

# scenario 18 - alt
# reduction.after.percent: 50
# remove effort
# depth.val = -54.864
# remove offshore effort
scenario_18alt <- scenario_table_edr[2,] %>%
  mutate(
    scenario_df_name = paste(delay_scenario, closure_scenario, restriction_scenario,"reduction_after_percent_50","Statewide","depth_val_30fathom_remove",sep="_"),
    #season.st.key = "season.st.date.key",
    preseason.days = 3,
    season.st.backstop = "NULL",
    early.data.method = "remove",
    delay.date = "NULL",
    delay.region = "NULL",
    delay.method = "lag",
    delay.method.fidelity = "spatial",
    closure.date = restriction_after_date,
    closure.region = "All",
    closure.method = "depth",
    closure.redist.percent = 0,
    depth.shallow = "NULL", 
    depth.deep = as.character(-54.864),
    reduction.before.date = "NULL",
    reduction.before.percent = 50,
    reduction.before.region = "NULL",
    reduction.after.date = restriction_after_date,
    reduction.after.percent = 50,
    reduction.after.region = "CenCA",
    reduction.after.redist = "FALSE",
    reduction.after.redist.percent = 0
  ) %>%
  dplyr::select(scenario_df_name, delay_scenario, closure_scenario, #season.st.key,        
                preseason.days, season.st.backstop, early.data.method, delay.date,
                delay.region, delay.method, delay.method.fidelity, closure.date, closure.region,
                closure.method, closure.redist.percent, depth.shallow, depth.deep, reduction.before.date, reduction.before.percent, reduction.before.region, reduction.after.date, reduction.after.percent, reduction.after.region, reduction.after.redist, reduction.after.redist.percent)


scenario_table_edr <- bind_rows(list(scenario_table_3, scenario_table_4, scenario_table_5, scenario_18,
                                     scenario_table_3alt, scenario_table_4alt, scenario_table_5alt, scenario_18alt))

write_rds(scenario_table_edr, here::here(
  "tradeoffs",
  "Management scenarios",
  "scenario_table_effort_depth_restrictions.RDS"
)
)

write_csv(scenario_table_edr, here::here(
  "tradeoffs",
  "Management scenarios",
  "scenario_table_effort_depth_restrictions.csv"
)
)

########### STOPPED HERE 052720 ###############


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
