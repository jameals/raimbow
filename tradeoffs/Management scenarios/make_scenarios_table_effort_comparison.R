### Make the scenario combinations 
delay_scenarios_effort_comparison <- c(
  "No_Delay",
  "CenCA_Marine_Life_Delay"
)

closure_scenarios_effort_comparison <- c(
  "No_Early_Closure",
  "CenCA_Early_Closure"
)

delay_methods_effort_comparison <- c(
  "Pile",
  "Lag",
  "Remove"
)

closure_methods_effort_comparison <- c(
  "Temporal",
  "Remove"
)

scenario_table_effort_comparison <- expand.grid(
  "delay_scenario" = delay_scenarios_effort_comparison,
  "closure_scenario" = closure_scenarios_effort_comparison,
  "delay.method" = delay_methods_effort_comparison,
  "closure.method" = closure_methods_effort_comparison
) 

# drop scenarios that don't include delays or early closures
scenario_table_effort_comparison <- scenario_table_effort_comparison %>%
  filter(
    delay_scenario == "No_Delay" & closure_scenario == "No_Early_Closure"
  )

# set delayed opening and early closure dates for scenarios
delayed.opening.date <- "2009-12-15"
early.closure.date <- "2010-04-01"

# 5th set of scenarios: just to compare effort redistribution

# early.data.method: set as remove for all scenarios unless asked to do otherwise
# delay.date: NULL or Dec 15
# delay.region: NULL or "CenCA"
# delay.method: "pile" or "remove". "lag" scenario captured above
# delay.method.fidelity: develop 2 complete sets of scenarios, 1 with this setting as "spatial" and 1 as "temporal". JS preferred / main text scenario setting is "spatial"
# closure.date: NULL or Apr 1
# closure.region: NULL, "All", "CenCA", "BIA"
# closure.method: set as "temporal" but compare to "remove"; must be "remove" when closure.region is "All"
# closure.redist.percent: develop 2 complete sets of scenarios, 1 with this setting as 100 and 1 as 10. JS preferred / main text scenario setting is 10 for closure.region== "CenCA", 100 for closure.region=="BI

# delay.method.fidelity: "spatial"
# closure.redist.percent: 100
scenario_table_5 <- scenario_table %>%
  mutate(
    scenario_df_name = paste(delay_scenario,closure_scenario,"delay_method_fidelity_spatial","closure_redist_percent_100",sep="_"),
    early.data.method = "remove",
    delay.date = ifelse(delay_scenario == "No_Delay", "NULL", delayed.opening.date),
    delay.region = ifelse(delay_scenario == "No_Delay", "NULL", "CenCA"),
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
    closure.redist.percent = 100
  ) %>%
  dplyr::select(scenario_df_name, delay_scenario, closure_scenario, early.data.method, delay.date,
                delay.region, delay.method, delay.method.fidelity, closure.date, closure.region, 
                closure.method, closure.redist.percent)

