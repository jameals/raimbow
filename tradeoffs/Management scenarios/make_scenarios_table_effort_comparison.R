# Compare the effects of:
# 1. CenCA delay (a) none, (b) remove, (c) pile up, (d) lag
# 2. CenCA early closure, (a) none, (b) remove, (c) displace
# 3. [1a,2a], [1a, 2b], [1a, 2c], [1b, 2a], [1b, 2b], [1b, 2c], [1c,2a], [1c, 2b], [1c, 2c], [1d, 2a], [1d, 2b], [1d, 2c]

library(tidyverse)

# set delayed opening and early closure dates for scenarios
delayed.opening.date <- "2009-12-15"
early.closure.date <- "2010-04-01"

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

# eliminate all status quo scenarios
scenario_table_effort_comparison <- scenario_table_effort_comparison[
-which(scenario_table_effort_comparison$delay_scenario == "No_Delay" & 
        scenario_table_effort_comparison$closure_scenario == "No_Early_Closure"
      ), ]

# add back in status quo scenario
scenario_table_effort_comparison <- scenario_table_effort_comparison %>%
  bind_rows(data.frame(
    delay_scenario="No_Delay", closure_scenario="No_Early_Closure", delay.method="NULL",closure.method="NULL"
  )
  )


# set delayed opening and early closure dates for scenarios
delayed.opening.date <- "2009-12-15"
early.closure.date <- "2010-04-01"

# next step: create if_else statements to populate all relevant columns

# 5th set of scenarios: just to compare effort redistribution

# early.data.method: set as remove for all scenarios unless asked to do otherwise
# delay.date: NULL or Dec 15
# delay.region: NULL or "CenCA"
# delay.method: "pile","remove", "lag" 
# delay.method.fidelity: "spatial"
# closure.date: NULL or Apr 1
# closure.region: NULL, "CenCA"
# closure.method: set as "temporal" but compare to "remove"
# closure.redist.percent: 100

# delay.method.fidelity: "spatial"
# closure.redist.percent: 100
scenario_table_5 <- scenario_table_effort_comparison %>%
  mutate(
    scenario_df_name = paste(delay_scenario,closure_scenario,delay.method,closure.method,sep="_"),
    early.data.method = "remove",
    delay.date = ifelse(delay_scenario == "No_Delay", "NULL", delayed.opening.date),
    delay.region = ifelse(delay_scenario == "No_Delay", "NULL", "CenCA"),
    delay.method.fidelity = "spatial",
    closure.date = ifelse(closure_scenario != "No_Early_Closure", early.closure.date, "NULL"),
    closure.region = ifelse(closure_scenario == "No_Early_Closure", "NULL",
                           "CenCA"),
    closure.redist.percent = 100
  ) %>%
  dplyr::select(scenario_df_name, delay_scenario, closure_scenario, early.data.method, delay.date,
                delay.region, delay.method, delay.method.fidelity, closure.date, closure.region, 
                closure.method, closure.redist.percent)


write_rds(scenario_table_5, here::here(
  "tradeoffs",
  "Management scenarios",
  "scenario_table_effort_comparison.RDS"
)
)

