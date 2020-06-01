library(tidyverse)
library(viridis)
library(ggrepel)

####################################################################
####################################################################

# 052920, 060120

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

scenario_table_5 <- read_rds(here::here(
  "tradeoffs",
  "Management scenarios",
  "scenario_table_effort_comparison.RDS"
  )
)

unique(tradeoff_df_effort_shift_scenariocomparisons_n$scenario_df_name) == unique(annual_statewide_df_n$scenario_df_name)

unique(scenario_table_5$scenario_df_name) == unique(annual_statewide_df_n$scenario_df_name)
unique(sort(scenario_table_5$scenario_df_name)) == unique(tradeoff_df_effort_shift_scenariocomparisons_n$scenario_df_name)

View(cbind(
  sort(unique(scenario_table_5$scenario_df_name)), 
  unique(tradeoff_df_effort_shift_scenariocomparisons_n$scenario_df_name)
  )
)

# consider mean_relative_hump_risk_n, which is the expected reduction in risk relative to sq for each scenario
tradeoff_df_annual <- tradeoff_df_effort_shift_scenariocomparisons_n %>% 
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
  left_join(scenario_table_5) %>%
  arrange(
    mean_relative_hump_risk_n,
    mean_relative_blwh_risk_n,
    mean_relative_dollars
  ) %>%
  mutate(
    scenario_category = c(
      "Status Quo",
      rep("CenCA_Early_Closure",3),
      rep("CenCA_Delay",4),
      rep("CenCA_Early_Closure_And_Delay",2),
      rep("CenCA_Early_Closure",3),
      "CenCA_Early_Closure_And_Delay",
      rep("CenCA_Delay",2),
      rep("CenCA_Early_Closure_And_Delay",3)
    )
  )

View(tradeoff_df_annual)

# pull out scenario components of interest, starting with remove/pile/lag and then with temporal/spatial fidelity. make a plot, facet by closure.method. drop SQ

ggplot(tradeoff_df_annual[-1,]) +
  geom_bar(aes(
    x = scenario_category,
    y = mean_relative_hump_risk_n,
    fill = delay.method
  ),
  stat="identity",
  position = position_dodge(width = 0.8)
  ) +
  #coord_flip() +
  facet_wrap(~closure.method) +
  theme_bw()

ggplot(tradeoff_df_annual[-1,]) +
  geom_bar(aes(
    x = scenario_category,
    y = mean_relative_blwh_risk_n,
    fill = delay.method
  ),
  stat="identity",
  position = position_dodge(width = 0.8)
  ) +
  #coord_flip() +
  facet_wrap(~closure.method) +
  theme_bw()

ggplot(tradeoff_df_annual[-1,]) +
  geom_bar(aes(
    x = scenario_category,
    y = mean_relative_dollars,
    fill = delay.method
  ),
  stat="identity",
  position = position_dodge(width = 0.8)
  ) +
  #coord_flip() +
  facet_wrap(~closure.method) +
  theme_bw()

# Conclusion: with scenario of interest = temporal for closure method:
# rank risk reduction greatest for humps with remove early, then with lag early, then pile early.
# rank risk reduction greatest for blues with remove early, then with pile early, then lag early.
# rank $ changes don't vary much between pile and lag methods

# Conclusion: with scenario of interest = remove for closure method:
# rank risk reduction greatest for humps with remove early, then with lag early, then pile early.
# rank risk reduction greatest for blues with remove early, then depends on what scenario for whether pile or lag led to greater reductions.
# rank $ changes don't vary much between pile and lag methods


