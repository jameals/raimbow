# 040720
# Make tradeoff data frames

# developed from "Simple early closure analysis.Rmd"
# Includes output variables relative risk and relative revenue reduction

library(tidyverse)
library(magrittr)


####################################################################
####################################################################

#050520

# the SW functions effort_mgmt() and risk_mgmt() together generate summary df's with the metrics below for time-areas open to fishing

# column headers for output df from effort_mgmt() and risk_mgmt(): 
# year_month (2009:2019, 11:12 and 1:7), crab_year (2009-10 to 2018-19), Region, DCRB_lbs, DCRB_rev, Num_DCRB_VMS_pings, Num_DCRB_Vessels, Num_Unique_DCRB_Vessels, risk_humpback, risk_blue
# DCRB_lbs, # total pounds crab landed in areas and times open to fishing
# DCRB_rev, # total $ crab landed in areas and times open to fishing
# Num_DCRB_VMS_pings, # Num_DCRB_VMS_pings in areas and times open to fishing
# Num_DCRB_Vessels, # total crab vessel days in areas and times open to fishing
# Num_Unique_DCRB_Vessels, # sum of unique crab vessels per 5km grid cell in areas and times open to fishing
# risk_humpback = Humpback_dens_mean * effort_val (defined in risk_mgmt() function) in areas and times open to fishing
# risk_blue = Blue_occurrence_mean * effort_val (defined in risk_mgmt() function) in areas and times open to fishing

# metadata for scenarios in scenario_table.RDS, see make_scenarios_table.R for descriptions: "scenario_df_name"       "delay_scenario"         "closure_scenario"       "early.data.method"      "delay.date", "delay.region", "delay.method.shift"     "delay.method.fidelity"  "closure.date"           "closure.region"         "closure.method"         "closure.redist.percent"


####################################################################
####################################################################

# read in the relevant data
#load("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/scenario_output_dataframes/scenario.output.df.noinfo_with.risk2020-05-05.RData")

# scenario_table <- read_rds(here::here(
#   "tradeoffs",
#   "Management scenarios",
#   "scenario_table.RDS"
#   )
#   )
#scenario_table

####################################################################
####################################################################

####################################################################
####################################################################

# make a function to create a df that reveals tradeoffs under each scenario

# response variables are calculated in terms of relative reduction: relative risk reduction to whales and relative revenue reduction to DCRB
# if max $ is X, then for each year we want $ in that year relative to max, and scaled to a %
# if max risk is Y, then for relative risk we want max risk to be zero and complete closure to be 100% reduction
# all relative metrics are relative to the year under consideration if both winter and spring DCRB season were open. based on revised approach 012420

# column headers for output df from effort_mgmt() and risk_mgmt(): 
# year_month (2009:2019, 11:12 and 1:7), crab_year (2009-10 to 2018-19), Region, DCRB_lbs, DCRB_rev, Num_DCRB_VMS_pings, Num_DCRB_Vessels, Num_Unique_DCRB_Vessels, risk_humpback, risk_blue

# 050520
# 050820

# may want to add functionality later using sym(), enquo(), etc. for scenario names, DCRB metrics, hump_risk_metric, blwh_risk_metric, pings_metric

tradeoff_df_function <- function(risk_list, scenario_names_table, annual_statewide_df_name, df_tradeoff_name) { 

  
  #risk_list <- risk_out_list
  #scenario_names_table <- scenario_table
  #hump_risk_metric <- risk_humpback
  #blwh_risk_metric <- risk_blue
  #pings_metric <- Num_DCRB_VMS_pings

  ### STEP 1
  
  # for each scenario df, (1) sum by crab_year (over year_month and Region) to get annual values for each crab_year, (2) join to metadata for scenario

  #browser()
  
  if (length(risk_list) != nrow(scenario_names_table))
    stop("x must contain the same number of scenarios:\n", 
         paste(c("risk_out_list", "scenario_table"), collapse = ", "))
  
  #browser()
  
  annual_statewide_df_internal <- purrr::map_df(1:nrow(scenario_names_table), function(i){
    df <- risk_list[[i]]  
    df %>%
      group_by(crab_year) %>%
      summarise(
        DCRB_lbs = sum(DCRB_lbs, na.rm=TRUE),
        DCRB_rev = sum(DCRB_rev, na.rm=TRUE),
        Num_DCRB_Vessels = sum(Num_DCRB_Vessels, na.rm=TRUE),
        
        Num_DCRB_VMS_pings = sum(Num_DCRB_VMS_pings, na.rm=TRUE),
        risk_humpback = sum(risk_humpback, na.rm=TRUE),
        risk_blue = sum(risk_blue, na.rm=TRUE)
      ) %>%
      add_column(
        scenario_names_table[i,]
      )
  })
  
  #browser()
  assign(annual_statewide_df_name, annual_statewide_df_internal, envir=.GlobalEnv)
  
  write_rds(annual_statewide_df_internal, paste0("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/annual_statewide_scenario_ouputs_",today(),".rds"))

# annual_statewide_df <- c()
# 
# for (i in 1:nrow(scenario_table)) # for testing. nrow(scenario_table[1:3,])
#   { 
#   
#   print(paste("Summarizing annual data for Scenario", i))
# 
#   annual_statewide_df_tmp <- risk_out_list[[i]] %>%
#     group_by(crab_year) %>%
#     summarise(
#       DCRB_lbs = sum(DCRB_lbs, na.rm=TRUE), 
#       DCRB_rev = sum(DCRB_rev, na.rm=TRUE), 
#       Num_DCRB_VMS_pings = sum(Num_DCRB_VMS_pings, na.rm=TRUE), 
#       Num_DCRB_Vessels = sum(Num_DCRB_Vessels, na.rm=TRUE), 
#       risk_humpback = sum(risk_humpback, na.rm=TRUE), 
#       risk_blue = sum(risk_blue, na.rm=TRUE)
#       ) %>%
#     add_column(
#       scenario_table[i,]
#       )
# 
#   annual_statewide_df %<>% bind_rows(annual_statewide_df_tmp)
#   
#   }
# 


# implement mutate for rel risk
# apply lines 115-178 to get relative change in risk

  ### STEP 2
  
  # make tradeoff df
  
  #browser()
  
  df_tradeoff_internal <- annual_statewide_df %>%
    group_by(crab_year) %>%
    
    # STATUS QUO
    # define status quo values for outputs of interest for crab.year (note status quo value should be the same for each scenario)
    mutate(
      hump_risk_under_statusquo = risk_humpback[which(scenario_df_name == "No_Delay_No_Early_Closure_delay_method_fidelity_spatial_closure_redist_percent_100")],
      blwh_risk_under_statusquo = risk_blue[which(scenario_df_name == "No_Delay_No_Early_Closure_delay_method_fidelity_spatial_closure_redist_percent_100")],
      pings_under_statusquo = Num_DCRB_VMS_pings[which(scenario_df_name == "No_Delay_No_Early_Closure_delay_method_fidelity_spatial_closure_redist_percent_100")],
      dollars_under_statusquo = DCRB_rev[which(scenario_df_name == "No_Delay_No_Early_Closure_delay_method_fidelity_spatial_closure_redist_percent_100")],
      pounds_under_statusquo = DCRB_lbs[which(scenario_df_name == "No_Delay_No_Early_Closure_delay_method_fidelity_spatial_closure_redist_percent_100")]
    ) %>%
    
    ungroup() %>%
    
    group_by(scenario_df_name, crab_year) %>%
    
    summarise(
      relative_hump_risk = 100 *  (1 - (
        risk_humpback / hump_risk_under_statusquo
      )),
      relative_blwh_risk = 100 *  (1 - (
        risk_blue / blwh_risk_under_statusquo
      )),
      relative_pings = 100 *  (1 - (
        Num_DCRB_VMS_pings / pings_under_statusquo
      )),
      relative_dollars = 100* (
        DCRB_rev / dollars_under_statusquo
      ),
      relative_pounds = 100* (
        DCRB_lbs / pounds_under_statusquo
      )
    ) #%>%
    
    # mutate(
    #   Scenario = unique(scenario_df_name)
    # )
  
  #browser()
  assign(df_tradeoff_name, df_tradeoff_internal, envir=.GlobalEnv)

  write_rds(df_tradeoff_internal, paste0("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/tradeoff_df_",today(),".rds"))

}


# JS stopped here 0740 040820. need to start at line 102 and try function with a toy df. 
# 041320 also need to make sure grouping by crab.year deals with month stuff 

# inputs to function: hump_risk_metric (original or normalized), blwh_risk_metric (original or normalized),  pings_metric (original or normalized),  
# tradeoff_df_function <- function(hump_risk_metric, blwh_risk_metric, pings_metric) 
#   {
#   df.tradeoff <- output_df_from_sw %>%
#     group_by(crab.year) %>%
#       
#       # STATUS QUO
#       # define status quo values for outputs of interest for crab.year (note status quo value should be the same for each scenario)
#       mutate(
#         hump_risk_under_statusquo = hump_risk_metric[which(
#           is.na(delay_time_scenario) == TRUE &
#             is.na(delay_domain_scenario) == TRUE &
#             is.na(closure_time_scenario) == TRUE &
#             is.na(closure_domain_scenario) == TRUE &
#             is.na(delay_approach) == TRUE &
#             is.na(delay_redistribution) == TRUE
#         )],
#         blwh_risk_under_statusquo = blwh_risk_metric[which(
#           is.na(delay_time_scenario) == TRUE &
#             is.na(delay_domain_scenario) == TRUE &
#             is.na(closure_time_scenario) == TRUE &
#             is.na(closure_domain_scenario) == TRUE &
#             is.na(delay_approach) == TRUE &
#             is.na(delay_redistribution) == TRUE
#         )],
#         pings_under_statusquo = pings_metric[which(
#           is.na(delay_time_scenario) == TRUE &
#             is.na(delay_domain_scenario) == TRUE &
#             is.na(closure_time_scenario) == TRUE &
#             is.na(closure_domain_scenario) == TRUE &
#             is.na(delay_approach) == TRUE &
#             is.na(delay_redistribution) == TRUE
#         )],
#         dollars_under_statusquo = sum_DCRB_rev[which(
#           is.na(delay_time_scenario) == TRUE &
#             is.na(delay_domain_scenario) == TRUE &
#             is.na(closure_time_scenario) == TRUE &
#             is.na(closure_domain_scenario) == TRUE &
#             is.na(delay_approach) == TRUE &
#             is.na(delay_redistribution) == TRUE
#         )],
#         pounds_under_statusquo = sum_DCRB_lbs[which(
#           is.na(delay_time_scenario) == TRUE &
#             is.na(delay_domain_scenario) == TRUE &
#             is.na(closure_time_scenario) == TRUE &
#             is.na(closure_domain_scenario) == TRUE &
#             is.na(delay_approach) == TRUE &
#             is.na(delay_redistribution) == TRUE
#         )]
#       ) %>%
#       ungroup() %>%
#       group_by(full_scenario_ID, crab.year) %>%
#       summarise(
#         relative_hump_risk = 100 *  (1 - (
#           hump_risk_metric / hump_risk_under_statusquo
#         )),
#         relative_blwh_risk = 100 *  (1 - (
#           blwh_risk_metric / blwh_risk_under_statusquo
#         )),
#         relative_pings = 100 *  (1 - (
#           pings_metric / pings_under_statusquo
#         )),
#         relative_dollars = 100* (
#           sum_DCRB_rev / dollars_under_statusquo
#         ),
#         relative_pounds = 100* (
#           sum_DCRB_lbs / pounds_under_statusquo
#         )
#       ) %>%
#       mutate(
#         Scenario = unique(full_scenario_ID)
#       )
#   df.tradeoff
#   
#   }

####################################################################
####################################################################




# ####################################################################
# ####################################################################
# 
# #041320
# 
# # I am thinking we want to generate summary df's with the metrics below for time-areas open to fishing, and then a separate set of summary df's with these metrics for time-areas closed to fishing
# 
# # column headers for output df from SW functions: 
# # year(2009:2019), crab_year (2009-10 to 2018-19), month, full_scenario_ID, delay_time_scenario (NA or Dec-15), delay_domain_scenario (NA, State, CenCA), closure_time_scenario (NA or Spring-Summer), closure_domain_scenario (NA, Sta, Cen, BIA), delay_approach (lag or pile up), delay_redistribution (cell fidelity or temporal fidelity)
# 
# # DCRB fishing activity
# # sum_DCRB_lbs = sum(DCRB_lbs), # total pounds crab landed in areas and times open to fishing
# # sum_DCRB_rev = sum(sum_DCRB_rev), # total $ crab landed in areas and times open to fishing
# # normalized_Num_DCRB_VMS_pings = , # Num_DCRB_VMS_pings per 5km grid cell in areas and times open to fishing, with pings normalized to 0-1
# # sum_Num_DCRB_VMS_pings = sum(Num_DCRB_VMS_pings), # total crab VMS pings in areas and times open to fishing
# # sum_Num_DCRB_Vessels = sum(Num_DCRB_Vessels), # total crab vessel days in areas and times open to fishing
# # mean_Num_Unique_DCRB_Vessels = mean(Num_Unique_DCRB_Vessels), # mean unique crab vessels per 5km grid cell in areas and times open to fishing
# # mean_normalized_Num_DCRB_VMS_pings = mean(normalized_Num_DCRB_VMS_pings), # mean crab VMS pings per 5km grid cell in areas and times open to fishing, with pings normalized to 0-1
# # sd_normalized_Num_DCRB_VMS_pings = sd(normalized_Num_DCRB_VMS_pings), # standard deviation crab VMS pings per 5km grid cell in areas and times open to fishing, with pings normalized to 0-1
# # sum_normalized_Num_DCRB_VMS_pings = sum(normalized_Num_DCRB_VMS_pings), # sum crab VMS pings per 5km grid cell in areas and times open to fishing, with pings normalized to 0-1
# 
# # blue whales
# # Blue_occurrence_mean = mean(Blue_occurrence_mean), # mean probability of blue whale occurrence per 5km grid cell in areas and times open to fishing. note we do not need to retain normalized_Blue_occurrence_mean because it is already scaled 0-1
# # Blue_occurrence_sd = sd(Blue_occurrence_mean), # standard deviation in probability of blue whale occurrence per 5km grid cell in areas and times open to fishing. need to think more about how to do this. StDev for each cell each month based on daily/bidaily predictions but StDev for each region based on monthly means within each cell,
# 
# # humpback whales
# # H_Avg_Abund = mean(H_Avg_Abund), # mean humpback whale abundance per 5km grid cell in areas and times open to fishing,
# # mean_normalized_H_Avg_Abund = mean(normalized_H_Avg_Abund), # mean humpback whale abundance per 5km grid cell in areas and times open to fishing, with humpback abundance normalized to 0-1
# # H_Avg_Abund_sd = sd(H_Avg_Abund), # standard deviation in predicted hump abundance per 5km grid cell in areas and times open to fishing. need to think more about how to do this. StDev for each cell each month based on daily/bidaily predictions but StDev for each region based on monthly means within each cell,
# # mean_normalized_H_Avg_Density = mean(normalized_H_Avg_Abund / 5km_grid_cell_area), # mean humpback whale density per 5km grid cell in areas and times open to fishing, with humpback abundance normalized to 0-1
# # sd_normalized_H_Avg_Density = sd(normalized_H_Avg_Abund / 5km_grid_cell_area), # standard deviation in predicted humpback whale density per 5km grid cell in areas and times open to fishing. need to think more about how to do this. StDev for each cell each month based on daily/bidaily predictions but StDev for each region based on monthly means within each cell,
# 
# # blue whale risk metrics
# # mean_blue_risk = mean(Blue_occurrence_mean * Num_DCRB_VMS_pings), # mean per 5km grid cell of the product of [Num_DCRB_VMS_pings and Blue_occurrence_mean], in areas and times open to fishing
# # sum_blue_risk = sum(Blue_occurrence_mean * Num_DCRB_VMS_pings), # sum of the product of [Num_DCRB_VMS_pings and Blue_occurrence_mean] in areas and times open to fishing
# # mean_blue_risk_density = mean((Blue_occurrence_mean * Num_DCRB_VMS_pings)/ 5km_grid_cell_area), # mean per 5km grid cell of the product of [Num_DCRB_VMS_pings and Blue_occurrence_mean], divided by area of 5km grid cell, in areas and times open to fishing
# # sum_blue_risk_density = sum((Blue_occurrence_mean * Num_DCRB_VMS_pings)/ 5km_grid_cell_area), # sum of the product of [Num_DCRB_VMS_pings and Blue_occurrence_mean], divided by area of 5km grid cell, in areas and times open to fishing
# # mean_normalized_blue_risk = mean(Blue_occurrence_mean *normalized_Num_DCRB_VMS_pings), # mean per 5km grid cell of the product of [normalized_Num_DCRB_VMS_pings and Blue_occurrence_mean], in areas and times open to fishing
# # sum_normalized_blue_risk = sum(Blue_occurrence_mean *normalized_Num_DCRB_VMS_pings), # sum of the product of [normalized_Num_DCRB_VMS_pings and Blue_occurrence_mean], in areas and times open to fishing
# # mean_normalized_blue_risk_density = mean((Blue_occurrence_mean *normalized_Num_DCRB_VMS_pings)/ 5km_grid_cell_area), # mean per 5km grid cell of the product of [normalized_Num_DCRB_VMS_pings and Blue_occurrence_mean], divided by area of 5km grid cell, in areas and times open to fishing
# # sum_normalized_blue_risk_density = sum((Blue_occurrence_mean *normalized_Num_DCRB_VMS_pings)/ 5km_grid_cell_area), # sum of the product of [normalized_Num_DCRB_VMS_pings and Blue_occurrence_mean], divided by area of 5km grid cell, in areas and times open to fishing
# 
# # humpback whale risk metrics
# # mean_hump_risk = mean(H_Avg_Abund * Num_DCRB_VMS_pings), # mean per 5km grid cell of the product of [Num_DCRB_VMS_pings and H_Avg_Abund], in areas and times open to fishing
# # sum_hump_risk = sum(H_Avg_Abund * Num_DCRB_VMS_pings), # sum of the product of [Num_DCRB_VMS_pings and H_Avg_Abund], in areas and times open to fishing
# # mean_hump_risk_density = mean((H_Avg_Abund * Num_DCRB_VMS_pings) / area_of_5kmgridcell), # mean per 5km grid cell of the product of [Num_DCRB_VMS_pings and H_Avg_Abund], divided by area of 5km grid cell, in areas and times open to fishing
# # sum_hump_risk_density = sum((H_Avg_Abund * Num_DCRB_VMS_pings) / area_of_5kmgridcell), # sum of the product of [Num_DCRB_VMS_pings and H_Avg_Abund], divided by area of 5km grid cell, in areas and times open to fishing
# # mean_normalized_hump_risk = mean(normalized_H_Avg_Abund * normalized_Num_DCRB_VMS_pings), # mean per 5km grid cell of the product of [normalized_Num_DCRB_VMS_pings and normalized_H_Avg_Abund], in areas and times open to fishing
# # sum_normalized_hump_risk = sum(normalized_H_Avg_Abund * normalized_Num_DCRB_VMS_pings), # sum of the product of [normalized_Num_DCRB_VMS_pings and normalized_H_Avg_Abund], in areas and times open to fishing
# # mean_normalized_hump_risk_density = mean((normalized_H_Avg_Abund * normalized_Num_DCRB_VMS_pings) / area_of_5kmgridcell), # mean per 5km grid cell of the product of [normalized_Num_DCRB_VMS_pings and normalized_H_Avg_Abund], divided by area of 5km grid cell, in areas and times open to fishing
# # sum_normalized_hump_risk_density = sum((normalized_H_Avg_Abund * normalized_Num_DCRB_VMS_pings) / area_of_5kmgridcell), # sum of the product of [normalized_Num_DCRB_VMS_pings and normalized_H_Avg_Abund], divided by area of 5km grid cell, in areas and times open to fishing
# 
# ####################################################################
# ####################################################################
# 
# ####################################################################
# ####################################################################
# 
# # ok if output looks something like what is described above, make a function to create a df that reveals tradeoffs under each scenario
# 
# # response variables are calculated in terms of relative reduction: relative risk reduction to whales and relative revenue reduction to DCRB
# # if max $ is X, then for each year we want $ in that year relative to max, and scaled to a %
# # if max risk is Y, then for relative risk we want max risk to be zero and complete closure to be 100% reduction
# # all relative metrics are relative to the year under consideration if both winter and spring DCRB season were open. based on revised approach 012420
# 
# # column headers for output df from SW functions: 
# # year(2009:2019), crab_year (2009-10 to 2018-19), month, full_scenario_ID, delay_time_scenario (NA or Dec-15), delay_domain_scenario (NA, State, CenCA), closure_time_scenario (NA or Spring-Summer), closure_domain_scenario (NA, Sta, Cen, BIA), delay_approach (NA, lag, pile up), delay_redistribution (NA, cell fidelity, temporal fidelity)
# 
# # JS stopped here 0740 040820. need to start at line 102 and try function with a toy df. 
# # 041320 also need to make sure grouping by crab.year deals with month stuff 
# 
# # inputs to function: hump_risk_metric (original or normalized), blwh_risk_metric (original or normalized),  pings_metric (original or normalized),  
# tradeoff_df_function <- function(hump_risk_metric, blwh_risk_metric, pings_metric) 
# {
#   df.tradeoff <- output_df_from_sw %>%
#     group_by(crab.year) %>%
#     # define status quo values for outputs of interest for crab.year (note status quo value should be the same for each scenario)
#     mutate(
#       hump_risk_under_statusquo = hump_risk_metric[which(
#         is.na(delay_time_scenario) == TRUE &
#           is.na(delay_domain_scenario) == TRUE &
#           is.na(closure_time_scenario) == TRUE &
#           is.na(closure_domain_scenario) == TRUE &
#           is.na(delay_approach) == TRUE &
#           is.na(delay_redistribution) == TRUE
#       )],
#       blwh_risk_under_statusquo = blwh_risk_metric[which(
#         is.na(delay_time_scenario) == TRUE &
#           is.na(delay_domain_scenario) == TRUE &
#           is.na(closure_time_scenario) == TRUE &
#           is.na(closure_domain_scenario) == TRUE &
#           is.na(delay_approach) == TRUE &
#           is.na(delay_redistribution) == TRUE
#       )],
#       pings_under_statusquo = pings_metric[which(
#         is.na(delay_time_scenario) == TRUE &
#           is.na(delay_domain_scenario) == TRUE &
#           is.na(closure_time_scenario) == TRUE &
#           is.na(closure_domain_scenario) == TRUE &
#           is.na(delay_approach) == TRUE &
#           is.na(delay_redistribution) == TRUE
#       )],
#       dollars_under_statusquo = sum_DCRB_rev[which(
#         is.na(delay_time_scenario) == TRUE &
#           is.na(delay_domain_scenario) == TRUE &
#           is.na(closure_time_scenario) == TRUE &
#           is.na(closure_domain_scenario) == TRUE &
#           is.na(delay_approach) == TRUE &
#           is.na(delay_redistribution) == TRUE
#       )],
#       pounds_under_statusquo = sum_DCRB_lbs[which(
#         is.na(delay_time_scenario) == TRUE &
#           is.na(delay_domain_scenario) == TRUE &
#           is.na(closure_time_scenario) == TRUE &
#           is.na(closure_domain_scenario) == TRUE &
#           is.na(delay_approach) == TRUE &
#           is.na(delay_redistribution) == TRUE
#       )]
#     ) %>%
#     ungroup() %>%
#     group_by(full_scenario_ID, crab.year) %>%
#     summarise(
#       relative_hump_risk = 100 *  (1 - (
#         hump_risk_metric / hump_risk_under_statusquo
#       )),
#       relative_blwh_risk = 100 *  (1 - (
#         blwh_risk_metric / blwh_risk_under_statusquo
#       )),
#       relative_pings = 100 *  (1 - (
#         pings_metric / pings_under_statusquo
#       )),
#       relative_dollars = 100* (
#         sum_DCRB_rev / dollars_under_statusquo
#       ),
#       relative_pounds = 100* (
#         sum_DCRB_lbs / pounds_under_statusquo
#       )
#     ) %>%
#     mutate(
#       Scenario = unique(full_scenario_ID)
#     )
#   df.tradeoff
#   
# }
# 
# ####################################################################
# ####################################################################


####################################################################
####################################################################


# make annual mean relative risk and relative impact to fishery

# df.tradeoff.annualmeans <- df.tradeoff %>%
#   group_by(Scenario, Scenario_long) %>%
#   summarise(relative_hump_risk = mean(relative_hump_risk),
#             relative_blwh_risk = mean(relative_blwh_risk),
#             relative_pings = mean(relative_pings),
#             relative_dollars = mean(relative_dollars)
#             relative_pounds = mean(relative_pounds)
#   )


####################################################################
####################################################################


##################################
# chunk below used for 12/20/19 # 
# comparisons are relative to the season/time of closure

# risk.df.mean_by_year_prepostApr_2009_2018_CAwide <- risk.df.mean_by_year_prepostApr_2009_2018 %>%
#   filter(B_or_A_April1 == "April 1 and After") %>%
#   group_by(crab.year) %>%
#   summarise(
#     num_5km_grid_cells = n(),
#     sum_H_Avg_Abund = sum(mean_H_Avg_Abund, na.rm=TRUE),
#     mean_H_Avg_Abund = mean(mean_H_Avg_Abund, na.rm=TRUE),
#     sd_H_Avg_Abund = sd(mean_H_Avg_Abund, na.rm=TRUE),
#     #mean_Hump_risk_pings = mean(mean_Hump_risk_pings,na.rm=TRUE),
#     sum_Hump_risk_pings = sum(sum_Hump_risk_pings,na.rm=TRUE),
#     sd_mean_Hump_risk_pings = sd(mean_Hump_risk_pings,na.rm=TRUE),
#     # mean_Hump_risk_vessels = mean(Hump_risk_vessels,na.rm=TRUE),
#     # mean_Hump_risk_vessels = mean(Hump_risk_vessels,na.rm=TRUE),
#     # mean_Hump_risk_lbs_per_vessel = mean(Hump_risk_lbs_per_vessel,na.rm=TRUE),
#     # mean_Hump_risk_dollars_per_vessel = mean(Hump_risk_dollars_per_vessel,na.rm=TRUE),
#     
#     mean_Blue_occurrence = mean(mean_Blue_occurrence, na.rm=TRUE),
#     sd_mean_Blue_occurrence = sd(mean_Blue_occurrence, na.rm=TRUE),
#     # mean_Blue_risk_lbs = mean(Blue_risk_lbs,na.rm=TRUE),
#     # mean_Blue_risk_dollars = mean(Blue_risk_dollars,na.rm=TRUE),
#     #mean_Blue_risk_pings = mean(mean_Blue_risk_pings,na.rm=TRUE),
#     sum_Blue_risk_pings = sum(sum_Blue_risk_pings,na.rm=TRUE),
#     sd_mean_Blue_risk_pings = sd(mean_Blue_risk_pings,na.rm=TRUE),
#     # mean_Blue_risk_vessels = mean(Blue_risk_vessels,na.rm=TRUE),
#     # mean_Blue_risk_lbs_per_vessel = mean(Blue_risk_lbs_per_vessel,na.rm=TRUE),
#     # mean_Blue_risk_dollars_per_vessel = mean(Blue_risk_dollars_per_vessel,na.rm=TRUE),
#     
#     sum_lbs_DCRB = sum(sum_lbs_DCRB,na.rm=TRUE),
#     mean_lbs_DCRB = mean(mean_lbs_DCRB,na.rm=TRUE),
#     sd_mean_lbs_DCRB = sd(mean_lbs_DCRB,na.rm=TRUE),
#     sum_dollars_DCRB = sum(sum_dollars_DCRB,na.rm=TRUE),
#     mean_dollars_DCRB = mean(mean_dollars_DCRB,na.rm=TRUE),
#     sd_dollars_DCRB = sd(mean_dollars_DCRB,na.rm=TRUE),
#     sum_Num_DCRB_VMS_pings = sum(sum_Num_DCRB_VMS_pings,na.rm=TRUE),
#     mean_Num_DCRB_VMS_pings = mean(mean_Num_DCRB_VMS_pings,na.rm=TRUE),
#     sd_Num_DCRB_VMS_pings = sd(mean_Num_DCRB_VMS_pings,na.rm=TRUE),
#     
#     mean_Num_DCRB_Vessels = mean(mean_Num_DCRB_Vessels,na.rm=TRUE),
#     Num_Unique_DCRB_Vessels = mean(Num_Unique_DCRB_Vessels, na.rm=TRUE),
#     mean_lbs_DCRB_per_vessel = mean(mean_lbs_DCRB_per_vessel,na.rm=TRUE),
#     mean_dollars_DCRB_per_vessel = mean(mean_dollars_DCRB_per_vessel,na.rm=TRUE)
#     
#   ) %>%
#   ungroup() %>%
#   mutate(
#     max_sum_Hump_risk_pings = max(sum_Hump_risk_pings, na.rm=TRUE),
#     max_sum_Blue_risk_pings = max(sum_Blue_risk_pings, na.rm=TRUE),
#     max_sum_dollars_DCRB = max(sum_dollars_DCRB, na.rm=TRUE)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     relative_Hump_risk_pings = 100 *  (1 - (sum_Hump_risk_pings/max_sum_Hump_risk_pings )),
#     relative_Blue_risk_pings = 100 * (1 - (sum_Blue_risk_pings/max_sum_Blue_risk_pings )),
#     relative_dollars_DCRB = 100* (sum_dollars_DCRB/max_sum_dollars_DCRB),
#    ) %>%
#   # select(crab.year, relative_Hump_risk_pings, relative_Blue_risk_pings, relative_dollars_DCRB) %>%
#   mutate(
#     Scenario = "Status quo"
#   )
# 
# glimpse(risk.df.mean_by_year_prepostApr_2009_2018_CAwide)

# # make Spring df, figure out max risk and $ across years, and relative reduction
# relative.risk.df.annually.byCAregion.Spring <- risk.df.annually.byCAregion.bySeason %>%
#   filter(B_or_A_April1 == "April 1 and After") %>%
#   group_by(Region) %>%
#   # mutate(
#   #   max_mean_Hump_risk_pings = max(mean_Hump_risk_pings, na.rm=TRUE),
#   #   max_mean_Blue_risk_pings = max(mean_Blue_risk_pings, na.rm=TRUE),
#   #   max_mean_dollars_DCRB = max(mean_dollars_DCRB, na.rm=TRUE)
#   # ) %>%
#   # ungroup() %>%
#   mutate(
#     relative_Hump_risk_pings = 100 *  (1 -(sum_Hump_risk_pings/unique(risk.df.mean_by_year_prepostApr_2009_2018_CAwide$max_sum_Hump_risk_pings ))),
#     relative_Blue_risk_pings = 100 * (1 - ( sum_Blue_risk_pings/unique(risk.df.mean_by_year_prepostApr_2009_2018_CAwide$max_sum_Blue_risk_pings ))),
#     relative_dollars_DCRB = 100* (sum_dollars_DCRB/unique(risk.df.mean_by_year_prepostApr_2009_2018_CAwide$max_sum_dollars_DCRB)) ) %>%
#   # select(crab.year, Region, relative_Hump_risk_pings, relative_Blue_risk_pings, relative_dollars_DCRB) %>%
#   mutate(
#     Scenario = "Regional spring closure"
#   )
# 
# glimpse(relative.risk.df.annually.byCAregion.Spring)
# 
# # # make df for CA-wide risk status quo
# # relative.risk.df.annually.CAwide.Spring <- relative.risk.df.annually.byCAregion.Spring %>%
# #   group_by(crab.year, Scenario) %>%
# #   summarise(
# #     relative_Hump_risk_pings = sum(relative_Hump_risk_pings, na.rm=TRUE),
# #     relative_Blue_risk_pings = sum(relative_Blue_risk_pings, na.rm=TRUE),
# #     relative_dollars_DCRB = sum(relative_dollars_DCRB, na.rm=TRUE)
# #   )
# 
# # make df for CA-wide risk cenCA closed
# relative.risk.df.annually.cenCAclosed.Spring <- relative.risk.df.annually.byCAregion.Spring %>%
#   filter(Region != "CenCA") %>%
#   select(-Region) %>%
#   ungroup() %>%
#   # group_by(crab.year, Scenario) %>%
#   # summarise(
#   #   relative_Hump_risk_pings = sum(relative_Hump_risk_pings, na.rm=TRUE),
#   #   relative_Blue_risk_pings = sum(relative_Blue_risk_pings, na.rm=TRUE),
#   #   relative_dollars_DCRB = sum(relative_dollars_DCRB, na.rm=TRUE)
#   # ) %>%
#   # ungroup() %>%
#   select(-Scenario) %>%
#   mutate(
#     Scenario = "Central CA Spring Closure"
#   ) %>%
#   select(crab.year, Scenario, relative_Hump_risk_pings, relative_Blue_risk_pings, relative_dollars_DCRB)
# 
# glimpse(relative.risk.df.annually.cenCAclosed.Spring)
# 
# df.tradeoff <- risk.df.mean_by_year_prepostApr_2009_2018_CAwide %>%
#   select(crab.year, Scenario, relative_Hump_risk_pings, relative_Blue_risk_pings, relative_dollars_DCRB) %>%
#   bind_rows(
#     relative.risk.df.annually.cenCAclosed.Spring
# )

############################