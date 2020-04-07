# 040720
# Make tradeoff data frames

# developed from "Simple early closure analysis.Rmd"
# Include new variables relative risk and relative revenue reduction

####################################################################
####################################################################

### REMOVE THIS CHUNK ONCE IT IS LINKED WITH OUTPUTS FROM SW'S FUNCTIONS

# make df. Year, Scenario, hump risk, blue risk, DCRB $
# scenarios: sq, CA, cenCA, BIAs

root.dir <- "/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/"
load(paste0(root.dir,"Output_Data/Scenario_Analysis_Data_2009_2018.RData"))

####################################################################
####################################################################



####################################################################
####################################################################

# response variables are calculated in terms of relative reduction: relative risk reduction to whales and relative revenue reduction to DCRB
# if max $ is X, then for each year we want $ in that year relative to max, and scaled to a %
# if max risk is Y, then for relative risk we want max risk to be zero and complete closure to be 100% reduction
# all relative metrics are relative to the year under consideration if both winter and spring DCRB season were open. based on revised approach 012420

con_df_weekly_years_5km_CA <- read_rds("~/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_daily_2009-2018_fishtix_humpback_blue_whales_grids.RDS")
glimpse(con_df_weekly_years_5km_CA)

scenario_table <- read_rds(here::here(
  "tradeoffs",
  "Management scenarios",
  "scenario_table.RDS"
  )
)
scenario_table

# I am thinking we want to generate summary df's with the metrics below for time-areas open to fishing, and then a separate set of summary df's with these metrics for time-areas closed to fishing

# column headers for output df from SW functions: 
# year, crab_year, delay_time_scenario, delay_domain_scenario, closure_time_scenario, closure_domain_scenario,

# DCRB fishing activity
# sum_DCRB_lbs = sum(DCRB_lbs), # total pounds crab landed in areas and times open to fishing
# sum_DCRB_rev = sum(sum_DCRB_rev), # total $ crab landed in areas and times open to fishing
# sum_Num_DCRB_VMS_pings = sum(Num_DCRB_VMS_pings), # total crab VMS pings in areas and times open to fishing
# sum_Num_DCRB_Vessels = sum(Num_DCRB_Vessels), # total crab vessel days in areas and times open to fishing
# mean_Num_Unique_DCRB_Vessels = mean(Num_Unique_DCRB_Vessels), # mean unique crab vessels per 5km grid cell in areas and times open to fishing
# mean_normalized_Num_DCRB_VMS_pings = mean(normalized_Num_DCRB_VMS_pings), # mean crab VMS pings per 5km grid cell in areas and times open to fishing, with pings normalized to 0-1
# sum_normalized_Num_DCRB_VMS_pings = sum(normalized_Num_DCRB_VMS_pings), # sum crab VMS pings per 5km grid cell in areas and times open to fishing, with pings normalized to 0-1

# blue whales
# Blue_occurrence_mean = mean(Blue_occurrence_mean), # mean probability of blue whale occurrence per 5km grid cell in areas and times open to fishing. note we do not need to retain normalized_Blue_occurrence_mean because it is already scaled 0-1
# Blue_occurrence_sd = sd(Blue_occurrence_mean), # standard deviation in probability of blue whale occurrence per 5km grid cell in areas and times open to fishing,

# humpback whales
# H_Avg_Abund = mean(H_Avg_Abund), # mean humpback whale abundance per 5km grid cell in areas and times open to fishing,
# mean_normalized_H_Avg_Abund = mean(normalized_H_Avg_Abund), # mean humpback whale abundance per 5km grid cell in areas and times open to fishing, with humpback abundance normalized to 0-1
# mean_normalized_H_Avg_Density = mean(normalized_H_Avg_Abund / 5km_grid_cell_area), # mean humpback whale density per 5km grid cell in areas and times open to fishing, with humpback abundance normalized to 0-1

# blue whale risk metrics
# mean_blue_risk = mean(Blue_occurrence_mean * Num_DCRB_VMS_pings), # mean per 5km grid cell of the product of [Num_DCRB_VMS_pings and Blue_occurrence_mean], in areas and times open to fishing
# sum_blue_risk = sum(Blue_occurrence_mean * Num_DCRB_VMS_pings), # sum of the product of [Num_DCRB_VMS_pings and Blue_occurrence_mean] in areas and times open to fishing
# mean_blue_risk_density = mean((Blue_occurrence_mean * Num_DCRB_VMS_pings)/ 5km_grid_cell_area), # mean per 5km grid cell of the product of [Num_DCRB_VMS_pings and Blue_occurrence_mean], divided by area of 5km grid cell, in areas and times open to fishing
# sum_blue_risk_density = sum((Blue_occurrence_mean * Num_DCRB_VMS_pings)/ 5km_grid_cell_area), # sum of the product of [Num_DCRB_VMS_pings and Blue_occurrence_mean], divided by area of 5km grid cell, in areas and times open to fishing
# mean_normalized_blue_risk = mean(Blue_occurrence_mean *normalized_Num_DCRB_VMS_pings), # mean per 5km grid cell of the product of [normalized_Num_DCRB_VMS_pings and Blue_occurrence_mean], in areas and times open to fishing
# sum_normalized_blue_risk = sum(Blue_occurrence_mean *normalized_Num_DCRB_VMS_pings), # sum of the product of [normalized_Num_DCRB_VMS_pings and Blue_occurrence_mean], in areas and times open to fishing
# mean_normalized_blue_risk_density = mean((Blue_occurrence_mean *normalized_Num_DCRB_VMS_pings)/ 5km_grid_cell_area), # mean per 5km grid cell of the product of [normalized_Num_DCRB_VMS_pings and Blue_occurrence_mean], divided by area of 5km grid cell, in areas and times open to fishing
# sum_normalized_blue_risk_density = sum((Blue_occurrence_mean *normalized_Num_DCRB_VMS_pings)/ 5km_grid_cell_area), # sum of the product of [normalized_Num_DCRB_VMS_pings and Blue_occurrence_mean], divided by area of 5km grid cell, in areas and times open to fishing

# humpback whale risk metrics
# mean_hump_risk = mean(H_Avg_Abund * Num_DCRB_VMS_pings), # mean per 5km grid cell of the product of [Num_DCRB_VMS_pings and H_Avg_Abund], in areas and times open to fishing
# sum_hump_risk = sum(H_Avg_Abund * Num_DCRB_VMS_pings), # sum of the product of [Num_DCRB_VMS_pings and H_Avg_Abund], in areas and times open to fishing
# mean_hump_risk_density = mean((H_Avg_Abund * Num_DCRB_VMS_pings) / area_of_5kmgridcell), # mean per 5km grid cell of the product of [Num_DCRB_VMS_pings and H_Avg_Abund], divided by area of 5km grid cell, in areas and times open to fishing
# sum_hump_risk_density = sum((H_Avg_Abund * Num_DCRB_VMS_pings) / area_of_5kmgridcell), # sum of the product of [Num_DCRB_VMS_pings and H_Avg_Abund], divided by area of 5km grid cell, in areas and times open to fishing
# mean_normalized_hump_risk = mean(normalized_H_Avg_Abund * normalized_Num_DCRB_VMS_pings), # mean per 5km grid cell of the product of [normalized_Num_DCRB_VMS_pings and normalized_H_Avg_Abund], in areas and times open to fishing
# sum_normalized_hump_risk = sum(normalized_H_Avg_Abund * normalized_Num_DCRB_VMS_pings), # sum of the product of [normalized_Num_DCRB_VMS_pings and normalized_H_Avg_Abund], in areas and times open to fishing
# mean_normalized_hump_risk_density = mean((normalized_H_Avg_Abund * normalized_Num_DCRB_VMS_pings) / area_of_5kmgridcell), # mean per 5km grid cell of the product of [normalized_Num_DCRB_VMS_pings and normalized_H_Avg_Abund], divided by area of 5km grid cell, in areas and times open to fishing
# sum_normalized_hump_risk_density = sum((normalized_H_Avg_Abund * normalized_Num_DCRB_VMS_pings) / area_of_5kmgridcell), # sum of the product of [normalized_Num_DCRB_VMS_pings and normalized_H_Avg_Abund], divided by area of 5km grid cell, in areas and times open to fishing

# make CA spring df, including values relative to max observed
glimpse(risk.df.mean_by_year_prepostApr_2009_2018)

# compare status quo to CAwide closure in spring
risk.df.mean_by_year_prepostApr_2009_2018_CAwide <- risk.df.mean_by_year_prepostApr_2009_2018 %>%
  #filter(B_or_A_April1 == "April 1 and After") %>%
  group_by(crab.year, B_or_A_April1) %>%
  summarise(
    num_5km_grid_cells = n(),
    
    sum_Hump_risk_pings = sum(sum_Hump_risk_pings,na.rm=TRUE),
    #sd_sum_Hump_risk_pings = sd(sum_Hump_risk_pings,na.rm=TRUE),
    
    sum_Blue_risk_pings = sum(sum_Blue_risk_pings,na.rm=TRUE),
    #sd_sum_Blue_risk_pings = sd(sum_Blue_risk_pings,na.rm=TRUE),
    
    # sum_lbs_DCRB = sum(sum_lbs_DCRB,na.rm=TRUE),
    # mean_lbs_DCRB = mean(mean_lbs_DCRB,na.rm=TRUE),
    # sd_mean_lbs_DCRB = sd(mean_lbs_DCRB,na.rm=TRUE),
    sum_dollars_DCRB = sum(sum_dollars_DCRB,na.rm=TRUE),
    # mean_dollars_DCRB = mean(mean_dollars_DCRB,na.rm=TRUE),
    # sd_dollars_DCRB = sd(mean_dollars_DCRB,na.rm=TRUE),
    # sum_Num_DCRB_VMS_pings = sum(sum_Num_DCRB_VMS_pings,na.rm=TRUE),
    # mean_Num_DCRB_VMS_pings = mean(mean_Num_DCRB_VMS_pings,na.rm=TRUE),
    # sd_Num_DCRB_VMS_pings = sd(mean_Num_DCRB_VMS_pings,na.rm=TRUE),
    
    # mean_Num_DCRB_Vessels = mean(mean_Num_DCRB_Vessels,na.rm=TRUE),
    # Num_Unique_DCRB_Vessels = mean(Num_Unique_DCRB_Vessels, na.rm=TRUE),
    # mean_lbs_DCRB_per_vessel = mean(mean_lbs_DCRB_per_vessel,na.rm=TRUE),
    # mean_dollars_DCRB_per_vessel = mean(mean_dollars_DCRB_per_vessel,na.rm=TRUE)
    
  ) %>%
  ungroup() %>%
  group_by(crab.year) %>%
  # mutate(
  #   max_sum_Hump_risk_pings = max(sum_Hump_risk_pings, na.rm=TRUE),
  #   max_sum_Blue_risk_pings = max(sum_Blue_risk_pings, na.rm=TRUE),
  #   max_sum_dollars_DCRB = max(sum_dollars_DCRB, na.rm=TRUE)
  # ) %>%
  # ungroup() %>%
  summarise(
    relative_Hump_risk_pings = 100 *  (1 - (
      sum_Hump_risk_pings[B_or_A_April1 == "Before April 1"] /
        ( sum_Hump_risk_pings[B_or_A_April1 == "Before April 1"] + sum_Hump_risk_pings[B_or_A_April1 == "April 1 and After"] )
    )),
    relative_Blue_risk_pings = 100 * (1 - (
      sum_Blue_risk_pings[B_or_A_April1 == "Before April 1"] /
        ( sum_Blue_risk_pings[B_or_A_April1 == "Before April 1"] + sum_Blue_risk_pings[B_or_A_April1 == "April 1 and After"] )
    )),
    relative_dollars_DCRB = 100* (sum_dollars_DCRB[B_or_A_April1 == "Before April 1"] / 
                                    ( sum_dollars_DCRB[B_or_A_April1 == "Before April 1"] + sum_dollars_DCRB[B_or_A_April1 == "April 1 and After"])) ,
  ) %>%
  # select(crab.year, relative_Hump_risk_pings, relative_Blue_risk_pings, relative_dollars_DCRB) %>%
  mutate(
    Scenario = "CAwide"
  )

glimpse(risk.df.mean_by_year_prepostApr_2009_2018_CAwide)

# compare status quo to cenCA closure in spring
risk.df.mean_by_year_prepostApr_2009_2018_cenCA <- risk.df.mean_by_year_prepostApr_2009_2018 %>%
  #filter(B_or_A_April1 == "April 1 and After") %>%
  group_by(crab.year, B_or_A_April1, Region) %>%
  summarise(
    num_5km_grid_cells = n(),
    
    sum_Hump_risk_pings = sum(sum_Hump_risk_pings,na.rm=TRUE),
    #sd_sum_Hump_risk_pings = sd(sum_Hump_risk_pings,na.rm=TRUE),
    
    sum_Blue_risk_pings = sum(sum_Blue_risk_pings,na.rm=TRUE),
    #sd_sum_Blue_risk_pings = sd(sum_Blue_risk_pings,na.rm=TRUE),
    
    # sum_lbs_DCRB = sum(sum_lbs_DCRB,na.rm=TRUE),
    # mean_lbs_DCRB = mean(mean_lbs_DCRB,na.rm=TRUE),
    # sd_mean_lbs_DCRB = sd(mean_lbs_DCRB,na.rm=TRUE),
    sum_dollars_DCRB = sum(sum_dollars_DCRB,na.rm=TRUE),
    # mean_dollars_DCRB = mean(mean_dollars_DCRB,na.rm=TRUE),
    # sd_dollars_DCRB = sd(mean_dollars_DCRB,na.rm=TRUE),
    # sum_Num_DCRB_VMS_pings = sum(sum_Num_DCRB_VMS_pings,na.rm=TRUE),
    # mean_Num_DCRB_VMS_pings = mean(mean_Num_DCRB_VMS_pings,na.rm=TRUE),
    # sd_Num_DCRB_VMS_pings = sd(mean_Num_DCRB_VMS_pings,na.rm=TRUE),
    
    # mean_Num_DCRB_Vessels = mean(mean_Num_DCRB_Vessels,na.rm=TRUE),
    # Num_Unique_DCRB_Vessels = mean(Num_Unique_DCRB_Vessels, na.rm=TRUE),
    # mean_lbs_DCRB_per_vessel = mean(mean_lbs_DCRB_per_vessel,na.rm=TRUE),
    # mean_dollars_DCRB_per_vessel = mean(mean_dollars_DCRB_per_vessel,na.rm=TRUE)
    
  ) %>%
  ungroup() %>%
  group_by(crab.year) %>%
  # mutate(
  #   max_sum_Hump_risk_pings = max(sum_Hump_risk_pings, na.rm=TRUE),
  #   max_sum_Blue_risk_pings = max(sum_Blue_risk_pings, na.rm=TRUE),
  #   max_sum_dollars_DCRB = max(sum_dollars_DCRB, na.rm=TRUE)
  # ) %>%
  # ungroup() %>%
  summarise(
    relative_Hump_risk_pings = 100 *  (1 - (
      ( sum_Hump_risk_pings[B_or_A_April1 == "Before April 1" & Region == "CenCA"] +
          sum_Hump_risk_pings[B_or_A_April1 == "Before April 1" & Region == "NorCA"] +
          sum_Hump_risk_pings[B_or_A_April1 == "April 1 and After" & Region == "NorCA"] ) /
        ( sum_Hump_risk_pings[B_or_A_April1 == "Before April 1" & Region == "CenCA"] +
            sum_Hump_risk_pings[B_or_A_April1 == "April 1 and After" & Region == "CenCA"] +
            sum_Hump_risk_pings[B_or_A_April1 == "Before April 1" & Region == "NorCA"] +
            sum_Hump_risk_pings[B_or_A_April1 == "April 1 and After" & Region == "NorCA"] )
    )),
    
    relative_Blue_risk_pings = 100 *  (1 - (
      ( sum_Blue_risk_pings[B_or_A_April1 == "Before April 1" & Region == "CenCA"] +
          sum_Blue_risk_pings[B_or_A_April1 == "Before April 1" & Region == "NorCA"] +
          sum_Blue_risk_pings[B_or_A_April1 == "April 1 and After" & Region == "NorCA"] ) /
        ( sum_Blue_risk_pings[B_or_A_April1 == "Before April 1" & Region == "CenCA"] +
            sum_Blue_risk_pings[B_or_A_April1 == "April 1 and After" & Region == "CenCA"] +
            sum_Blue_risk_pings[B_or_A_April1 == "Before April 1" & Region == "NorCA"] +
            sum_Blue_risk_pings[B_or_A_April1 == "April 1 and After" & Region == "NorCA"] )
    )),
    
    relative_dollars_DCRB = 100 *
      ( sum_dollars_DCRB[B_or_A_April1 == "Before April 1" & Region == "CenCA"] +
          sum_dollars_DCRB[B_or_A_April1 == "Before April 1" & Region == "NorCA"] +
          sum_dollars_DCRB[B_or_A_April1 == "April 1 and After" & Region == "NorCA"] ) /
      ( sum_dollars_DCRB[B_or_A_April1 == "Before April 1" & Region == "CenCA"] +
          sum_dollars_DCRB[B_or_A_April1 == "April 1 and After" & Region == "CenCA"] +
          sum_dollars_DCRB[B_or_A_April1 == "Before April 1" & Region == "NorCA"] +
          sum_dollars_DCRB[B_or_A_April1 == "April 1 and After" & Region == "NorCA"] )
  ) %>%
  # select(crab.year, relative_Hump_risk_pings, relative_Blue_risk_pings, relative_dollars_DCRB) %>%
  mutate(
    Scenario = "cenCA"
  )

glimpse(risk.df.mean_by_year_prepostApr_2009_2018_cenCA)

# compare status quo to BIA closure in spring
risk.df.mean_by_year_prepostApr_2009_2018_BIAs <- risk.df.mean_by_year_prepostApr_2009_2018 %>%
  #filter(B_or_A_April1 == "April 1 and After") %>%
  group_by(crab.year, B_or_A_April1, BIA_bm_or_mn) %>%
  summarise(
    num_5km_grid_cells = n(),
    
    sum_Hump_risk_pings = sum(sum_Hump_risk_pings,na.rm=TRUE),
    #sd_sum_Hump_risk_pings = sd(sum_Hump_risk_pings,na.rm=TRUE),
    
    sum_Blue_risk_pings = sum(sum_Blue_risk_pings,na.rm=TRUE),
    #sd_sum_Blue_risk_pings = sd(sum_Blue_risk_pings,na.rm=TRUE),
    
    # sum_lbs_DCRB = sum(sum_lbs_DCRB,na.rm=TRUE),
    # mean_lbs_DCRB = mean(mean_lbs_DCRB,na.rm=TRUE),
    # sd_mean_lbs_DCRB = sd(mean_lbs_DCRB,na.rm=TRUE),
    sum_dollars_DCRB = sum(sum_dollars_DCRB,na.rm=TRUE),
    # mean_dollars_DCRB = mean(mean_dollars_DCRB,na.rm=TRUE),
    # sd_dollars_DCRB = sd(mean_dollars_DCRB,na.rm=TRUE),
    # sum_Num_DCRB_VMS_pings = sum(sum_Num_DCRB_VMS_pings,na.rm=TRUE),
    # mean_Num_DCRB_VMS_pings = mean(mean_Num_DCRB_VMS_pings,na.rm=TRUE),
    # sd_Num_DCRB_VMS_pings = sd(mean_Num_DCRB_VMS_pings,na.rm=TRUE),
    
    # mean_Num_DCRB_Vessels = mean(mean_Num_DCRB_Vessels,na.rm=TRUE),
    # Num_Unique_DCRB_Vessels = mean(Num_Unique_DCRB_Vessels, na.rm=TRUE),
    # mean_lbs_DCRB_per_vessel = mean(mean_lbs_DCRB_per_vessel,na.rm=TRUE),
    # mean_dollars_DCRB_per_vessel = mean(mean_dollars_DCRB_per_vessel,na.rm=TRUE)
    
  ) %>%
  ungroup() %>%
  group_by(crab.year) %>%
  # mutate(
  #   max_sum_Hump_risk_pings = max(sum_Hump_risk_pings, na.rm=TRUE),
  #   max_sum_Blue_risk_pings = max(sum_Blue_risk_pings, na.rm=TRUE),
  #   max_sum_dollars_DCRB = max(sum_dollars_DCRB, na.rm=TRUE)
  # ) %>%
  # ungroup() %>%
  summarise(
    relative_Hump_risk_pings = 100 *  (1 - (
      ( sum_Hump_risk_pings[B_or_A_April1 == "Before April 1" & BIA_bm_or_mn == "Inside BIA"] +
          sum_Hump_risk_pings[B_or_A_April1 == "Before April 1" & BIA_bm_or_mn == "Outside BIA"] +
          sum_Hump_risk_pings[B_or_A_April1 == "April 1 and After" & BIA_bm_or_mn == "Outside BIA"] ) /
        ( sum_Hump_risk_pings[B_or_A_April1 == "Before April 1" & BIA_bm_or_mn == "Inside BIA"] +
            sum_Hump_risk_pings[B_or_A_April1 == "April 1 and After" & BIA_bm_or_mn == "Inside BIA"] +
            sum_Hump_risk_pings[B_or_A_April1 == "Before April 1" & BIA_bm_or_mn == "Outside BIA"] +
            sum_Hump_risk_pings[B_or_A_April1 == "April 1 and After" & BIA_bm_or_mn == "Outside BIA"] )
    )),
    
    relative_Blue_risk_pings = 100 *  (1 - (
      ( sum_Blue_risk_pings[B_or_A_April1 == "Before April 1" & BIA_bm_or_mn == "Inside BIA"] +
          sum_Blue_risk_pings[B_or_A_April1 == "Before April 1" & BIA_bm_or_mn == "Outside BIA"] +
          sum_Blue_risk_pings[B_or_A_April1 == "April 1 and After" & BIA_bm_or_mn == "Outside BIA"] ) /
        ( sum_Blue_risk_pings[B_or_A_April1 == "Before April 1" & BIA_bm_or_mn == "Inside BIA"] +
            sum_Blue_risk_pings[B_or_A_April1 == "April 1 and After" & BIA_bm_or_mn == "Inside BIA"] +
            sum_Blue_risk_pings[B_or_A_April1 == "Before April 1" & BIA_bm_or_mn == "Outside BIA"] +
            sum_Blue_risk_pings[B_or_A_April1 == "April 1 and After" & BIA_bm_or_mn == "Outside BIA"] )
    )),
    
    relative_dollars_DCRB = 100 *
      ( sum_dollars_DCRB[B_or_A_April1 == "Before April 1" & BIA_bm_or_mn == "Inside BIA"] +
          sum_dollars_DCRB[B_or_A_April1 == "Before April 1" & BIA_bm_or_mn == "Outside BIA"] +
          sum_dollars_DCRB[B_or_A_April1 == "April 1 and After" & BIA_bm_or_mn == "Outside BIA"] ) /
      ( sum_dollars_DCRB[B_or_A_April1 == "Before April 1" & BIA_bm_or_mn == "Inside BIA"] +
          sum_dollars_DCRB[B_or_A_April1 == "April 1 and After" & BIA_bm_or_mn == "Inside BIA"] +
          sum_dollars_DCRB[B_or_A_April1 == "Before April 1" & BIA_bm_or_mn == "Outside BIA"] +
          sum_dollars_DCRB[B_or_A_April1 == "April 1 and After" & BIA_bm_or_mn == "Outside BIA"] )
  ) %>%
  # select(crab.year, relative_Hump_risk_pings, relative_Blue_risk_pings, relative_dollars_DCRB) %>%
  mutate(
    Scenario = "allBIAs"
  )

glimpse(risk.df.mean_by_year_prepostApr_2009_2018_BIAs)
```

df.tradeoff <- risk.df.mean_by_year_prepostApr_2009_2018_CAwide %>%
  select(crab.year, Scenario, relative_Hump_risk_pings, relative_Blue_risk_pings, relative_dollars_DCRB) %>%
  bind_rows(
    risk.df.mean_by_year_prepostApr_2009_2018_cenCA,
    risk.df.mean_by_year_prepostApr_2009_2018_BIAs
  ) %>%
  mutate(
    Scenario_long = c(
      rep("California-wide Spring Closure",dim(risk.df.mean_by_year_prepostApr_2009_2018_CAwide)[1]),
      rep("Central California  Spring Closure",dim(risk.df.mean_by_year_prepostApr_2009_2018_cenCA)[1]),
      rep("Blue and Humpback BIA  Spring Closure",dim(risk.df.mean_by_year_prepostApr_2009_2018_CAwide)[1])
    )
  )

df.tradeoff.annualmeans <- df.tradeoff %>%
  group_by(Scenario, Scenario_long) %>%
  summarise(relative_Hump_risk_pings = mean(relative_Hump_risk_pings),
            relative_Blue_risk_pings = mean(relative_Blue_risk_pings),
            relative_dollars_DCRB = mean(relative_dollars_DCRB)
  )




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