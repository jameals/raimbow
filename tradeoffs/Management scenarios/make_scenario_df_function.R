# 022720
# Dependencies: run prep_data_for_scenario_df_function.R first

library(foreign)
library(lubridate)
library(tidyverse)
library(reshape2)
library(scales)
library(zoo)
library(ggrepel)
library(sf)
library(data.table)
library(wesanderson)
library(viridis)
library(here)
#library(ggerr)

con_df_weekly_years_5km_CA <- read_rds("~/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_2009-2018_fishtix_blue_humpback_whales_grids.RDS") %>%
  mutate(
    season = ifelse(B_or_A_April1 == "Before April 1", "Winter", "Spring-Summer") # add column to df for spring_summer v winter
  )

scenario_table <- read_rds(here::here(
  "tradeoffs",
  "Management scenarios",
  "scenario_table.RDS"
  )
)


con_df_weekly_years_5km_CA %>%
  #mutate(spatial_domain = ifelse(is_empty(spatial_domain)==TRUE,NA,spatial_domain),
         #time_period = ifelse(is_empty(time_period)==TRUE,NA,time_period),
  #       ) %>% 
  group_by(crab.year, Region, season)


scenario_summary_function <- function(df, spatial_domain, scenario_lag, scenario_df_name) {
  
}