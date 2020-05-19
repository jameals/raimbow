# 041320
# Make time series plots: plot_ts_byRegion_byPeriod_function

# developed from "Simple early closure analysis.Rmd"
# Include new variables relative risk and relative revenue reduction

# making plots formatted for Word and Power Point: https://twitter.com/trevorabranch/status/1238163076215529474?s=12

library(tidyverse)
library(viridis)
library(ggrepel)

####################################################################
####################################################################


####################################################################
####################################################################

### REMOVE THIS CHUNK ONCE IT IS LINKED WITH OUTPUTS FROM SW'S FUNCTIONS

library(tidyverse)

# make df. Year, Scenario, hump risk, blue risk, DCRB $
# scenarios: sq, CA, cenCA, BIAs

root.dir <- "/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/"
load(paste0(root.dir,"Output_Data/Scenario_Analysis_Data_2009_2018.RData"))

# make df for cenCA vs norCA by season
risk.df.annually.byCAregion.bySeason <- risk.df.mean_by_year_prepostApr_2009_2018 %>%
  group_by(crab.year, B_or_A_April1, Region) %>% 
  summarise(
    num_5km_grid_cells = n(),
    sum_H_Avg_Abund = sum(mean_H_Avg_Abund, na.rm=TRUE),
    mean_H_Avg_Abund = mean(mean_H_Avg_Abund, na.rm=TRUE),
    sd_H_Avg_Abund = sd(mean_H_Avg_Abund, na.rm=TRUE),
    #mean_Hump_risk_pings = mean(mean_Hump_risk_pings,na.rm=TRUE),
    sum_Hump_risk_pings = sum(sum_Hump_risk_pings,na.rm=TRUE),
    sd_mean_Hump_risk_pings = sd(mean_Hump_risk_pings,na.rm=TRUE),
    # mean_Hump_risk_vessels = mean(Hump_risk_vessels,na.rm=TRUE),
    # mean_Hump_risk_vessels = mean(Hump_risk_vessels,na.rm=TRUE),
    # mean_Hump_risk_lbs_per_vessel = mean(Hump_risk_lbs_per_vessel,na.rm=TRUE),
    # mean_Hump_risk_dollars_per_vessel = mean(Hump_risk_dollars_per_vessel,na.rm=TRUE),
    
    mean_Blue_occurrence = mean(mean_Blue_occurrence, na.rm=TRUE),
    sd_mean_Blue_occurrence = sd(mean_Blue_occurrence, na.rm=TRUE),
    # mean_Blue_risk_lbs = mean(Blue_risk_lbs,na.rm=TRUE),
    # mean_Blue_risk_dollars = mean(Blue_risk_dollars,na.rm=TRUE),
    #mean_Blue_risk_pings = mean(mean_Blue_risk_pings,na.rm=TRUE),
    sum_Blue_risk_pings = sum(sum_Blue_risk_pings,na.rm=TRUE),
    sd_mean_Blue_risk_pings = sd(mean_Blue_risk_pings,na.rm=TRUE),
    # mean_Blue_risk_vessels = mean(Blue_risk_vessels,na.rm=TRUE),
    # mean_Blue_risk_lbs_per_vessel = mean(Blue_risk_lbs_per_vessel,na.rm=TRUE),
    # mean_Blue_risk_dollars_per_vessel = mean(Blue_risk_dollars_per_vessel,na.rm=TRUE),
    
    sum_lbs_DCRB = sum(sum_lbs_DCRB,na.rm=TRUE),
    mean_lbs_DCRB = mean(mean_lbs_DCRB,na.rm=TRUE),
    sd_mean_lbs_DCRB = sd(mean_lbs_DCRB,na.rm=TRUE),
    sum_dollars_DCRB = sum(sum_dollars_DCRB,na.rm=TRUE),
    mean_dollars_DCRB = mean(mean_dollars_DCRB,na.rm=TRUE),
    sd_dollars_DCRB = sd(mean_dollars_DCRB,na.rm=TRUE),
    sum_Num_DCRB_VMS_pings = sum(sum_Num_DCRB_VMS_pings,na.rm=TRUE),
    mean_Num_DCRB_VMS_pings = mean(mean_Num_DCRB_VMS_pings,na.rm=TRUE),
    sd_Num_DCRB_VMS_pings = sd(mean_Num_DCRB_VMS_pings,na.rm=TRUE),
    
    mean_Num_DCRB_Vessels = mean(mean_Num_DCRB_Vessels,na.rm=TRUE),
    Num_Unique_DCRB_Vessels = mean(Num_Unique_DCRB_Vessels, na.rm=TRUE),
    mean_lbs_DCRB_per_vessel = mean(mean_lbs_DCRB_per_vessel,na.rm=TRUE),
    mean_dollars_DCRB_per_vessel = mean(mean_dollars_DCRB_per_vessel,na.rm=TRUE)
  )

# make plotting year column for seasonal df's
risk.df.annually.byCAregion.bySeason$plotting.year <- as.numeric(substr(risk.df.annually.byCAregion.bySeason$crab.year,6,9))

# make plotting column for region-season and BIA-season
risk.df.annually.byCAregion.bySeason <- 
  risk.df.annually.byCAregion.bySeason %>%
  mutate(
    Region.Season = paste0(Region," ", B_or_A_April1)
  )

glimpse(risk.df.annually.byCAregion.bySeason)


# con_df_weekly_years_5km_CA <- read_rds("~/Documents/RAIMBOW/Processed Data/VMS/CA_DCRB_vms_fishing_daily_2009-2018_fishtix_humpback_blue_whales_grids.RDS")
# glimpse(con_df_weekly_years_5km_CA)
# 
# scenario_table <- read_rds(here::here(
#   "tradeoffs",
#   "Management scenarios",
#   "scenario_table.RDS"
# )
# )
# scenario_table

####################################################################
####################################################################

library(viridis)
library(ggrepel)

####################################################################
####################################################################

####################################################################
####################################################################

# this function plots a time series of response_var (eg, H_Avg_Abund) for each time_col (eg year) for each region_var (eg CenCA and NorCA) and time_var (eg Winter and Spring)

# https://stackoverflow.com/questions/5106782/use-of-ggplot-within-another-function-in-r
plot_ts_byRegion_byPeriod_function <- function(
  df, time_col, response_var, time_var, region_var, yaxis_lab) #
{
  p_tmp <- ggplot(
    df, #df
    aes_string(
      x=time_col,
      y=response_var,
      colour=region_var
      )
  ) + 
    geom_point(aes_string(shape=region_var), size=4) +
    geom_line(aes_string(linetype=time_var)) + 
    scale_colour_viridis(option="D", discrete=TRUE, begin=0.2, end=0.8)+
    ylab(yaxis_lab) +
    xlab("") +
    theme_classic() +
    theme(legend.title = element_blank(),
          title = element_text(size = 26),
          legend.text = element_text(size = 20),
          legend.position = c(.15, .85),
          axis.text.x = element_text(hjust = 1,size = 18, angle = 60),
          axis.text.y = element_text(size = 18),
          axis.title = element_text(size = 20),
          strip.text = element_text(size=18))
  p_tmp
}

# example implementation of code commented out below

# Humpbacks
# annually, by norCA vs cenCA
# png(paste0(plot.filepath, "Summed_humpback_abundance_CA_cenCA_v_norCA_2009-18.png"), width = 10, height = 8, units = "in", res = 300)
# plot_ts_byRegion_byPeriod_function(df=risk.df.annually.byCAregion, 
#            time_col="year", 
#            response_var="sum_H_Avg_Abund", 
#            time_var = NULL,
#            region_var="Region", 
#            yaxis_lab="Predicted abundance of humpback whales" #,shape_var="Region"
# )
# dev.off()

# Humpbacks
# annually, by norCA vs cenCA and Winter/Spring
# png(paste0(plot.filepath, "Summed_humpback_abundance_CA_cenCA_v_norCA_2009-18.png"), width = 10, height = 8, units = "in", res = 300)
# plot_ts_byRegion_byPeriod_function(df=risk.df.annually.byCAregion.bySeason, 
#                               time_col="plotting.year", 
#                               response_var="sum_H_Avg_Abund", 
#                               time_var = "B_or_A_April1", 
#                               region_var="Region", 
#                               yaxis_lab="Predicted abundance of humpback whales" #,shape_var="Region"
# )
# dev.off()

####################################################################
####################################################################




