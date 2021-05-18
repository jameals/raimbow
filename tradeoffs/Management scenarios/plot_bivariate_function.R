# 041320
# Make time series plots: plot_bivariate_byRegion_byPeriod_function

# developed from "Simple early closure analysis.Rmd"
# Include new variables relative risk and relative revenue reduction

# making plots formatted for Word and Power Point: https://twitter.com/trevorabranch/status/1238163076215529474?s=12

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

# this function creates bivariate plots of fishing_col on the x axis (eg mean_dollars_DCRB) and whaleRisk_col on the y axis (eg mean_Hump_risk_pings). Points are labelled according to label_col (eg crab_year). Plots are faceted by time_var (eg Winter or Spring) and region_var (eg CenCA or NorCA)

#Plotting function
plot_bivariate_byRegion_byPeriod_function <- function(
  df, fishing_col, whaleRisk_col, label_col, region_var, time_var, xaxis_lab, yaxis_lab
) #
{
  p_tmp <- ggplot(
    df, #df
    aes_string(
      x=fishing_col,
      y=whaleRisk_col,
      label=label_col
    )
  ) +
    geom_point(size=2) + 
    geom_text_repel() + 
    facet_wrap(as.formula(paste(time_var, "~", region_var)),nrow=2) +
    scale_x_continuous(trans = "log10") +  
    ylab(yaxis_lab) +
    xlab(paste0("log10"," ", xaxis_lab)) +
    theme_classic() +
    theme(
      title = element_text(size = 26),
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(size = 18),
      axis.title = element_text(size = 20),
      strip.text = element_text(size=18))
  p_tmp
}

# code for debugging function commented out below
#  Plots of whales and DCRB fishing over last 10 years

# plot.filepath2 <- paste0(root.dir,"Figures/Whales and fishing plots/")
# df's of interest: risk.df.annually.byCAregion, risk.df.annually.byCAregion.bySeason, risk.df.annually.byAllBIAs, risk.df.annually.byAllBIAs.bySeason

# Humpbacks
# status quo, consider relationship between whales and DCRB pings in winter and spring by region
# png(paste0(plot.filepath2, "sum_H_Avg_Abund_sum_Num_DCRB_VMS_pings_B_or_A_April1_CA_cenCA_v_norCA_2009-18.png"), width = 10, height = 8, units = "in", res = 300)
# plot_bivariate_byRegion_byPeriod_function(df = risk.df.annually.byCAregion.bySeason, 
#             fishing_col = "sum_Num_DCRB_VMS_pings", 
#             whaleRisk_col = "sum_H_Avg_Abund", 
#             label_col = "crab.year", 
#             region_var = "Region", 
#             time_var = "B_or_A_April1", 
#             xaxis_lab = "Dungeness crab VMS pings", 
#             yaxis_lab = "Predicted abundance of humpback whales") #\n (monthly mean number per 5km grid cell)
# dev.off()

####################################################################
####################################################################

####################################################################
####################################################################

## even more example implementations

# status quo, consider relationship between whale risk and DCRB $ in winter and spring by region
# png(paste0(plot.filepath2, "sum_H_Avg_Abund_sum_dollars_DCRB_B_or_A_April1_CA_cenCA_v_norCA_2009-18.png"), width = 10, height = 8, units = "in", res = 300)
# plot_bivariate_byRegion_byPeriod_function(df = risk.df.annually.byCAregion.bySeason, 
#             fishing_col = "sum_dollars_DCRB", 
#             whaleRisk_col = "sum_H_Avg_Abund", 
#             label_col = "crab.year", 
#             region_var = "Region", 
#             time_var = "B_or_A_April1", 
#             xaxis_lab = "Dungeness crab revenue ($)", #\n (monthly mean per 5km grid cell)
#             yaxis_lab = "Predicted abundance of humpback whales")
# dev.off()
# 
# # Blues
# # status quo, consider relationship between whales and DCRB pings in winter and spring by region
# png(paste0(plot.filepath2, "mean_Blue_occurrence_sum_Num_DCRB_VMS_pings_B_or_A_April1_CA_cenCA_v_norCA_2009-18.png"), width = 10, height = 8, units = "in", res = 300)
# plot_bivariate_byRegion_byPeriod_function(df = risk.df.annually.byCAregion.bySeason, 
#             fishing_col = "sum_Num_DCRB_VMS_pings", 
#             whaleRisk_col = "mean_Blue_occurrence", 
#             label_col = "crab.year", 
#             region_var = "Region", 
#             time_var = "B_or_A_April1", 
#             xaxis_lab = "Dungeness crab VMS pings", #\n (monthly mean per 5km grid cell)
#             yaxis_lab = "Predicted occurrence of blue whales")#\n (monthly mean per 5km grid cell)
# dev.off()
# 
# # status quo, consider relationship between whale risk and DCRB $ in winter and spring by region
# png(paste0(plot.filepath2, "mean_Blue_occurrence_sum_dollars_DCRB_B_or_A_April1_CA_cenCA_v_norCA_2009-18.png"), width = 10, height = 8, units = "in", res = 300)
# plot_bivariate_byRegion_byPeriod_function(df = risk.df.annually.byCAregion.bySeason, 
#             fishing_col = "sum_dollars_DCRB", 
#             whaleRisk_col = "mean_Blue_occurrence", 
#             label_col = "crab.year", 
#             region_var = "Region", 
#             time_var = "B_or_A_April1", 
#             xaxis_lab = "Dungeness crab revenue ($)", 
#             yaxis_lab = "Predicted occurrence of blue whales")
# dev.off()
# 
# # status quo, consider relationship between DCRB pings and DCRB $ in winter and spring by region
# png(paste0(plot.filepath2, "mean_dollars_DCRB_mean_Num_DCRB_VMS_pings_B_or_A_April1_CA_cenCA_v_norCA_2009-18.png"), width = 10, height = 8, units = "in", res = 300)
# plot_bivariate_byRegion_byPeriod_function(df = risk.df.annually.byCAregion.bySeason, 
#             fishing_col = "mean_dollars_DCRB", 
#             whaleRisk_col = "mean_Num_DCRB_VMS_pings", 
#             label_col = "crab.year", 
#             region_var = "Region", 
#             time_var = "B_or_A_April1", 
#             xaxis_lab = "Dungeness crab revenue\n (monthly mean per 5km grid cell)", 
#             yaxis_lab = "Dungeness crab pings\n (monthly mean number per 5km grid cell)")
# dev.off()

####################################################################
####################################################################

