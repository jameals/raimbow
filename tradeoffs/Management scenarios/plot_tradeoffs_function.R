# 041320
# Make tradeoff plots

# developed from "Simple early closure analysis.Rmd"
# Include new variables relative risk and relative revenue reduction

# making plots formatted for Word and Power Point: https://twitter.com/trevorabranch/status/1238163076215529474?s=12

####################################################################
####################################################################

# quick try at a plot for new output 051920
library(ggerr)
library(viridis)


# make nice scenario names
unique(df_tradeoff_focal_scenarios$scenario_df_name)
df_tradeoff_focal_scenarios$pretty_scenario_names <- c(
    rep("Early Closure BIAs", length(unique(df_tradeoff_focal_scenarios$crab_year))),
    rep("Delay Statewide, Early Closure BIAs", length(unique(df_tradeoff_focal_scenarios$crab_year))),
    rep("Delay CenCA, Early Closure BIAs", length(unique(df_tradeoff_focal_scenarios$crab_year))),
    rep("Status Quo", length(unique(df_tradeoff_focal_scenarios$crab_year))),
    rep("Delay Statewide", length(unique(df_tradeoff_focal_scenarios$crab_year))),
    rep("Delay CenCA", length(unique(df_tradeoff_focal_scenarios$crab_year))),
    rep("Early Closure Statewide", length(unique(df_tradeoff_focal_scenarios$crab_year))),
    rep("Delay Statewide, Early Closure Statewide", length(unique(df_tradeoff_focal_scenarios$crab_year))),
    rep("Delay CenCA, Early Closure Statewide", length(unique(df_tradeoff_focal_scenarios$crab_year))),
    rep("Early Closure CenCA", length(unique(df_tradeoff_focal_scenarios$crab_year))),
    rep("Delay Statewide, Early Closure CenCA", length(unique(df_tradeoff_focal_scenarios$crab_year))),
    rep("Delay CenCA, Early Closure CenCA", length(unique(df_tradeoff_focal_scenarios$crab_year)))
    )  
# drop scenario 25 because it is status quo?

# make summed normalized risk for both whales
df_tradeoff_focal_scenarios <- df_tradeoff_focal_scenarios %>%
  mutate(
    mean_whale_risk_n = (relative_hump_risk_n + relative_blwh_risk_n)/2
  )

### normalized humpbacks
png(paste0(here::here("tradeoffs",
                      "Management scenarios",
                      "figures"), 
           "/Tradeoff plot - normalized humpbacks only.png"), 
    width = 10, height = 8, units = "in", res = 300)

to_hump_n <- ggplot(
  df_tradeoff_focal_scenarios, #df
  aes(
    x=relative_dollars,
    y=relative_hump_risk_n,
    #label=crab_year,
    #label=pretty_scenario_names,
    colour=pretty_scenario_names
  )
) + # group=1 tells ggplot that there is only 1 group
  geom_point(size=2, alpha=0.6) +
  #geom_label_repel() +
  stat_err(spread = "se", mult = 2, width=.1) + 
  stat_err(geom="point", size=7, alpha = 0.8) + 
  stat_err(spread = "se", mult = 2, geom = "errorbarh", height = .1) +
  scale_colour_viridis(option="B", discrete=TRUE) + #, begin=0.2, end=0.8)+
  #geom_text() + 
  #geom_point(data=df.tradeoff.annualmeans, size=10) +
  ylab("Relative reduction in risk to humpbacks\n(normalized)") +
  xlab("Relative revenue to the Dungeness crab fishery") +
  ylim(-50,100) +
  guides(colour = guide_legend("Scenario")) + #,  shape = guide_legend("Scenario")) +
  theme_classic() +
  theme(
    title = element_text(size = 26),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title = element_text(size = 20),
    strip.text = element_text(size=18)#,
    #legend.position = "none"
  )
to_hump_n
dev.off()

### normalized blues
png(paste0(here::here("tradeoffs",
                      "Management scenarios",
                      "figures"), 
           "/Tradeoff plot - normalized blues only.png"), 
    width = 10, height = 8, units = "in", res = 300)

to_blue_n <- ggplot(
  df_tradeoff_focal_scenarios, #df
  aes(
    x=relative_dollars,
    y=relative_blwh_risk_n,
    #label=crab_year,
    colour=pretty_scenario_names
  )
) + # group=1 tells ggplot that there is only 1 group
  geom_point(size=2, alpha=0.6) +
  #geom_label_repel() +
  stat_err(spread = "se", mult = 2, width=.1) + 
  stat_err(geom="point", size=7, alpha = 0.8) + 
  stat_err(spread = "se", mult = 2, geom = "errorbarh", height = .1) +
  scale_colour_viridis(option="B", discrete=TRUE) + #, begin=0.2, end=0.8)+
  #geom_text() + 
  #geom_point(data=df.tradeoff.annualmeans, size=10) +
  ylab("Relative reduction in risk to blues\n(normalized)") +
  xlab("Relative revenue to the Dungeness crab fishery") +
  ylim(-50,100) +
  guides(colour = guide_legend("Scenario")) + #,  shape = guide_legend("Scenario")) +
  theme_classic() +
  theme(
    title = element_text(size = 26),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title = element_text(size = 20),
    strip.text = element_text(size=18)#,
    #legend.position = "none"
  )
to_blue_n
dev.off()

### normalized both
png(paste0(here::here("tradeoffs",
                      "Management scenarios",
                      "figures"), 
           "/Tradeoff plot - normalized humpbacks and blues.png"), 
    width = 10, height = 8, units = "in", res = 300)

to_both_n <- ggplot(
  df_tradeoff_focal_scenarios, #df
  aes(
    x=relative_dollars,
    y=mean_whale_risk_n,
    #label=crab_year,
    colour=pretty_scenario_names
  )
) + # group=1 tells ggplot that there is only 1 group
  geom_point(size=2, alpha=0.6) +
  #geom_label_repel() +
  stat_err(spread = "se", mult = 2, width=.1) + 
  stat_err(geom="point", size=7, alpha = 0.8) + 
  stat_err(spread = "se", mult = 2, geom = "errorbarh", height = .1) +
  scale_colour_viridis(option="B", discrete=TRUE) + #, begin=0.2, end=0.8)+
  #geom_text() + 
  #geom_point(data=df.tradeoff.annualmeans, size=10) +
  ylab("Relative reduction in risk to\nhumpback and blue whales (normalized)") +
  xlab("Relative revenue to the Dungeness crab fishery") +
  ylim(-50,100) +
  guides(colour = guide_legend("Scenario")) + #,  shape = guide_legend("Scenario")) +
  theme_classic() +
  theme(
    title = element_text(size = 26),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title = element_text(size = 20),
    strip.text = element_text(size=18)#,
    #legend.position = "none"
  )
to_both_n
dev.off()



#############
# quick try at a plot for new output 050820
library(ggerr)

ggplot(
  df_tradeoff, #df
  aes(
    x=relative_dollars,
    y=relative_hump_risk,
    #label=crab.year,
    colour=scenario_df_name
  )
) + # group=1 tells ggplot that there is only 1 group
  geom_point(size=2, alpha=0.6) +
  stat_err(spread = "se", mult = 2, width=.1) + 
  stat_err(geom="point", size=7, alpha = 0.8) + 
  stat_err(spread = "se", mult = 2, geom = "errorbarh", height = .1) +
  #geom_text() + 
  #geom_point(data=df.tradeoff.annualmeans, size=10) +
  ylab("Relative reduction in risk to humpbacks") +
  xlab("Relative revenue to the Dungeness crab fishery") +
  #guides(color = guide_legend("Scenario"),  shape = guide_legend("Scenario")) +
  theme_classic() +
  theme(
    title = element_text(size = 26),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title = element_text(size = 20),
    strip.text = element_text(size=18),
    legend.position = "none"
  )

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

# START HERE to make this into a function
  
png(paste0(plot.filepath, "within_year_relative_dollars_DCRB_relative_Hump_risk_pings_2009-18_annualmeans.png"), width = 10, height = 8, units = "in", res = 300)
p_0_h <- ggplot(
  df.tradeoff, #df
  aes(
    x=relative_dollars_DCRB,
    y=relative_Hump_risk_pings,
    #label=crab.year,
    colour=Scenario_long
  )
) + # group=1 tells ggplot that there is only 1 group
  geom_point(size=2, alpha=0.6) +
  stat_err(spread = "se", mult = 2, width=.1) + 
  stat_err(geom="point", size=7, alpha = 0.8) + 
  stat_err(spread = "se", mult = 2, geom = "errorbarh", height = .1) +
  #geom_text() + 
  #geom_point(data=df.tradeoff.annualmeans, size=10) +
  ylab("Relative reduction in risk to humpbacks") +
  xlab("Relative revenue to the Dungeness crab fishery") +
  guides(color = guide_legend("Scenario"),  shape = guide_legend("Scenario")) +
  theme_classic() +
  theme(
    title = element_text(size = 26),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title = element_text(size = 20),
    strip.text = element_text(size=18)
  )
p_0_h
dev.off()

png(paste0(plot.filepath, "within_year_relative_dollars_DCRB_relative_Blue_risk_pings_2009-18_annualmeans.png"), width = 10, height = 8, units = "in", res = 300)
p_0_b <- ggplot(
  df.tradeoff, #df
  aes(
    x=relative_dollars_DCRB,
    y=relative_Blue_risk_pings,
    #label=crab.year,
    colour=Scenario_long
  )
) + # group=1 tells ggplot that there is only 1 group
  geom_point(size=2, alpha=0.6) +
  stat_err(spread = "se", mult = 2, width=.1) + 
  stat_err(geom="point", size=7, alpha = 0.8) + 
  stat_err(spread = "se", mult = 2, geom = "errorbarh", height = .1) +
  #geom_text() + 
  #geom_point(data=df.tradeoff.annualmeans, size=10) +
  ylab("Relative reduction in risk to blues") +
  xlab("Relative revenue to the Dungeness crab fishery") +
  guides(color = guide_legend("Scenario"),  shape = guide_legend("Scenario")) +
  theme_classic() +
  theme(
    title = element_text(size = 26),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title = element_text(size = 20),
    strip.text = element_text(size=18)
  )
p_0_b
dev.off()

png(paste0(plot.filepath, "within_year_relative_dollars_DCRB_relative_Hump_risk_pings_2009-18.png"), width = 10, height = 8, units = "in", res = 300)
p_1 <- ggplot(
  df.tradeoff, #df
  aes(
    x=relative_dollars_DCRB,
    y=relative_Hump_risk_pings,
    label=crab.year,
    colour=Scenario_long
  )
) + # group=1 tells ggplot that there is only 1 group
  geom_point(size=2) + # aes_string(x=time_col,y=response_var,colour=grouping_var,shape=shape_var),
  geom_text_repel() + 
  #facet_wrap(as.formula(paste(time_var, "~", region_var)),nrow=2) +
  #scale_x_continuous(trans = "log10") +  
  ylab("Relative reduction in risk to humpbacks") +
  xlab("Relative revenue to the Dungeness crab fishery") +
  theme_classic() +
  theme(
    title = element_text(size = 26),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title = element_text(size = 20),
    strip.text = element_text(size=18))
p_1
dev.off()

png(paste0(plot.filepath, "within_year_relative_dollars_DCRB_relative_Blue_risk_pings_2009-18.png"), width = 10, height = 8, units = "in", res = 300)
p_2 <- ggplot(
  df.tradeoff, #df
  aes(
    x=relative_dollars_DCRB,
    y=relative_Blue_risk_pings,
    label=crab.year,
    colour=Scenario_long
  )
) + # group=1 tells ggplot that there is only 1 group
  geom_point(size=2) + # aes_string(x=time_col,y=response_var,colour=grouping_var,shape=shape_var),
  geom_text_repel() + 
  #facet_wrap(as.formula(paste(time_var, "~", region_var)),nrow=2) +
  #scale_x_continuous(trans = "log10") +  
  ylab("Relative reduction in risk to blues") +
  xlab("Relative revenue to the Dungeness crab fishery") +
  theme_classic() +
  theme(
    title = element_text(size = 26),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title = element_text(size = 20),
    strip.text = element_text(size=18))
p_2
dev.off()
