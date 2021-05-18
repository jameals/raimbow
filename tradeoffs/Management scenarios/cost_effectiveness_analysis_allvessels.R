library(tidyverse)
library(foreign) # read.dbf()
library(here)
library(lubridate)
library(sf)
library(viridis)
library(ggrepel)
library(magrittr)
library(ggerr)
library(scales)
library(maps)
library(rnaturalearth)
library(gridExtra)
library(ggpubr)
library(knitr)


path_df_tradeoff_focal_scenarios <- "/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/df_tradeoff_focal_scenarios_2020-06-12.rds" 
path_df_tradeoff_focal_scenarios_small_vessels <- "/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/df_tradeoff_focal_scenarios_small_vessels_2020-06-16.rds"
path_figures <- "/Users/jameal.samhouri/Dropbox/Projects/In progress/RAIMBOW/raimbow/tradeoffs/Management scenarios/figures"


## all vessels first

# read in tradeoff df, make pretty scenario names, and join with annual df
df_tradeoff_focal_scenarios <- read_rds(path_df_tradeoff_focal_scenarios)
glimpse(df_tradeoff_focal_scenarios)

# make nice scenario names
#unique(df_tradeoff_focal_scenarios$scenario_df_name)
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
  rep("Delay CenCA, Early Closure CenCA", length(unique(df_tradeoff_focal_scenarios$crab_year))),
  rep("Statewide 50% Effort Reduction Apr-Jul", length(unique(df_tradeoff_focal_scenarios$crab_year))),
  rep("CenCA 50% Effort Reduction Apr-Jul", length(unique(df_tradeoff_focal_scenarios$crab_year))),
  rep("Statewide <30 fathom Depth Restriction Apr-Jul", length(unique(df_tradeoff_focal_scenarios$crab_year))),
  rep("CenCA <30 fathom Depth Restriction Apr-Jul", length(unique(df_tradeoff_focal_scenarios$crab_year))),
  rep("Statewide 50% Effort Reduction, <30 fathom Depth Restriction Apr-Jul", length(unique(df_tradeoff_focal_scenarios$crab_year))),
  rep("CenCA 50% Effort Reduction, <30 fathom Depth Restriction Apr-Jul", length(unique(df_tradeoff_focal_scenarios$crab_year)))
)  

names(df_tradeoff_focal_scenarios)
# check range of costs and benefits
range(df_tradeoff_focal_scenarios$relative_dollars)
range(df_tradeoff_focal_scenarios$relative_hump_risk_n)
range(df_tradeoff_focal_scenarios$relative_blwh_risk_n)

df_tradeoff_focal_scenarios_simple <- df_tradeoff_focal_scenarios %>%
  select(crab_year, pretty_scenario_names, 
         relative_hump_risk_n, relative_blwh_risk_n, relative_dollars) %>%
  filter(
    pretty_scenario_names != "Status Quo"
  ) %>%
  mutate(
    plotting_year = as.numeric(substr(crab_year,6,9)),
    pre_post = ifelse(plotting_year >=2015 & plotting_year <2019, "2014-2018",
                      ifelse(plotting_year <2015, "2009-2014", "2018-2019")
    ),
    relative_dollars_cost = 100 - relative_dollars,
    costs_benefits_hump = relative_dollars_cost / relative_hump_risk_n,
    costs_benefits_blwh = relative_dollars_cost / relative_blwh_risk_n
  ) %>%
  select(-relative_dollars)
glimpse(df_tradeoff_focal_scenarios_simple)

df_tradeoff_focal_scenarios_simple_quantile <- df_tradeoff_focal_scenarios_simple %>%
  group_by(pre_post, pretty_scenario_names) %>%
  summarise(
    quantile = scales::percent(c(0.025, 0.25, 0.5, 0.75, 0.975)),
    relative_hump_risk_n = quantile(relative_hump_risk_n, c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm=TRUE),
    relative_blwh_risk_n = quantile(relative_blwh_risk_n, c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm=TRUE),
    relative_dollars_cost = quantile(relative_dollars_cost, c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm=TRUE),
    costs_benefits_hump = quantile(costs_benefits_hump, c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm=TRUE),
    costs_benefits_blwh = quantile(costs_benefits_blwh, c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm=TRUE)
    ) %>%
  arrange(
    quantile
  )
glimpse(df_tradeoff_focal_scenarios_simple_quantile)


# cost effectiveness blues, boxplot
plot_blue_ce_prepost_bp <- ggplot(
  data = df_tradeoff_focal_scenarios_simple,
  aes(
    x = pretty_scenario_names,
    y = costs_benefits_blwh,
    colour = factor(pre_post)
  )
)  +
  geom_boxplot(position=position_dodge(width=0.5)) +#, width=0.1, color="grey", alpha=0.2) +
  geom_point(position=position_dodge(width=0.5), alpha=0.6)+
  xlab("Scenario") +
  ylab("Cost effectiveness for blue whales\n(% revenue loss / % risk reduction )") +
  #ylim(0,0.4) +
  scale_colour_viridis_d(option="cividis", begin=0.2, end=0.8, direction = -1) +
  scale_x_discrete(labels = wrap_format(30)) + 
  coord_flip() +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    title = element_text(size = 26),
    legend.text = element_text(size = 18),
    #legend.position = c(.1, .95),
    axis.text.x = element_text(size=14), #hjust = 1, size = 14, angle = 60
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16)
    #legend.position = "none"
  )
plot_blue_ce_prepost_bp

png(paste0(path_figures, "/plot_blue_costeffectiveness_bp.png"), width = 14, height = 10, units = "in", res = 300)
plot_blue_ce_prepost_bp
invisible(dev.off())

# cost effectiveness humps, boxplot
plot_hump_ce_prepost_bp <- ggplot(
  data = df_tradeoff_focal_scenarios_simple,
  aes(
    x = pretty_scenario_names,
    y = costs_benefits_hump,
    colour = factor(pre_post)
  )
)  +
  geom_boxplot(position=position_dodge(width=0.5)) +#, width=0.1, color="grey", alpha=0.2) +
  geom_point(position=position_dodge(width=0.5), alpha=0.6)+
  xlab("Scenario") +
  ylab("Cost effectiveness for humpback whales\n(% revenue loss / % risk reduction )") +
  #ylim(0,0.4) +
  scale_colour_viridis_d(option="cividis", begin=0.2, end=0.8, direction = -1) +
  scale_x_discrete(labels = wrap_format(30)) + 
  coord_flip() +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    title = element_text(size = 26),
    legend.text = element_text(size = 18),
    #legend.position = c(.1, .95),
    axis.text.x = element_text(size=14), #hjust = 1, size = 14, angle = 60
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16)
    #legend.position = "none"
  )
plot_hump_ce_prepost_bp

png(paste0(path_figures, "/plot_hump_costeffectiveness_bp.png"), width = 14, height = 10, units = "in", res = 300)
plot_hump_ce_prepost_bp
invisible(dev.off())



##################################
##################################
### CONSIDER CHANGES IN RANK C-E

# arrange from greatest risk during 2014-18 to least
df_rank <- df_tradeoff_focal_scenarios_simple_quantile %>%
  filter(quantile == "50%") %>%
  group_by(pretty_scenario_names, pre_post) %>%
  arrange(
    costs_benefits_hump,
    costs_benefits_blwh,
    pretty_scenario_names
  ) %>%
  #tibble::rowid_to_column("scenario_rank")
  ungroup() %>%
  mutate(scenario_rank = row_number()) %>%
  rename(
    median_relative_hump_risk_n = relative_hump_risk_n,
    median_relative_blwh_risk_n = relative_blwh_risk_n,
    median_relative_dollars_cost = relative_dollars_cost,
    median_costs_benefits_hump = costs_benefits_hump,
    median_costs_benefits_blwh = costs_benefits_blwh
  ) %>%
  select(-quantile) %>%
  group_by(pretty_scenario_names) %>%
  mutate(
    scenario_min_rank = min(scenario_rank) 
  )
  
  #group_by(pretty_scenario_names) %>%
  # arrange(
  #   scenario_rank,
  #   group_by = pretty_scenario_names
  # )

glimpse(df_rank)


df_tradeoff_focal_scenarios_simple_rank <- df_tradeoff_focal_scenarios_simple %>%
  left_join(df_rank, by = c("pre_post", "pretty_scenario_names"))
glimpse(df_tradeoff_focal_scenarios_simple_rank)

plot_ce_hump_prepost_statewide_connect <- ggplot(
  data = df_tradeoff_focal_scenarios_simple_rank,
  aes(
    x = reorder(pretty_scenario_names,-scenario_min_rank),
    y = costs_benefits_hump,
    group = interaction(factor(pre_post),plotting_year),
    colour = factor(pre_post)
  )
)  +
  #geom_boxplot(position=position_dodge(width=0.5)) +#, width=0.1, color="grey", alpha=0.2) +
  geom_point(position=position_dodge(width=0.5))+
  geom_line(position=position_dodge(width=0.5))+
  geom_text_repel(aes(label=crab_year),
                  alpha=0.6) +
  xlab("Scenario") +
  ylab("Cost effectiveness for humpback whales\n(% revenue loss / % risk reduction )") +
  ylim(-0.1,2.3) +
  scale_colour_viridis_d(option = "cividis",begin=0.2, end=0.8, direction = -1) +
  scale_x_discrete(labels = wrap_format(30)) + 
  coord_flip() +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    title = element_text(size = 26),
    legend.text = element_text(size = 18),
    #legend.position = c(.1, .95),
    axis.text.x = element_text(size=14), #hjust = 1, size = 14, angle = 60
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16)
    #legend.position = "none"
  )

png(paste0(path_figures, "/plot_ce_hump_prepost_statewide_connect.png"), width = 14, height = 10, units = "in", res = 300)
plot_ce_hump_prepost_statewide_connect
invisible(dev.off())

plot_ce_blue_prepost_statewide_connect <- ggplot(
  data = df_tradeoff_focal_scenarios_simple_rank,
  aes(
    x = reorder(pretty_scenario_names,-scenario_min_rank),
    y = costs_benefits_blwh,
    group = interaction(factor(pre_post),plotting_year),
    colour = factor(pre_post)
  )
)  +
  #geom_boxplot(position=position_dodge(width=0.5)) +#, width=0.1, color="grey", alpha=0.2) +
  geom_point(position=position_dodge(width=0.5))+
  geom_line(position=position_dodge(width=0.5))+
  geom_text_repel(aes(label=crab_year),
                  alpha=0.6) +
  xlab("Scenario") +
  ylab("Cost effectiveness for blue whales\n(% revenue loss / % risk reduction )") +
  ylim(-0.1,2.3) +
  scale_colour_viridis_d(option = "cividis",begin=0.2, end=0.8, direction = -1) +
  scale_x_discrete(labels = wrap_format(30)) + 
  coord_flip() +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    title = element_text(size = 26),
    legend.text = element_text(size = 18),
    #legend.position = c(.1, .95),
    axis.text.x = element_text(size=14), #hjust = 1, size = 14, angle = 60
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16)
    #legend.position = "none"
  )

png(paste0(path_figures, "/plot_ce_blue_prepost_statewide_connect.png"), width = 14, height = 10, units = "in", res = 300)
plot_ce_blue_prepost_statewide_connect
invisible(dev.off())
