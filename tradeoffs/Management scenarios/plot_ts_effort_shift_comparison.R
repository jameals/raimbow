library(tidyverse)
library(viridis)
library(ggrepel)

####################################################################
####################################################################

# 051820
# start with comparison of scenarios 2, 4 from ppt:
# 1) CenCA delay with spatial fidelity, Remove, Lag, Pile. scenario_table[c(1,4,7),]
# 2) CenCA early closure with temporal fidelity, Remove, Displace. scenario_table[c(2,11),]
# possibly come back later and consider scenario 9 from ppt:
# 3) #1 and #2. scenario_table[c(3, 6, 9, 12, 15, 18),]

# run make_tradeoff_dataframes_function_effort_comparison.R first from effort_shift_comparison.R
annual_statewide_df_n <- read_rds("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/annual_statewide_effort_shift_scenariocomparisons_n_2020-05-19.rds")
glimpse(annual_statewide_df_n)

# make plotting year column for seasonal df's
annual_statewide_df_n$plotting.year <- as.numeric(substr(annual_statewide_df_n$crab_year,6,9))
glimpse(annual_statewide_df_n)

### subsets of scenarios ###

# delay scenario rows. 1,4,7
# delay_rows <- c(
#   1:9,
#   28:36,
#   55:63
# )

# annual_statewide_df_n_delay <- annual_statewide_df_n[delay_rows,]
annual_statewide_df_n_delay <- annual_statewide_df_n %>%
  filter(
    number_id == "1" | number_id == "4" | number_id == "7"
  )

# early closure scenario rows. 2, 11
# early_closure_rows <- c(
#   10:18,
#   118:126
# )

# annual_statewide_df_n_early_closure <- annual_statewide_df_n[early_closure_rows,]
annual_statewide_df_n_early_closure <- annual_statewide_df_n %>%
  filter(
    number_id == "2" | number_id == "11"
  )


# plotting

# 1) CenCA delay with spatial fidelity, Remove, Lag, Pile. scenario_table[c(1,4,7),]

### humpbacks ###

png(paste0(here::here("tradeoffs",
                      "Management scenarios",
                      "figures"), 
           "/Comparison of humpback whale risk with alternative effort shift methods - delayed opening cenCA.png"), 
    width = 10, height = 8, units = "in", res = 300)
p_delay_hump <- ggplot(
  annual_statewide_df_n_delay, #df
  aes(
    x=plotting.year,
    y=risk_humpback,
    colour=delay.method
  )
) + 
  geom_point(aes(shape=delay.method), size=4) +
  geom_line(aes(linetype=delay.method)) + 
  scale_colour_viridis(option="D", discrete=TRUE, begin=0.2, end=0.8)+
  ylab("Risk to humpback whales") +
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
p_delay_hump
dev.off()

### normalized humpbacks ###

png(paste0(here::here("tradeoffs",
                      "Management scenarios",
                      "figures"), 
           "/Comparison of normalized humpback whale risk with alternative effort shift methods - delayed opening cenCA.png"), 
    width = 10, height = 8, units = "in", res = 300)
p_delay_hump_n <- ggplot(
  annual_statewide_df_n_delay, #df
  aes(
    x=plotting.year,
    y=n_risk_humpback,
    colour=delay.method
  )
) + 
  geom_point(aes(shape=delay.method), size=4) +
  geom_line(aes(linetype=delay.method)) + 
  scale_colour_viridis(option="D", discrete=TRUE, begin=0.2, end=0.8)+
  ylab("Risk to humpback whales\n(normalized)") +
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
p_delay_hump_n
dev.off()

### blues ###

png(paste0(here::here("tradeoffs",
                      "Management scenarios",
                      "figures"), 
           "/Comparison of blue whale risk with alternative effort shift methods - delayed opening cenCA.png"), 
    width = 10, height = 8, units = "in", res = 300)
p_delay_blue <- ggplot(
  annual_statewide_df_n_delay, #df
  aes(
    x=plotting.year,
    y=risk_blue,
    colour=delay.method
  )
) + 
  geom_point(aes(shape=delay.method), size=4) +
  geom_line(aes(linetype=delay.method)) + 
  scale_colour_viridis(option="D", discrete=TRUE, begin=0.2, end=0.8)+
  ylab("Risk to blue whales") +
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
p_delay_blue
dev.off()

### normalized blues ###

png(paste0(here::here("tradeoffs",
                      "Management scenarios",
                      "figures"), 
           "/Comparison of normalized blue whale risk with alternative effort shift methods - delayed opening cenCA.png"), 
    width = 10, height = 8, units = "in", res = 300)
p_delay_blue_n <- ggplot(
  annual_statewide_df_n_delay, #df
  aes(
    x=plotting.year,
    y=n_risk_blue,
    colour=delay.method
  )
) + 
  geom_point(aes(shape=delay.method), size=4) +
  geom_line(aes(linetype=delay.method)) + 
  scale_colour_viridis(option="D", discrete=TRUE, begin=0.2, end=0.8)+
  ylab("Risk to blue whales\n(normalized)") +
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
p_delay_blue_n
dev.off()

# 2) CenCA early closure with temporal fidelity, Remove, Displace. scenario_table[c(2,11),]

### humpbacks ###

png(paste0(here::here("tradeoffs",
                      "Management scenarios",
                      "figures"), 
           "/Comparison of humpback whale risk with alternative effort shift methods - early closure cenCA.png"), 
    width = 10, height = 8, units = "in", res = 300)
p_early_closure_hump <- ggplot(
  annual_statewide_df_n_early_closure, #df
  aes(
    x=plotting.year,
    y=risk_humpback,
    colour=closure.method
  )
) + 
  geom_point(aes(shape=closure.method), size=4) +
  geom_line(aes(linetype=closure.method)) + 
  scale_colour_viridis(option="D", discrete=TRUE, begin=0.2, end=0.8)+
  ylab("Risk to humpback whales") +
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
p_early_closure_hump
dev.off()

### normalized humpbacks ###

png(paste0(here::here("tradeoffs",
                      "Management scenarios",
                      "figures"), 
           "/Comparison of normalized humpback whale risk with alternative effort shift methods - early closure cenCA.png"), 
    width = 10, height = 8, units = "in", res = 300)
p_early_closure_hump_n <- ggplot(
  annual_statewide_df_n_early_closure, #df
  aes(
    x=plotting.year,
    y=n_risk_humpback,
    colour=closure.method
  )
) + 
  geom_point(aes(shape=closure.method), size=4) +
  geom_line(aes(linetype=closure.method)) + 
  scale_colour_viridis(option="D", discrete=TRUE, begin=0.2, end=0.8)+
  ylab("Risk to humpback whales\n(normalized)") +
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
p_early_closure_hump_n
dev.off()

### blues ###

png(paste0(here::here("tradeoffs",
                      "Management scenarios",
                      "figures"), 
           "/Comparison of blue whale risk with alternative effort shift methods - early closure cenCA.png"), 
    width = 10, height = 8, units = "in", res = 300)
p_early_closure_blue <- ggplot(
  annual_statewide_df_n_early_closure, #df
  aes(
    x=plotting.year,
    y=risk_blue,
    colour=closure.method
  )
) + 
  geom_point(aes(shape=closure.method), size=4) +
  geom_line(aes(linetype=closure.method)) + 
  scale_colour_viridis(option="D", discrete=TRUE, begin=0.2, end=0.8)+
  ylab("Risk to blue whales") +
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
p_early_closure_blue
dev.off()

### normalized blues ###

png(paste0(here::here("tradeoffs",
                      "Management scenarios",
                      "figures"), 
           "/Comparison of normalized blue whale risk with alternative effort shift methods - early closure cenCA.png"), 
    width = 10, height = 8, units = "in", res = 300)
p_early_closure_blue_n <- ggplot(
  annual_statewide_df_n_early_closure, #df
  aes(
    x=plotting.year,
    y=n_risk_blue,
    colour=closure.method
  )
) + 
  geom_point(aes(shape=closure.method), size=4) +
  geom_line(aes(linetype=closure.method)) + 
  scale_colour_viridis(option="D", discrete=TRUE, begin=0.2, end=0.8)+
  ylab("Risk to blue whales\n(normalized)") +
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
p_early_closure_blue_n
dev.off()
