# Figure 2 - % change in risk in differnet scenarios
# (simple overlap risk metric, hypothetical static fishing effort scenario, hypothetical static whale distribtuion scenario)

#-----------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(viridis)
library(stringr) 
library(ggpubr)

#-----------------------------------------------------------------------------------

# the data exists in different scripts, but all % change values are in Table 1
# manually put into a df



df_percent_change_in_risk <- data.frame(
                                  Species = c("Humpback whale", "Humpback whale", "Humpback whale", "Humpback whale", "Humpback whale", "Humpback whale",
                                           "Blue whale", "Blue whale", "Blue whale", "Blue whale", "Blue whale", "Blue whale"),
                                  Scenario = c("Simple risk metric", "Static fishing", "Static whales", "Simple risk metric", "Static fishing", "Static whales",
                                               "Simple risk metric", "Static fishing", "Static whales", "Simple risk metric", "Static fishing", "Static whales"),
                                  Comparison = c("Pre-regulations vs 2019", "Pre-regulations vs 2019", "Pre-regulations vs 2019", "Pre-regulations vs 2020", "Pre-regulations vs 2020", "Pre-regulations vs 2020",
                                                 "Pre-regulations vs 2019", "Pre-regulations vs 2019", "Pre-regulations vs 2019", "Pre-regulations vs 2020", "Pre-regulations vs 2020", "Pre-regulations vs 2020"),
                                  Percent_change = c(-78, -70, -23, -51, -10, -45,
                                                     -12, 11, -21, -20, 24, -34)
                                  )




df_percent_change_in_risk_HW <- df_percent_change_in_risk %>% 
  filter(Species == "Humpback whale")

plot_percent_change_HW <- ggplot(df_percent_change_in_risk_HW, aes(x=Scenario, y=Percent_change, fill=Comparison)) + 
  geom_bar(stat='identity',color="black", position=position_dodge())  +
  scale_fill_manual(name="Comparison", 
                    labels = c("Pre-regulations vs 2019", "Pre-regulations vs 2020"), 
                    values = c("Pre-regulations vs 2019"="white", "Pre-regulations vs 2020"="grey")) + 

  #scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ylim(-80, 10) + 
  ylab("% change in risk") + 
  xlab("") + 
  geom_hline(yintercept=0)+
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 25),
        #legend.position = c(.85, .25),
        legend.position = "none",
        axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0,color="black"),
        #axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x = element_line(color="white"),
        axis.text.y = element_text(size = 40,color="black"),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  ) 
plot_percent_change_HW



df_percent_change_in_risk_BW <- df_percent_change_in_risk %>% 
  filter(Species == "Blue whale")

plot_percent_change_BW <- ggplot(df_percent_change_in_risk_BW, aes(x=Scenario, y=Percent_change, fill=Comparison)) + 
  geom_bar(stat='identity',color="black", position=position_dodge())  +
  scale_fill_manual(name="Comparison", 
                    labels = c("Pre-regulations vs 2019", "Pre-regulations vs 2020"), 
                    values = c("Pre-regulations vs 2019"="white", "Pre-regulations vs 2020"="grey")) + 
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ylim(-40, 30) + 
  ylab("% change in risk") + 
  xlab("") + 
  geom_hline(yintercept=0)+
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 25),
        #legend.position = c(.8, .85),
        legend.position = "none",
        axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0,color="black"),
        #axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x = element_line(color="white"),
        axis.text.y = element_text(size = 40,color="black"),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  ) 
plot_percent_change_BW



# save plots
path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
png(paste0(path_figures, "/BW_percent_change_in_risk_v2.png"), width = 22, height = 18, units = "in", res = 400)
ggarrange(plot_percent_change_BW,
          ncol=1,
          nrow=1
          #legend="top",
          #labels="auto",
          #vjust=8,
          #hjust=-0.2
)
invisible(dev.off())


path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
png(paste0(path_figures, "/HW_percent_change_in_risk_v2.png"), width = 22, height = 18, units = "in", res = 400)
ggarrange(plot_percent_change_HW,
          ncol=1,
          nrow=1
          #legend="top",
          #labels="auto",
          #vjust=8,
          #hjust=-0.2
)
invisible(dev.off())























