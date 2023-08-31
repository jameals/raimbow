#making dotplots of effect sizes for best AIC models


#-------------------------------------------------------------------------------------------------

library(here)
library(ggplot2)
library(tidyverse)
library(sdmTMB)
library(sf)
library(ggcorrplot)
library(mgcv)
library(ggeffects)
library(tictoc)
library(plotmo)
library(viridis)
library(ggridges)
library(ggpubr)


#-------------------------------------------------------------------------------------------------

###WINTER


fit19b_winter <-  read_rds(here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC','winter','after fixing fuel and crab price',"fit19b_winter.rds"))

#adapt code eric provided
pars_winter <- tidy(fit19b_winter, effects = "fixed", conf.int = TRUE)

pars_winter$model <- c("winter")
#change the terms to something tidier
pars_winter$term <- c("2009-2010", "2010-2011", "2011-2012", "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017", 
                      "2017-2018", "2018-2019", "2019-2020", "HMOS_2", "HMOS_3", "HMOS_4", "HMOS_5", "HMOS_6",  "HMOS_7", "HMOS_8",                      
                      "HMOS_9", "HMOS_10",  "Fishing State", "SST", "Wind", "Depth_poly1",  "Depth_poly2", "Bottom O2",                             
                      "Depth_sd", "Faults",  "Canyon dist",  "Port dist", "Fuel price",  "Crab price",  "Dist closed area",   
                      "Depth_poly1 : bottom O2", "Depth_poly2 : bottom O2")



###SUMMER


fit19b_summer <-  read_rds(here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC','summer','after fixing fuel and crab ppp',"fit19b_summer.rds"))

#adapt code eric provided
pars_summer <- tidy(fit19b_summer, effects = "fixed", conf.int = TRUE)

pars_summer$model <- c("summer")
#change the terms to something tidier
pars_summer$term <- c("2009-2010", "2010-2011", "2011-2012", "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017", 
                      "2017-2018", "2018-2019", "2019-2020", "HMOS_2", "HMOS_3", "HMOS_4", "HMOS_5", "HMOS_6",  "HMOS_7", "HMOS_8",                      
                      "HMOS_9", "HMOS_10",  "HMOS_11", "HMOS_12", "HMOS_13", "HMOS_14", "HMOS_15", "HMOS_16", "HMOS_17", "HMOS_18", "HMOS_19",
                      "Fishing State","WA pot reduction", "SST", "Wind", "Depth_poly1",  "Depth_poly2", "Bottom O2",                             
                      "Depth_sd", "Faults",  "Canyon dist",  "Port dist", "Fuel price",  "Crab price",  "Dist closed area",   
                      "Depth_poly1 : bottom O2", "Depth_poly2 : bottom O2")



###ALL DATA

fit16b_all_data <-  read_rds(here::here('DCRB_sdmTMB', 'exported model objects', 'model selection via AIC', 'all data', 'after fixes',"fit16b_all_data.rds"))

#adapt code eric provided
pars_all_data <- tidy(fit16b_all_data, effects = "fixed", conf.int = TRUE)

pars_all_data$model <- c("all data")
#change the terms to something tidier
pars_all_data$term <- c("2009-2010", "2010-2011", "2011-2012", "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017", 
                      "2017-2018", "2018-2019", "2019-2020", "January", "February", "March", "April", "May", "June",
                      "July", "August", "September",  "WA pot reduction", "SST", "Wind",  "Depth_poly1",  "Depth_poly2", "Depth_sd", 
                       "Faults",  "Canyon dist",  "Port dist", "Fuel price",  "Crab price", "Bottom O2", "Fishing State", 
                      "Dist closed area", "Fishing State : Dist closed area")



#-------------------------------------------------------------------------------------------------


# look into plotting data on same plot, color code by model (winter, summer, all data)

#should we separate season, and month, and HMOS into separate dot plots? -- what about interactions

pars_winter_season <- pars_winter %>% filter(term %in% c("2009-2010", "2010-2011", "2011-2012", "2012-2013", 
                                                         "2013-2014", "2014-2015", "2015-2016", "2016-2017", 
                                                         "2017-2018", "2018-2019", "2019-2020"))

pars_summer_season <- pars_summer %>% filter(term %in% c("2009-2010", "2010-2011", "2011-2012", "2012-2013", 
                                                         "2013-2014", "2014-2015", "2015-2016", "2016-2017", 
                                                         "2017-2018", "2018-2019", "2019-2020"))

pars_all_data_season <- pars_all_data %>% filter(term %in% c("2009-2010", "2010-2011", "2011-2012", "2012-2013", 
                                                         "2013-2014", "2014-2015", "2015-2016", "2016-2017", 
                                                         "2017-2018", "2018-2019", "2019-2020"))

pars_season <- rbind(pars_winter_season, pars_summer_season, pars_all_data_season) %>% 
  mutate(estimate_backtransformed = exp(estimate),
         conf.low_backtransformed = exp(conf.low),
         conf.high_backtransformed = exp(conf.high))



##### FIGURE - SEASON EFFECT SIZE (DOTPLOT) ##### 
dotplot_season <- ggplot(pars_season, aes(term, estimate)) +
  #geom_hline(yintercept = 0,linetype="dotted",size = 1)+
  geom_linerange(aes(ymin=conf.low, ymax = conf.high, 
                  color=model, linetype=model),
                 size = 3, position=position_dodge(width = 0.7)) +
  geom_point(aes(color=model, shape=model), position=position_dodge(width = 0.7), size=6.5) +
  scale_linetype_manual(values=c( "solid", "solid","solid"))+
  scale_shape_manual(values=c(16, 15, 8)) +
  #grayscale plot
  #scale_color_manual(values=c('black', 'gray70','grey50'))+
  #color plot
  scale_color_manual(values=c("#e9072b", "#99d9d9", "#355464" ))+
  theme_bw() +
  xlab("") +
  ylab("Coefficient Estimate") +
  ##change the odder of items
  scale_x_discrete(limits = c("2009-2010", "2010-2011", "2011-2012", "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017", 
                              "2017-2018", "2018-2019", "2019-2020")) +
  #ylim(c(-3,3))+
  #coord_flip() +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title.align = .5,
    legend.title = element_text(size = 50),
    legend.text = element_text(size = 45),
    legend.key.size = unit(1.9, units = "cm"),
    axis.text.x = element_text(angle = 45, size = 45, colour = 'black', hjust=1),
    axis.text.y = element_text(size = 45, colour = 'black'),
    axis.title = element_text(size = 50),
    axis.line = element_line(colour = 'black', size = 2),
    axis.ticks.length=unit(.25, "cm"),
    axis.ticks=element_line(size=2, colour = 'black'),
    strip.text = element_text(size=50, colour = 'black'),
    strip.background = element_blank(),
    strip.placement = "left",
    plot.margin = unit(c(0,0,0,30), "pt")
  ) 
dotplot_season

# #export for main text figure 
# path_figures <- "C:/Users/lrie0/OneDrive/NOAA/Riekkola et al - predicting fishing effort/Figures"
# png(paste0(path_figures, "/dotplot_season.png"), width = 25, height = 14, units = "in", res = 500)
# ggarrange(dotplot_season,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())


#########################################################################################################

##MOST PREDICTORS
pars_winter_me <- pars_winter %>% filter(term %in% c("SST", "Wind",  "Depth_sd", "Faults",  
                                                     "Canyon dist",  "Port dist",  "Fuel price",  "Crab price", 
                                                     "Bottom O2", "Fishing State", "Dist closed area", 
                                                     "Depth_poly1 : bottom O2", "Depth_poly2 : bottom O2", "Depth_poly1",  "Depth_poly2"))

pars_summer_me <- pars_summer %>% filter(term %in% c("WA pot reduction", "SST", "Wind",  "Depth_sd", "Faults",  
                                                     "Canyon dist",  "Port dist",  "Fuel price",  "Crab price", 
                                                     "Bottom O2", "Fishing State", "Dist closed area", 
                                                     "Depth_poly1 : bottom O2", "Depth_poly2 : bottom O2", "Depth_poly1",  "Depth_poly2"))

pars_all_data_me <- pars_all_data %>% filter(term %in% c("WA pot reduction", "SST", "Wind",  "Depth_sd", "Faults",  
                                                         "Canyon dist",  "Port dist",  "Fuel price",  "Crab price", 
                                                         "Bottom O2", "Fishing State", "Dist closed area", 
                                                         "Fishing State : Dist closed area", "Depth_poly1",  "Depth_poly2"))

pars_me <- rbind(pars_winter_me, pars_summer_me, pars_all_data_me)

pars_me <- pars_me %>% 
  mutate(term = ifelse(term == 'Depth_sd','Rugosity',term))


##### FIGURE - MOST VARIABLES ##### 
ggplot(pars_me, aes(term, estimate)) +
  geom_hline(yintercept = 0, linetype='dotted')+
  geom_linerange(aes(ymin=conf.low, ymax = conf.high, color=model, linetype=model),size = 0.9, position=position_dodge(width = 0.7)) +
  geom_point(aes(color=model, shape=model), position=position_dodge(width = 0.7), size=2.2) +
  scale_linetype_manual(values=c("solid", "dashed","solid"))+
  scale_shape_manual(values=c(8, 16, 15)) +
  scale_color_manual(values=c('black','grey50', 'gray70'))+
  theme_bw() +
  xlab("") +
  ylab("Coefficient Estimate") +
  ##change the odder of items
  scale_x_discrete(limits = c("WA pot reduction", "SST", "Wind",  "Depth_sd", "Faults",  
                              "Canyon dist",  "Port dist",  "Fuel price",  "Crab price", 
                               "Fishing State", "Dist closed area", 
                              "Fishing State : Dist closed area", "Bottom O2",
                              "Depth_poly1 : bottom O2", "Depth_poly2 : bottom O2","Depth_poly1",  "Depth_poly2")) +
  #ylim(c(-2,3))+
  coord_flip() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title.align = .5,
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1, units = "cm"),
    axis.text.x = element_text(hjust = 1,size = 12, colour = 'black'),
    axis.text.y = element_text(size = 12, colour = 'black'),
    axis.title = element_text(size = 12),
    axis.line = element_line(colour = 'black', size = 0.7),
    axis.ticks.length=unit(.1, "cm"),
    axis.ticks=element_line(size=0.7, colour = 'black'),
    strip.text = element_text(size=12, colour = 'black'),
    strip.background = element_blank(),
    strip.placement = "left"
  ) 


pars_me_no_depth <- pars_me %>% filter(!term %in% c("Depth_poly1", "Depth_poly2", "Depth_poly1 : bottom O2", "Depth_poly2 : bottom O2")) %>% 
  mutate(estimate_backtransformed = exp(estimate),
         conf.low_backtransformed = exp(conf.low),
         conf.high_backtransformed = exp(conf.high))

#pars_me_no_depth$model <- factor(pars_me_no_depth$model, levels = c("winter", "summer", "all data"))


dotplot_covars <- ggplot(pars_me_no_depth, aes(term, estimate)) +
  geom_hline(yintercept = 0, linetype='dotted',size = 1)+ #exp(0)=1
  geom_linerange(aes(ymin=conf.low, ymax = conf.high, 
                     color=model, linetype=model),
                     size = 3.5, position=position_dodge(width = 0.7)) +
  geom_point(aes(color=model, shape=model), position=position_dodge(width = 0.7), size=6.5) +
  scale_linetype_manual(values=c( "solid", "solid","solid"))+
  scale_shape_manual(values=c(16, 15, 8)) +
  #grayscale plot
  #scale_color_manual(values=c('black', 'gray70','grey50'))+
  #color plot
  scale_color_manual(values=c("#e9072b", "#99d9d9", "#355464" ))+
  theme_bw() +
  xlab("") +
  ylab("Coefficient Estimate") +
  ##change the odder of items
  scale_x_discrete(limits = c("WA pot reduction", "SST", "Wind",  "Rugosity", "Faults",  
                              "Canyon dist",  "Port dist",  "Fuel price",  "Crab price", 
                              "Fishing State : Dist closed area", "Fishing State", "Dist closed area", 
                              "Bottom O2")) +
  #ylim(c(-1,11))+
  coord_flip() +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title.align = .5,
    legend.title = element_text(size = 50),
    legend.text = element_text(size = 45),
    legend.key.size = unit(1.9, units = "cm"),
    axis.text.x = element_text(size = 45, colour = 'black'),
    axis.text.y = element_text(size = 45, colour = 'black'),
    axis.title = element_text(size = 50),
    axis.line = element_line(colour = 'black', size = 2),
    axis.ticks.length=unit(.25, "cm"),
    axis.ticks=element_line(size=2, colour = 'black'),
    strip.text = element_text(size=50, colour = 'black'),
    strip.background = element_blank(),
    strip.placement = "left",
    plot.margin = unit(c(0,0,0,30), "pt")
  ) 
dotplot_covars

# #export for main text figure - depth curve
# path_figures <- "C:/Users/lrie0/OneDrive/NOAA/Riekkola et al - predicting fishing effort/Figures"
# png(paste0(path_figures, "/dotplot_covars.png"), width = 25, height = 14, units = "in", res = 500)
# ggarrange(dotplot_covars,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())


pars_me_depth_only <- pars_me %>% filter(term %in% c("Depth_poly1", "Depth_poly2", "Depth_poly1 : bottom O2", "Depth_poly2 : bottom O2"))

ggplot(pars_me_depth_only, aes(term, estimate)) +
  geom_hline(yintercept = 0, linetype='dotted')+
  geom_linerange(aes(ymin=conf.low, ymax = conf.high, color=model, linetype=model),size = 0.9, position=position_dodge(width = 0.5)) +
  geom_point(aes(color=model, shape=model), position=position_dodge(width = 0.5), size=2.2) +
  scale_linetype_manual(values=c("solid", "dashed","solid"))+
  scale_shape_manual(values=c(8, 16, 15)) +
  scale_color_manual(values=c('black','grey50', 'gray70'))+
  theme_bw() +
  xlab("") +
  ylab("Coefficient Estimate") +
  ##change the odder of items
  scale_x_discrete(limits = c("Depth_poly1 : bottom O2", "Depth_poly2 : bottom O2", "Depth_poly1",  "Depth_poly2")) +
  #ylim(c(-2,3))+
  coord_flip() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title.align = .5,
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1, units = "cm"),
    axis.text.x = element_text(hjust = 1,size = 12, colour = 'black'),
    axis.text.y = element_text(size = 12, colour = 'black'),
    axis.title = element_text(size = 12),
    axis.line = element_line(colour = 'black', size = 0.7),
    axis.ticks.length=unit(.1, "cm"),
    axis.ticks=element_line(size=0.7, colour = 'black'),
    strip.text = element_text(size=12, colour = 'black'),
    strip.background = element_blank(),
    strip.placement = "left"
  ) 










#########################################################################################################
##MONTH AND HMOS

pars_winter_HMOS <- pars_winter %>% filter(term %in% c("HMOS_2", "HMOS_3", "HMOS_4", "HMOS_5", "HMOS_6",  "HMOS_7", 
                                                       "HMOS_8",  "HMOS_9", "HMOS_10"))

pars_summer_HMOS <- pars_summer %>% filter(term %in% c("HMOS_2", "HMOS_3", "HMOS_4", "HMOS_5", "HMOS_6",  "HMOS_7", 
                                                       "HMOS_8", "HMOS_9", "HMOS_10",  "HMOS_11", "HMOS_12", "HMOS_13",
                                                       "HMOS_14", "HMOS_15", "HMOS_16", "HMOS_17", "HMOS_18", "HMOS_19"))

pars_all_data_month <- pars_all_data %>% filter(term %in% c("January", "February", "March", "April", "May", "June",
                                                            "July", "August", "September"))

pars_month_HMOS<- rbind(pars_winter_HMOS, pars_summer_HMOS, pars_all_data_month) %>% 
  mutate(estimate_backtransformed = exp(estimate),
         conf.low_backtransformed = exp(conf.low),
         conf.high_backtransformed = exp(conf.high))




##### FIGURE - MONTH/HMOS EFFECT SIZE (DOTPLOT) ##### 
##KEEP IN LOG SCALE##
dotplot_month_HMOS <-ggplot(pars_month_HMOS, aes(term, estimate)) +
  #geom_hline(yintercept = 0,linetype="dotted",size = 1)+
  geom_linerange(aes(ymin=conf.low, ymax = conf.high, 
                     color=model, linetype=model),size =3, 
                     position=position_dodge(width = 0.7)) +
  geom_point(aes(color=model, shape=model), position=position_dodge(width = 0.7), size=6.5) +
  scale_linetype_manual(values=c( "solid", "solid","solid"))+
  scale_shape_manual(values=c(16, 15, 8)) +
  #grayscale plot
  #scale_color_manual(values=c('black', 'gray70','grey50'))+
  #color plot
  scale_color_manual(values=c("#e9072b", "#99d9d9", "#355464" ))+
  theme_bw() +
  xlab("") +
  ylab("Coefficient Estimate") +
  ##change the odder of items
  scale_x_discrete(limits = c("HMOS_2", "HMOS_3", "HMOS_4", "HMOS_5", "HMOS_6",  "HMOS_7", 
                              "HMOS_8", "HMOS_9", "HMOS_10",  "HMOS_11", "HMOS_12", "HMOS_13",
                              "HMOS_14", "HMOS_15", "HMOS_16", "HMOS_17", "HMOS_18", "HMOS_19",
                              "January", "February", "March", "April", "May", "June",
                              "July", "August", "September"),
                   labels = c("HMOS_2", "HMOS_3", "HMOS_4", "HMOS_5", "HMOS_6",  "HMOS_7", 
                              "HMOS_8", "HMOS_9", "HMOS_10",  "HMOS_11", "HMOS_12", "HMOS_13",
                              "HMOS_14", "HMOS_15", "HMOS_16", "HMOS_17", "HMOS_18", "HMOS_19",
                              "January", "February", "March", "April", "May", "June",
                              "July", "August", "September")) +
  #ylim(c(-15,5))+
  #coord_flip() +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title.align = .5,
    legend.title = element_text(size = 50),
    legend.text = element_text(size = 45),
    legend.key.size = unit(1.9, units = "cm"),
    axis.text.x = element_text(size = 35, colour = 'black',angle = 45, hjust = 1), #hjust = 1,
    axis.text.y = element_text(size = 45, colour = 'black'),
    axis.title = element_text(size = 50),
    axis.line = element_line(colour = 'black', size = 2),
    axis.ticks.length=unit(.25, "cm"),
    axis.ticks=element_line(size=2, colour = 'black'),
    strip.text = element_text(size=50, colour = 'black'),
    strip.background = element_blank(),
    strip.placement = "left",
    plot.margin = unit(c(0,0,0,30), "pt")
  ) 
dotplot_month_HMOS

# #export for main text figure - depth curve
# path_figures <- "C:/Users/lrie0/OneDrive/NOAA/Riekkola et al - predicting fishing effort/Figures"
# png(paste0(path_figures, "/dotplot_month_HMOS.png"), width = 25, height = 14, units = "in", res = 500)
# ggarrange(dotplot_month_HMOS,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())

############################################################################################################
############################################################################################################

















