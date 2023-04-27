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






ggplot(pars_winter, aes(term, estimate)) +
  geom_linerange(aes(ymin=conf.low, ymax = conf.high),col=viridis(1)) +
  geom_point(col=viridis(1)) +
  theme_bw() +
  xlab("") +
  ylab("Estimate") + 
  ##change the odder of items
  scale_x_discrete(limits = c("2009-2010", "2010-2011", "2011-2012", "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017", 
                              "2017-2018", "2018-2019", "2019-2020", "HMOS_2", "HMOS_3", "HMOS_4", "HMOS_5", "HMOS_6",  "HMOS_7", "HMOS_8",                      
                              "HMOS_9", "HMOS_10",  "Fishing State", "SST", "Wind", "Bottom O2",                             
                              "Depth_sd", "Faults",  "Canyon dist",  "Port dist", "Fuel price",  "Crab price",  "Dist closed area",   
                              "Depth_poly1 : bottom O2", "Depth_poly2 : bottom O2", "Depth_poly1",  "Depth_poly2")) +
  ylim(c(-15,15))+
  coord_flip()



pars_winter_HMOS <- pars_winter %>% filter(term %in% c("HMOS_2", "HMOS_3", "HMOS_4", "HMOS_5", "HMOS_6",  "HMOS_7", "HMOS_8",                      
                                                       "HMOS_9", "HMOS_10"))
ggplot(pars_winter_HMOS, aes(term, estimate)) +
  geom_linerange(aes(ymin=conf.low, ymax = conf.high),col=viridis(1)) +
  geom_point(col=viridis(1)) +
  theme_bw() +
  xlab("") +
  ylab("Estimate") + 
  ##change the odder of items
  scale_x_discrete(limits = c("HMOS_10", "HMOS_9","HMOS_8", "HMOS_7", "HMOS_6", 
                              "HMOS_5", "HMOS_4", "HMOS_3",  "HMOS_2"))+
  #ylim(c(-5,5))+
  coord_flip()



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


ggplot(pars_summer, aes(term, estimate)) +
  geom_linerange(aes(ymin=conf.low, ymax = conf.high),col=viridis(1)) +
  geom_point(col=viridis(1)) +
  theme_bw() +
  xlab("") +
  ylab("Estimate") + 
  ##change the odder of items
  scale_x_discrete(limits = c("2009-2010", "2010-2011", "2011-2012", "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017", 
                              "2017-2018", "2018-2019", "2019-2020", "HMOS_2", "HMOS_3", "HMOS_4", "HMOS_5", "HMOS_6",  "HMOS_7", "HMOS_8",                      
                              "HMOS_9", "HMOS_10",  "HMOS_11", "HMOS_12", "HMOS_13", "HMOS_14", "HMOS_15", "HMOS_16", "HMOS_17", "HMOS_18", "HMOS_19",
                              "Fishing State", "WA pot reduction", "SST", "Wind", "Bottom O2",                             
                              "Depth_sd", "Faults",  "Canyon dist",  "Port dist", "Fuel price",  "Crab price",  "Dist closed area",   
                              "Depth_poly1 : bottom O2", "Depth_poly2 : bottom O2", "Depth_poly1",  "Depth_poly2")) +
  ylim(c(-15,15))+
  coord_flip()




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

ggplot(pars_all_data, aes(term, estimate)) +
  geom_linerange(aes(ymin=conf.low, ymax = conf.high),col=viridis(1)) +
  geom_point(col=viridis(1)) +
  theme_bw() +
  xlab("") +
  ylab("Estimate") + 
  ##change the odder of items
  scale_x_discrete(limits = c("2009-2010", "2010-2011", "2011-2012", "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017", 
                              "2017-2018", "2018-2019", "2019-2020", "January", "February", "March", "April", "May", "June",
                              "July", "August", "September",  "WA pot reduction", "SST", "Wind",  "Depth_sd", 
                              "Faults",  "Canyon dist",  "Port dist", "Fuel price",  "Crab price", "Bottom O2", "Fishing State", 
                              "Dist closed area", "Fishing State : Dist closed area", "Depth_poly1",  "Depth_poly2")) +
  ylim(c(-15,15))+
  coord_flip()


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

pars_season <- rbind(pars_winter_season, pars_summer_season, pars_all_data_season)

ggplot(pars_season, aes(term, estimate)) +
  geom_linerange(aes(ymin=conf.low, ymax = conf.high,color=model), position=position_dodge(width = 0.5)) +
  geom_point(aes(color=model), position=position_dodge(width = 0.5)) +
  theme_bw() +
  xlab("") +
  ylab("Estimate") + 
  ##change the odder of items
  scale_x_discrete(limits = c("2009-2010", "2010-2011", "2011-2012", "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017", 
                              "2017-2018", "2018-2019", "2019-2020")) +
  ylim(c(-15,5))+
  coord_flip()







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

ggplot(pars_me, aes(term, estimate)) +
  geom_linerange(aes(ymin=conf.low, ymax = conf.high,color=model), position=position_dodge(width = 0.5)) +
  geom_point(aes(color=model), position=position_dodge(width = 0.5)) +
  theme_bw() +
  xlab("") +
  ylab("Estimate") + 
  ##change the odder of items
  scale_x_discrete(limits = c("WA pot reduction", "SST", "Wind",  "Depth_sd", "Faults",  
                              "Canyon dist",  "Port dist",  "Fuel price",  "Crab price", 
                               "Fishing State", "Dist closed area", 
                              "Fishing State : Dist closed area", "Bottom O2",
                              "Depth_poly1 : bottom O2", "Depth_poly2 : bottom O2","Depth_poly1",  "Depth_poly2")) +
  ylim(c(-2,3))+
  geom_hline(yintercept = 0, linetype='dotted')+
  coord_flip() 




##MONTH AND HMOS

pars_winter_HMOS <- pars_winter %>% filter(term %in% c("HMOS_2", "HMOS_3", "HMOS_4", "HMOS_5", "HMOS_6",  "HMOS_7", 
                                                       "HMOS_8",  "HMOS_9", "HMOS_10"))

pars_summer_HMOS <- pars_summer %>% filter(term %in% c("HMOS_2", "HMOS_3", "HMOS_4", "HMOS_5", "HMOS_6",  "HMOS_7", 
                                                       "HMOS_8", "HMOS_9", "HMOS_10",  "HMOS_11", "HMOS_12", "HMOS_13",
                                                       "HMOS_14", "HMOS_15", "HMOS_16", "HMOS_17", "HMOS_18", "HMOS_19"))

pars_all_data_month <- pars_all_data %>% filter(term %in% c("January", "February", "March", "April", "May", "June",
                                                            "July", "August", "September"))

pars_month_HMOS<- rbind(pars_winter_HMOS, pars_summer_HMOS, pars_all_data_month)

ggplot(pars_month_HMOS, aes(term, estimate)) +
  geom_linerange(aes(ymin=conf.low, ymax = conf.high,color=model), position=position_dodge(width = 0.5)) +
  geom_point(aes(color=model), position=position_dodge(width = 0.5)) +
  theme_bw() +
  xlab("") +
  ylab("Estimate") + 
  ##change the odder of items
  scale_x_discrete(limits = c("HMOS_2", "HMOS_3", "HMOS_4", "HMOS_5", "HMOS_6",  "HMOS_7", 
                              "HMOS_8", "HMOS_9", "HMOS_10",  "HMOS_11", "HMOS_12", "HMOS_13",
                              "HMOS_14", "HMOS_15", "HMOS_16", "HMOS_17", "HMOS_18", "HMOS_19",
                              "January", "February", "March", "April", "May", "June",
                              "July", "August", "September")) +
  #ylim(c(-15,5))+
  geom_hline(yintercept = 0, linetype='dotted')+
  coord_flip()




























