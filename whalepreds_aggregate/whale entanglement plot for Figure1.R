##whale entanglement plot for Figure1

#-----------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(viridis)
library(ggpubr)
library(car)


#-----------------------------------------------------------------------------------

#bring in data file from Lauren Saez

whale_entl <- read_csv(here::here('wdfw','data','From_Lauren_confirmed_entanglements_1982_2022_simple.csv'))

whale_entl_bw_hw <- whale_entl %>% 
  filter(Common_Name %in% c("Humpback Whale", "Humpback whale", "Blue Whale"))

whale_entl_bw_hw$Common_Name[whale_entl_bw_hw$Common_Name=="Humpback whale"]<-"Humpback Whale"

#whale_entl_bw_hw_2005_2020 <- whale_entl_bw_hw %>% 
#  filter(Year > 2004 & Year < 2021)

whale_entl_bw_hw_2005_2020_not_gillnets <- whale_entl_bw_hw %>% 
  filter(!Entanglement_Fishery %in% c("Net", "Gillnet", "Drift Gillnet, CA","Salmon troller commercial, CA","Gillnet tribal, WA","Drift gillnet"))

whale_entl_bw_hw_2005_2020_SummerWinter <- whale_entl_bw_hw_2005_2020_not_gillnets %>% 
  mutate(SummerWinter = case_when(
    Month %in% c(10, 11, 12,1,2,3,4)  ~ 'OctApr',
    Month %in% c(5,6,7,8,9)  ~ 'MaySep'
  ))

whale_entl_bw_hw_2005_2020_SummerWinter_yearmonth <- whale_entl_bw_hw_2005_2020_SummerWinter %>% 
  mutate(Month = factor(Month, levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12'))) %>% 
  mutate(year_month = factor(paste0(Year,"_",Month)))







###
#bar chart
#whale_entl_bw_hw_2005_2020_SummerWinter_yearmonth






#----------------------------

#create all year and SummerWinter combos
Year <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
SummerWinter <- as.factor(c("OctApr", "MaySep")) 
Region <- as.factor(c("all_WC", "WA")) 
year_SummerWinter_combos <- crossing(Year, SummerWinter, Region)


summary_all_WC_ent_2005_2020 <- whale_entl_bw_hw_2005_2020_SummerWinter_yearmonth %>% 
  group_by(Year, SummerWinter) %>% 
  summarise(count_entl_all_WC = n()) %>% 
  mutate(Region  =  "all_WC" )

summary_WA_ent_2005_2020 <- whale_entl_bw_hw_2005_2020_SummerWinter_yearmonth %>% 
  filter(Entanglement_Fishery %in% c("Dungeness crab commercial, WA","Dungeness crab commercial (OR & WA)", "Dungeness crab recreational, WA", "Dungeness crab commercial tribal, WA","Dungeness crab commercial, WA  + tribal","Commercial Dungeness crab, WA")) %>% 
  group_by(Year, SummerWinter) %>% 
  summarise(count_entl_WA = n()) %>% 
  mutate(Region  =  "WA" )

entl_bw_hw_WC_and_WA <-  left_join(year_SummerWinter_combos, summary_all_WC_ent_2005_2020, by=c("Year", "SummerWinter", "Region")) %>%
  left_join(., summary_WA_ent_2005_2020, by=c("Year", "SummerWinter", "Region")) %>%
  mutate(count_entl_all_WC = ifelse(is.na(count_entl_all_WC),0,count_entl_all_WC)) %>%
  mutate(count_entl_WA = ifelse(is.na(count_entl_WA),0,count_entl_WA)) %>%
  mutate(total_count = count_entl_all_WC + count_entl_WA)



ID <- 2005:2020

#this is pretty good, 
entl_bar_plot <-   ggplot(data=entl_bw_hw_WC_and_WA, aes(Year, total_count, fill =forcats::fct_rev(SummerWinter ))) + 
  geom_col(data = . %>% filter( Region=="all_WC"), position = position_dodge(width = 0.9), color='black', alpha = 0.2) +
  geom_col(data = . %>% filter( Region=="WA"), position = position_dodge(width = 0.9), color='black', alpha = 1) +
  scale_fill_manual(values=c("#DFE667", "#54C2CC"))+
  geom_tile(aes(y=NA_integer_, alpha = factor(Region))) + 
  scale_alpha_manual(values = c(0.2,1)) +
  scale_x_continuous("Year", labels = as.character(ID), breaks = ID)+
  ylab("Entanglements") + 
  theme_classic() +
  theme(legend.title = element_blank(),
          #title = element_text(size = 26),
          legend.text = element_text(size = 25),
          legend.position = c(.15, .75),
          axis.text.x = element_text(hjust = 1,size = 40, angle = 60),
          #axis.text.x=element_blank(),
          axis.text.y = element_text(size = 40),
          axis.title = element_text(size = 50),
          strip.text = element_text(size=40),
          strip.background = element_blank(),
          strip.placement = "left"
    )   
entl_bar_plot


#-----------------
#monthly cumulative as sent by Blake


summary_cumulative_bw_hw_2005_2020_by_month <- whale_entl_bw_hw_2005_2020_SummerWinter %>%
  mutate(TimePeriod = case_when(
    Year < 2014  ~ '1998-2013',
    Year > 2013 & Year < 2021  ~ '2014-2020'
  )) %>% 
  filter(!is.na(TimePeriod)) %>% 
  group_by(Common_Name, TimePeriod, SummerWinter) %>% 
  summarise(count_entl = n()) 


other_bar_plot <- ggplot(data=summary_cumulative_bw_hw_2005_2020_by_month, aes(SummerWinter, count_entl, fill =Common_Name)) + 
  geom_col(position = position_dodge2(width = 1, preserve = "single")) +
  scale_fill_manual(values=c("blue", "gray"))+
  facet_wrap(~ TimePeriod) + 
  ylab("Entanglements") +
  scale_x_discrete(limits = rev) +
  xlab("") + 
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 25),
        legend.position = c(.15, .75),
        axis.text.x = element_text(vjust = 0.5,size = 40, angle = 0),
        #axis.text.x=element_blank(),
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  ) 
other_bar_plot





summary_cumulative_bw_hw_Region <- whale_entl_bw_hw_2005_2020_SummerWinter %>%
  mutate(TimePeriod = case_when(
    Year < 2014  ~ '1998-2013',
    Year > 2013 & Year < 2021  ~ '2014-2020'
  )) %>% 
  filter(!is.na(TimePeriod)) %>% 
  group_by(Common_Name, TimePeriod, SummerWinter) %>% 
  summarise(count_entl = n()) %>% 
  mutate(Region  =  "all_WC" )

summary_cumulative_bw_hw_WA <- whale_entl_bw_hw_2005_2020_SummerWinter %>% 
  filter(Entanglement_Fishery %in% c("Dungeness crab commercial, WA","Dungeness crab commercial (OR & WA)", "Dungeness crab recreational, WA", "Dungeness crab commercial tribal, WA","Dungeness crab commercial, WA  + tribal","Commercial Dungeness crab, WA")) %>% 
  mutate(TimePeriod = case_when(
    Year < 2014  ~ '1998-2013',
    Year > 2013 & Year < 2021  ~ '2014-2020'
  )) %>% 
  filter(!is.na(TimePeriod)) %>% 
  group_by(Common_Name, TimePeriod, SummerWinter) %>% 
  summarise(count_entl_WA = n()) %>% 
  mutate(Region  =  "WA" )



summary_cumulative_bw_hw_WC_WA_prepost2014 <-  full_join(summary_cumulative_bw_hw_Region, summary_cumulative_bw_hw_WA, 
                                                         by=c("Common_Name", "TimePeriod", "SummerWinter","Region")) %>%
  mutate(count_entl = ifelse(is.na(count_entl),0,count_entl)) %>%
  mutate(count_entl_WA = ifelse(is.na(count_entl_WA),0,count_entl_WA)) %>%
  mutate(total_count = count_entl + count_entl_WA) %>% 
  add_row(Common_Name  = 'Blue Whale', TimePeriod = '2014-2020', SummerWinter = 'MaySep', Region = 'WA', total_count=0)


entl_bar_plotx <-   ggplot(data=summary_cumulative_bw_hw_WC_WA_prepost2014, aes(SummerWinter, total_count, fill =Common_Name)) + 
  geom_col(data = . %>% filter( Region=="all_WC"), position = position_dodge(width = 0.9), color='black', alpha = 0.2) +
  geom_col(data = . %>% filter( Region=="WA"), position = position_dodge(width = 0.9), color='black', alpha = 1) +
  scale_fill_manual(values=c("#DFE667", "#54C2CC"))+
  geom_tile(aes(y=NA_integer_, alpha = factor(Region))) + 
  scale_alpha_manual(values = c(0.2,1)) +
  facet_wrap(~ TimePeriod) + 
  ylab("Entanglements") + 
  scale_x_discrete(limits = rev) +
  xlab("") + 
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 25),
        legend.position = c(.15, .75),
        axis.text.x = element_text(vjust = 0.5,size = 40, angle = 0),
        #axis.text.x=element_blank(),
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )   
entl_bar_plotx
















#-------------------------

summary_all_WC_ent_2005_2020 <- whale_entl_bw_hw_2005_2020_SummerWinter_yearmonth %>% 
  group_by(Year, SummerWinter) %>% 
  summarise(count_entl_all_WC = n())

summary_WA_ent_2005_2020 <- whale_entl_bw_hw_2005_2020_SummerWinter_yearmonth %>% 
  filter(State == "WA") %>% 
  group_by(Year, SummerWinter) %>% 
  summarise(count_entl_WA = n())


summary_all_WC_WA_ent_2005_2020 <- whale_entl_bw_hw_2005_2020_SummerWinter_yearmonth %>% 
  mutate(Region  = 
           ifelse(State == "WA", "WA", "all_WC" )) %>% 
  group_by(Year, SummerWinter, Region) %>% 
  summarise(count_entl = n())




entl_bw_hw_WC_and_WA <- full_join(summary_all_WC_ent_2005_2020, summary_WA_ent_2005_2020, 
                                  by = c("Year", "SummerWinter"))


#create all year and SummerWinter combos
Year <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
SummerWinter <- as.factor(c("DecApr", "MaySep")) 
Region <- as.factor(c("all_WC", "WA")) 
year_SummerWinter_combos <- crossing(Year, SummerWinter, Region)


whale_entl_bw_hw_2005_2020_combos <- year_SummerWinter_combos %>% 
  left_join(summary_all_WC_WA_ent_2005_2020, by = c("Year", "SummerWinter", "Region")) 

ID <- 2005:2020


ent_bar <-ggplot() +
  geom_col(data=whale_entl_bw_hw_2005_2020_combos, aes(x=Year, y=count_entl_all_WC, fill=factor(SummerWinter)), color='black', position = "dodge", alpha = 0.2) + 
  geom_col(data=whale_entl_bw_hw_2005_2020_combos, aes(x=Year, y=count_entl_WA, fill=factor(SummerWinter)), color='black', position = "dodge") +
  ylab("Entanglements") + 
  scale_x_continuous("Year", labels = as.character(ID), breaks = ID)+
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        #legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 15, angle = 60),
        #axis.text.x=element_blank(),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text = element_text(size=15),
        strip.background = element_blank(),
        strip.placement = "left"
  )
ent_bar




ent_bar2 <- whale_entl_bw_hw_2005_2020_combos %>% 
  group_by(SummerWinter, Year) %>% 
  mutate(cum_tot = cumsum(count_entl)) %>% 
  ggplot(aes(Year, cum_tot, fill =SummerWinter)) + 
  geom_col(data = . %>% filter( Region=="all_WC"), position = position_dodge(width = 0.9), alpha = 0.3) +
  geom_col(data = . %>% filter( Region=="WA"), position = position_dodge(width = 0.9), alpha = 0.6) +
  geom_tile(aes(y=NA_integer_, alpha = factor(Region))) + 
  scale_alpha_manual(values = c(0.3,1)) +
  scale_x_continuous("Year", labels = as.character(ID), breaks = ID)+
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        #legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 15, angle = 60),
        #axis.text.x=element_blank(),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text = element_text(size=15),
        strip.background = element_blank(),
        strip.placement = "left"
  )
ent_bar2














#---------------------------------
whale_entl_hw <- whale_entl_bw_hw_2005_2020_SummerWinter_yearmonth %>% 
  filter(Common_Name == "Humpback Whale" | Common_Name =="Humpback whale") %>% 
  #group_by(year_month) %>% 
  group_by(Year) %>% 
  summarise(count_entl = n()) %>% 
  add_row(Year = 2013, count_entl = NA) %>% 
  arrange(Year)


#create all year month combos
year <- c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")
Year <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
month <- as.factor(c("12", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")) 
month <- ordered(month, levels = c("12", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))

year_month_combos <- crossing(year, month) %>% 
  mutate(year_month = factor(paste0(year,"_",month)))

whale_entl_hw_combos <- Year %>% 
  left_join(whale_entl_hw, by = c("Year")) #%>% 
#mutate(count_entl  = 
#         ifelse(is.na(count_entl ), 0, count_entl ))


ordered.ids <- factor(whale_entl_hw_combos$year_month, levels=whale_entl_hw_combos$year_month)

rects <- tibble(xmin=c("2005_5", "2006_5", "2007_5", "2008_5", "2009_5", "2010_5", "2011_5", "2012_5", "2013_5", "2014_5", "2015_5", "2016_5", "2017_5", "2018_5", "2019_5", "2020_5"), #
                xmax=c("2005_9", "2006_9", "2007_9", "2008_9", "2009_9", "2010_9", "2011_9", "2012_9", "2013_9", "2014_9", "2015_9", "2016_9", "2017_9", "2018_9", "2019_9", "2020_9"), #
                ymin=-Inf,ymax=Inf)

ts_hump_entl_2005_2020 <- ggplot() +
  #geom_rect(data=rects,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill="red",alpha=0.3)+
  geom_point(data = whale_entl_hw, 
             aes(x = Year, y = count_entl,
                 group = 1) , size=4) +
  geom_line(data = whale_entl_hw, 
            aes(x = Year, y = count_entl,
                group = 1)) +
  
  #geom_point(data = whale_entl_hw_combos, 
  #           aes(x = factor(year_month, levels=ordered.ids), y = count_entl,
  #               group = 1) , size=4) +
  #geom_line(data = whale_entl_hw_combos, 
  #          aes(x = factor(year_month, levels=ordered.ids), y = count_entl,
  #              group = 1)) +
  #scale_x_discrete(limits=ordered.ids,breaks=ordered.ids[seq(1,length(ordered.ids),by=4)])+
  ylab("Humpback whale entanglements") + 
  #xlab("Year_month") +
  xlab("") +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        #legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 15, angle = 60),
        #axis.text.x=element_blank(),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text = element_text(size=15),
        strip.background = element_blank(),
        strip.placement = "left"
  )
ts_hump_entl_2005_2020





whale_entl_bw <- whale_entl_bw_hw_2005_2020_SummerWinter_yearmonth %>% 
  filter(Common_Name == "Blue Whale") %>% 
  group_by(year_month) %>% 
  summarise(count_entl = n())

#create all year month combos
year <- c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")
month <- as.factor(c("12", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")) 
month <- ordered(month, levels = c("12", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))

year_month_combos <- crossing(year, month) %>% 
  mutate(year_month = factor(paste0(year,"_",month)))

whale_entl_bw_combos <- year_month_combos %>% 
  left_join(whale_entl_bw, by = c("year_month")) #%>% 
#mutate(count_entl  = 
#         ifelse(is.na(count_entl ), 0, count_entl ))


ordered.ids <- factor(whale_entl_bw_combos$year_month, levels=whale_entl_bw_combos$year_month)

rects <- tibble(xmin=c("2005_5", "2006_5", "2007_5", "2008_5", "2009_5", "2010_5", "2011_5", "2012_5", "2013_5", "2014_5", "2015_5", "2016_5", "2017_5", "2018_5", "2019_5", "2020_5"), #
                xmax=c("2005_9", "2006_9", "2007_9", "2008_9", "2009_9", "2010_9", "2011_9", "2012_9", "2013_9", "2014_9", "2015_9", "2016_9", "2017_9", "2018_9", "2019_9", "2020_9"), #
                ymin=-Inf,ymax=Inf)

ts_blue_entl_2005_2020 <- ggplot() +
  geom_rect(data=rects,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill="red",alpha=0.3)+
  geom_point(data = whale_entl_bw_combos, 
             aes(x = factor(year_month, levels=ordered.ids), y = count_entl,
                 group = 1) , size=4) +
  geom_line(data = whale_entl_bw_combos, 
            aes(x = factor(year_month, levels=ordered.ids), y = count_entl,
                group = 1)) +
  scale_x_discrete(limits=ordered.ids,breaks=ordered.ids[seq(1,length(ordered.ids),by=4)])+
  ylab("Blue whale entanglements") + 
  #xlab("Year_month") +
  xlab("") +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        #legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 15, angle = 60),
        #axis.text.x=element_blank(),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text = element_text(size=15),
        strip.background = element_blank(),
        strip.placement = "left"
  )
ts_blue_entl_2005_2020





summary_whale_entl_bw_hw_2005_2020 <- whale_entl_bw_hw_2005_2020_SummerWinter_yearmonth %>% 
  group_by(Year, State, SummerWinter) %>% 
  summarise(count_entl = n())

# summary_whale_entl_bw_hw_2005_2020_WA <- whale_entl_bw_hw_2005_2020_SummerWinter_yearmonth %>% 
#   filter(State == "WA") %>% 
#   group_by(year_month) %>% 
#   summarise(count_entl = n())

#create all year month combos
Year <- c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")
Year <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
month <- as.factor(c("12", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")) 
month <- ordered(month, levels = c("12", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
SummerWinter <- as.factor(c("DecApr", "MaySep")) 

year_month_combos <- crossing(ear, month) %>% 
  mutate(year_month = factor(paste0(year,"_",month)))

year_SummerWinter_combos <- crossing(Year, SummerWinter)

summary_whale_entl_bw_hw_2005_2020_combos <- year_SummerWinter_combos %>% 
  left_join(summary_whale_entl_bw_hw_2005_2020, by = c("Year", "SummerWinter")) 

# summary_whale_entl_bw_hw_2005_2020_WA_combos <- year_month_combos %>% 
#   left_join(summary_whale_entl_bw_hw_2005_2020_WA, by = c("year_month")) 

ordered.ids <- factor(summary_whale_entl_bw_hw_2005_2020_combos$year_month, levels=summary_whale_entl_bw_hw_2005_2020_combos$year_month)



ent_bar <-ggplot() +
  geom_bar(data=summary_whale_entl_bw_hw_2005_2020_combos, aes(x=year_month, y=count_entl), stat="identity") +
  #geom_bar(data=summary_whale_entl_bw_hw_2005_2020_WA_combos, aes(x=year_month, y=count_entl), stat="identity", fill="blue") +
  scale_x_discrete(limits=ordered.ids,breaks=ordered.ids[seq(1,length(ordered.ids),by=4)])+
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        #legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 15, angle = 60),
        #axis.text.x=element_blank(),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text = element_text(size=15),
        strip.background = element_blank(),
        strip.placement = "left"
  )
ent_bar













