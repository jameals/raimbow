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
  filter(!Entanglement_Fishery %in% c("Net", "Gillnet", "Drift Gillnet, CA","Salmon troller commercial, CA","Gillnet tribal, WA","Drift gillnet", "Recreational hook and line"))

whale_entl_bw_hw_2005_2020_SummerWinter <- whale_entl_bw_hw_2005_2020_not_gillnets %>% 
  mutate(SummerWinter = case_when(
    Month %in% c(10, 11, 12,1,2,3,4)  ~ 'OctApr',
    Month %in% c(5,6,7,8,9)  ~ 'MaySep'
  ))

whale_entl_bw_hw_2005_2020_SummerWinter_yearmonth <- whale_entl_bw_hw_2005_2020_SummerWinter %>% 
  mutate(Month = factor(Month, levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12'))) %>% 
  mutate(year_month = factor(paste0(Year,"_",Month)))





#----------------------------



#monthly cumulative plot as suggested by Blake


summary_cumulative_bw_hw_2005_2020_by_month <- whale_entl_bw_hw_2005_2020_SummerWinter %>%
  mutate(TimePeriod = case_when(
    Year < 2014  ~ '1998-2013',
    Year > 2013 & Year < 2021  ~ '2014-2020'
  )) %>% 
  filter(!is.na(TimePeriod)) %>% 
  group_by(Common_Name, TimePeriod, SummerWinter) %>% 
  summarise(count_entl = n()) %>%
  mutate(Years = case_when(
    TimePeriod == '1998-2013' ~ 16,
    TimePeriod == '2014-2020' ~ 7
  )) %>% 
  mutate(entl_per_year = count_entl/Years) %>% 
  #adding the NAs here causes the bars not to line up with the x-axis labels properly
  #ungroup() %>% 
  #add_row(Common_Name  = c('Blue Whale','Blue Whale','Blue Whale'), 
  #          TimePeriod = c('1998-2013', '1998-2013','2014-2020'), 
  #        SummerWinter = c('OctApr','MaySep','OctApr'), 
  #        entl_per_year= c(NA,NA,NA)) %>% 
  mutate(Months = Years * 12) %>% 
  mutate(entl_per_month = count_entl/Months)

 summary_cumulative_bw_hw_2005_2020_by_month <- summary_cumulative_bw_hw_2005_2020_by_month %>% 
   mutate(SummerWinter = case_when(
     SummerWinter == 'MaySep' ~ 'May-Sep',
     SummerWinter == 'OctApr' ~ 'Oct-Apr'
   ))

other_bar_plot <- ggplot(data=summary_cumulative_bw_hw_2005_2020_by_month, aes(SummerWinter, entl_per_month, fill =Common_Name)) + 
  geom_col(width = 0.9, position = position_dodge2(width = 1, preserve = "single")) +
  #geom_col(width = 0.95, position = position_dodge(1, preserve = "single")) +
  scale_fill_manual(values=c("blue", "gray"))+
  facet_wrap(~ TimePeriod) + 
  ylab("Mean entanglement reports per month") +
  scale_x_discrete(limits = rev) +
  xlab("") + 
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 50),
        legend.key.size = unit(2, units = "cm"),
        legend.position = c(.2, .75),
        axis.text.x = element_text(vjust = 0.5,size = 50, angle = 0, color='black'),
        #axis.text.x=element_blank(),
        axis.text.y = element_text(size = 40, color='black'),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=55),
        strip.background = element_blank(),
        strip.placement = "left"
  ) 
other_bar_plot

#save figure - panel for Figure 1
path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
png(paste0(path_figures, "/confirmed_bw_hw_entl_1998-2020_by month.png"), width = 22, height = 18, units = "in", res = 400)
ggarrange(other_bar_plot,
          ncol=1,
          nrow=1
          #legend="top",
          #labels="auto",
          #vjust=8,
          #hjust=-0.2
)
invisible(dev.off())

#-------------------------------------

#older plotting option, showing entl both across West Coast as well as WA specifically

#create all year and SummerWinter combos
# Year <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
# SummerWinter <- as.factor(c("OctApr", "MaySep")) 
# Region <- as.factor(c("all_WC", "WA")) 
# year_SummerWinter_combos <- crossing(Year, SummerWinter, Region)
# 
# 
# summary_all_WC_ent_2005_2020 <- whale_entl_bw_hw_2005_2020_SummerWinter_yearmonth %>% 
#   group_by(Year, SummerWinter) %>% 
#   summarise(count_entl_all_WC = n(),
#             n_months_WC = n_distinct(Month)) %>% 
#   mutate(Region  =  "all_WC" )
# 
# summary_WA_ent_2005_2020 <- whale_entl_bw_hw_2005_2020_SummerWinter_yearmonth %>% 
#   filter(Entanglement_Fishery %in% c("Dungeness crab commercial, WA","Dungeness crab commercial (OR & WA)", "Dungeness crab recreational, WA", "Dungeness crab commercial tribal, WA","Dungeness crab commercial, WA  + tribal","Commercial Dungeness crab, WA")) %>% 
#   group_by(Year, SummerWinter) %>% 
#   summarise(count_entl_WA = n(),
#             n_months_WA = n_distinct(Month)) %>% 
#   mutate(Region  =  "WA" )
# 
# entl_bw_hw_WC_and_WA <-  left_join(year_SummerWinter_combos, summary_all_WC_ent_2005_2020, by=c("Year", "SummerWinter", "Region")) %>%
#   left_join(., summary_WA_ent_2005_2020, by=c("Year", "SummerWinter", "Region")) %>%
#   mutate(count_entl_all_WC = ifelse(is.na(count_entl_all_WC),0,count_entl_all_WC)) %>%
#   mutate(count_entl_WA = ifelse(is.na(count_entl_WA),0,count_entl_WA)) %>%
#   mutate(total_count = count_entl_all_WC + count_entl_WA)
# ##the distinct months have to be calc across both df, and prob only then calc the entl/month
# 
# 
# ID <- 2005:2020
# 
# #this is pretty good, 
# entl_bar_plot <-   ggplot(data=entl_bw_hw_WC_and_WA, aes(Year, total_count, fill =forcats::fct_rev(SummerWinter ))) + 
#   geom_col(data = . %>% filter( Region=="all_WC"), position = position_dodge(width = 0.9), color='black', alpha = 0.2) +
#   geom_col(data = . %>% filter( Region=="WA"), position = position_dodge(width = 0.9), color='black', alpha = 1) +
#   scale_fill_manual(values=c("#DFE667", "#54C2CC"))+
#   geom_tile(aes(y=NA_integer_, alpha = factor(Region))) + 
#   scale_alpha_manual(values = c(0.2,1)) +
#   scale_x_continuous("Year", labels = as.character(ID), breaks = ID)+
#   ylab("Entanglements") + 
#   theme_classic() +
#   theme(legend.title = element_blank(),
#           #title = element_text(size = 26),
#           legend.text = element_text(size = 25),
#           legend.position = c(.15, .75),
#           axis.text.x = element_text(hjust = 1,size = 40, angle = 60),
#           #axis.text.x=element_blank(),
#           axis.text.y = element_text(size = 40),
#           axis.title = element_text(size = 50),
#           strip.text = element_text(size=40),
#           strip.background = element_blank(),
#           strip.placement = "left"
#     )   
# entl_bar_plot

