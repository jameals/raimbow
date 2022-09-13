# joining logbooks (May-Sep, all effort in WA waters) and PacFin fishtickets

#-----------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(rnaturalearth) #devtools::install_github("ropensci/rnaturalearthhires")
library(sf)
library(rgeos)
library(viridis)
library(ggpubr)

#-----------------------------------------------------------------------------------

#read in all logs fished in WA waters (landed in either WA or in OR)
#data not summarised to grid level yet - retains individual stringline IDs
path_WA_landed_all_logs <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/traps_g_all_logs_2014_2020_clipped_to_WA_waters_20220126.rds"
WA_landed_all_logs_clipped_to_WA_waters <- readRDS(path_WA_landed_all_logs)

# interested only in May-Sep period
WA_landed_all_logs_clipped_to_WA_waters_MaySep <- WA_landed_all_logs_clipped_to_WA_waters %>% 
  filter(is_May_Sep == 'Y')

#each row is a single simulated pot, only need one record per SetID (stringline)
WA_landed_all_logs_clipped_to_WA_waters_MaySep_uniques <- WA_landed_all_logs_clipped_to_WA_waters_MaySep %>% 
  group_by(SetID) %>% 
  filter(row_number()==1)
#nrow(WA_landed_all_logs_clipped_to_WA_waters_MaySep_uniques)   #20144

#Fishticket and landing date columns have been dropped during logbook pipeline, 
#bring them back from an earlier version of raw data  
logs <- read_csv(here('wdfw', 'data','WDFW-Dcrab-logbooks-compiled_stackcoords_2009-2020.csv'),col_types = 'ccdcdccTcccccdTddddddddddddddddiddccddddcddc')
#SetID is the same between the two files
logs_selected_columns <- logs %>% 
  select(SetID, FishTicket1, FishTicket2, FishTicket3, FishTicket4, Vessel, License, FederalID, LandingDate ) 

#join Fihsticket, landing date etc columns back to the more processed logbooks
#getting a duplication of data: "This is because there were two y1=1 values in the d2 dataset so the join will merge these on twice." 
joined_df <- WA_landed_all_logs_clipped_to_WA_waters_MaySep_uniques %>% 
  left_join(logs_selected_columns,by="SetID") %>%
  distinct #add a distinct command to remove duplication
#nrow(joined_df) #20144 - when add distinct command
# cases where fishticket info based on SetID was found
nrow(joined_df %>% filter(!is.na(FishTicket1)))
#20074 --> 99.65%
#cases where Fishticket info based on SetID was NOT found
nrow(joined_df %>% filter(is.na(FishTicket1)))
#70 --> 0.35% of stringlines in WA logs don't have a fishticket number recorded
#manually checked bunch of them and all were cases where raw logs didn't have a Fishticket1 recorded in them
#but only 9 stringlines (0.04%) don't have a landing date

length(unique(joined_df$FishTicket1)) #4392



#read in updated PacFin fishticket data
fishtix_raw <- read_rds(here('wdfw', 'data','pacfin_compiled_2004thru2021.rds')) 
# df is large so subset to years of interest, cut out all california records
# because dealing with May-Sep months, years of interest are 2014, 2015, 2016, 2017, 2018, 2019, 2020
fishtix_2014_2020 <- fishtix_raw %>% 
  filter(AGENCY_CODE != 'C') %>% 
  filter(LANDING_YEAR %in% c(2014, 2015, 2016, 2017, 2018, 2019, 2020)) %>%
  #now added a filter for DCRB labels only 
  #doesn't make much difference as only 0.5% of matches where non DCRB
  filter(PACFIN_SPECIES_CODE == "DCRB") %>%
  #filter to be commercial catch only, so don't bring in e.g. personal catch
  filter(REMOVAL_TYPE_NAME=="COMMERCIAL (NON-EFP)")


#the FishTicket1 column in WA logs, and the FISH_TICKET_ID column in pacfin data don't match
#but we can join files using landing date, and FederalID in logs with VESSEL_NUM in fishtix
library(stringr)
test_df <- joined_df %>% 
  select(SetID, FederalID)
#FederalID column in logs has a space between the first 3 and last 3 digits, while pacfin ticket don't have this gap
test_df_new <-as.data.frame(apply(test_df,2, str_remove_all, " ")) 
test_df_2 <- joined_df %>% 
  left_join(test_df_new,by="SetID") %>% 
  rename(FederalID = FederalID.y)

test_join <- test_df_2 %>% 
  left_join(fishtix_2014_2020, 
            by = c("FederalID" = "VESSEL_NUM",
                   "LandingDate" = "LANDING_DATE"))


nrow(test_join %>%  filter(!is.na(FISH_TICKET_ID))) / nrow(test_join) *100 
##99.06% of logbook data found a FISHTICKET ID from pacfin data


#In the raw PacFin data, sometimes a FISH_TICKET_ID has multiple rows, as the catch of the same FISH_TICKET_ID
# has been split between different WA areas (e.g. 60A1, 60A2, 60B). And each of those will get joined to 
#each stringline which causes repeating data
#the second reason why we see same FISH_TICKET_ID repeated in the joined data is because of multiple stringlines 
#belonging to the same FISH_TICKET_ID (when all catch came from one area)


#we are not trying to match lbs or $ gained to sreas in space
#so just sum up everything for a ticket. but work on df where tix not yet joined to logbooks - we don't want to sum
#lbs or $ across several stringlines that are all linked to the same ticket
duplicated_tickets_fixed <- fishtix_2014_2020 %>% 
  group_by(FISH_TICKET_ID) %>% 
  summarise(total_EXVESSEL_REVENUE = sum(EXVESSEL_REVENUE),
            total_LANDED_WEIGHT_LBS = sum(LANDED_WEIGHT_LBS))

#now join that to the df that has logs and strings joined to Fichticket ID
test_join_fixed <- test_join %>%
  select(-EXVESSEL_REVENUE, -LANDED_WEIGHT_LBS) %>% 
  #just keep one record, as there currently is a row for each Set_ID, ticket ID and catch area combination
  group_by(SetID) %>% 
  filter(row_number()==1) %>% 
  left_join(duplicated_tickets_fixed, by ="FISH_TICKET_ID") %>% 
  filter(!is.na(FISH_TICKET_ID)) #we should also remove cases where no match to PacFin
#now cases of multiple rows per fish ticket are different strings (one per row) that belong to same fish ticket ID


#FTID in pacfin looks to match Fishticket1 (and Fishticket2/Fishticket3... if that column also exists)...
#could be harder to join that way as info in more than 1 column (Fishticket1, Fishticket2...)
#test_join_by_FTID_and_Fishticket1 <- test_df_2 %>% 
#  left_join(fishtix_2014_2020, 
#            by = c("FishTicket1" = "FTID",
#                   "LandingDate" = "LANDING_DATE"))
#more NAs this way than by joining with landing date and vessel ID, 
#because some data is in Fishticket1 column and some in Fishticket2 column
#perhaps slightly better success joining if don't join by landing date as well...
#nrow(test_join_by_FTID_and_Fishticket1 %>%  filter(!is.na(FISH_TICKET_ID))) / nrow(test_join_by_FTID_and_Fishticket1) *100 
#97.27 -- when join by ticket ID and date, so slightly poorer match rate than by vessel ID and landing date
#99.19 if only join by "FishTicket1" = "FTID", so almost the same as if join by date and vessel ID

#--------------------------------------------
#adjust for inflation (or could have used AFI column in PacFin data)

cpi_raw <- read_csv(here('wdfw', 'data', 'cpi_2021.csv'),col_types='idc')

# add a conversion factor to 2014 $$
cpi <- cpi_raw %>% 
  mutate(convert2014=1/(annual_average/236.7)) %>% 
  filter(year>2013) %>% 
  filter(year<2021) %>% 
  dplyr::select(year,convert2014) %>% 
  rename(LANDING_YEAR = year)

test_join_uniques_adj_inf <- test_join_fixed %>% 
  left_join(cpi, by = c('LANDING_YEAR')) %>% 
  mutate(EXVESSEL_REVENUE_adj = total_EXVESSEL_REVENUE * convert2014)
#originally also adjusted PRICE_PER_POUND_adj = PRICE_PER_POUND * convert2014, but we don't actually use it
#plus complex now as needed to fix the repeating fishticket IDs
  


#-----------------------------------------------------------------------------------------------


######################################################################################
#monthly revenue by vessel 

monthly_rev_by_vessel <- test_join_uniques_adj_inf %>% 
  rename(License = License.x) %>% 
  #here we don't want to repeatedly count the same FISHTICKET IDs
  group_by(Vessel.x, License, season, month_name, FISH_TICKET_ID) %>% 
  filter(row_number()==1) %>% 
  summarise(monthly_rev = sum(EXVESSEL_REVENUE_adj)) #EXVESSEL_REVENUE_adj has been adjusted for repeating ticket IDs
  

#Read in and join license & pot limit info
WA_pot_limit_info <- read_csv(here::here('wdfw', 'data','WA_pot_limit_info_May2021.csv'))

WA_pot_limit_info <- WA_pot_limit_info %>% 
  rename(License = License_ID)

#join Pot_Limit info. 
monthly_rev_by_vessel <- left_join(monthly_rev_by_vessel, WA_pot_limit_info,by=c("License"))
glimpse(monthly_rev_by_vessel)



monthly_rev_by_vessel_JulSep <- monthly_rev_by_vessel %>% 
  filter(month_name %in% c('July', 'August', 'September')) %>% 
  filter(season != '2019-2020') %>% 
  mutate(pre_post_reg = 
           ifelse(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018'), "pre-reg", season)) %>% 
  group_by(Vessel.x, season, pre_post_reg, Pot_Limit) %>% 
  summarise(mean_monthly_rev = mean(monthly_rev, na.rm = T))


monthly_rev_JulSep <- ggplot()+
  geom_violin(data = monthly_rev_by_vessel_JulSep %>%  filter(pre_post_reg =="pre-reg"), aes(x=pre_post_reg, y=mean_monthly_rev/10000, group=as.factor(Pot_Limit), color=as.factor(Pot_Limit)), lwd=2) + #,size=2.5  group=season,
  geom_violin(data = monthly_rev_by_vessel_JulSep %>%  filter(pre_post_reg !="pre-reg"), aes(x=pre_post_reg, y=mean_monthly_rev/10000, group=as.factor(Pot_Limit), color=as.factor(Pot_Limit)), lwd=2) + #,size=2.5  group=season,
  scale_color_manual(name="", values = c("#8bd8bd","#243665")) +
  ylab("Mean monthly revenue/vessel ($x10^4)") +
  xlab("") + 
  scale_x_discrete(limits = rev, labels=c("pre-reg" = "pre-regulations", "2018-2019" = "2019")) +
  theme_classic()+
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        #legend.text = element_text(size = 20),
        #legend.position = c(.85, .85),
        legend.position = 'none',
        axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0, color='black'),
        axis.text.y = element_text(size = 40, color='black'),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
monthly_rev_JulSep

# path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
# png(paste0(path_figures, "/mean_monthly_revenue_by_vessel_prePreg_vs_2019_ADJ_INFL_and_repeating_fishtix.png"), width = 15, height = 14, units = "in", res = 400)
# ggarrange(monthly_rev_JulSep,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())


monthly_rev_by_vessel_MaySep <- monthly_rev_by_vessel %>% 
  filter(season != '2018-2019') %>% 
  mutate(pre_post_reg = 
           ifelse(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018'), "pre-reg", season)) %>% 
  group_by(Vessel.x, season, pre_post_reg, Pot_Limit) %>% 
  summarise(mean_monthly_rev = mean(monthly_rev, na.rm = T))


monthly_rev_MaySep <- ggplot()+
  geom_violin(data = monthly_rev_by_vessel_MaySep %>%  filter(pre_post_reg =="pre-reg"), aes(x=pre_post_reg, y=mean_monthly_rev/10000, group=as.factor(Pot_Limit), color=as.factor(Pot_Limit)), lwd=2) + #,size=2.5  group=season,
  geom_violin(data = monthly_rev_by_vessel_MaySep %>%  filter(pre_post_reg !="pre-reg"), aes(x=pre_post_reg, y=mean_monthly_rev/10000, group=as.factor(Pot_Limit), color=as.factor(Pot_Limit)), lwd=2) + #,size=2.5  group=season,
  scale_color_manual(name="", values = c("#8bd8bd","#243665")) +
  #ylab("Mean monthly revenue/vessel (x10^4)") +
  ylab("") +
  xlab("") + 
  scale_x_discrete(limits = rev, labels=c("pre-reg" = "pre-regulations", "2019-2020" = "2020")) +
  theme_classic()+
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 50),
        #legend.position = c(.85, .85),
        legend.position="none",
        axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0, color='black'),
        axis.text.y = element_text(size = 40, color='black'),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
monthly_rev_MaySep

# path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
# png(paste0(path_figures, "/mean_monthly_revenue_by_vessel_prePreg_vs_2020_ADJ_INFL_and_repeating_fishtix.png"), width = 15, height = 14, units = "in", res = 400)
# ggarrange(monthly_rev_MaySep,
#           ncol=1,
#           nrow=1
#           #legend="bottom"
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())


### GLM on mean monthly revenue/vessel
## pre-post-reg not a significant predictor in any comparison (Jul-Sep, May-Sep, 300 or 500 tier)

#JUL-SEP
monthly_rev_by_vessel_JulSep_300 <- monthly_rev_by_vessel_JulSep %>% 
  filter(Pot_Limit == 300)
mod_monthly_rev_JulSep_300 <- glm(mean_monthly_rev ~ pre_post_reg,
                                  family=gaussian, data=monthly_rev_by_vessel_JulSep_300, na.action = na.omit) #family = gaussian(link = "log")
summary(mod_monthly_rev_JulSep_300)
#plot(mod_monthly_rev_JulSep_300)


monthly_rev_by_vessel_JulSep_500 <- monthly_rev_by_vessel_JulSep %>% 
  filter(Pot_Limit == 500)
mod_monthly_rev_JulSep_500 <- glm(mean_monthly_rev ~ pre_post_reg,
                                  family=gaussian, data=monthly_rev_by_vessel_JulSep_500, na.action = na.omit) #family = gaussian(link = "log")
summary(mod_monthly_rev_JulSep_500)
#plot(mod_monthly_rev_JulSep_500)


#MAY-SEP
monthly_rev_by_vessel_MaySep_300 <- monthly_rev_by_vessel_MaySep %>% 
  filter(Pot_Limit == 300)
mod_monthly_rev_MaySep_300 <- glm(mean_monthly_rev ~ pre_post_reg,
                 family=gaussian, data=monthly_rev_by_vessel_MaySep_300, na.action = na.omit) #family = gaussian(link = "log")
summary(mod_monthly_rev_MaySep_300)
#plot(mod_monthly_rev_MaySep_300)


monthly_rev_by_vessel_MaySep_500 <- monthly_rev_by_vessel_MaySep %>% 
  filter(Pot_Limit == 500)
mod_monthly_rev_MaySep_500 <- glm(mean_monthly_rev ~ pre_post_reg,
                                  family=gaussian, data=monthly_rev_by_vessel_MaySep_500, na.action = na.omit) #family = gaussian(link = "log")
summary(mod_monthly_rev_MaySep_500)
#plot(mod_monthly_rev_MaySep_500)




#---------------------------------------------------------------------------------
## instead of violin plots, do line plots for $, lbs and CPUE  

group_by(FISH_TICKET_ID) %>% 
  filter(row_number()==1) %>%
  
  
## plot both Jul-Sep and May-Sep lines on one plot

#Jul-Sep data
summary_pacfin_data_JulSep <- test_join_uniques_adj_inf %>% #test_join_uniques_adj_inf each row is stringline
  #filter(REMOVAL_TYPE_NAME == "COMMERCIAL (NON-EFP)") %>% #this filtering has already been done
  #filter(PACFIN_SPECIES_CODE == "DCRB") %>% #this filtering has already been done
  filter(month_name %in% c('July','August','September')) %>%
  filter(season != '2019-2020') %>%
  #each row is a string, but we want only one record per FISH_TICKET_ID
  group_by(FISH_TICKET_ID) %>% 
  filter(row_number()==1) %>%
  group_by(season) %>%
  summarise(sum_revenue = sum(EXVESSEL_REVENUE_adj, na.rm=T),
            sum_weight_lbs = sum(total_LANDED_WEIGHT_LBS, na.rm=T)
  ) %>% 
  mutate(
    pre_post_reg = ifelse(season == '2018-2019', '2018-2019', 'pre-reg')) %>% 
  mutate(season2 = season) %>% 
  separate(season2, into = c("season_1", "season_2"), sep = "-") %>% 
  add_row(season_2 = '2020', sum_revenue = NA, sum_weight_lbs = NA)

summary_pacfin_data_JulSep$season_2 <- as.numeric(summary_pacfin_data_JulSep$season_2)


#May-Sep data
summary_pacfin_data_MaySep <- test_join_uniques_adj_inf %>%
  #filter(REMOVAL_TYPE_NAME == "COMMERCIAL (NON-EFP)") %>% this filtering has already been done
  filter(season != '2018-2019') %>%
  #filter(PACFIN_SPECIES_CODE == "DCRB") %>% #this filtering has already been done
  #each row is a string, but we want only one record per FISH_TICKET_ID
  group_by(FISH_TICKET_ID) %>% 
  filter(row_number()==1) %>%
  group_by(season) %>%
  summarise(sum_revenue = sum(EXVESSEL_REVENUE_adj, na.rm=T),
            sum_weight_lbs = sum(total_LANDED_WEIGHT_LBS, na.rm=T)
  ) %>% 
  mutate(
    pre_post_reg = ifelse(season == '2019-2020', '2019-2020', 'pre-reg')) %>% 
  mutate(season2 = season) %>% 
  separate(season2, into = c("season_1", "season_2"), sep = "-")  

summary_pacfin_data_MaySep$season_2 <- as.numeric(summary_pacfin_data_MaySep$season_2)



##REVENUE
sum_JulSep_and_MaySep_rev_line <- ggplot() +
  #Jul-Sep
  geom_line(data = summary_pacfin_data_JulSep, aes(x = season_2, y = sum_revenue/100000,group = 1), lwd=1.8) +
  geom_point(data = summary_pacfin_data_JulSep, aes(x = season_2, y = sum_revenue/100000, group = 1), size=5) +
  geom_segment(data = summary_pacfin_data_JulSep,aes(x = 2018.7, y = 7.908069, xend = 2019.3, yend = 7.908069), linetype='dashed', lwd=1.8) +
  #May-Sep
  geom_line(data = summary_pacfin_data_MaySep, aes(x = season_2, y = sum_revenue/100000,group = 1), lwd=1.8, color='gray') +
  geom_point(data = summary_pacfin_data_MaySep, aes(x = season_2, y = sum_revenue/100000, group = 1), size=5, color='gray') +
  geom_segment(data = summary_pacfin_data_MaySep,aes(x = 2019.7, y = 17.2023, xend = 2020.3, yend = 17.2023), linetype='dashed', color='gray', lwd=1.8) +
  scale_y_continuous(limits=c(NA, 44), breaks=c(10,20,30,40)) +
  scale_x_continuous(breaks=seq(2014, 2020, 1))+
  ylab("Revenue ($ x10^5)") +
  #xlab("Season") +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 0.5,size = 40, color='black'), #, angle = 60
        axis.text.y = element_text(size = 40, color='black'),
        axis.title = element_text(size = 50),
        axis.title.x=element_blank(),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
sum_JulSep_and_MaySep_rev_line


# path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
# png(paste0(path_figures, "/total_revenue_prePreg_vs_2019_2020_LINE_and_repeating_fishtix.png"), width = 22, height = 14, units = "in", res = 400)
# ggarrange(sum_JulSep_and_MaySep_rev_line,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())



#GLM
#Jul-Sep
hist(summary_pacfin_data_JulSep$sum_revenue)
mod1_rev_JulSep <- glm(sum_revenue ~ pre_post_reg,
                       family=gaussian, data=summary_pacfin_data_JulSep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_rev_JulSep) #pre-post-reg not a significant predictor of revenue
hist(mod1_rev_JulSep$residuals)


pre_reg_mean_revenue_JulSep <- summary_pacfin_data_JulSep %>% 
  group_by(pre_post_reg) %>% 
  summarise(median_revenue = median(sum_revenue))

##ADJUSTED FOR INFLATION, and fixed repeating fishticket IDs
#% change from pre-reg MEDIAN to 2019
(918543.8-790806.9)/790806.9*100 ##16.15


##GLM##
#May-Sep
hist(summary_pacfin_data_MaySep$sum_revenue)
mod1_rev_MaySep <- glm(sum_revenue ~ pre_post_reg ,
                       family=gaussian, data=summary_pacfin_data_MaySep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_rev_MaySep) #pre-post-reg not a significant predictor of revenue
hist(mod1_rev_MaySep$residuals)
#plot(mod1_rev_MaySep)


pre_reg_mean_revenue_MaySep <- summary_pacfin_data_MaySep %>% 
  group_by(pre_post_reg) %>% 
  summarise(median_revenue = median(sum_revenue))

##ADJUSTED FOR INFLATION, and fixed repeating fishticket IDs
#% change from pre-reg MEDIAN to 2020
(1619516-1720230)/1720230*100 ##-5.85

#---------------

##LANDINGS
sum_JulSep_and_MaySep_lbs_line <- ggplot() +
  #Jul-Sep
  geom_line(data = summary_pacfin_data_JulSep, aes(x = season_2, y = sum_weight_lbs/100000,group = 1), lwd=1.8) +
  geom_point(data = summary_pacfin_data_JulSep, aes(x = season_2, y = sum_weight_lbs/100000, group = 1), size=5) +
  geom_segment(data = summary_pacfin_data_JulSep,aes(x = 2018.7, y = 1.85442, xend = 2019.3, yend = 1.85442), linetype='dashed', lwd=1.8) +
  #May-Sep
  geom_line(data = summary_pacfin_data_MaySep, aes(x = season_2, y = sum_weight_lbs/100000,group = 1), lwd=1.8,color='gray') +
  geom_point(data = summary_pacfin_data_MaySep, aes(x = season_2, y = sum_weight_lbs/100000,group = 1), size=5,color='gray') +
  geom_segment(data = summary_pacfin_data_MaySep,aes(x = 2019.7, y = 3.56729, xend = 2020.3, yend = 3.56729), linetype='dashed', lwd=1.8,color='gray') +
  scale_y_continuous(limits=c(NA, 10), breaks=c(3,6,9)) +
  scale_x_continuous(breaks=seq(2014, 2020, 1))+
  ylab("Landings (lbs x 10^5)") +
  #xlab("Season") +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 0.5,size = 40, color='black'), #, angle = 60
        axis.text.y = element_text(size = 40, color='black'),
        axis.title = element_text(size = 50),
        axis.title.x=element_blank(),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
sum_JulSep_and_MaySep_lbs_line


# path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
# png(paste0(path_figures, "/total_landings_prePreg_vs_2019_2020_LINE_adj_for_repeating_fishtix.png"), width = 22, height = 14, units = "in", res = 400)
# ggarrange(sum_JulSep_and_MaySep_lbs_line,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())


##GLM##
#Jul-Sep
hist(summary_pacfin_data_JulSep$sum_weight_lbs)
mod1_lbs_JulSep <- glm(sum_weight_lbs ~ pre_post_reg,
                       family=gaussian, data=summary_pacfin_data_JulSep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_lbs_JulSep) #pre-post-reg not a significant predictor of revenue
hist(mod1_lbs_JulSep$residuals)
#plot(mod1_lbs_JulSep)


pre_reg_mean_landings_JulSep <- summary_pacfin_data_JulSep %>% 
  group_by(pre_post_reg) %>% 
  summarise(median_landings = median(sum_weight_lbs))

#% change from pre-reg MEDIAN to 2019
(218133-185442)/185442*100 ##17.63%



##GLM##
#May-Sep
hist(summary_pacfin_data_MaySep$sum_weight_lbs)

mod1_lbs_MaySep <- glm(sum_weight_lbs ~ pre_post_reg,
                       family=gaussian, data=summary_pacfin_data_MaySep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_lbs_MaySep) #pre-post-reg not a significant predictor of revenue
hist(mod1_lbs_MaySep$residuals)
#plot(mod1_lbs_MaySep)


pre_reg_mean_landings_MaySep <- summary_pacfin_data_MaySep %>% 
  group_by(pre_post_reg) %>% 
  summarise(median_landings = median(sum_weight_lbs))

#% change from pre-reg MEDIAN to 2020
(355348-356729)/356729*100 ##-0.39%




#---------
#CPUE
#first trying to figure out how many pots belong to one fish ticket

efficiency_CPUE <-  test_join_fixed %>% 
  #filter(REMOVAL_TYPE_NAME == "COMMERCIAL (NON-EFP)") %>% #already filtered for this
  #each row is a string line, multiple string lines in a FISHTICKET
  group_by(FISH_TICKET_ID) %>% 
  summarise(sum_pots_per_ticket = sum(PotsFished, na.rm = T))

efficiency_CPUE <- left_join(efficiency_CPUE,
                             test_join_uniques_adj_inf %>% select(total_LANDED_WEIGHT_LBS, EXVESSEL_REVENUE_adj, season, month_name,FISH_TICKET_ID),
                             by = "FISH_TICKET_ID"
                    ) %>% 
  #the code adds all unique SetIDs, but we only want one record per FISH_TICKET_ID 
           group_by(FISH_TICKET_ID) %>% 
          filter(row_number()==1) 


##Jul-Sep
summary_efficiency_CPUE_v2_JulSep <- efficiency_CPUE %>% 
  filter(season != '2019-2020') %>% 
  filter(month_name %in% c('July', 'August', 'September')) %>% 
  group_by(season) %>% 
  summarise(total_pots = sum(sum_pots_per_ticket, na.rm = T),
            total_dollars = sum(EXVESSEL_REVENUE_adj, na.rm = T),
            dollar_per_pot = total_dollars/total_pots,
            
            total_lbs = sum(total_LANDED_WEIGHT_LBS , na.rm = T),
            lbs_per_pot = total_lbs/total_pots) %>% 
  mutate(pre_post_reg = 
           ifelse(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018'), "pre-reg", season)) %>% 
  mutate(season2 = season) %>% 
  separate(season2, into = c("season_1", "season_2"), sep = "-")

summary_efficiency_CPUE_v2_JulSep$season_2 <- as.numeric(summary_efficiency_CPUE_v2_JulSep$season_2)


#May-Sep
summary_efficiency_CPUE_v2_MaySep <- efficiency_CPUE %>% 
  filter(season != '2018-2019')  %>% 
  #group_by(season, pre_post_reg) %>% 
  group_by(season) %>% 
  summarise(total_pots = sum(sum_pots_per_ticket, na.rm = T),
            total_dollars = sum(EXVESSEL_REVENUE_adj, na.rm = T),
            dollar_per_pot = total_dollars/total_pots,
            
            total_lbs = sum(total_LANDED_WEIGHT_LBS , na.rm = T),
            lbs_per_pot = total_lbs/total_pots) %>% 
  mutate(pre_post_reg = 
           ifelse(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018'), "pre-reg", season)) %>% 
  mutate(season2 = season) %>% 
  separate(season2, into = c("season_1", "season_2"), sep = "-")

summary_efficiency_CPUE_v2_MaySep$season_2 <- as.numeric(summary_efficiency_CPUE_v2_MaySep$season_2)



#CPUE: lbs/pot
CPUE_ts_lbs_JulSep_MaySep_line <- ggplot() +
  #Jul-Sep
  geom_line(data = summary_efficiency_CPUE_v2_JulSep, aes(x = season_2, y = lbs_per_pot,group = 1), lwd=1.8) +
  geom_point(data = summary_efficiency_CPUE_v2_JulSep, aes(x = season_2, y = lbs_per_pot, group = 1), size=5) +
  geom_segment(data = summary_efficiency_CPUE_v2_JulSep,aes(x = 2018.7, y = 2.252429, xend = 2019.3, yend = 2.252429), linetype='dashed', lwd=1.8) +
  #May-Sep
  geom_line(data = summary_efficiency_CPUE_v2_MaySep, aes(x = season_2, y = lbs_per_pot,group = 1), lwd=1.8, color='gray') +
  geom_point(data = summary_efficiency_CPUE_v2_MaySep, aes(x = season_2, y = lbs_per_pot, group = 1), size=5, color='gray') +
  geom_segment(data = summary_efficiency_CPUE_v2_MaySep,aes(x = 2019.7, y = 2.182817, xend = 2020.3, yend = 2.182817), linetype='dashed', lwd=1.8, color='gray') +
  scale_x_continuous(breaks=seq(2014, 2020, 1))+
  ylab("Mean lbs/pot") +
  #xlab("Season") +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 0.5,size = 40, color='black'), #, angle = 60
        axis.text.y = element_text(size = 40, color='black'),
        axis.title = element_text(size = 50),
        axis.title.x=element_blank(),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
CPUE_ts_lbs_JulSep_MaySep_line

# path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
# png(paste0(path_figures, "/CPUE_pounds_per_pot_prePreg_2019_2020_LINE_adj_for_repeating_fishtix.png"), width = 22, height = 14, units = "in", res = 400)
# ggarrange(CPUE_ts_lbs_JulSep_MaySep_line,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())


#CPUE: $/pot
CPUE_ts_dollar_JulSep_MaySep_line <- ggplot() +
  #Jul-Sep
  geom_line(data = summary_efficiency_CPUE_v2_JulSep, aes(x = season_2, y = dollar_per_pot,group = 1), lwd=1.8) +
  geom_point(data = summary_efficiency_CPUE_v2_JulSep, aes(x = season_2, y = dollar_per_pot, group = 1), size=5) +
  geom_segment(data = summary_efficiency_CPUE_v2_JulSep,aes(x = 2018.7, y = 10.542249, xend = 2019.3, yend = 10.542249), linetype='dashed', lwd=1.8) +
  #May-Sep
  geom_line(data = summary_efficiency_CPUE_v2_MaySep, aes(x = season_2, y = dollar_per_pot,group = 1), lwd=1.8, color='gray') +
  geom_point(data = summary_efficiency_CPUE_v2_MaySep, aes(x = season_2, y = dollar_per_pot, group = 1), size=5, color='gray') +
  geom_segment(data = summary_efficiency_CPUE_v2_MaySep,aes(x = 2019.7, y = 10.52605, xend = 2020.3, yend = 10.52605), linetype='dashed', lwd=1.8, color='gray') +
  scale_x_continuous(breaks=seq(2014, 2020, 1))+
  ylab("Mean $/pot") +
  #xlab("Season") +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 0.5,size = 40, color='black'), #, angle = 60
        axis.text.y = element_text(size = 40, color='black'),
        axis.title = element_text(size = 50),
        axis.title.x=element_blank(),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
CPUE_ts_dollar_JulSep_MaySep_line

# path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
# png(paste0(path_figures, "/CPUE_dollar_per_pot_prePreg_2019_2020_LINE_adj_for_infl_and_repeating_fishtix.png"), width = 22, height = 14, units = "in", res = 400)
# ggarrange(CPUE_ts_dollar_JulSep_MaySep_line,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())



##GLM##
#May-Sep
hist(summary_efficiency_CPUE_v2_MaySep$dollar_per_pot)
hist(summary_efficiency_CPUE_v2_MaySep$lbs_per_pot)

mod1_CPUE_lbs_MaySep <- glm(lbs_per_pot ~ pre_post_reg,
                            family=gaussian, data=summary_efficiency_CPUE_v2_MaySep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_CPUE_lbs_MaySep) #pre-post-reg not a significant predictor 
hist(mod1_CPUE_lbs_MaySep$residuals)
#plot(mod1_CPUE_lbs_MaySep)

mod1_CPUE_dollar_MaySep <- glm(dollar_per_pot ~ pre_post_reg,
                               family=gaussian, data=summary_efficiency_CPUE_v2_MaySep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_CPUE_dollar_MaySep) #pre-post-reg IS a significant predictor if don't adjust for infaltion, NOT significant if do adjust for infaltion
hist(mod1_CPUE_dollar_MaySep$residuals)
#plot(mod1_CPUE_dollar_MaySep)


# % change
pre_reg_mean_CPUE_dollar_MaySep <- summary_efficiency_CPUE_v2_MaySep %>% 
  group_by(pre_post_reg) %>% 
  summarise(median_dollar_per_pot = median(dollar_per_pot),
            median_lbs_per_pot = median(lbs_per_pot))

#% change from pre-reg mean to 2020
#ADJUSTED FOR INFALTION
#MEDIAN:
(12.38787-10.52605)/10.52605*100 #17.69%
#LBS
(2.718099-2.182817)/2.182817*100 ##24.52 if do median




##GLM##
#Jul-Sep
hist(summary_efficiency_CPUE_v2_JulSep$dollar_per_pot)
hist(summary_efficiency_CPUE_v2_JulSep$lbs_per_pot)

mod1_CPUE_lbs_JulSep <- glm(lbs_per_pot ~ pre_post_reg,
                            family=gaussian, data=summary_efficiency_CPUE_v2_JulSep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_CPUE_lbs_JulSep) #pre-post-reg not a significant predictor 
hist(mod1_CPUE_lbs_JulSep$residuals)
#plot(mod1_CPUE_lbs_JulSep)

mod1_CPUE_dollar_JulSep <- glm(dollar_per_pot ~ pre_post_reg,
                               family=gaussian, data=summary_efficiency_CPUE_v2_JulSep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_CPUE_dollar_JulSep) #pre-post-reg not a significant predictor 
hist(mod1_CPUE_dollar_JulSep$residuals)
#plot(mod1_CPUE_dollar_JulSep)


# % change
pre_reg_mean_CPUE_dollar_JulSep <- summary_efficiency_CPUE_v2_JulSep %>% 
  group_by(pre_post_reg) %>% 
  summarise(median_dollar_per_pot = median(dollar_per_pot),
            median_lbs_per_pot = median(lbs_per_pot))

#% change in mean from pre-reg  to 2019
#ADJUSTED FOR INFALTION
#MEDIAN
(8.248517-10.542249)/10.542249*100 #-21.76%
#LBS
#MEDIAN
(1.958937-2.252429)/2.252429*100 ##-13.03


#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------


#OLD CODE NOT USED IN PAPER AT THE END



#plotting


# do separate comparisons for Jul-Sep 2019 vs pre-regs, and May-Sep 2020 vs pre-regs

# sum revenue and landings per month (but keep months separate) - as did with risk plots

#Jul-Sep
summary_pacfin_data_JulSep <- test_join_uniques_adj_inf %>% 
  filter(REMOVAL_TYPE_NAME == "COMMERCIAL (NON-EFP)") %>%
  filter(PACFIN_SPECIES_CODE == "DCRB") %>%
  filter(month_name %in% c('July','August','September')) %>%
  filter(season != '2019-2020') %>%
  #filter(LANDED_WEIGHT_LBS < 10000) %>% ##this doesn't make a difference in main conclusions
  group_by(season, month_name) %>%
  summarise(sum_revenue = sum(EXVESSEL_REVENUE_adj, na.rm=T),
            sum_weight_lbs = sum(LANDED_WEIGHT_LBS, na.rm=T),
            avg_price_per_pound = mean(PRICE_PER_POUND_adj, na.rm=T)
  ) %>% 
  mutate(
    pre_post_reg = ifelse(season == '2018-2019', '2018-2019', 'pre-reg'))

summary_pacfin_data_JulSep$month_name <- factor(summary_pacfin_data_JulSep$month_name, levels = c('July', 'August', 'September'))


##### as a boxplot
##REVENUE
sum_JulSep_rev_box <- ggplot() +
  geom_violin(data = summary_pacfin_data_JulSep, aes(x = pre_post_reg, y = sum_revenue/100000), lwd=2) +
  geom_dotplot(data = summary_pacfin_data_JulSep, aes(x = pre_post_reg, y = sum_revenue/100000), binaxis='y', stackdir='center', dotsize=0.6) +
  stat_summary(data = summary_pacfin_data_JulSep, aes(x = pre_post_reg, y = sum_revenue/100000),
               fun = "median",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  ylab("Revenue ($ x10^5)") +
  #xlab("Season") +
  scale_x_discrete(limits = rev, labels=c("pre-reg" = "pre-regulations", "2018-2019" = "2019")) +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 0.5,size = 40), #, angle = 60
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        axis.title.x=element_blank(),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
sum_JulSep_rev_box

# path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
# png(paste0(path_figures, "/total_revenue_prePreg_vs_2019_ADJ_INFL_dots_and_median.png"), width = 16, height = 14, units = "in", res = 400)
# ggarrange(sum_JulSep_rev_box,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())


##GLM##
hist(summary_pacfin_data_JulSep$sum_revenue)

mod1_rev_JulSep <- glm(sum_revenue ~ pre_post_reg + month_name,
                       family=gaussian, data=summary_pacfin_data_JulSep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_rev_JulSep) #pre-post-reg not a significant predictor of revenue
hist(mod1_rev_JulSep$residuals)
#plot(mod1_rev_JulSep)


pre_reg_mean_revenue_JulSep <- summary_pacfin_data_JulSep %>% 
  group_by(pre_post_reg) %>% 
  summarise(mean_revenue = mean(sum_revenue),
            median_revenue = median(sum_revenue))

##ADJUSTED FOR INFLATION
#% change from pre-reg mean to 2019
(271656.5-407971.0)/407971.0*100
#-33.41 --> but pre-reg mean high due to high year of 2014
#% change from pre-reg MEDIAN to 2019
(337486.4-254010.8)/254010.8*100 ##32.86

##LANDINGS
sum_JulSep_lbs_box <- ggplot() +
  geom_violin(data = summary_pacfin_data_JulSep, aes(x = pre_post_reg, y = sum_weight_lbs/100000), lwd=2) +
  geom_dotplot(data = summary_pacfin_data_JulSep, aes(x = pre_post_reg, y = sum_weight_lbs/100000), binaxis='y', stackdir='center', dotsize=0.6) +
  stat_summary(data = summary_pacfin_data_JulSep, aes(x = pre_post_reg, y = sum_weight_lbs/100000),
               fun = "median",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  ylab("Landings (lbs x 10^5)") +
  #xlab("Season") +
  scale_x_discrete(limits = rev, labels=c("pre-reg" = "pre-regulations", "2018-2019" = "2019")) +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 0.5,size = 40), #, angle = 60
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        axis.title.x=element_blank(),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
sum_JulSep_lbs_box

# path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
# png(paste0(path_figures, "/total_landings_prePreg_vs_2019_dots_and_median.png"), width = 16, height = 14, units = "in", res = 400)
# ggarrange(sum_JulSep_lbs_box,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())


##GLM##
hist(summary_pacfin_data_JulSep$sum_weight_lbs)

mod1_lbs_JulSep <- glm(sum_weight_lbs ~ pre_post_reg + month_name,
                       family=gaussian, data=summary_pacfin_data_JulSep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_lbs_JulSep) #pre-post-reg not a significant predictor of revenue
hist(mod1_lbs_JulSep$residuals)
#plot(mod1_lbs_JulSep)


pre_reg_mean_landings_JulSep <- summary_pacfin_data_JulSep %>% 
  group_by(pre_post_reg) %>% 
  summarise(mean_landings = mean(sum_weight_lbs),
            median_landings = median(sum_weight_lbs))

#% change from pre-reg mean to 2019
(64253.67-93850.27)/93850.27*100
#-31.54 --> but pre-reg mean high due to high year of 2014
#% change from pre-reg MEDIAN to 2019
(80293-60698)/60698*100 ##32.28278




####################
#May-Sep
summary_pacfin_data_MaySep <- test_join_uniques_adj_inf %>%
  filter(REMOVAL_TYPE_NAME == "COMMERCIAL (NON-EFP)") %>%
  filter(season != '2018-2019') %>%
  filter(PACFIN_SPECIES_CODE == "DCRB") %>% #doesn't make a difference
  #filter(LANDED_WEIGHT_LBS < 10000) %>% #doesn't make a difference
  group_by(season, month_name) %>%
  summarise(sum_revenue = sum(EXVESSEL_REVENUE_adj, na.rm=T),
            sum_weight_lbs = sum(LANDED_WEIGHT_LBS, na.rm=T),
            avg_price_per_pound = mean(PRICE_PER_POUND_adj, na.rm=T)
  ) %>% 
  mutate(
    pre_post_reg = ifelse(season == '2019-2020', '2019-2020', 'pre-reg'))

summary_pacfin_data_MaySep$month_name <- factor(summary_pacfin_data_MaySep$month_name, levels = c('May', 'June', 'July', 'August', 'September'))


##REVENUE
sum_MaySep_rev_box <- ggplot() +
  geom_violin(data = summary_pacfin_data_MaySep, aes(x = pre_post_reg, y = sum_revenue/100000), lwd=2) +
  geom_dotplot(data = summary_pacfin_data_MaySep, aes(x = pre_post_reg, y = sum_revenue/100000), binaxis='y', stackdir='center', dotsize=0.6) +
  stat_summary(data = summary_pacfin_data_MaySep, aes(x = pre_post_reg, y = sum_revenue/100000),
               fun = "median",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  #ylab("Revenue ($ x10^5)") +
  ylab("") +
  #xlab("Season") +
  scale_x_discrete(limits = rev, labels=c("pre-reg" = "pre-regulations", "2019-2020" = "2020")) +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 0.5,size = 40), #, angle = 60
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        axis.title.x=element_blank(),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
sum_MaySep_rev_box

# path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
# png(paste0(path_figures, "/total_revenue_prePreg_vs_2020_ADJ_INFL_dots_and_median.png"), width = 16, height = 14, units = "in", res = 400)
# ggarrange(sum_MaySep_rev_box,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())


##GLM##
hist(summary_pacfin_data_MaySep$sum_revenue)

mod1_rev_MaySep <- glm(sum_revenue ~ pre_post_reg + month_name,
                       family=gaussian, data=summary_pacfin_data_MaySep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_rev_MaySep) #pre-post-reg not a significant predictor of revenue
hist(mod1_rev_MaySep$residuals)
#plot(mod1_rev_MaySep)


pre_reg_mean_revenue_MaySep <- summary_pacfin_data_MaySep %>% 
  group_by(pre_post_reg) %>% 
  summarise(mean_revenue = mean(sum_revenue),
            median_revenue = median(sum_revenue))

##ADJUSTED FOR INFLATION
#% change from pre-reg mean to 2019
(315489.1-389050.9)/389050.9*100
#-18.91 --> but pre-reg mean high due to high year of 2014
#% change from pre-reg MEDIAN to 2019
(277670.6-248558.9)/248558.9*100 ##11.71



##LANDINGS
sum_MaySep_lbs_box <- ggplot() +
  geom_violin(data = summary_pacfin_data_MaySep, aes(x = pre_post_reg, y = sum_weight_lbs/100000), lwd=2) +
  geom_dotplot(data = summary_pacfin_data_MaySep, aes(x = pre_post_reg, y = sum_weight_lbs/100000), binaxis='y', stackdir='center', dotsize=0.6) +
  stat_summary(data = summary_pacfin_data_MaySep, aes(x = pre_post_reg, y = sum_weight_lbs/100000),
               fun = "median",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  #ylab("Landings (lbs x 10^5)") +
  ylab("") +
  #xlab("Season") +
  scale_x_discrete(limits = rev, labels=c("pre-reg" = "pre-regulations", "2019-2020" = "2020")) +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 0.5,size = 40), #, angle = 60
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        axis.title.x=element_blank(),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
sum_MaySep_lbs_box

# path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
# png(paste0(path_figures, "/total_landings_prePreg_vs_2020_ADJ_INFL_dots_and_median.png"), width = 16, height = 14, units = "in", res = 400)
# ggarrange(sum_MaySep_lbs_box,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())


##GLM##
hist(summary_pacfin_data_MaySep$sum_weight_lbs)

mod1_lbs_MaySep <- glm(sum_weight_lbs ~ pre_post_reg + month_name,
                       family=gaussian, data=summary_pacfin_data_MaySep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_lbs_MaySep) #pre-post-reg not a significant predictor of revenue
hist(mod1_lbs_MaySep$residuals)
#plot(mod1_lbs_MaySep)


pre_reg_mean_landings_MaySep <- summary_pacfin_data_MaySep %>% 
  group_by(pre_post_reg) %>% 
  summarise(mean_landings = mean(sum_weight_lbs),
            median_landings = median(sum_weight_lbs))

#% change from pre-reg mean to 2019
(69152.00-83601.32)/83601.32*100
#-17.28 --> but pre-reg mean high due to high year of 2014
#% change from pre-reg MEDIAN to 2019
(77146-55860)/55860*100 ##38.10598


#########################################



################
#one way of doing price per pound

test_join_uniques_price_per_pound <- test_join_uniques_adj_inf %>% 
  filter(REMOVAL_TYPE_NAME == "COMMERCIAL (NON-EFP)") %>% 
  mutate(month_name = factor(month_name, levels = c('May','June','July','August','September','October','November'))) %>% 
  mutate(pre_post_reg = 
           ifelse(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018'), "pre-reg", season))

# avg_ppp_post_reg <- test_join_uniques_price_per_pound %>% 
#   filter(pre_post_reg != 'pre-reg') %>% 
#   group_by(season, month_name) %>% 
#   summarise(PRICE_PER_POUND = mean(PRICE_PER_POUND))
# 
# avg_ppp_pre_reg <- test_join_uniques_price_per_pound %>% 
#   filter(pre_post_reg == 'pre-reg') %>% 
#   group_by(season, month_name, pre_post_reg) %>% 
#   summarise(PRICE_PER_POUND = mean(PRICE_PER_POUND))

# ppp_in_MaySep <- ggplot()+
#   #geom_violin(data = test_join_uniques_price_per_pound %>%  filter(pre_post_reg=='pre-reg'), aes(x= month_name, y= PRICE_PER_POUND, fill=pre_post_reg), lwd=1) + 
#   geom_violin(data = avg_ppp_pre_reg, aes(x= month_name, y= PRICE_PER_POUND, fill=pre_post_reg), lwd=1) + 
#   scale_fill_manual(values=c("white")) +
# 
#   geom_point(data = avg_ppp_post_reg, aes(x= month_name, y= PRICE_PER_POUND, color=season), size=5) + 
#   scale_color_manual(values=c("black", "gray80")) +
# 
#   #scale_colour_brewer(palette = "PRGn") +
#   ylab("Price per pound") +
#   xlab("") + 
#   #guides(color = guide_legend(override.aes = list(size = 2))) +
#   theme_bw()+
#   theme(#panel.background = element_rect(fill = 'gray92'),
#     legend.title = element_blank(),
#     #title = element_text(size = 32),
#     legend.text = element_text(size=20),
#     axis.text.x = element_text(hjust = 0.5,size = 20),
#     axis.text.y = element_text(size = 20),
#     axis.title = element_text(size = 20),
#     #legend.position = c(0.9, 0.8) +
#     legend.position = c(.86, .8)
#   )
# ppp_in_MaySep


## another way of doing price per pound
avg_ppp_post_reg_MaySep <- test_join_uniques_price_per_pound %>% 
  filter(pre_post_reg == '2019-2020') %>% 
  group_by(season, pre_post_reg) %>% 
  summarise(PRICE_PER_POUND = mean(PRICE_PER_POUND_adj))

avg_ppp_post_reg_JulSep <- test_join_uniques_price_per_pound %>% 
  filter(month_name %in% c('July','August','September')) %>% 
  filter(pre_post_reg == '2018-2019') %>% 
  group_by(season, pre_post_reg) %>% 
  summarise(PRICE_PER_POUND = mean(PRICE_PER_POUND_adj))


avg_ppp_pre_reg_MaySep <- test_join_uniques_price_per_pound %>% 
  filter(pre_post_reg == 'pre-reg') %>% 
  group_by(season, pre_post_reg) %>% 
  summarise(PRICE_PER_POUND = mean(PRICE_PER_POUND_adj))

avg_ppp_pre_reg_JulSep <- test_join_uniques_price_per_pound %>% 
  filter(month_name %in% c('July','August','September')) %>% 
  filter(pre_post_reg == 'pre-reg') %>% 
  group_by(season, pre_post_reg) %>% 
  summarise(PRICE_PER_POUND = mean(PRICE_PER_POUND_adj))



ppp_in_JulSep <- ggplot()+
  geom_violin(data = avg_ppp_pre_reg_JulSep, aes(x= pre_post_reg, y= PRICE_PER_POUND, fill=pre_post_reg), lwd=2) + 
  scale_fill_manual(values=c("white")) +
  #geom_dotplot(data = avg_ppp_pre_reg_JulSep, aes(x= pre_post_reg, y= PRICE_PER_POUND), binaxis = "y", stackdir = "center") + 
  
  geom_point(data = avg_ppp_post_reg_JulSep, aes(x= pre_post_reg, y= PRICE_PER_POUND), size=8) + 
  scale_color_manual(values=c("black")) +
  
  #scale_colour_brewer(palette = "PRGn") +
  ylab("Average price per pound ($)") +
  xlab("") + 
  scale_x_discrete(limits = rev, labels=c("pre-reg" = "pre-regulations", "2018-2019" = "2019")) +
  
  theme_classic()+
  theme(#panel.background = element_rect(fill = 'gray92'),
    legend.title = element_blank(),
    #title = element_text(size = 32),
    legend.text = element_text(size=40),
    axis.text.x = element_text(hjust = 0.5,size = 40),
    axis.text.y = element_text(size = 40),
    axis.title = element_text(size = 50),
    #legend.position = c(.86, .2),
    legend.position = "none"
  )
ppp_in_JulSep


# path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
# png(paste0(path_figures, "/mean_price_per_pound_prePreg_vs_2019_ADJ_INFL.png"), width = 22, height = 14, units = "in", res = 400)
# ggarrange(ppp_in_JulSep,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())



ppp_in_MaySep <- ggplot()+
  geom_violin(data = avg_ppp_pre_reg_MaySep, aes(x= pre_post_reg, y= PRICE_PER_POUND, fill=pre_post_reg), lwd=2) + 
  scale_fill_manual(values=c("white")) +
  #geom_dotplot(data = avg_ppp_pre_reg_MaySep, aes(x= pre_post_reg, y= PRICE_PER_POUND), binaxis = "y", stackdir = "center") + 
  
  geom_point(data = avg_ppp_post_reg_MaySep, aes(x= pre_post_reg, y= PRICE_PER_POUND), size=8) + 
  scale_color_manual(values=c("black")) +
  
  #scale_colour_brewer(palette = "PRGn") +
  ylab("") +
  xlab("") + 
  scale_x_discrete(limits = rev, labels=c("pre-reg" = "pre-regulations", "2019-2020" = "2020")) +
  
  theme_classic()+
  theme(#panel.background = element_rect(fill = 'gray92'),
    legend.title = element_blank(),
    #title = element_text(size = 32),
    legend.text = element_text(size=40),
    axis.text.x = element_text(hjust = 0.5,size = 40),
    axis.text.y = element_text(size = 40),
    axis.title = element_text(size = 50),
    #legend.position = c(.86, .2),
    legend.position = "none"
  )
ppp_in_MaySep


# path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
# png(paste0(path_figures, "/mean_price_per_pound_prePreg_vs_2020_ADJ_INFL.png"), width = 22, height = 14, units = "in", res = 400)
# ggarrange(ppp_in_MaySep,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())





#################################################################


################################

# number of vessels that were active in May-Sep of each year as per fishticket data
#but note that this is now things landed in WA, but may have fished off OR

active_vessels_in_MaySep_by_season_fishtix <- fishtix_2014_2020  %>% 
  #only interested in May-Sep
  filter(LANDING_MONTH %in% 5:9) %>% 
  #group only by season
  group_by(LANDING_YEAR, LANDING_MONTH) %>% 
  summarise(
    n_unique_vessels=n_distinct(VESSEL_ID), na.rm=TRUE)


# vessels_in_MaySep_by_season_plot_fishtix <- ggplot(active_vessels_in_MaySep_by_season_fishtix, aes(x= LANDING_MONTH, y= n_unique_vessels, group=as.factor(LANDING_YEAR), color=as.factor(LANDING_YEAR)))+
#   geom_line(size=1.5, lineend = "round") + 
#   ylab("No. active vessels in May-Sep in WA \n(unique vessels in fishtix)") +
#   xlab("Season") + 
#   theme(legend.title = element_blank(),
#         #title = element_text(size = 32),
#         legend.text = element_text(size=20),
#         axis.text.x = element_text(hjust = 1,size = 20, angle = 90),
#         axis.text.y = element_text(size = 20),
#         axis.title = element_text(size = 20),
#         #legend.position = c(0.9, 0.8) +
#         legend.position="bottom"
#   )
# vessels_in_MaySep_by_season_plot_fishtix








########################################################################################
#efficiency - CPUE
test_join_uniques
test_join

efficiency_CPUE <-  test_join %>% 
  #exclude personal use catch
  filter(REMOVAL_TYPE_NAME == "COMMERCIAL (NON-EFP)") %>% 
  #each row is a string line, multiple string lines in a FISHTICKET
  group_by(FISH_TICKET_ID) %>% 
  summarise(sum_pots_per_ticket = sum(PotsFished, na.rm = T))

efficiency_CPUE <- left_join(efficiency_CPUE,
                             test_join_uniques_adj_inf %>% select(LANDED_WEIGHT_LBS, EXVESSEL_REVENUE_adj, season, month_name),
                             by = "FISH_TICKET_ID"
)

efficiency_CPUE_v2 <- efficiency_CPUE %>% 
  mutate(dollar_per_pot = EXVESSEL_REVENUE_adj/sum_pots_per_ticket, na.rm = T,
         lbs_per_pot = LANDED_WEIGHT_LBS/sum_pots_per_ticket, na.rm = T)

efficiency_CPUE_v2$month_name <- factor(efficiency_CPUE_v2$month_name, levels = c('May', 'June', 'July', 'August', 'September'))

#calculating CPUE this way takes 'mean of a mean'
# summary_efficiency_CPUE_v2_MaySep <- efficiency_CPUE_v2 %>% 
#   filter(season != '2018-2019')  %>% 
#   #group_by(season, pre_post_reg) %>% 
#   group_by(season, month_name) %>% 
#   summarise(mean_dollar_per_pot = mean(dollar_per_pot, na.rm = T),
#             median_dollar_per_pot = median(dollar_per_pot, na.rm = T),
#             mean_lbs_per_pot = mean(lbs_per_pot, na.rm = T),
#             median_lbs_per_pot = median(lbs_per_pot, na.rm = T)) %>% 
#   mutate(pre_post_reg = 
#            ifelse(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018'), "pre-reg", season))

#calculating CPUE a different way
summary_efficiency_CPUE_v2_MaySep <- efficiency_CPUE %>% 
  filter(season != '2018-2019')  %>% 
  #group_by(season, pre_post_reg) %>% 
  group_by(season, month_name) %>% 
  summarise(total_pots = sum(sum_pots_per_ticket, na.rm = T),
            total_dollars = sum(EXVESSEL_REVENUE_adj, na.rm = T),
            dollar_per_pot = total_dollars/total_pots,
            
            total_lbs = sum(LANDED_WEIGHT_LBS, na.rm = T),
            lbs_per_pot = total_lbs/total_pots) %>% 
  mutate(pre_post_reg = 
           ifelse(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018'), "pre-reg", season))

summary_efficiency_CPUE_v2_MaySep$month_name <- factor(summary_efficiency_CPUE_v2_MaySep$month_name, levels = c('May', 'June', 'July', 'August', 'September'))


CPUE_ts_lbs_MaySep <- ggplot()+
  geom_violin(data = summary_efficiency_CPUE_v2_MaySep %>%  filter(pre_post_reg =="pre-reg"), aes(x=pre_post_reg, y=lbs_per_pot), lwd=2) + #,size=2.5  group=season,
  geom_dotplot(data = summary_efficiency_CPUE_v2_MaySep %>%  filter(pre_post_reg =="pre-reg"), aes(x = pre_post_reg, y = lbs_per_pot), binaxis='y', stackdir='center', dotsize=0.6) +
  stat_summary(data = summary_efficiency_CPUE_v2_MaySep %>%  filter(pre_post_reg =="pre-reg"), aes(x = pre_post_reg, y = lbs_per_pot),
               fun = "median",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  geom_violin(data = summary_efficiency_CPUE_v2_MaySep %>%  filter(pre_post_reg !="pre-reg"), aes(x=pre_post_reg, y=lbs_per_pot), lwd=2) + #,size=2.5  group=season,
  geom_dotplot(data = summary_efficiency_CPUE_v2_MaySep %>%  filter(pre_post_reg !="pre-reg"), aes(x = pre_post_reg, y = lbs_per_pot), binaxis='y', stackdir='center', dotsize=0.6) +
  stat_summary(data = summary_efficiency_CPUE_v2_MaySep %>%  filter(pre_post_reg !="pre-reg"), aes(x = pre_post_reg, y = lbs_per_pot),
               fun = "median",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  #ylab("Mean lbs/pot") +
  ylab("") +
  xlab("") + 
  scale_x_discrete(limits = rev, labels=c("pre-reg" = "pre-regulations", "2019-2020" = "2020")) +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.35, .85),
        axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0),
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
CPUE_ts_lbs_MaySep

# path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
# png(paste0(path_figures, "/CPUE_pounds_per_pot_prePreg_vs_2020_dots_and_median.png"), width = 16, height = 14, units = "in", res = 400)
# ggarrange(CPUE_ts_lbs_MaySep,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())



CPUE_ts_dollar_MaySep <- ggplot()+
  geom_violin(data = summary_efficiency_CPUE_v2_MaySep %>%  filter(pre_post_reg =="pre-reg"), aes(x=pre_post_reg, y=dollar_per_pot), lwd=2) + #,size=2.5  group=season,
  geom_dotplot(data = summary_efficiency_CPUE_v2_MaySep %>%  filter(pre_post_reg =="pre-reg"), aes(x = pre_post_reg, y = dollar_per_pot), binaxis='y', stackdir='center', dotsize=0.6) +
  stat_summary(data = summary_efficiency_CPUE_v2_MaySep %>%  filter(pre_post_reg =="pre-reg"), aes(x = pre_post_reg, y = dollar_per_pot),
               fun = "median",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  geom_violin(data = summary_efficiency_CPUE_v2_MaySep %>%  filter(pre_post_reg !="pre-reg"), aes(x=pre_post_reg, y=dollar_per_pot), lwd=2) + #,size=2.5  group=season,
  geom_dotplot(data = summary_efficiency_CPUE_v2_MaySep %>%  filter(pre_post_reg !="pre-reg"), aes(x = pre_post_reg, y = dollar_per_pot), binaxis='y', stackdir='center', dotsize=0.6) +
  stat_summary(data = summary_efficiency_CPUE_v2_MaySep %>%  filter(pre_post_reg !="pre-reg"), aes(x = pre_post_reg, y = dollar_per_pot),
               fun = "median",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  #ylab("Mean $/pot") +
  ylab("") +
  xlab("") + 
  scale_x_discrete(limits = rev, labels=c("pre-reg" = "pre-regulations", "2019-2020" = "2020")) +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.35, .85),
        axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0),
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
CPUE_ts_dollar_MaySep

# path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
# png(paste0(path_figures, "/CPUE_dollars_per_pot_prePreg_vs_2020_ADJ_INFL_dots_and_median.png"), width = 16, height = 14, units = "in", res = 400)
# ggarrange(CPUE_ts_dollar_MaySep,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())


##GLM##
hist(summary_efficiency_CPUE_v2_MaySep$dollar_per_pot)
hist(summary_efficiency_CPUE_v2_MaySep$lbs_per_pot)

mod1_CPUE_lbs_MaySep <- glm(lbs_per_pot ~ pre_post_reg + month_name,
                            family=gaussian, data=summary_efficiency_CPUE_v2_MaySep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_CPUE_lbs_MaySep) #pre-post-reg not a significant predictor 
hist(mod1_CPUE_lbs_MaySep$residuals)
#plot(mod1_CPUE_lbs_MaySep)

mod1_CPUE_dollar_MaySep <- glm(dollar_per_pot ~ pre_post_reg + month_name,
                               family=gaussian, data=summary_efficiency_CPUE_v2_MaySep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_CPUE_dollar_MaySep) #pre-post-reg IS a significant predictor if don't adjust for infaltion, NOT significant if do adjust for infaltion
hist(mod1_CPUE_dollar_MaySep$residuals)
#plot(mod1_CPUE_dollar_MaySep)


# % change
pre_reg_mean_CPUE_dollar_MaySep <- summary_efficiency_CPUE_v2_MaySep %>% 
  group_by(pre_post_reg) %>% 
  summarise(mean_dollar_per_pot = mean(dollar_per_pot),
            median_dollar_per_pot = median(dollar_per_pot),
            mean_lbs_per_pot = mean(lbs_per_pot),
            median_lbs_per_pot = median(lbs_per_pot))

#% change from pre-reg mean to 2020
#ADJUSTED FOR INFALTION
(10.4-8.92)/8.92*100 ##16.59%
#MEDIAN:
(10.6-7.92)/7.92*100 #33.84%
#LBS
(2.41-2.00)/2.00*100 ##20.5 if do mean
(2.30-1.70)/1.70*100 ##35.29 if do median



##Jul-Sep
#calculating CPUE this way takes 'mean of a mean'
# summary_efficiency_CPUE_v2_JulSep <- efficiency_CPUE_v2 %>% 
#   filter(season != '2019-2020') %>% 
#   filter(month_name %in% c('July', 'August', 'September')) %>% 
#   #group_by(season, pre_post_reg) %>% 
#   group_by(season, month_name) %>% 
#   summarise(mean_dollar_per_pot = mean(dollar_per_pot, na.rm = T),
#             median_dollar_per_pot = median(dollar_per_pot, na.rm = T),
#             mean_lbs_per_pot = mean(lbs_per_pot, na.rm = T),
#             median_lbs_per_pot = median(lbs_per_pot, na.rm = T)) %>% 
#   mutate(pre_post_reg = 
#            ifelse(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018'), "pre-reg", season))
# 
#calculating CPUE a different way
summary_efficiency_CPUE_v2_JulSep <- efficiency_CPUE %>% 
  filter(season != '2019-2020') %>% 
  filter(month_name %in% c('July', 'August', 'September')) %>% 
  group_by(season, month_name) %>% 
  summarise(total_pots = sum(sum_pots_per_ticket, na.rm = T),
            total_dollars = sum(EXVESSEL_REVENUE_adj, na.rm = T),
            dollar_per_pot = total_dollars/total_pots,
            
            total_lbs = sum(LANDED_WEIGHT_LBS, na.rm = T),
            lbs_per_pot = total_lbs/total_pots) %>% 
  mutate(pre_post_reg = 
           ifelse(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018'), "pre-reg", season))

summary_efficiency_CPUE_v2_JulSep$month_name <- factor(summary_efficiency_CPUE_v2_JulSep$month_name, levels = c('July', 'August', 'September'))


CPUE_ts_dollar_JulSep <- ggplot()+
  geom_violin(data = summary_efficiency_CPUE_v2_JulSep %>%  filter(pre_post_reg =="pre-reg"), aes(x=pre_post_reg, y=dollar_per_pot), lwd=2) + #,size=2.5  group=season,
  geom_dotplot(data = summary_efficiency_CPUE_v2_JulSep %>%  filter(pre_post_reg =="pre-reg"), aes(x = pre_post_reg, y = dollar_per_pot), binaxis='y', stackdir='center', dotsize=0.6) +
  stat_summary(data = summary_efficiency_CPUE_v2_JulSep %>%  filter(pre_post_reg =="pre-reg"), aes(x = pre_post_reg, y = dollar_per_pot),
               fun = "median",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  geom_violin(data = summary_efficiency_CPUE_v2_JulSep %>%  filter(pre_post_reg !="pre-reg"), aes(x=pre_post_reg, y=dollar_per_pot), lwd=2) + #,size=2.5  group=season,
  geom_dotplot(data = summary_efficiency_CPUE_v2_JulSep %>%  filter(pre_post_reg !="pre-reg"), aes(x = pre_post_reg, y = dollar_per_pot), binaxis='y', stackdir='center', dotsize=0.6) +
  stat_summary(data = summary_efficiency_CPUE_v2_JulSep %>%  filter(pre_post_reg !="pre-reg"), aes(x = pre_post_reg, y = dollar_per_pot),
               fun = "median",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  ylab("Mean $/pot") +
  xlab("") + 
  scale_x_discrete(limits = rev, labels=c("pre-reg" = "pre-regulations", "2018-2019" = "2019")) +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.35, .85),
        axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0),
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
CPUE_ts_dollar_JulSep

# path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
# png(paste0(path_figures, "/CPUE_dollars_per_pot_prePreg_vs_2019_ADJ_INFL_dots_and_median.png"), width = 16, height = 14, units = "in", res = 400)
# ggarrange(CPUE_ts_dollar_JulSep,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())



CPUE_ts_lbs_JulSep <- ggplot()+
  geom_violin(data = summary_efficiency_CPUE_v2_JulSep %>%  filter(pre_post_reg =="pre-reg"), aes(x=pre_post_reg, y=lbs_per_pot), lwd=2) + #,size=2.5  group=season,
  geom_dotplot(data = summary_efficiency_CPUE_v2_JulSep %>%  filter(pre_post_reg =="pre-reg"), aes(x = pre_post_reg, y = lbs_per_pot), binaxis='y', stackdir='center', dotsize=0.6) +
  stat_summary(data = summary_efficiency_CPUE_v2_JulSep %>%  filter(pre_post_reg =="pre-reg"), aes(x = pre_post_reg, y = lbs_per_pot),
               fun = "median",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  geom_violin(data = summary_efficiency_CPUE_v2_JulSep %>%  filter(pre_post_reg !="pre-reg"), aes(x=pre_post_reg, y=lbs_per_pot), lwd=2) + #,size=2.5  group=season,
  geom_dotplot(data = summary_efficiency_CPUE_v2_JulSep %>%  filter(pre_post_reg !="pre-reg"), aes(x = pre_post_reg, y = lbs_per_pot), binaxis='y', stackdir='center', dotsize=0.6) +
  stat_summary(data = summary_efficiency_CPUE_v2_JulSep %>%  filter(pre_post_reg !="pre-reg"), aes(x = pre_post_reg, y = lbs_per_pot),
               fun = "median",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  ylab("Mean lbs/pot") +
  xlab("") + 
  scale_x_discrete(limits = rev, labels=c("pre-reg" = "pre-regulations", "2018-2019" = "2019")) +
  theme_classic() +
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.35, .85),
        axis.text.x = element_text(hjust = 0.5,size = 40, angle = 0),
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
CPUE_ts_lbs_JulSep

# path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
# png(paste0(path_figures, "/CPUE_pounds_per_pot_prePreg_vs_2019_dots_and_median.png"), width = 16, height = 14, units = "in", res = 400)
# ggarrange(CPUE_ts_lbs_JulSep,
#           ncol=1,
#           nrow=1
#           #legend="top",
#           #labels="auto",
#           #vjust=8,
#           #hjust=-0.2
# )
# invisible(dev.off())


##GLM##
hist(summary_efficiency_CPUE_v2_JulSep$dollar_per_pot)
hist(summary_efficiency_CPUE_v2_JulSep$lbs_per_pot)

mod1_CPUE_lbs_JulSep <- glm(lbs_per_pot ~ pre_post_reg + month_name,
                            family=gaussian, data=summary_efficiency_CPUE_v2_JulSep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_CPUE_lbs_JulSep) #pre-post-reg not a significant predictor 
hist(mod1_CPUE_lbs_JulSep$residuals)
#plot(mod1_CPUE_lbs_JulSep)

mod1_CPUE_dollar_JulSep <- glm(dollar_per_pot ~ pre_post_reg + month_name,
                               family=gaussian, data=summary_efficiency_CPUE_v2_JulSep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_CPUE_dollar_JulSep) #pre-post-reg not a significant predictor 
hist(mod1_CPUE_dollar_JulSep$residuals)
#plot(mod1_CPUE_dollar_JulSep)



# % change
pre_reg_mean_CPUE_dollar_JulSep <- summary_efficiency_CPUE_v2_JulSep %>% 
  group_by(pre_post_reg) %>% 
  summarise(mean_dollar_per_pot = mean(dollar_per_pot),
            median_dollar_per_pot = median(dollar_per_pot),
            mean_lbs_per_pot = mean(lbs_per_pot),
            median_lbs_per_pot = median(lbs_per_pot))

#% change in mean from pre-reg  to 2019
#ADJUSTED FOR INFALTION
(5.25-9.98)/9.98*100 #-47.39%
#MEDIAN
(5.18-8.45)/8.45*100 #-38.70%
#LBS
(1.25-2.33)/2.33*100 ##-46.35 
#MEDIAN
(1.26-1.90)/1.90*100 ##-33.68 
