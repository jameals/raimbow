#test joining logbooks (May-Sep, all effort in WA waters) and fishtickets

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

#Fishticket and landing date columns have been dropped during pipeline, bring them back from an earlier version of raw data  
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
  filter(LANDING_YEAR %in% c(2014, 2015, 2016, 2017, 2018, 2019, 2020))


#the FishTicket1 column in WA logs, and the FISH_TICKET_ID column in pacfin data don't match

#but can join files using landing date, and FederalID in logs with VESSEL_NUM in fishtix


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
#some but not all SetIDs occur multiple times after joining with pacfin data - unsure why... 
  #--> this seems to have happened to those stringlines that had data in both Fishticket1 and Fishticket2 columns
#nrow(test_join) #26306 #but the number of unique SetIDs is still the same as before (20144)

#each row is one stringline, but multiple stringlines may have been on one fishticket
#keep only one record per fishticket
test_join_uniques <- test_join %>% 
  group_by(FISH_TICKET_ID) %>% 
  filter(row_number()==1)


length(unique(joined_df$FishTicket1)) #4392 unique Fishticket1 values in WA logs
length(unique(test_join_uniques$FishTicket1)) #4352 unique Fishticket1 values after joining with pacfin
#--> so 99% of WA Fishticket1 numbers also found a fishticket info from pacfin?
# is this the best way of measuring how much data was 'lost' (doesn't have fishticket landing info),
#or how much of logbook data didn't find matching pacfin fishticket info



# NOMINAL_TO_ACTUAL_PACFIN_SPECIES_NAME column is not always DCRB
#but is DCRB in 99.5% of tickets

#also in test_join_uniques:
#PACFIN_SPECIES_CODE  n_rows
#CHNK                   7
#COHO                   1
#DCRB                   4569
#PHLB                   5
#SABL                   4
#SPRW                   4
#NA                     1



#FTID in pacfin looks to match Fishticket1 (and Fishticket2/Fishticket3... if that column also exists)...
#could be harder to join that way as info in more than 1 column (Fishticket1, Fishticket2...)
test_join_by_FTID_and_Fishticket1 <- test_df_2 %>% 
  left_join(fishtix_2014_2020, 
            by = c("FishTicket1" = "FTID",
                   "LandingDate" = "LANDING_DATE"))
#more NAs this way than by joining with landing date and vessel ID, 
#because some data is in Fishticket1 column and some in Fishticket2 column
#perhaps slightly better success joining if don't join by landing date as well...


#--------------------------------------------
#adjust for inflation

cpi_raw <- read_csv(here('wdfw', 'data', 'cpi_2021.csv'),col_types='idc')

# add a conversion factor to 2014 $$
cpi <- cpi_raw %>% 
  mutate(convert2014=1/(annual_average/236.7)) %>% 
  filter(year>2013) %>% 
  filter(year<2021) %>% 
  dplyr::select(year,convert2014) %>% 
  rename(LANDING_YEAR = year)

test_join_uniques_adj_inf <- test_join_uniques %>% 
  left_join(cpi, by = c('LANDING_YEAR')) %>% 
  mutate(EXVESSEL_REVENUE_adj = EXVESSEL_REVENUE * convert2014,
         PRICE_PER_POUND_adj = PRICE_PER_POUND * convert2014)
  


#-----------------------------------------------------------------------------------------------
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
  stat_summary(data = summary_pacfin_data_JulSep, aes(x = pre_post_reg, y = sum_revenue/100000),
               fun = "median",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  geom_dotplot(data = summary_pacfin_data_JulSep, aes(x = pre_post_reg, y = sum_revenue/100000), binaxis='y', stackdir='center', dotsize=0.6) +
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

path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
png(paste0(path_figures, "/total_revenue_prePreg_vs_2019_ADJ_INFL_dots_and_median.png"), width = 16, height = 14, units = "in", res = 400)
ggarrange(sum_JulSep_rev_box,
          ncol=1,
          nrow=1
          #legend="top",
          #labels="auto",
          #vjust=8,
          #hjust=-0.2
)
invisible(dev.off())


##GLM##
hist(summary_pacfin_data_JulSep$sum_revenue)

mod1_rev_JulSep <- glm(sum_revenue ~ pre_post_reg + month_name,
                 family=gaussian, data=summary_pacfin_data_JulSep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_rev_JulSep) #pre-post-reg not a significant predictor of revenue
hist(mod1_rev_JulSep$residuals)

plot(mod1_rev_JulSep)


pre_reg_mean_revenue_JulSep <- summary_pacfin_data_JulSep %>% 
  group_by(pre_post_reg) %>% 
  summarise(mean_revenue = mean(sum_revenue),
            median_revenue = median(sum_revenue))

#% change from pre-reg mean to 2019
(293462.5-413615.7)/413615.7*100
#-29.05 --> but pre-reg mean high due to high year of 2014
#% change from pre-reg MEDIAN to 2019
(364576.5-264532.4)/264532.4*100 ##37.82
##ADJUSTED FOR INFLATION
#% change from pre-reg mean to 2019
(271656.5-407971.0)/407971.0*100
#-33.41 --> but pre-reg mean high due to high year of 2014
#% change from pre-reg MEDIAN to 2019
(337486.4-254010.8)/254010.8*100 ##32.86

##LANDINGS
sum_JulSep_lbs_box <- ggplot() +
  geom_violin(data = summary_pacfin_data_JulSep, aes(x = pre_post_reg, y = sum_weight_lbs/100000), lwd=2) +
  stat_summary(data = summary_pacfin_data_JulSep, aes(x = pre_post_reg, y = sum_weight_lbs/100000),
               fun = "median",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  geom_dotplot(data = summary_pacfin_data_JulSep, aes(x = pre_post_reg, y = sum_weight_lbs/100000), binaxis='y', stackdir='center', dotsize=0.6) +
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

path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
png(paste0(path_figures, "/total_landings_prePreg_vs_2019_dots_and_median.png"), width = 16, height = 14, units = "in", res = 400)
ggarrange(sum_JulSep_lbs_box,
          ncol=1,
          nrow=1
          #legend="top",
          #labels="auto",
          #vjust=8,
          #hjust=-0.2
)
invisible(dev.off())


##GLM##
hist(summary_pacfin_data_JulSep$sum_weight_lbs)

mod1_lbs_JulSep <- glm(sum_weight_lbs ~ pre_post_reg + month_name,
                       family=gaussian, data=summary_pacfin_data_JulSep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_lbs_JulSep) #pre-post-reg not a significant predictor of revenue
hist(mod1_lbs_JulSep$residuals)

plot(mod1_lbs_JulSep)


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
  stat_summary(data = summary_pacfin_data_MaySep, aes(x = pre_post_reg, y = sum_revenue/100000),
               fun = "median",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  geom_dotplot(data = summary_pacfin_data_MaySep, aes(x = pre_post_reg, y = sum_revenue/100000), binaxis='y', stackdir='center', dotsize=0.6) +
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

path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
png(paste0(path_figures, "/total_revenue_prePreg_vs_2020_ADJ_INFL_dots_and_median.png"), width = 16, height = 14, units = "in", res = 400)
ggarrange(sum_MaySep_rev_box,
          ncol=1,
          nrow=1
          #legend="top",
          #labels="auto",
          #vjust=8,
          #hjust=-0.2
)
invisible(dev.off())


##GLM##
hist(summary_pacfin_data_MaySep$sum_revenue)

mod1_rev_MaySep <- glm(sum_revenue ~ pre_post_reg + month_name,
                       family=gaussian, data=summary_pacfin_data_MaySep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_rev_MaySep) #pre-post-reg not a significant predictor of revenue
hist(mod1_rev_MaySep$residuals)

plot(mod1_rev_MaySep)


pre_reg_mean_revenue_MaySep <- summary_pacfin_data_MaySep %>% 
  group_by(pre_post_reg) %>% 
  summarise(mean_revenue = mean(sum_revenue),
            median_revenue = median(sum_revenue))

#% change from pre-reg mean to 2019
(344945.4-397420.4)/397420.4*100
#-13.20 --> but pre-reg mean high due to high year of 2014
#% change from pre-reg MEDIAN to 2019
(303595.9-257379.8)/257379.8*100 #17.95638
##ADJUSTED FOR INFLATION
#% change from pre-reg mean to 2019
(315489.1-389050.9)/389050.9*100
#-18.91 --> but pre-reg mean high due to high year of 2014
#% change from pre-reg MEDIAN to 2019
(277670.6-248558.9)/248558.9*100 ##11.71



##LANDINGS
sum_MaySep_lbs_box <- ggplot() +
  geom_violin(data = summary_pacfin_data_MaySep, aes(x = pre_post_reg, y = sum_weight_lbs/100000), lwd=2) +
  stat_summary(data = summary_pacfin_data_MaySep, aes(x = pre_post_reg, y = sum_weight_lbs/100000),
               fun = "median",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  geom_dotplot(data = summary_pacfin_data_MaySep, aes(x = pre_post_reg, y = sum_weight_lbs/100000), binaxis='y', stackdir='center', dotsize=0.6) +
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

path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
png(paste0(path_figures, "/total_landings_prePreg_vs_2020_ADJ_INFL_dots_and_median.png"), width = 16, height = 14, units = "in", res = 400)
ggarrange(sum_MaySep_lbs_box,
          ncol=1,
          nrow=1
          #legend="top",
          #labels="auto",
          #vjust=8,
          #hjust=-0.2
)
invisible(dev.off())


##GLM##
hist(summary_pacfin_data_MaySep$sum_weight_lbs)

mod1_lbs_MaySep <- glm(sum_weight_lbs ~ pre_post_reg + month_name,
                       family=gaussian, data=summary_pacfin_data_MaySep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_lbs_MaySep) #pre-post-reg not a significant predictor of revenue
hist(mod1_lbs_MaySep$residuals)

plot(mod1_lbs_MaySep)


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


path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
png(paste0(path_figures, "/mean_price_per_pound_prePreg_vs_2019_ADJ_INFL.png"), width = 22, height = 14, units = "in", res = 400)
ggarrange(ppp_in_JulSep,
          ncol=1,
          nrow=1
          #legend="top",
          #labels="auto",
          #vjust=8,
          #hjust=-0.2
)
invisible(dev.off())



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


path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
png(paste0(path_figures, "/mean_price_per_pound_prePreg_vs_2020_ADJ_INFL.png"), width = 22, height = 14, units = "in", res = 400)
ggarrange(ppp_in_MaySep,
          ncol=1,
          nrow=1
          #legend="top",
          #labels="auto",
          #vjust=8,
          #hjust=-0.2
)
invisible(dev.off())





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


summary_efficiency_CPUE_v2_MaySep <- efficiency_CPUE_v2 %>% 
  filter(season != '2018-2019')  %>% 
  #group_by(season, pre_post_reg) %>% 
  group_by(season, month_name) %>% 
  summarise(mean_dollar_per_pot = mean(dollar_per_pot, na.rm = T),
            median_dollar_per_pot = median(dollar_per_pot, na.rm = T),
            mean_lbs_per_pot = mean(lbs_per_pot, na.rm = T),
            median_lbs_per_pot = median(lbs_per_pot, na.rm = T)) %>% 
  mutate(pre_post_reg = 
           ifelse(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018'), "pre-reg", season))



CPUE_ts_lbs_MaySep <- ggplot()+
  geom_violin(data = summary_efficiency_CPUE_v2_MaySep %>%  filter(pre_post_reg =="pre-reg"), aes(x=pre_post_reg, y=mean_lbs_per_pot), lwd=2) + #,size=2.5  group=season,
  stat_summary(data = summary_efficiency_CPUE_v2_MaySep %>%  filter(pre_post_reg =="pre-reg"), aes(x = pre_post_reg, y = mean_lbs_per_pot),
               fun = "median",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  geom_dotplot(data = summary_efficiency_CPUE_v2_MaySep %>%  filter(pre_post_reg =="pre-reg"), aes(x = pre_post_reg, y = mean_lbs_per_pot), binaxis='y', stackdir='center', dotsize=0.6) +
  geom_violin(data = summary_efficiency_CPUE_v2_MaySep %>%  filter(pre_post_reg !="pre-reg"), aes(x=pre_post_reg, y=mean_lbs_per_pot), lwd=2) + #,size=2.5  group=season,
  stat_summary(data = summary_efficiency_CPUE_v2_MaySep %>%  filter(pre_post_reg !="pre-reg"), aes(x = pre_post_reg, y = mean_lbs_per_pot),
               fun = "median",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  geom_dotplot(data = summary_efficiency_CPUE_v2_MaySep %>%  filter(pre_post_reg !="pre-reg"), aes(x = pre_post_reg, y = mean_lbs_per_pot), binaxis='y', stackdir='center', dotsize=0.6) +
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

path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
png(paste0(path_figures, "/CPUE_pounds_per_pot_prePreg_vs_2020_dots_and_median.png"), width = 16, height = 14, units = "in", res = 400)
ggarrange(CPUE_ts_lbs_MaySep,
          ncol=1,
          nrow=1
          #legend="top",
          #labels="auto",
          #vjust=8,
          #hjust=-0.2
)
invisible(dev.off())



CPUE_ts_dollar_MaySep <- ggplot()+
  geom_violin(data = summary_efficiency_CPUE_v2_MaySep %>%  filter(pre_post_reg =="pre-reg"), aes(x=pre_post_reg, y=mean_dollar_per_pot), lwd=2) + #,size=2.5  group=season,
  stat_summary(data = summary_efficiency_CPUE_v2_MaySep %>%  filter(pre_post_reg =="pre-reg"), aes(x = pre_post_reg, y = mean_dollar_per_pot),
               fun = "median",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  geom_dotplot(data = summary_efficiency_CPUE_v2_MaySep %>%  filter(pre_post_reg =="pre-reg"), aes(x = pre_post_reg, y = mean_dollar_per_pot), binaxis='y', stackdir='center', dotsize=0.6) +
  geom_violin(data = summary_efficiency_CPUE_v2_MaySep %>%  filter(pre_post_reg !="pre-reg"), aes(x=pre_post_reg, y=mean_dollar_per_pot), lwd=2) + #,size=2.5  group=season,
  stat_summary(data = summary_efficiency_CPUE_v2_MaySep %>%  filter(pre_post_reg !="pre-reg"), aes(x = pre_post_reg, y = mean_dollar_per_pot),
               fun = "median",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  geom_dotplot(data = summary_efficiency_CPUE_v2_MaySep %>%  filter(pre_post_reg !="pre-reg"), aes(x = pre_post_reg, y = mean_dollar_per_pot), binaxis='y', stackdir='center', dotsize=0.6) +
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

path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
png(paste0(path_figures, "/CPUE_dollars_per_pot_prePreg_vs_2020_ADJ_INFL_dots_and_median.png"), width = 16, height = 14, units = "in", res = 400)
ggarrange(CPUE_ts_dollar_MaySep,
          ncol=1,
          nrow=1
          #legend="top",
          #labels="auto",
          #vjust=8,
          #hjust=-0.2
)
invisible(dev.off())


##GLM##
hist(summary_efficiency_CPUE_v2_MaySep$mean_dollar_per_pot)
hist(summary_efficiency_CPUE_v2_MaySep$mean_lbs_per_pot)

mod1_CPUE_lbs_MaySep <- glm(mean_lbs_per_pot ~ pre_post_reg + month_name,
                            family=gaussian, data=summary_efficiency_CPUE_v2_MaySep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_CPUE_lbs_MaySep) #pre-post-reg not a significant predictor 
hist(mod1_CPUE_lbs_MaySep$residuals)
plot(mod1_CPUE_lbs_MaySep)

mod1_CPUE_dollar_MaySep <- glm(mean_dollar_per_pot ~ pre_post_reg + month_name,
                               family=gaussian, data=summary_efficiency_CPUE_v2_MaySep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_CPUE_dollar_MaySep) #pre-post-reg IS a significant predictor if don't adjust for infaltion, NOT significant if do adjust for infaltion
hist(mod1_CPUE_dollar_MaySep$residuals)
plot(mod1_CPUE_dollar_MaySep)


#this way you won't be taking the mean of a mean
pre_reg_mean_CPUE_dollar_MaySep <- efficiency_CPUE_v2 %>% 
  filter(season != '2018-2019')  %>% 
  mutate(pre_post_reg = 
           ifelse(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018'), "pre-reg", season)) %>% 
  group_by(pre_post_reg) %>% 
  summarise(mean_dollar_per_pot = mean(dollar_per_pot, na.rm = T),
            mean_lbs_per_pot = mean(lbs_per_pot, na.rm = T)) 

#this would be taking mean of a mean
# pre_reg_mean_CPUE_dollar_MaySep <- summary_efficiency_CPUE_v2_MaySep %>% 
#   group_by(pre_post_reg) %>% 
#   summarise(mean_mean_dollar_per_pot = mean(mean_dollar_per_pot))

#% change from pre-reg mean to 2020
(17.0-13.0)/13.0*100 ##30.76923
#ADJUSTED FOR INFALTION
(15.5-12.7)/12.7*100 ##22.05 --> same (22%) if do median
#LBS
(3.33-2.76)/2.76*100 ##20.6 if do mean
(1.98-1.75)/1.75*100 ##13.14 if do median



##Jul-Sep
summary_efficiency_CPUE_v2_JulSep <- efficiency_CPUE_v2 %>% 
  filter(season != '2019-2020') %>% 
  filter(month_name %in% c('July', 'August', 'September')) %>% 
  #group_by(season, pre_post_reg) %>% 
  group_by(season, month_name) %>% 
  summarise(mean_dollar_per_pot = mean(dollar_per_pot, na.rm = T),
            median_dollar_per_pot = median(dollar_per_pot, na.rm = T),
            mean_lbs_per_pot = mean(lbs_per_pot, na.rm = T),
            median_lbs_per_pot = median(lbs_per_pot, na.rm = T)) %>% 
  mutate(pre_post_reg = 
           ifelse(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018'), "pre-reg", season))


CPUE_ts_dollar_JulSep <- ggplot()+
  geom_violin(data = summary_efficiency_CPUE_v2_JulSep %>%  filter(pre_post_reg =="pre-reg"), aes(x=pre_post_reg, y=mean_dollar_per_pot), lwd=2) + #,size=2.5  group=season,
  stat_summary(data = summary_efficiency_CPUE_v2_JulSep %>%  filter(pre_post_reg =="pre-reg"), aes(x = pre_post_reg, y = mean_dollar_per_pot),
               fun = "median",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  geom_dotplot(data = summary_efficiency_CPUE_v2_JulSep %>%  filter(pre_post_reg =="pre-reg"), aes(x = pre_post_reg, y = mean_dollar_per_pot), binaxis='y', stackdir='center', dotsize=0.6) +
  geom_violin(data = summary_efficiency_CPUE_v2_JulSep %>%  filter(pre_post_reg !="pre-reg"), aes(x=pre_post_reg, y=mean_dollar_per_pot), lwd=2) + #,size=2.5  group=season,
  stat_summary(data = summary_efficiency_CPUE_v2_JulSep %>%  filter(pre_post_reg !="pre-reg"), aes(x = pre_post_reg, y = mean_dollar_per_pot),
               fun = "median",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  geom_dotplot(data = summary_efficiency_CPUE_v2_JulSep %>%  filter(pre_post_reg !="pre-reg"), aes(x = pre_post_reg, y = mean_dollar_per_pot), binaxis='y', stackdir='center', dotsize=0.6) +
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

path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
png(paste0(path_figures, "/CPUE_dollars_per_pot_prePreg_vs_2019_ADJ_INFL_dots_and_median.png"), width = 16, height = 14, units = "in", res = 400)
ggarrange(CPUE_ts_dollar_JulSep,
          ncol=1,
          nrow=1
          #legend="top",
          #labels="auto",
          #vjust=8,
          #hjust=-0.2
)
invisible(dev.off())



CPUE_ts_lbs_JulSep <- ggplot()+
  geom_violin(data = summary_efficiency_CPUE_v2_JulSep %>%  filter(pre_post_reg =="pre-reg"), aes(x=pre_post_reg, y=mean_lbs_per_pot), lwd=2) + #,size=2.5  group=season,
  stat_summary(data = summary_efficiency_CPUE_v2_JulSep %>%  filter(pre_post_reg =="pre-reg"), aes(x = pre_post_reg, y = mean_lbs_per_pot),
               fun = "median",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  geom_dotplot(data = summary_efficiency_CPUE_v2_JulSep %>%  filter(pre_post_reg =="pre-reg"), aes(x = pre_post_reg, y = mean_lbs_per_pot), binaxis='y', stackdir='center', dotsize=0.6) +
  geom_violin(data = summary_efficiency_CPUE_v2_JulSep %>%  filter(pre_post_reg !="pre-reg"), aes(x=pre_post_reg, y=mean_lbs_per_pot), lwd=2) + #,size=2.5  group=season,
  stat_summary(data = summary_efficiency_CPUE_v2_JulSep %>%  filter(pre_post_reg !="pre-reg"), aes(x = pre_post_reg, y = mean_lbs_per_pot),
               fun = "median",
               geom = "crossbar", 
               width = 0.25,
               colour = "red") +
  geom_dotplot(data = summary_efficiency_CPUE_v2_JulSep %>%  filter(pre_post_reg !="pre-reg"), aes(x = pre_post_reg, y = mean_lbs_per_pot), binaxis='y', stackdir='center', dotsize=0.6) +
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

path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
png(paste0(path_figures, "/CPUE_pounds_per_pot_prePreg_vs_2019_dots_and_median.png"), width = 16, height = 14, units = "in", res = 400)
ggarrange(CPUE_ts_lbs_JulSep,
          ncol=1,
          nrow=1
          #legend="top",
          #labels="auto",
          #vjust=8,
          #hjust=-0.2
)
invisible(dev.off())


##GLM##
hist(summary_efficiency_CPUE_v2_JulSep$mean_dollar_per_pot)
hist(summary_efficiency_CPUE_v2_JulSep$mean_lbs_per_pot)

mod1_CPUE_lbs_JulSep <- glm(mean_lbs_per_pot ~ pre_post_reg + month_name,
                       family=gaussian, data=summary_efficiency_CPUE_v2_JulSep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_CPUE_lbs_JulSep) #pre-post-reg not a significant predictor 
hist(mod1_CPUE_lbs_JulSep$residuals)
plot(mod1_CPUE_lbs_JulSep)

mod1_CPUE_dollar_JulSep <- glm(mean_dollar_per_pot ~ pre_post_reg + month_name,
                            family=gaussian, data=summary_efficiency_CPUE_v2_JulSep, na.action = na.omit) #family = gaussian(link = "log")
summary(mod1_CPUE_dollar_JulSep) #pre-post-reg not a significant predictor 
hist(mod1_CPUE_dollar_JulSep$residuals)
plot(mod1_CPUE_dollar_JulSep)










######################################################################################
#monthly revenue by vessel

monthly_rev_by_vessel <- test_join_uniques_adj_inf %>% 
  rename(License = License.x) %>% 
  group_by(Vessel.x, License, season, month_name) %>% 
  summarise(monthly_rev = sum(EXVESSEL_REVENUE_adj)) 
  

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
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
monthly_rev_JulSep

path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
png(paste0(path_figures, "/mean_monthly_revenue_by_vessel_prePreg_vs_2019_ADJ_INFL.png"), width = 15, height = 14, units = "in", res = 400)
ggarrange(monthly_rev_JulSep,
          ncol=1,
          nrow=1
          #legend="top",
          #labels="auto",
          #vjust=8,
          #hjust=-0.2
)
invisible(dev.off())


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
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 50),
        strip.text = element_text(size=40),
        strip.background = element_blank(),
        strip.placement = "left"
  )
monthly_rev_MaySep

path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
png(paste0(path_figures, "/mean_monthly_revenue_by_vessel_prePreg_vs_2020_ADJ_INFL.png"), width = 15, height = 14, units = "in", res = 400)
ggarrange(monthly_rev_MaySep,
          ncol=1,
          nrow=1
          #legend="bottom"
          #labels="auto",
          #vjust=8,
          #hjust=-0.2
)
invisible(dev.off())


### GLM on mean monthly revenue/vessel
## pre-post-reg not a significant predictor in any comparison (Jul-Sep, May-Sep, 300 or 500 tier)

#JUL-SEP
monthly_rev_by_vessel_JulSep_300 <- monthly_rev_by_vessel_JulSep %>% 
  filter(Pot_Limit == 300)
mod_monthly_rev_JulSep_300 <- glm(mean_monthly_rev ~ pre_post_reg,
                                  family=gaussian, data=monthly_rev_by_vessel_JulSep_300, na.action = na.omit) #family = gaussian(link = "log")
summary(mod_monthly_rev_JulSep_300)
plot(mod_monthly_rev_JulSep_300)


monthly_rev_by_vessel_JulSep_500 <- monthly_rev_by_vessel_JulSep %>% 
  filter(Pot_Limit == 500)
mod_monthly_rev_JulSep_500 <- glm(mean_monthly_rev ~ pre_post_reg,
                                  family=gaussian, data=monthly_rev_by_vessel_JulSep_500, na.action = na.omit) #family = gaussian(link = "log")
summary(mod_monthly_rev_JulSep_500)
plot(mod_monthly_rev_JulSep_500)


#MAY-SEP
monthly_rev_by_vessel_MaySep_300 <- monthly_rev_by_vessel_MaySep %>% 
  filter(Pot_Limit == 300)
mod_monthly_rev_MaySep_300 <- glm(mean_monthly_rev ~ pre_post_reg,
                 family=gaussian, data=monthly_rev_by_vessel_MaySep_300, na.action = na.omit) #family = gaussian(link = "log")
summary(mod_monthly_rev_MaySep_300)
plot(mod_monthly_rev_MaySep_300)


monthly_rev_by_vessel_MaySep_500 <- monthly_rev_by_vessel_MaySep %>% 
  filter(Pot_Limit == 500)
mod_monthly_rev_MaySep_500 <- glm(mean_monthly_rev ~ pre_post_reg,
                                  family=gaussian, data=monthly_rev_by_vessel_MaySep_500, na.action = na.omit) #family = gaussian(link = "log")
summary(mod_monthly_rev_MaySep_500)
plot(mod_monthly_rev_MaySep_500)
























#---------------------------------------------------------------------------------
## OLD ###
#---------------------------------------------------------------------------------

#initial test joining of logs and pacfin data with only 2014 data

path_WA_landed_WA_logs <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/traps_g_WA_landed_WA_logs_2014_2020_clipped_to_WA_waters_20220126.rds"
WA_landed_WA_logs_clipped_to_WA_waters <- readRDS(path_WA_landed_WA_logs)

WA_landed_WA_logs_clipped_to_WA_waters <- WA_landed_WA_logs_clipped_to_WA_waters %>% 
  select(-path, -layer) %>%  #columns that have been added in QGIS step, when joining files
  #because the data was used in QGIS, ESRI abbreviates column names. change names back to original form
  #new_name = old_name
  st_set_geometry(NULL) %>% #remove geometry as it is slowing everything down
  rename(
    PotsFished = PtsFshd,
    line_length_m = ln_lng_,
    GRID5KM_ID = GRID5KM,
    NGDC_GRID = NGDC_GR,
    is_port_or_bay = is_pr__,
    Landing_logbook_state = Lndng__,
    month_name = mnth_nm,
    season_month = ssn_mnt,
    month_interval = mnth_nt, 
    season_month_interval = ssn_mn_, 
    is_May_Sep = is_My_S
  ) 


#for now test with 2014 fishtix, so filter to that season
#also interested only in May-Sep period
WA_landed_WA_logs_clipped_to_WA_waters_2013_2014 <- WA_landed_WA_logs_clipped_to_WA_waters %>% 
  filter(season=='2013-2014') %>% 
  filter(is_May_Sep == 'Y')

#each row is a single pot, only need one record per SetID
#length(unique(WA_landed_WA_logs_clipped_to_WA_waters_2013_2014$SetID)) #3792
WA_landed_WA_logs_clipped_to_WA_waters_2013_2014_uniques <- WA_landed_WA_logs_clipped_to_WA_waters_2013_2014 %>% 
  group_by(SetID) %>% 
  filter(row_number()==1)
#nrow(WA_landed_WA_logs_clipped_to_WA_waters_2013_2014_uniques)   #3792


#an earlier version of raw data still had Fishticket column 
logs <- read_csv(here('wdfw', 'data','WDFW-Dcrab-logbooks-compiled_stackcoords_2009-2020.csv'),col_types = 'ccdcdccTcccccdTddddddddddddddddiddccddddcddc')
#SetID should still be the same between teh two files
logs_selected_columns <- logs %>% 
  select(SetID, FishTicket1, FishTicket2, FishTicket3, FishTicket4, Vessel, License, FederalID, LandingDate ) #Vessel, License, FederalID, 



#try joining Fihsticket columns to more processed logbooks
#getting a duplication of data: "This is because there were two y1=1 values in your d2 dataset 
  #so the join will merge these on twice." #nrow(joined_df) #7584 
joined_df <- WA_landed_WA_logs_clipped_to_WA_waters_2013_2014_uniques %>% 
  left_join(logs_selected_columns,by="SetID") %>%
  distinct #add a distinct command to remove duplicaiton
#nrow(joined_df) #3792 - when add distinct command
# cases where found fishticket
nrow(joined_df %>% filter(!is.na(FishTicket1)))
#3768 --> 99.37%
#cases where no Fishticket found
nrow(joined_df %>% filter(is.na(FishTicket1)))
#24 --> 0.006%
#manually checked and all were cases where raw logs didn't have a fishticket



#read in the actual fishticket file
fishtix_2014 <- read_csv(here('wdfw', 'data','fish tickets 2014.csv'),col_types = 'ddccccccccccccdd') 

#the FishTicket1 column in logs, and the FISH_TICKET_ID column in pacfin data don't match
#test_join <- joined_df %>% 
#  left_join(fishtix_2014, by = c("FishTicket1" = "FISH_TICKET_ID"))

#but it might be possible to join using landing date and Federal ID in logs with VESSEL_NUM in fishtix

library(stringr)
test_df <- joined_df %>% 
  select(SetID, FederalID)
test_df_new <-as.data.frame(apply(test_df,2, str_remove_all, " ")) 
test_df_2 <- joined_df %>% 
  left_join(test_df_new,by="SetID") %>% 
  rename(FederalID = FederalID.y)

#landing date needs to be date, not character
fishtix_2014_v2 <- fishtix_2014 %>% 
  mutate(LANDING_DATE=as.Date(LANDING_DATE,"%d-%b-%y"))

test_join <- test_df_2 %>% 
  left_join(fishtix_2014_v2, 
            by = c("FederalID" = "VESSEL_NUM",
                   "LandingDate" = "LANDING_DATE"))
####NOTE HERE THAT EACH ROW IS ONE STRINGLINE, BUT MULTIPLE STRINGLINES MAY HAVE BEEN ON ONE FISHTICKET ON ONE LANDING DATE
###SO HERE WOULD WANT TO KEEP ONLY ONE RECORD PER FISHTICKET
#nrow(test_join) #5102 
# cases where found fishticket info
nrow(test_join %>% filter(!is.na(EXVESSEL_REVENUE)))
#5038 --> 98.75%
#cases where no Fishticket info found
nrow(test_join %>% filter(is.na(EXVESSEL_REVENUE)))
#64 --> 0.013% #this is 9 fishtickets that didn't match to Fishtix2014 file, 
#some seem to be cases where set date and landing date don't match
#plus 21 strings with no Fishticket number





#try with OR landed WA logs (Q999999), clipped to WA waters

path_OR_landed_WA_logs <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/traps_g_OR_landed_WA_logs_Q999999_2014_2020_clipped_to_WA_waters_20220119.rds"
OR_landed_WA_logs_clipped_to_WA_waters <- readRDS(path_OR_landed_WA_logs)

OR_landed_WA_logs_clipped_to_WA_waters <- OR_landed_WA_logs_clipped_to_WA_waters %>% 
  #because the data was used in QGIS, ESRI abbreviates column names. change names back to original form
  #new_name = old_name
  st_set_geometry(NULL) %>% #remove geometry as it is slowing everything down
  rename(
    PotsFished = PtsFshd,
    line_length_m = ln_lng_,
    GRID5KM_ID = GRID5KM,
    NGDC_GRID = NGDC_GR,
    is_port_or_bay = is_pr__,
    Landing_logbook_state = Lndng__,
    month_name = mnth_nm,
    season_month = ssn_mnt,
    month_interval = mnth_nt, 
    season_month_interval = ssn_mn_, 
    is_May_Sep = is_My_S
  ) 

#for now test with 2014 fishtix, so filter to that season
#also interested only in May-Sep period
OR_landed_WA_logs_clipped_to_WA_waters_2013_2014 <- OR_landed_WA_logs_clipped_to_WA_waters %>% 
  filter(season=='2013-2014') %>% 
  filter(is_May_Sep == 'Y')

#each row is a single pot, only need one record per SetID
#length(unique(WA_landed_WA_logs_clipped_to_WA_waters_2013_2014$SetID)) #3792
OR_landed_WA_logs_clipped_to_WA_waters_2013_2014_uniques <- OR_landed_WA_logs_clipped_to_WA_waters_2013_2014 %>% 
  group_by(SetID) %>% 
  filter(row_number()==1)
#nrow(OR_landed_WA_logs_clipped_to_WA_waters_2013_2014_uniques)   #6


#logs_selected_columns 
#try joining Fihsticket columns to more processed logbooks
#getting a duplication of data: "This is because there were two y1=1 values in your d2 dataset 
#so the join will merge these on twice." #nrow(joined_df) #7584 
joined_df_Q999999 <- OR_landed_WA_logs_clipped_to_WA_waters_2013_2014_uniques %>% 
  left_join(logs_selected_columns,by="SetID") %>%
  distinct #add a distinct command to remove duplicaiton
#nrow(joined_df_Q999999) #6 - when add distinct command
# cases where found fishticket
nrow(joined_df_Q999999 %>% filter(!is.na(FishTicket1)))
#6


#read in the actual fishticket file
glimpse(fishtix_2014) 

#join using landing date and Federal ID in logs with VESSEL_NUM in fishtix
library(stringr)
test_df_Q999999 <- joined_df_Q999999 %>% 
  select(SetID, FederalID)
test_df_Q999999_new <-as.data.frame(apply(test_df_Q999999,2, str_remove_all, " ")) 
test_df_Q999999_2 <- joined_df_Q999999 %>% 
  left_join(test_df_Q999999_new,by="SetID") %>% 
  rename(FederalID = FederalID.y)

#landing date needs to be date, not character
glimpse(fishtix_2014_v2)

test_join_Q999999 <- test_df_Q999999_2 %>% 
  left_join(fishtix_2014_v2, 
            by = c("FederalID" = "VESSEL_NUM",
                   "LandingDate" = "LANDING_DATE"))
#nrow(test_join_Q999999) #6 --> it was 6 stringlines, all Q99999 so were they all landed on same fishticket??
#might need to make an assumption that there would be only one fishticket per vessel per day

##WHAT IF STRINGLINES WERE PART IN WA WATERS AND PART IN OR WATERS?? 
#--> only seems to have happened in 2013-2014 season. 35% of pots were on WA side (stringlines continue to OR)
#so could make the assumption that 35% of catch came from that side
