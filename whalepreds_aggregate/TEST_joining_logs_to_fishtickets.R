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
#nrow(test_join) #26306 #but the number of unique SetIDs is still the same as before (20144)

#each row is one stringline, but multiple stringlines may have been on one fishticket
#keep only one record per fishticket
test_join_uniques <- test_join %>% 
  group_by(FISH_TICKET_ID) %>% 
  filter(row_number()==1)


length(unique(joined_df$FishTicket1)) #4392 unique Fishticket1 values in WA logs
length(unique(test_join_uniques$FishTicket1)) #4352 unique Fishticket1 values after joining with pacfin
#--> so 99% of WA Fishticket1 numbers also found a fishticket info from pacfin?
# need to figure out the best way of measuring how much data was 'lost' (doesn't have fishticket landing info),
#how much of logbook data didn't find matching pacfin fishticket info


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



#FTID in pacfin looks to match Fishticket1 (and Fishticket2 if that also exists)...
#could be harder to join that way as info in 2 columns (Fishticket1 and Fishticket2)
test_join_by_FTID_and_Fishticket1 <- test_df_2 %>% 
  left_join(fishtix_2014_2020, 
            by = c("FishTicket1" = "FTID",
                   "LandingDate" = "LANDING_DATE"))
#more NAs this way than by joining with landing date and vessel ID, 
#possibly because some data is in Fishticket1 column and some in Fishticket2 column
#perhaps slightly better success joining if don't join by landing date as well...






#plotting

#columns of interest to summarise LANDED_WEIGHT_LBS and EXVESSEL_REVENUE or AFI_EXVESSEL_REVENUE??

# do separate comparisons for Jul-Sep 2019 vs pre-regs, and May-Sep 2020 vs pre-regs


#Jul-Sep
summary_pacfin_data_JulSep <- test_join_uniques %>% 
  filter(month_name %in% c('July','August','September')) %>%   
  filter(season != '2019-2020') %>% 
  group_by(season) %>% 
  summarise(sum_revenue = sum(EXVESSEL_REVENUE, na.rm=T),
            sum_AFI_exvessel_revenue = sum(AFI_EXVESSEL_REVENUE, na.rm=T),
            sum_weight_lbs = sum(LANDED_WEIGHT_LBS, na.rm=T),
            avg_price_per_pound = mean(PRICE_PER_POUND)
  )


sum_JulSep_rev_ts <- ggplot(summary_pacfin_data_JulSep)+
  geom_line(aes(x=season, y=sum_revenue, group=1),size=1, lineend = "round") + 
  geom_point(aes(x=season, y=sum_revenue, group=1),size=2.5) + 
  #geom_line(aes(x=season, y=avg_price_per_pound, group=1),size=1, lineend = "round", colour='pink') + 
  ylab("Revenue $ (sum Jul-Sep)") +
  xlab("Season") + 
  #geom_hline(yintercept=1241764, linetype="dashed", 
  # color = "red", size=2)+ #average across 5 pre-reg seasons Jul-Sep
  #geom_hline(yintercept=673505.2, linetype="dashed", 
  # color = "blue", size=2)+ # average across 4 pre-reg seasons (excluding 2014-2014)
  theme_bw()+
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        strip.text = element_text(size=20),
        strip.background = element_blank(),
        strip.placement = "left"
  )
sum_JulSep_rev_ts




sum_JulSep_landings_ts <- ggplot(summary_pacfin_data_JulSep, aes(x=season, y=sum_weight_lbs, group=1))+
  geom_line(size=1, lineend = "round") + 
  geom_point(size=2.5) + 
  ylab("Landings lbs (sum Jul-Sep)") +
  xlab("Season") + 
  theme_bw()+
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        strip.text = element_text(size=20),
        strip.background = element_blank(),
        strip.placement = "left"
  )
sum_JulSep_landings_ts


path_figures <- "C:/Users/Leena.Riekkola/Projects/raimbow/whalepreds_aggregate/figures"

png(paste0(path_figures, "/ts_sum_revenue_landings_JulSep_2019_vs_pre_reg.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(sum_JulSep_rev_ts,
          sum_JulSep_landings_ts,
          ncol=1,
          nrow=2,
          #legend="top",
          #labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())





#May-Sep
summary_pacfin_data_MaySep <- test_join_uniques %>% 
  filter(season != '2018-2019') %>% 
  group_by(season) %>% 
  summarise(sum_revenue = sum(EXVESSEL_REVENUE, na.rm=T),
            sum_AFI_exvessel_revenue = sum(AFI_EXVESSEL_REVENUE, na.rm=T),
            sum_weight_lbs = sum(LANDED_WEIGHT_LBS, na.rm=T),
            avg_price_per_pound = mean(PRICE_PER_POUND, na.rm=T)
  )


sum_MaySep_rev_ts <- ggplot(summary_pacfin_data_MaySep)+
  geom_line(aes(x=season, y=sum_revenue, group=1),size=1, lineend = "round") + 
  geom_point(aes(x=season, y=sum_revenue, group=1),size=2.5) + 
  #geom_line(aes(x=season, y=avg_price_per_pound, group=1),size=1, lineend = "round", colour='pink') + 
  ylab("Revenue $ (sum May-Sep)") +
  xlab("Season") + 
  #geom_hline(yintercept=1988695, linetype="dashed", 
            # color = "red", size=2)+ #average across 5 pre-reg seasons
  #geom_hline(yintercept=1451688, linetype="dashed", 
            # color = "blue", size=2)+ # average across 4 pre-reg seasons (excluding 2014-2014)
  theme_bw()+
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        strip.text = element_text(size=20),
        strip.background = element_blank(),
        strip.placement = "left"
  )
sum_MaySep_rev_ts
#28% drop in revenue in 2020 from 2018, drop in area size was 33% from 2018, also almost 7% drop in average $/lbs (see below)

sum_MaySep_landings_ts <- ggplot(summary_pacfin_data_MaySep, aes(x=season, y=sum_weight_lbs, group=1))+
  geom_line(size=1, lineend = "round") + 
  geom_point(size=2.5) + 
  ylab("Landings lbs (sum May-Sep)") +
  xlab("Season") + 
  theme_bw()+
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        strip.text = element_text(size=20),
        strip.background = element_blank(),
        strip.placement = "left"
  )
sum_MaySep_landings_ts
#18% drop in landed pounds in 2020 from 2018


path_figures <- "C:/Users/Leena.Riekkola/Projects/raimbow/whalepreds_aggregate/figures"

png(paste0(path_figures, "/ts_sum_revenue_landings_MaySep_2020_vs_pre_reg.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(sum_MaySep_rev_ts,
          sum_MaySep_landings_ts,
          ncol=1,
          nrow=2,
          #legend="top",
          #labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())






MaySep_average_price_ts <- ggplot(summary_pacfin_data_MaySep, aes(x=season, y=avg_price_per_pound, group=1))+
  geom_line(size=1, lineend = "round") + 
  geom_point(size=2.5) + 
  ylab("Average $/lbs (May-Sep)") +
  xlab("Season") + 
  theme_bw()+
  theme(legend.title = element_blank(),
        #title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.position = c(.15, .85),
        axis.text.x = element_text(hjust = 1,size = 20, angle = 60),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        strip.text = element_text(size=20),
        strip.background = element_blank(),
        strip.placement = "left"
  )
MaySep_average_price_ts
#-6.9% drop in average $/lbs from 2018 to 2020


path_figures <- "C:/Users/Leena.Riekkola/Projects/raimbow/whalepreds_aggregate/figures"

png(paste0(path_figures, "/ts_avg_price_per_pound_MaySep_2020_vs_pre_reg.png"), width = 14, height = 10, units = "in", res = 300)
ggarrange(MaySep_average_price_ts,
          ncol=1,
          nrow=1,
          #legend="top",
          #labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())












#---------------------------------------------------------------------------------

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
