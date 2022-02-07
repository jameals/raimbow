#test joining logbooks and fishtickets

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

#start with WA landed WA logs, clipped to WA waters

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
