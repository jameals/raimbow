## data for Blake's wind energy project

#-----------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(sf)
library(rgeos)
library(viridis)
library(ggpubr)

#-----------------------------------------------------------------------------------

#read in WA logbook data -- note that Q99999 fish tickets have been removed
#i.e. data includes logbooks landed in WA, regardless if fished in WA or OR waters
#data not summarised to grid level yet - retains individual stringline IDs
#original data was for Riekkola et al paper, covering 2013/14 to 2019/20 season
#additional data processed for 2010/11 to 2012/13 seasons

# read in both
path_WA_landed_2011_2013 <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/traps_g_WA_logs_2011_2013.rds"
WA_landed_logs_2011_2013 <- readRDS(path_WA_landed_2011_2013) %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>% 
  st_set_geometry(NULL)

path_WA_landed_2014_2020 <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/traps_g_WA_logs_2014_2020_20220119.rds"
WA_landed_logs_2014_2020 <- readRDS(path_WA_landed_2014_2020) %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>% 
  st_set_geometry(NULL)

#join the dfs
WA_landed_logs_2011_2020 <- rbind(WA_landed_logs_2011_2013,WA_landed_logs_2014_2020)



# create columns for season, month etc
WA_landed_logs_2011_2020 <- WA_landed_logs_2011_2020 %>% 
  mutate(
    season = str_sub(SetID,1,9),
    month_name = month(SetDate, label=TRUE, abbr = FALSE)
  )



#each row is a single simulated pot, we want to keep first and last pot per SetID (stringline)
#to denote start and end location of stringlines
WA_landed_logs_2011_2020_start_end_locs_only <- WA_landed_logs_2011_2020 %>% 
  group_by(SetID) %>% 
  slice(c(1, n())) %>%
  ungroup()
# the above df will have the start and end locations for each stringline
#but we also want a df that only has 1 row per stringline (makes some of the below steps easier)
WA_landed_logs_2011_2020_one_loc_only <- WA_landed_logs_2011_2020_start_end_locs_only %>% 
  group_by(SetID) %>% 
  filter(row_number()==1) %>%
  ungroup()

#Fishticket and landing date columns have been dropped during pipeline, bring them back from an earlier version of raw data  
logs <- read_csv(here('wdfw', 'data','WDFW-Dcrab-logbooks-compiled_stackcoords_2009-2020.csv'),col_types = 'ccdcdccTcccccdTddddddddddddddddiddccddddcddc')
#SetID is the same between the two files
logs_selected_columns <- logs %>% 
  select(SetID, FishTicket1, FishTicket2, FishTicket3, FishTicket4, Vessel, License, FederalID, LandingDate ) 



#join Fihsticket, landing date etc columns back to the more processed logbooks
#getting a duplication of data: "This is because there were two y1=1 values in the d2 dataset so the join will merge these on twice." 
joined_df <- WA_landed_logs_2011_2020_one_loc_only %>% 
  left_join(logs_selected_columns,by="SetID") %>%
  distinct #add a distinct command to remove duplication

# cases where fishticket info based on SetID was found
nrow(joined_df %>% filter(!is.na(FishTicket1)))
#177911 when using df where only 1 line per stringline --> 99.21%
#cases where Fishticket info based on SetID was NOT found
nrow(joined_df %>% filter(is.na(FishTicket1)))
#1412 when using df where only 1 line per stringline --> 0.79% of stringlines in WA logs don't have a fishticket number recorded

#  155 stringlines across 2010/11 to 2019/20 seasons (0.09%) don't have a landing date


#-----------------------------------------------------------------

#read in updated PacFin fishticket data
fishtix_raw <- read_rds(here('wdfw', 'data','pacfin_compiled_2004thru2021.rds')) 
# df is large so subset to years of interest, cut out all california records
fishtix_2010_2020 <- fishtix_raw %>% 
  filter(AGENCY_CODE != 'C') %>% 
  filter(LANDING_YEAR %in% c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)) %>% 
  #we can also choose some columns as there are so many in the pacfin data
  select(FISH_TICKET_ID, LANDING_DATE, VESSEL_NUM, FTID, AGENCY_CODE, PACFIN_SPECIES_CODE, 
         NOMINAL_TO_ACTUAL_PACFIN_SPECIES_CODE, AFI_EXVESSEL_REVENUE,LANDED_WEIGHT_LBS) %>% 
  #we'll also restrict data here to be only DCRB records
  filter(PACFIN_SPECIES_CODE == "DCRB")


#-------------
# We could join files using landing date, and FederalID in logs with VESSEL_NUM in fishtix
# but the other way of joining seems to be a little better
# so skip this section

# library(stringr)
# test_df <- joined_df %>% 
#   select(SetID, FederalID)
# #FederalID column in logs has a space between the first 3 and last 3 digits, while pacfin ticket don't have this gap
# test_df_new <-as.data.frame(apply(test_df,2, str_remove_all, " ")) 
# test_df_2 <- joined_df %>% 
#   left_join(test_df_new,by="SetID") %>% 
#   rename(FederalID = FederalID.y)
# #length(unique(joined_df$SetID)) #179323
# 
# 
# test_join <- test_df_2 %>% 
#   left_join(fishtix_2010_2020, 
#             by = c("FederalID" = "VESSEL_NUM",
#                    "LandingDate" = "LANDING_DATE"))
# #93.1% of data found a FISHTICKET ID from pacfin data
# #some but not all SetIDs occur multiple times after joining with pacfin data - unsure why... 
# #sometimes this seems to have happened to those stringlines that had data in both Fishticket1 and Fishticket2 columns
# # sometimes the fishticket info has been split, e.g. FISH_TICKET_ID==47988149 has two rows, with different landing weights 
# #because there are 2 CFT_ID vlaues
# #nrow(test_join) #242131 (or 241584 if earlier filter to DCRB code only)
# #but the number of unique SetIDs is still the same as before (179323)
# 
# 
# #each row is one stringline, but multiple stringlines may have been on one fishticket
# #keep only one record per fishticket
# test_join_uniques <- test_join %>% 
#   group_by(FISH_TICKET_ID) %>% 
#   filter(row_number()==1)
# 
# length(unique(joined_df$FishTicket1)) #28617 unique Fishticket1 values in WA logs
# length(unique(test_join_uniques$FishTicket1)) #25823 unique Fishticket1 values after joining with pacfin
# #--> so 90.24% of WA Fishticket1 numbers also found a fishticket info from pacfin?
# #this may vary between years

#-------------

# use this for matching logs and fishtix

#FTID in pacfin looks to match Fishticket1 (and Fishticket2/Fishticket3, if that column also exists)...
#could be harder to join that way as info in more than 1 column (Fishticket1, Fishticket2...)
test_join_by_FTID_and_Fishticket1 <- joined_df %>% 
  left_join(fishtix_2010_2020, 
            by = c("FishTicket1" = "FTID",
                   "LandingDate" = "LANDING_DATE"))
#98.2% of logbook data found a FISHTICKET ID from pacfin data

#if there is no pacfin FISHTICKET ID, then there won't be any exvessel renevue data
test_join_by_FTID_and_Fishticket1_noNA <- test_join_by_FTID_and_Fishticket1 %>% 
  filter(!is.na(FISH_TICKET_ID))


#-------------------------------------------------------------------------------

#sometimes the same PacFin FISH_TICKET_ID has multiple rows in the original data
#this can be e.g. when catch is from 2 different catch areas, but only recorded as 1 fishticket

#how many PacFin Fisticket IDs have multiple rows?
tix_summary <- fishtix_2010_2020 %>% 
  group_by(FISH_TICKET_ID) %>%
  summarise(n_rows_per_ticket=n())

nrow(tix_summary %>% filter(n_rows_per_ticket > 1)) / nrow(tix_summary) *100
#28.5% of data has more than 1 row per fishticket record


#it would be hard to try to figure which string specifically matches which part (row) of the fishticket
#so just sum up everything for a ticket
test_join_by_FTID_and_Fishticket1_noNA_dup_tickets_fixed <- test_join_by_FTID_and_Fishticket1_noNA %>% 
  group_by(FISH_TICKET_ID) %>% 
  summarise(total_AFI_EXVESSEL_REVENUE = sum(AFI_EXVESSEL_REVENUE))

joined_logs_and_tix <- test_join_by_FTID_and_Fishticket1_noNA %>%
  select(-AFI_EXVESSEL_REVENUE, -LANDED_WEIGHT_LBS) %>% 
  #just keep one record
  group_by(SetID) %>% 
  filter(row_number()==1) %>% 
  left_join(test_join_by_FTID_and_Fishticket1_noNA_dup_tickets_fixed, by ="FISH_TICKET_ID")

#then we allocate the total $s for a ticket to its stringlines proportionately 
#based on number of pots each sting had

revenue_per_string_per_ticket <- joined_logs_and_tix %>% 
  group_by(FISH_TICKET_ID) %>% 
  mutate(total_pots_per_ticket = sum(PotsFished)) %>% 
  ungroup() %>% 
  mutate(prop_of_pots_in_string = PotsFished / total_pots_per_ticket,
         prop_of_rev_for_string = total_AFI_EXVESSEL_REVENUE * prop_of_pots_in_string)



#-------------------------------------------------------------------------------

#make each string mappable by having it as two points
#one denoting start, the other end

reduced_columns <- revenue_per_string_per_ticket %>% 
  select(SetID, FishTicket1:prop_of_rev_for_string)
  
#use the df we had earlier, where we want kept first and last pot per SetID (stringline)
#to denote start and end location of stringlines
revenue_per_string_per_pot <-  WA_landed_logs_2011_2020_start_end_locs_only %>% 
  #into that join new features
  left_join(reduced_columns, by = "SetID")







