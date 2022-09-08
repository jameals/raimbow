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
# path_WA_landed_2011_2013 <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/traps_g_WA_logs_2011_2013.rds"
# WA_landed_logs_2011_2013 <- readRDS(path_WA_landed_2011_2013) %>%
#   dplyr::mutate(lon = sf::st_coordinates(.)[,1],
#                 lat = sf::st_coordinates(.)[,2]) %>% 
#   st_set_geometry(NULL)
# 
# path_WA_landed_2014_2020 <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/traps_g_WA_logs_2014_2020_20220119.rds"
# WA_landed_logs_2014_2020 <- readRDS(path_WA_landed_2014_2020) %>%
#   dplyr::mutate(lon = sf::st_coordinates(.)[,1],
#                 lat = sf::st_coordinates(.)[,2]) %>% 
#   st_set_geometry(NULL)
# 
# #join the dfs
# WA_landed_logs_2011_2020 <- rbind(WA_landed_logs_2011_2013,WA_landed_logs_2014_2020)


#alternatively, ran 2009/10 to 2019/20 seasons in one go
path_WA_landed_2010_2020 <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/traps_g_WA_logs_2010_2020_20220906.rds"
WA_landed_logs_2010_2020 <- readRDS(path_WA_landed_2010_2020) %>%
  #x and y are point locations
  dplyr::mutate(x = sf::st_coordinates(.)[,1],
                y = sf::st_coordinates(.)[,2]) %>% 
  st_set_geometry(NULL) %>% #crs was CA_Curr_Lamb_Azi_Equal_Area
  #remove unnecessary columns
  select(-NGDC_GRID,-AREA,-is_port_or_bay)

#--------------------------------------------


# create columns for season, month etc
WA_landed_logs_2010_2020 <- WA_landed_logs_2010_2020 %>% 
  mutate(
    season = str_sub(SetID,1,9),
    month_name = month(SetDate, label=TRUE, abbr = FALSE)
  )



#each row is a single simulated pot, we want to keep first and last pot per SetID (stringline)
#to denote start and end location of stringlines
WA_landed_logs_2010_2020_start_end_locs_only <- WA_landed_logs_2010_2020 %>% 
  group_by(SetID) %>% 
  slice(c(1, n())) %>%
  ungroup()
# the above df will have the start and end locations for each stringline
#but we also want a df that only has 1 row per stringline (makes some of the below steps easier)
WA_landed_logs_2010_2020_one_loc_only <- WA_landed_logs_2010_2020_start_end_locs_only %>% 
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
joined_df <- WA_landed_logs_2010_2020_one_loc_only %>% 
  left_join(logs_selected_columns,by="SetID") %>%
  distinct #add a distinct command to remove duplication

# cases where fishticket info based on SetID was found
nrow(joined_df %>% filter(!is.na(FishTicket1)))
#2010-2020 data: 190673 when using df where only 1 line per stringline --> 99.24%
#cases where Fishticket info based on SetID was NOT found
nrow(joined_df %>% filter(is.na(FishTicket1)))
#2010-2020 data: 1454 when using df where only 1 line per stringline --> 0.76% of stringlines in WA raw logs didn't have a fishticket number recorded
#this is the same as if look at logbooks pre logbook-pipeline, 0.7% are missing data in Fishticket1 column

#-----------------------------------------------------------------

#read in updated PacFin fishticket data
fishtix_raw <- read_rds(here('wdfw', 'data','pacfin_compiled_2004thru2021.rds')) 
# df is large so subset to years of interest, cut out all california records
fishtix_2010_2020 <- fishtix_raw %>% 
  filter(AGENCY_CODE != 'C') %>% 
  filter(LANDING_YEAR %in% c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)) %>% 
  #we can also choose some columns as there are so many in the pacfin data
  select(FISH_TICKET_ID, LANDING_DATE, VESSEL_NUM, FTID, AGENCY_CODE, PACFIN_SPECIES_CODE, 
         NOMINAL_TO_ACTUAL_PACFIN_SPECIES_CODE, AFI_EXVESSEL_REVENUE,LANDED_WEIGHT_LBS,REMOVAL_TYPE_NAME) %>% 
  #we'll also restrict data here to be only DCRB records
  filter(PACFIN_SPECIES_CODE == "DCRB")
  # %>% filter(REMOVAL_TYPE_NAME=="COMMERCIAL (NON-EFP)")
#you could also consider filtering for commercial catch only here, 
#in which case 98.85% of data will find a match to pacfin fish tickets later on 


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
#just join to Fishticket1 (98.03% of data matches), slighty better match if don't also match by landing date
join_by_FTID_and_Fishticket1 <- joined_df %>% 
  left_join(fishtix_2010_2020, 
            by = c("FishTicket1" = "FTID"))

nrow(join_by_FTID_and_Fishticket1 %>%  filter(!is.na(FISH_TICKET_ID))) / nrow(join_by_FTID_and_Fishticket1) *100
#2010-2020 data: 99.01% of logbook data found a FISHTICKET ID from pacfin data, if only match by FTID & FishTicket1
#note that some of the ticketed data is labelled as 'personal use', 'research' or 'other' 
#personal use and research won't have $ value associated with them
#about 12% of Fish_TICKET_IDs have multiple REMOVAL_TYPE_NAME


#here is a breakdown of the number of string per season that DID NOT find a match from Fishticket data
# summary_no_tix_match <- join_by_FTID_and_Fishticket1 %>%
#   filter(is.na(FISH_TICKET_ID)) %>%
#   group_by(season) %>%
#   summarise(n_strings_no_tix_match = n())
# 
# total_n_strings_per_season <- join_by_FTID_and_Fishticket1 %>%
#  group_by(season) %>%
#  summarise(n_strings = n())
# 
# summary_no_tix_match <- summary_no_tix_match %>% 
#   left_join(total_n_strings_per_season, by = 'season') 
# 
# summary_no_tix_match <- summary_no_tix_match %>% 
#   mutate(percent_strings_no_tix_match = n_strings_no_tix_match/n_strings*100)
# 
# season    n_strings_no_tix_match  n_strings percent_strings_no_tix_match
# 2009-2010                    512     17411                       2.94  
# 2010-2011                    205     19999                       1.03  
# 2011-2012                    105     18220                       0.576 
# 2012-2013                   1068     24918                       4.29  
# 2013-2014                    170     26213                       0.649 
# 2014-2015                    140     25123                       0.557 
# 2015-2016                    149     24880                       0.599 
# 2016-2017                     63     27715                       0.227 
# 2017-2018                     78     26107                       0.299 
# 2018-2019                     35     27025                       0.130 
# 2019-2020                      6     18271                       0.0328




#if there is no pacfin FISHTICKET ID match, then we won't have exvessel revenue data
join_by_FTID_and_Fishticket1_noNA <- join_by_FTID_and_Fishticket1 %>% 
  filter(!is.na(FISH_TICKET_ID))


#-------------------------------------------------------------------------------

#sometimes the same PacFin FISH_TICKET_ID has multiple rows in the original data
#this can be e.g. when catch is from 2 different catch areas, but only recorded as 1 fishticket
#also part of a ticket may have been personal use

#how many PacFin Fisticket IDs have multiple rows?
tix_summary <- fishtix_2010_2020 %>% 
  group_by(FISH_TICKET_ID) %>%
  summarise(n_rows_per_ticket=n())

nrow(tix_summary %>% filter(n_rows_per_ticket > 1)) / nrow(tix_summary) *100
#2010-2020 data: 6.11% // 3.19% if have already removed non-commercial catch


#it would be hard to try to figure which string specifically matches which part (row) of the fishticket
#so just sum up everything for a ticket. but work on non-joined to logbooks - we don't want to sum
#revenue for stringlines that are all linked to the same ticket
#in cases that were personal catch, or spoiled catch, the revenue is listed as 0
duplicated_tickets_fixed <- fishtix_2010_2020 %>% 
  group_by(FISH_TICKET_ID) %>% 
  summarise(total_AFI_EXVESSEL_REVENUE_in_dollars = sum(AFI_EXVESSEL_REVENUE))


#no join that to the df that has logs and strings joined to Fichticket ID
joined_logs_and_tix <- join_by_FTID_and_Fishticket1_noNA %>%
  select(-AFI_EXVESSEL_REVENUE, -LANDED_WEIGHT_LBS) %>% 
  #just keep one record, as there will be a row for each ticket ID, catch area and commercial/personal case
  group_by(SetID) %>% 
  filter(row_number()==1) %>% 
  left_join(duplicated_tickets_fixed, by ="FISH_TICKET_ID")
#now cases of multiple rows per fish ticket are different strings (one per row) that belong to same fish ticket ID

#then we allocate the total $s for a ticket to its stringlines proportionately 
#based on number of pots each sting had
revenue_per_string_per_ticket <- joined_logs_and_tix %>% 
  group_by(FISH_TICKET_ID) %>% 
  mutate(total_pots_per_ticket = sum(PotsFished)) %>% 
  ungroup() %>% 
  mutate(prop_of_pots_in_string = PotsFished / total_pots_per_ticket,
         prop_of_rev_for_string_in_dollars = total_AFI_EXVESSEL_REVENUE * prop_of_pots_in_string)



#-------------------------------------------------------------------------------

#make each string mappable by having it as two points
#one denoting start, the other end

reduced_columns <- revenue_per_string_per_ticket %>% 
  select(SetID, FishTicket1:prop_of_rev_for_string)
  
#use the df we had earlier, where we want kept first and last pot per SetID (stringline)
#to denote start and end location of stringlines
revenue_per_string_for_mapping <-  WA_landed_logs_2010_2020_start_end_locs_only %>% 
  #into that join new features
  left_join(reduced_columns, by = "SetID") %>% 
  # joining to df 'WA_landed_logs_2010_2020_start_end_locs_only'
  #creates cases of no FISH_TICKET_ID --> remove them
  filter(!is.na(FISH_TICKET_ID))

##about 6.6% of data are personal use, research or 'other'
#might want to filter out personal catch cases here
#If filtered personal catch out earlier, there are 2 fish tickets that are commercial catch
#but still had $0 recorded. One was recorded as 'discard', other as 'unspecified'


#some other strange cases:
#View(fishtix_raw %>% filter(FISH_TICKET_ID==533568475)) 
#--> lots of logbook strings linked to ticket, but ticket says only 20lbs landed
#looking at 'raw' logbook, each string had anywhere between 3 and 32 crabs on it

