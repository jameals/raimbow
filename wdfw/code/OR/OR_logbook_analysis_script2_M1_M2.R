## This script has been modified from the mapping functions for WDFW logbook data to fit OR data:
# creating df with both adjustment methods (M1 and M2) for double counting of traps

library(tidyverse)
library(lubridate)
library(here)
library(sf)
library(raster)
library(fasterize)
select <- dplyr::select
library(rnaturalearth)
library(viridis)
library(magrittr)
library(gridExtra)
library(nngeo)
library(fuzzyjoin)


#-------------------------------------------------------------

# Start with traps_g df (traps are simulated and joined to grid, script 1)  
# RDS can be found in Kiteworks folder
traps_g_raw <- read_rds(here::here('wdfw', 'data', 'OR', 'OR_traps_g_all_logs_2013_2018.rds'))


# create columns for season, month etc
traps_g <- traps_g_raw %>% 
  st_set_geometry(NULL) %>% 
  mutate(
    season = str_sub(SetID,1,9),
    month_name = month(SetDate, label=TRUE, abbr = FALSE),
    season_month = paste0(season,"_",month_name),
    month_interval = paste0(month_name, 
                            "_", 
                            ifelse(day(SetDate)<=15,1,2)
    ),
    season_month_interval = paste0(season, 
                                   "_", 
                                   month_interval)
  )


###BIT OF A MESS, TRYING TO GET POT LIMIT FOR OR#####

#OR permits: 
#in folder from ODFW had OregonCrabPermitData2007-2019.xlsx, saved as csv 
# Read in and join license & pot limit info
OR_pot_limit_info_raw <- read_csv(here::here('wdfw', 'data', 'OR', 'OregonCrabPermitData2007-2019.csv'))

OR_pot_limit_info <- OR_pot_limit_info_raw %>% 
  rename(Vessel = Docnum,
         PermitNumber = Number)

OR_pot_limit_info %<>%
  mutate(Begindate=as.Date(Begindate,"%m/%d/%Y"),
         Enddate=as.Date(Enddate,"%m/%d/%Y"))


OR_pot_limit_info %>% distinct(Potlimit) # 500, 300, 200
#OR permits - a permit can change from vessel to vessel sometimes 
#but does the pot limit for a given permit number stay the same?
test <- OR_pot_limit_info %>%                              
  group_by(PermitNumber) %>%
  summarise(count = n_distinct(Potlimit))
# Yes, except for 2 instances: Permit Numbers 96125 and 96262 have 2 unique pot limit values
cases <- OR_pot_limit_info %>% 
  filter(PermitNumber == 96125 | PermitNumber == 96262)
#96125: for 12 years pot limit is 300, but in 2014 it is 500 - assume mistake for now
#96262: for 12 years pot limit is 300, but in 2008 it is 200 - assume mistake for now, also possibly outside yers of interest anyway
OR_pot_limit_info %<>%
  mutate(Potlimit = ifelse(PermitNumber == 96125 | PermitNumber == 96262, 300, Potlimit))
    
OR_pot_limit_info_v2 <- OR_pot_limit_info %>% 
  filter(Year >= 2013) %>% 
  select(PermitNumber, Vessel, Begindate, Enddate, Potlimit)

#vector size too large, might need to try running in subsets
traps_g_joined <- fuzzy_left_join(
  traps_g_20132014, OR_pot_limit_info_v2,
  by = c(
    "Vessel" = "Vessel",
    "SetDate" = "Begindate",
    "SetDate" = "Enddate"
  ),
  match_fun = list(`==`, `>=`, `<=`)
) #%>%
  #select(column_name, category = category.x, column_name, column_name)

subset_test <-  traps_g %>% 
  sample_n(10000)
traps_g_joined_subset <- fuzzy_left_join(
  subset_test, OR_pot_limit_info_v2,
  by = c(
    "Vessel" = "Vessel",
    "SetDate" = "Begindate",
    "SetDate" = "Enddate"
  ),
  match_fun = list(`==`, `>=`, `<=`)
)
#write_csv(traps_g_joined_subset,here::here('wdfw', 'data','OR', "traps_g_joined_subset.csv"))





#Majority of vessels only ever have one pot limit
#but some cases that a vessels has had different pot limit values, even in the same year - different permits?
test2 <- OR_pot_limit_info %>%                              
  group_by(Docnum, Number) %>%
  summarise(count = n_distinct(Potlimit))
test3 <- OR_pot_limit_info %>%                              
  group_by(Docnum, Year) %>%
  summarise(count = n_distinct(Potlimit))
#96(??) instances where single vessel ID has more than 1 pot limits in a single year
#Do these changes happen in the middle of season? or between seasons?
multi_pot_counts <- test3 %>% 
  filter(count == 2)
vessels_multi_pot_counts <- unique(multi_pot_counts$Docnum)
subset_vessels_multi_pot_counts <- OR_pot_limit_info %>% 
  dplyr::filter(Docnum %in% vessels_multi_pot_counts)
subset_vessels_multi_pot_counts_2013onwards <- subset_vessels_multi_pot_counts %>% 
  dplyr::filter(Year >= 2013)
test4 <- subset_vessels_multi_pot_counts_2013onwards %>%                              
  group_by(Docnum, Year) %>%
  summarise(count = n_distinct(Potlimit))
#If only focus on 2013 onwards
OR_pot_limit_info_2013onwards <- OR_pot_limit_info %>% 
  dplyr::filter(Year >= 2013)
test5 <- OR_pot_limit_info_2013onwards %>%                              
  group_by(Docnum, Year) %>%
  summarise(count = n_distinct(Potlimit))
multi_pot_counts_2013onwards <- test5 %>% 
  filter(count == 2)
#35 vessels left
vessels_multi_pot_counts_2013onwards <- unique(multi_pot_counts_2013onwards$Docnum)
subset_vessels_multi_pot_counts_2013onwards <- OR_pot_limit_info_2013onwards %>% 
  dplyr::filter(Docnum %in% vessels_multi_pot_counts_2013onwards)


#what if we tried to link Docnum/VesselID with Number/PermitNumber in an earlier step
#can they be easily joined?
#How many 'Docnum' (=VesselID) have different 'Number' (=Permit Number)?
testxx <- OR_pot_limit_info %>%                              
  group_by(Vessel) %>%
  summarise(count = n_distinct(PermitNumber))
#136
#if looking at post 2013
testyy <- OR_pot_limit_info %>%  
  filter(Year >= 2013) %>% 
  group_by(Vessel) %>%
  summarise(count = n_distinct(PermitNumber))
#56

#So these vessels only ever have one permit number, so the permit number can be joined to vessel ID
Docnum_with_only_1_permit_number_2013onwards <- testyy %>% 
  filter(count == 1) 
List_Docnum_with_only_1_permit_number_2013onwards <- unique(Docnum_with_only_1_permit_number_2013onwards$Vessel)

OR_pot_limit_info_2013onwards <- OR_pot_limit_info %>% 
  dplyr::filter(Year >= 2013)

traps_g_Docnum_with_only_1_permit_number_2013onwards <- traps_g %>% 
  filter(Vessel %in% List_Docnum_with_only_1_permit_number_2013onwards)
traps_g_Docnum_with_only_1_permit_number_2013onwards %<>%
  left_join(OR_pot_limit_info_2013onwards, by = c("Vessel")) #%>% 
  #drop_na(Pot_Limit)


#these have more than one permit number, so the permit number CANNOT be easily joined to vessel ID
Docnum_with_more_than_1_permit_number_2013onwards <- testyy %>% 
  filter(count > 1)
List_Docnum_with_more_than_1_permit_number_2013onwards <- unique(Docnum_with_more_than_1_permit_number_2013onwards$Vessel)

subset_Docnum_with_more_than_1_permit_number_2013onwards <- OR_pot_limit_info_2013onwards %>% 
  dplyr::filter(Vessel %in% List_Docnum_with_more_than_1_permit_number_2013onwards)





###BACK TO ORIGINAL WA CODE, NOT YET EDITED FOR OR####

WA_pot_limit_info %<>%
  rename(License = License_ID)

# join Pot_Limit to traps_g 
traps_g %<>%
  left_join(WA_pot_limit_info,by=c("License")) %>% 
  drop_na(Pot_Limit) #2 NAs for cases with no license info unless correct it with drop_na(Pot_Limit)


# apply 2019 summer pot limit reduction, which took effect July 1, 2019 
# and was in effect through the end of the season (Sept. 15, 2019)
## make a new column for summer pot limit reduction
traps_g %<>% 
  mutate(Pot_Limit_SummerReduction = Pot_Limit)
## split df to pre and post reduction periods
df1 <- traps_g %>%
  filter(!season_month %in% c('2018-2019_July', '2018-2019_August', '2018-2019_September'))
df2 <- traps_g %>%
  filter(season_month %in% c('2018-2019_July', '2018-2019_August', '2018-2019_September'))
## adjust pot limit post 1 July 2019
df2 %<>% 
  mutate(Pot_Limit_SummerReduction = ifelse(Pot_Limit_SummerReduction==500, 330, 200))
## join dfs back together  
traps_g <- rbind(df1,df2)


# apply weighting based on permitted max pot number (this is Method 2 or M2)
adj_traps_g <- traps_g %>% 
  filter(!is.na(GRID5KM_ID)) %>% 
  # count up traps for vessel in 2-week period
  group_by(season_month_interval, Vessel, License, Pot_Limit, Pot_Limit_SummerReduction) %>%  
  summarise(
    M2_n_traps_vessel=n(), na.rm=TRUE 
  ) %>% 
  # create a column with weighting - proportion of max allowed traps
  # divide pot limit by number of simulated traps
  # because you want to up-weight traps < pot_limit, and downweight traps > pot_limit
  mutate(trap_limit_weight = Pot_Limit_SummerReduction/M2_n_traps_vessel) %>% 
  ungroup()

# join the "weighting key" back to the simulated pots data
traps_g %<>%
  left_join(adj_traps_g,by=c('season_month_interval','Vessel','License','Pot_Limit','Pot_Limit_SummerReduction'))

# do Method 1/M1 calculations/adjustment, group by season_month_interval 
M1_summtraps <- traps_g %>% 
  filter(!is.na(GRID5KM_ID)) %>% 
  # count the total number of traps in each grid cell in each set
  group_by(season_month_interval, Vessel, License, GRID5KM_ID,grd_x,grd_y,SetID,AREA) %>%  
  summarise(
    M1_ntraps_vessel_set_cell=n()
  ) %>% 
  # average the number of pots per vessel per grid cell
  ungroup() %>% 
  group_by(season_month_interval, Vessel, License, GRID5KM_ID,grd_x,grd_y,AREA) %>% 
  summarise(
    M1_ntraps_vessel_cell=mean(M1_ntraps_vessel_set_cell)#,
    #sd_traps_vessel_cell=sd(ntraps_vessel_set_cell) # want to come back and think about how to aggregate uncertainty
  ) %>% 
  # finally, sum the total traps per grid cell, across vessels
  ungroup() %>% 
  group_by(season_month_interval, GRID5KM_ID,grd_x,grd_y,AREA) %>% 
  summarise(
    M1_tottraps=sum(M1_ntraps_vessel_cell),
  # add count of unique vessels for confidentiality check, this could be done here or in mapping phase
    nvessels=n_distinct(Vessel,na.rm=T) 
  ) %>% 
  # trap density (in sq. km) is total traps divided by area (which is in sq. m) of each cell
  mutate(
    M1_trapdens=M1_tottraps/(AREA/1e6)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(M1_tottraps))
glimpse(M1_summtraps)


# Now sum pots for M2 (just like for M1), use 'trap_limit_weight' to sum, instead of n().
traps_summ <- traps_g %>% 
  group_by(season_month_interval,GRID5KM_ID,NGDC_GRID,grd_x,grd_y, AREA) %>%  
  # this is the new/key step -- weighted_traps is the M2 version of 'tottraps' column of M1 method
  summarise(M2_tottraps=sum(trap_limit_weight)) %>%  
  mutate(
    M2_trapdens=M2_tottraps/(AREA/1e6)
  ) %>% ungroup() %>% 
  filter(!is.na(M2_tottraps))
glimpse(traps_summ) 


# join results from M1 and M2 
adj_summtraps <- left_join(M1_summtraps,traps_summ, by=c("season_month_interval", "GRID5KM_ID", "grd_x", "grd_y", "AREA"))
glimpse(adj_summtraps) 


# fivekm_grid_polys_shore_lamb.shp shapefile: 5km grid cells that fall partially within any of the bays or estuaries 
# will have a separate polygon within said bay or estuary that will have the same Grid5km_ID value as the adjacent 
# portion of the 5km grid cell that does not fall within the bay or estuary. 
# Therefore, if you want to calculate total area of each grid cell that falls in water, 
# sum the total area for that grid cell by its Grid5km_ID value.

# joining data for portions of grids with same grid ID
# Somehow this chunk of code 'breaks' trying to scale from 0-1 for making difference maps
adj_summtraps %<>%
  group_by(season_month_interval,GRID5KM_ID, grd_x,grd_y) %>% #remove NGDC_GRID as a grouping factor
  summarise(
    AREA = sum(AREA),
    M1_tottraps = sum(M1_tottraps),
    nvessels = sum(nvessels),
    M1_trapdens = M1_tottraps/(AREA/1e6),
    M2_tottraps = sum(M2_tottraps),
    M2_trapdens = M2_tottraps/(AREA/1e6)
  )
glimpse(adj_summtraps)


adj_summtraps %<>%
  separate(season_month_interval, into = c("season", "month_name", "period"), sep = "_") %>%
  mutate(season_month = paste0(season,"_",month_name)) %>%
  mutate(month_name = factor(month_name, levels = c('December','January','February','March','April','May','June','July','August','September','October','November'))) %>% 
  filter(!is.na(month_name)) %>% 
  mutate(season_month_interval = paste0(season_month,"_",period)) %>% 
  mutate(month_interval = paste0(month_name,"_",period)) %>%
  mutate(month_interval = factor(month_interval, levels = c('December_1','December_2','January_1','January_2','February_1','February_2','March_1','March_2','April_1', 'April_2','May_1','May_2','June_1','June_2','July_1','July_2','August_1','August_2','September_1','September_2','October_1','October_2','November_1','November_2')))
glimpse(adj_summtraps)

#write_rds(adj_summtraps,here::here('wdfw','data',"adj_summtraps.rds"))
#write_rds(adj_summtraps,here::here('wdfw','data',"adj_summtraps_2.rds")) #make a different version where don't run
#the code on lines 129-139, i.e. don't join the grid IDs that are in few pieces



#----------------------------------------------------------------------------

# Few visuals comparing the M1 and M2 methods
pairs(~ M1_trapdens + M2_trapdens, data = adj_summtraps)
pairs(~ M1_tottraps + M2_tottraps, data = adj_summtraps)

library(ggplot2)                    
library(GGally)

ggpairs(adj_summtraps[, c(10, 12)])
ggpairs(adj_summtraps, columns = c(10, 12), ggplot2::aes(colour=season))

ggpairs(adj_summtraps[, c(8, 11)])
ggpairs(adj_summtraps, columns = c(8, 11), ggplot2::aes(colour=season))


#-----------------------------------------------------------------------------------------

# difference maps
# the following mapping code can be used to make maps to see whether the relative spatial 
# distribution of effort varies between M1 and M2 -- no major difference observed


# Note that fixing the repeating grid IDs on lines 146-156 seems to break the scaling function below
# Leave this for now, and come back to try to fix it later


dat <- read_rds(here::here('wdfw','data','adj_summtraps.rds')) #this file saved in this script line 152
dat_v2 <- read_rds(here::here('wdfw','data','adj_summtraps_v2.rds')) #this file saved in this script line 153

# scaling M1 and M2 densities to range between 0-1
#This one is not fine
dat_scale01 <- dat %>%
  mutate(M1_trapdens_scaled = scales::rescale(M1_trapdens, to=c(0,1)),
         M2_trapdens_scaled = scales::rescale(M2_trapdens, to=c(0,1)),
         #calculate the difference as M2 minus M1
         scaled_M2_minus_M1 = M2_trapdens_scaled - M1_trapdens_scaled
  )

#This one seems fine
dat_scale01_v2 <- dat_v2 %>%
  mutate(M1_trapdens_scaled = scales::rescale(M1_trapdens, to=c(0,1)),
         M2_trapdens_scaled = scales::rescale(M2_trapdens, to=c(0,1)),
         #calculate the difference as M2 minus M1
         scaled_M2_minus_M1 = M2_trapdens_scaled - M1_trapdens_scaled
  )

#both centered around 0 but very different spread
dat_scale01 %>%
  ggplot()+
  geom_density(aes(scaled_M2_minus_M1))

dat_scale01_v2 %>%
  ggplot()+
  geom_density(aes(scaled_M2_minus_M1))


#to make the maps will also need to load the various grid and shapefiles, see scipt 6
# 
# map_traps <- function(gridded_traps,saveplot=TRUE){
#   
#   # labels for plot titles
#   month_label=unique(gridded_traps$month_name)
#   period_label=unique(gridded_traps$period)
#   season_label=paste("Season:",unique(gridded_traps$season))
#   t1 <- paste0(season_label,"\n",month_label,", ",period_label, " M2 - M1")
#   
#   bbox = c(800000,1650000,1013103,1970000)
#   
#   diff_map_out <- gridded_traps %>% 
#     ggplot()+
#     geom_tile(aes(grd_x,grd_y,fill=scaled_M2_minus_M1),na.rm=T,alpha=0.8)+
#     geom_sf(data=coaststates,col=NA,fill='gray50')+
#     geom_sf(data=MA_shp,col="black", size=0.5, fill=NA)+
#     geom_sf(data=QSMA_shp,col="black", linetype = "11", size=0.5, fill=NA)+
#     scale_fill_viridis(na.value='grey70',option="A",limits=c(-1,1),oob=squish)+
#     coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]),datum=NA)+
#     labs(x='',y='',fill='M2-M1 variance',title=t1)
# 
#   # saving
#   if(saveplot){
#     pt <- unique(gridded_traps$season_month_interval)
#     ggsave(here('wdfw','maps',paste0(pt,'.png')),diff_map_out,w=6,h=5)
#   }
#   return(diff_map_out)
# }
# 
# # Loop and save comparison maps
# tm <- proc.time()
# all_maps <- purrr::map(unique(dat_scale01$season_month_interval),function(x){
#   dat_scale01 %>% 
#     filter(season_month_interval==x) %>% 
#     map_traps()
# })
# proc.time()-tm



#----------------------------


# difference maps of M2-M1 methods for May-Sep period
# This code is only relevant if want to map the difference between M1 and M2 methods -- no major difference observed

# # First average M1 and M2 trap densities for each grid cell, then scale M1 and M2 densities 0-1
# MaySep_summtrapsWA_scale01 <- MaySep_summtrapsWA %>% 
#   mutate(M1_mean_trapdens_scaled = scales::rescale(mean_M1_trapdens, to=c(0,1)),
#          M2_mean_trapdens_scaled = scales::rescale(mean_M2_trapdens, to=c(0,1)),
#          #calculate the difference as M2 minus M1
#          scaled_M2_minus_M1 = M2_mean_trapdens_scaled - M1_mean_trapdens_scaled
#   )
# glimpse(MaySep_summtrapsWA_scale01)
# 
# 
# MaySep_summtrapsWA_scale01 %>% 
#   ggplot()+
#   geom_density(aes(scaled_M2_minus_M1))
# 
# 
# # then make difference maps of scaled May 1- Sep 15
# map_maysep <- function(MaySep_summtrapsWA_scale01 ,saveplot=TRUE){
#   
#   # labels for plot titles
#   season_label=unique(MaySep_summtrapsWA_scale01 $season)
#   
#   bbox = c(800000,1650000,1013103,1970000)
#   
#   MaySep_scaled_map_out <- MaySep_summtrapsWA_scale01  %>% 
#     ggplot()+
#     geom_tile(aes(grd_x,grd_y,fill=scaled_M2_minus_M1),na.rm=T,alpha=0.8)+
#     geom_sf(data=coaststates,col=NA,fill='gray50')+
#     geom_sf(data=MA_shp,col="black", size=0.5, fill=NA)+
#     geom_sf(data=QSMA_shp,col="black", linetype = "11", size=0.5, fill=NA)+
#     scale_fill_viridis(na.value='grey70',option="A",limits=c(-1,1),oob=squish)+
#     coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))+
#     labs(x='',y='',fill='M2 - M1',title=paste0('May 1 - Sep 15\n',season_label))
#   
#   # saving
#   if(saveplot){
#     pt <- unique(MaySep_summtrapsWA_scale01 $season)
#     ggsave(here('wdfw','maps', 'difference_maps',paste0('May 1 - Sep 15 ',pt,'.png')),MaySep_scaled_map_out,w=6,h=5)
#   }
#   return(MaySep_scaled_map_out)
# }
# 
# # Loop and save maps
# tm <- proc.time()
# all_maps <- purrr::map(unique(MaySep_summtrapsWA_scale01_v2 $season),function(x){
#   MaySep_summtrapsWA_scale01_v2  %>% 
#     filter(season==x) %>% 
#     map_maysep()
# })
# proc.time()-tm

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------