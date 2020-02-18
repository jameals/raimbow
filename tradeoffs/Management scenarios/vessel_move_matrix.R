########## Create matrix of vessel movement ##########
#
# Jan 13, 2020 - M. Fisher
#
######################################################


# Set up --------------------------------------------------------------------

library(lubridate)
library(reshape2)
library(tidyr)
library(dplyr)
library(grid)
library(gridExtra)
library(here)


myports=c("CCA","ERA","BGA","BDA","SFA","MNA","MRA")
r="closure"




# Data --------------------------------------------------------------------

# Read in ticket data
## from crab ports
fishtix <- read.csv(here::here("output","metier_assignments",paste0("FishTix_crab2015_metiers_k1_recoded_CA_CrabPorts.csv")), stringsAsFactors = FALSE)

## from other ports
noncrab_tix <- read.csv(here::here("input_data","processed","fish_tickets_crab2015_processed_for_networks.csv")) %>%
  filter(agid=="C") %>%
  filter(!(pcgroup %in% myports))

# Closure dates, 2015-16
dates_df <- read.delim(here::here("input_data","crab_open_dates.txt")); head(dates_df)
# x crab_year pcgroup     odate
# 1      2015     CCA 5/12/2016
# 2      2015     ERA 5/12/2016
# 3      2015     BGA 5/12/2016
# 4      2015     BDA 3/26/2016
# 5      2015     SFA 3/26/2016
# 6      2015     MRA 3/26/2016

# Edit date objects
dates_df$odate <- mdy(dates_df$odate)
fishtix$tdate <- date(parse_date_time(fishtix$tdate, orders=c("ymd", "mdy")))
noncrab_tix$tdate <- date(parse_date_time(noncrab_tix$tdate, orders=c("ymd", "mdy")))

###########################################################################
########################## large / small vessels ##########################

# Vessel IDs that Changed Port --------------------------------------------
## this reads in a matrix which has recorded which vessels were inactive at the given port, but active elsewhere in 2015. 
for(p in myports){
  ## separate matrices for small and large vessels
  for(s in c("large","small")){
    tmpdat <- read.csv(here::here("output/networks/crab_vessel",paste0(p,"_closure_Avesselmat_",s,"_v7_newVL.csv"))) %>%
      ## grab vessel ID and column indicating whether the vessel was at a different port group
      dplyr::select(X,other_port) %>%
      rename(drvid=X) %>%
      mutate(size=s, pcgroup=p) %>%
      ## filter for vessels at a different port group
      filter(other_port == 1)
    tmpdat$drvid <- as.character(tmpdat$drvid)
    ## save in one data frame
    if(p==myports[1] & s=="large"){
      mydat=tmpdat
    }else{
      mydat <- rbind(mydat,tmpdat)
    }
  }
}



# Ports of Landing --------------------------------------------------------

# For each port group, find vessels that moved in other ports
for(p in myports){
  open_info <- filter(dates_df, pcgroup == p)
  ## get vessels that fished in port group 'p' in 2014, which were active elsewhere in 2015
  vessels <- filter(mydat, pcgroup==p)
  odate <- open_info$odate
  ## pull non-crab port fish ticket data for given vessels, during response period
  sub_noncrab_tix <- noncrab_tix %>%
    filter(drvid %in% vessels$drvid) %>%
    ## grab only fish tickets from closure period
    mutate(opening_date = paste0(crab_year+1, "-", month(odate), "-", day(odate))) %>%
    mutate(response = ifelse(ymd(tdate) < ymd(opening_date), "closure", "open")) %>%
    filter(response=="closure") %>%
    dplyr::select(drvid,pcgroup,spid_recode) %>% distinct() %>%
    rename(metier.spid=spid_recode, pcgroup15=pcgroup)
  
  ## pull fish ticket data for given vessels, during response period
  sub_fishtix <- fishtix %>%
    filter(drvid %in% vessels$drvid) %>%
    filter(!(pcgroup == p)) %>%
    ## grab only fish tickets from closure period
    mutate(opening_date = paste0(crab_year+1, "-", month(odate), "-", day(odate))) %>%
    mutate(response = ifelse(ymd(tdate) < ymd(opening_date), "closure", "open")) %>%
    filter(response=="closure") %>%
    dplyr::select(drvid,pcgroup,metier.2010) %>% distinct() %>%
    rename(metier.spid=metier.2010, pcgroup15=pcgroup) %>%
    rbind(sub_noncrab_tix)
  
  ## combine and return data frames
  tmpdat <- vessels %>%
    dplyr::select(drvid,size,pcgroup) %>%
    rename(pcgroup14=pcgroup) %>%
    left_join(sub_fishtix,by="drvid")
  ## add to a single data frame
  if(p=="CCA"){
    movedat <- tmpdat
  } else{
    movedat <- rbind(movedat,tmpdat)
  }
}

write.csv(movedat, here::here("output","resilience","DCrab_Vessels_Other_Port_Details.csv"),row.names=FALSE)



# Create Matrix -----------------------------------------------------------
matdat <- movedat %>%
  dplyr::select(-metier.spid) %>% distinct() %>%
  group_split(size)

matdat.l <- matdat[[1]] %>%
  dplyr::select(-size) %>%
  group_by(pcgroup14,pcgroup15) %>%
  summarise(nvessels=length(unique(drvid))) %>%
  pivot_wider(id_cols=pcgroup14,names_from=pcgroup15,values_from=nvessels) %>%
  ungroup(pcgroup14) %>%
  dplyr::select(pcgroup14,CCA,ERA,BGA,BDA,SFA,MNA,MRA,SBA,LAA) %>%
  mutate(pcgroup14=factor(pcgroup14,levels=myports)) %>% arrange(pcgroup14)
# pcgroup14   CCA   ERA   BGA   BDA   SFA   MNA   MRA   SBA   LAA
# 1 CCA         
# 2 ERA         
# 3 BGA          
# 4 BDA           
# 5 SFA         
# 6 MNA         
# 7 MRA          


matdat.s <- matdat[[2]] %>%
  dplyr::select(-size) %>%
  group_by(pcgroup14,pcgroup15) %>%
  summarise(nvessels=length(unique(drvid))) %>%
  pivot_wider(id_cols=pcgroup14,names_from=pcgroup15,values_from=nvessels) %>%
  ungroup(pcgroup14) %>%
  dplyr::select(pcgroup14,ERA,BGA,BDA,SFA,MNA,MRA) %>%
  mutate(pcgroup14=factor(pcgroup14,levels=myports)) %>% arrange(pcgroup14)

# pcgroup14   ERA   BGA   BDA   SFA   MNA   MRA
# 1 CCA          
# 2 ERA          
# 3 BGA        
# 4 BDA   
# 5 SFA       




matdat.l.noncon <- matdat[[1]] %>%
  dplyr::select(-size) %>%
  group_by(pcgroup14,pcgroup15) %>%
  summarise(nvessels=length(unique(drvid))) %>%
  filter(nvessels > 2) %>% # get rid of confidential data
  pivot_wider(id_cols=pcgroup14,names_from=pcgroup15,values_from=nvessels) %>%
  ungroup(pcgroup14) %>%
  dplyr::select(pcgroup14,CCA,ERA,BGA,BDA,SFA) %>%
  mutate(pcgroup14=factor(pcgroup14,levels=myports)) %>% arrange(pcgroup14)

# pcgroup14   CCA   ERA   BGA   BDA   SFA
# 1 CCA          NA    NA    NA    NA     5
# 2 ERA          NA    NA    NA     4     5
# 3 BDA          NA    NA     3    NA    NA
# 4 SFA           3     4    NA    NA    NA


matdat.s.noncon <- matdat[[2]] %>%
  dplyr::select(-size) %>%
  group_by(pcgroup14,pcgroup15) %>%
  summarise(nvessels=length(unique(drvid))) %>%
  filter(nvessels > 2) %>%
  pivot_wider(id_cols=pcgroup14,names_from=pcgroup15,values_from=nvessels) %>%
  ungroup(pcgroup14) %>%
  dplyr::select(pcgroup14,BGA,MRA) %>%
  mutate(pcgroup14=factor(pcgroup14,levels=myports)) %>% arrange(pcgroup14)

# pcgroup14   BGA   MRA
# 1 BDA           4    NA
# 2 SFA          NA     3





##########################################################################
########################### all d.crab vessels ###########################

# Vessel IDs that Changed Port --------------------------------------------
for(p in myports){
  tmpdat <- read.csv(here::here("output/networks/crab_vessel",paste0(p,"_closure_Avesselmat_NA_v7_newVL.csv"))) %>%
    dplyr::select(X,other_port) %>%
    rename(drvid=X) %>%
    mutate(pcgroup=p) %>%
    filter(other_port == 1)
  tmpdat$drvid <- as.character(tmpdat$drvid)
  if(p==myports[1]){
    mydat=tmpdat
  }else{
    mydat <- rbind(mydat,tmpdat)
  }
}


# Ports of Landing --------------------------------------------------------

# For each port group, find vessels that moved in other ports
for(p in myports){
  open_info <- filter(dates_df, pcgroup == p)
  vessels <- filter(mydat, pcgroup==p)
  odate <- open_info$odate
  ## pull non-crab port fish ticket data for given vessels, during response period
  sub_noncrab_tix <- noncrab_tix %>%
    filter(drvid %in% vessels$drvid) %>%
    mutate(opening_date = paste0(crab_year+1, "-", month(odate), "-", day(odate))) %>%
    mutate(response = ifelse(ymd(tdate) < ymd(opening_date), "closure", "open")) %>%
    filter(response=="closure") %>%
    dplyr::select(drvid,pcgroup,spid_recode) %>% distinct() %>%
    rename(metier.spid=spid_recode, pcgroup15=pcgroup)
  
  ## pull fish ticket data for given vessels, during response period
  sub_fishtix <- fishtix %>%
    filter(drvid %in% vessels$drvid) %>%
    filter(!(pcgroup == p)) %>%
    mutate(opening_date = paste0(crab_year+1, "-", month(odate), "-", day(odate))) %>%
    mutate(response = ifelse(ymd(tdate) < ymd(opening_date), "closure", "open")) %>%
    filter(response=="closure") %>%
    dplyr::select(drvid,pcgroup,metier.2010) %>% distinct() %>%
    rename(metier.spid=metier.2010, pcgroup15=pcgroup) %>%
    rbind(sub_noncrab_tix)
  
  ## combine and return data frames
  tmpdat <- vessels %>%
    dplyr::select(drvid,pcgroup) %>%
    rename(pcgroup14=pcgroup) %>%
    left_join(sub_fishtix,by="drvid")
  
  if(p=="CCA"){
    movedat <- tmpdat
  } else{
    movedat <- rbind(movedat,tmpdat)
  }
}



# Create Matrix -----------------------------------------------------------
matdat <- movedat %>%
  dplyr::select(-metier.spid) %>% distinct() %>%
  group_by(pcgroup14,pcgroup15) %>%
  summarise(nvessels=length(unique(drvid))) %>%
  pivot_wider(id_cols=pcgroup14,names_from=pcgroup15,values_from=nvessels) %>%
  ungroup(pcgroup14) %>%
  dplyr::select(pcgroup14,CCA,ERA,BGA,BDA,SFA,MNA,MRA,SBA,LAA) %>%
  mutate(pcgroup14=factor(pcgroup14,levels=myports)) %>% arrange(pcgroup14)


matdat.noncon <- movedat %>%
  dplyr::select(-metier.spid) %>% distinct() %>%
  group_by(pcgroup14,pcgroup15) %>%
  summarise(nvessels=length(unique(drvid))) %>%
  filter(nvessels > 2) %>%
  pivot_wider(id_cols=pcgroup14,names_from=pcgroup15,values_from=nvessels) %>%
  ungroup(pcgroup14) %>%
  dplyr::select(pcgroup14,CCA,ERA,BGA,BDA,SFA,MNA,MRA) %>%
  mutate(pcgroup14=factor(pcgroup14,levels=myports)) %>% arrange(pcgroup14)

# pcgroup14   ERA   BGA   BDA   SFA   MNA   MRA
# 1 CCA          NA    NA    NA    NA     6    NA    NA
# 2 ERA          NA    NA    NA     4     6    NA    NA
# 3 BGA          NA    NA    NA     3     3    NA    NA
# 4 BDA          NA     3     7    NA    NA    NA    NA
# 5 SFA           3     4    NA    NA    NA     3     4




matdat.l.noncon <- matdat[[1]] %>%
  dplyr::select(-size) %>%
  group_by(pcgroup14,pcgroup15) %>%
  summarise(nvessels=length(unique(drvid))) %>%
  filter(nvessels > 2) %>% # get rid of confidential data
  pivot_wider(id_cols=pcgroup14,names_from=pcgroup15,values_from=nvessels) %>%
  ungroup(pcgroup14) %>%
  dplyr::select(pcgroup14,CCA,ERA,BGA,BDA,SFA) %>%
  mutate(pcgroup14=factor(pcgroup14,levels=myports)) %>% arrange(pcgroup14)

# pcgroup14   CCA   ERA   BGA   BDA   SFA
# 1 CCA          NA    NA    NA    NA     5
# 2 ERA          NA    NA    NA     4     5
# 3 BDA          NA    NA     3    NA    NA
# 4 SFA           3     4    NA    NA    NA


matdat.s.noncon <- matdat[[2]] %>%
  dplyr::select(-size) %>%
  group_by(pcgroup14,pcgroup15) %>%
  summarise(nvessels=length(unique(drvid))) %>%
  filter(nvessels > 2) %>%
  pivot_wider(id_cols=pcgroup14,names_from=pcgroup15,values_from=nvessels) %>%
  ungroup(pcgroup14) %>%
  dplyr::select(pcgroup14,BGA,MRA) %>%
  mutate(pcgroup14=factor(pcgroup14,levels=myports)) %>% arrange(pcgroup14)

# pcgroup14   BGA   MRA
# 1 BDA           4    NA
# 2 SFA          NA     3
