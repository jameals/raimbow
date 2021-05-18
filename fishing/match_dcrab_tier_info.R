## Match tier information for CA crab vessels

library(tidyverse)
library(magrittr)

# CA tier info
tiers <- read_csv(here::here('data','raw','dcrab_tiers.csv'),col_types = 'dcidcccc')

tiers %<>% rename(vesselID=`FG VESSEL ID`,permit_number=`LE PERMIT NUM`)
# vessels and permit numbers from tier info
unique_vessels <- unique(tiers$vesselID)
unique_permits <- unique(tiers$permit_number)

# Fish tickets with day of season indicator (pre-VMS match)
fishtix <- read_rds(here::here('data','processed','fish tickets','fish_tickets_w_dayofseason.rds'))
# vessel IDs from the (processed) fish tickets
unique_drvid <- unique(fishtix$drvid)

# try matching
matched_vesselIDs <- intersect(unique_drvid,unique_vessels)

# these don't match
# try with a set of raw fish tickets
raw_fishtix <- read_csv(here::here('data','raw','fish tickets','fish tickets 2016-2017.csv'))

# pull out some candidate variables to check for overlap
test_raw_ids <- raw_fishtix %>% 
  filter(AGENCY_CODE=="C") %>% 
  select(AGENCY_CODE,VESSEL_REGISTRATION_ID,FISHER_LICENSE_NUM,VESSEL_ID,VESSEL_NUM)

glimpse(test_raw_ids)

test_raw_ids %<>% mutate(FISHER_LICENSE_NUM=as.integer(FISHER_LICENSE_NUM))

unique_raw_ids <- unique(test_raw_ids$FISHER_LICENSE_NUM)

# do these match?
matched_raw_ids <- intersect(unique_raw_ids,unique_vessels)
# they match a little (16 vessels), so FISHER_LICENSE_NUM might be promising?
# let's pull these ID values from all years' data
pull_ids <- function(fname) {
  ids <- read_csv(fname) %>% 
    filter(AGENCY_CODE=="C") %>% 
    select(AGENCY_CODE,VESSEL_REGISTRATION_ID,FISHER_LICENSE_NUM,VESSEL_ID,VESSEL_NUM) %>% 
    mutate(FISHER_LICENSE_NUM=as.double(FISHER_LICENSE_NUM),VESSEL_NUM=as.character(VESSEL_NUM)) %>% 
    distinct()
  ids
}

fnames <- list.files(here::here('data','raw','fish tickets'),full.names = T)
# pull out all these IDs from all raw fish tickets
raw_ids <- purrr::map_df(fnames,pull_ids) %>% 
  mutate(is_in_fishtix="Y")

glimpse(raw_ids)

# Now try the join again
tiers_matched <- tiers %>% 
  select(vesselID,TIER) %>% 
  left_join(raw_ids,by=c('vesselID'="FISHER_LICENSE_NUM"))

# how many matched?
sum(tiers_matched$is_in_fishtix=="Y",na.rm=T)
# only 304 total vessels matched (maybe this is enough?)
