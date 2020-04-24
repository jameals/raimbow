###############################################################################
# Script for running effort_mgmt() function (currently set to Sam's file paths)

# library(dplyr)
# library(lubridate)
# 
# x.hump <- readRDS("../raimbow-local/Outputs/Humpback_5km_long_monthly.rds") %>%
#   mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
#   select(GRID5KM_ID, year_month, Humpback_dens_mean, Humpback_dens_se)
# 
# x.orig <- readRDS("../raimbow-local/Data/fishing/CA_DCRB_vms_fishing_daily_2009-2018_fishtix_humpback_blue_whales_grids.RDS") %>%
#   select(-year_mo, -contains("risk"), -contains("H_Avg_Abund")) %>%
#   left_join(x.hump, by = c("year_month", "GRID5KM_ID"))
# rm(x.hump)
# 
# 
# # source("tradeoffs/Management scenarios/Funcs_management_scenarios.R")
# d <- effort_mgmt(
#   x = x.orig,
#   delay.date = as.Date("2009-12-15"),
#   delay.region = "CenCA",
#   delay.method.shift = "lag",
#   delay.method.fidelity = "temporal",
#   closure.date = as.Date("2010-04-01"),
#   closure.region = "CenCA",
#   closure.method = "temporal",
#   closure.redist.percent = 10
# )


###############################################################################
# Input format for x: 'CA_DCRB_vms_fishing_daily_2009-2018_fishtix_humpback_blue_whales_grids.RDS'

# Implement effect of management scenarios on fishing effort
# See 'Readme for Funcs_management_scenarios.docx' for pseudocode
effort_mgmt <- function(x, delay.date = NULL, delay.region = NULL, 
                        delay.method.shift = NULL, delay.method.fidelity = NULL, 
                        closure.date = NULL, closure.region = NULL, 
                        closure.method = NULL, closure.redist.percent = 100) {
  ### Inputs
  # x: data.frame; expected to has same format at data frame from Jameal
  # delay.date: Date; date for which the fishery will open in 2009-10 crab season.
  #   If NULL, then there is no delayed opening
  #   If NULL, then there is no early (e.g. spring) closure
  # delay.region: character; one of NULL, "All", "CenCA", "BIA"
  # delay.method.shift: character; if used, either "pile" or "lag"
  # delay.method.fidelity: character; method of redistribution, 
  #   if used, either "spatial" (fidelity) or "temporal" (fidelity)
  # closure.date: Date; date for which the fishery will close in 2009-10 crab season
  # closure.region: character; see 'delay.region'
  # closure.method: character; if used, either "remove" or "temporal (fidelity)
  # closure.redist.percent: numeric; default is 100. If used, 
  #   percentage of effort to redistribute that is kept
  
  ### Output
  # Data frame with shifted effort and the following columns added:
  #   date_record: class Date, date of effort
  #   date_past_season_end: logical; indicates if date_record is past the 
  #     original end date for this Region in this crab year
  #   date_past_region_end: logical; indicates if date_record is past the 
  #     lawful end date (July 15 for central CA and July 31 otherwise) 
  #     for this Region in this crab year
  
  
  stopifnot(
    require(dplyr), 
    require(lubridate), 
    require(purrr), 
    require(tidyr)
  )
  
  
  #----------------------------------------------------------------------------
  # Input checks
  
  # Basic checks
  stopifnot(
    inherits(x, "data.frame"), 
    is.null(delay.date) | inherits(delay.date, "Date"), 
    is.null(closure.date) | inherits(closure.date, "Date"), 
    is.null(delay.method.shift) | (delay.method.shift %in% c("pile", "lag")), 
    is.null(delay.method.fidelity) | (delay.method.fidelity %in% c("spatial", "temporal")), 
    is.null(closure.method) | (closure.method %in% c("remove", "temporal")), 
    inherits(closure.redist.percent, c("numeric", "integer")), 
    dplyr::between(closure.redist.percent, 0, 100)
  )
  
  
  # Data frame name checks
  names.x.fish <- c(
    "crab_year", "GRID5KM_ID", "Region", "year_month", "day_of_year", 
    "BIA_bm_noNAs", "BIA_mn_noNAs", "BIA_bm_or_mn", 
    "DCRB_lbs", "DCRB_rev", "Num_DCRB_VMS_pings", 
    "Num_DCRB_Vessels", "Num_Unique_DCRB_Vessels"
  )
  names.x.info <- c(
    "crab_year", "year_month", "GRID5KM_ID", "BAND_25KM", "BAND_50KM", "CA_OFFSHOR", "Region", 
    "Blue_occurrence_mean", "Blue_occurrence_sd", "normalized_Blue_occurrence_mean", 
    "Humpback_dens_mean", "Humpback_dens_se"
  )
  if (!(all(names.x.info %in% names(x)) & all(names.x.fish %in% names(x))))
    stop("x does not contain all required columns:\n", 
         paste(unique(c(names.x.fish, names.x.info)), collapse = ", "))
  
  
  # Check at least one ~valid management scenario is specified
  if (is.null(delay.date) & is.null(closure.date)) 
    stop("At least one of delay.date or closure.date must not be NULL")
  
  if (!is.null(delay.date) & !is.null(closure.date)) 
    if (delay.date >= closure.date)
      stop("delay.date must come before closure.date")
  
  
  # Check date values are for the 2009-10 fishing season, and date-region interplay
  if (!is.null(delay.date)) { 
    if (length(delay.region) != 1 | is.null(delay.region)) 
      stop("If a delayed opening date is provided, ", 
           "delay.region must not be NULL and of length 1")
    
    if (!between(delay.date, as.Date("2009-11-15"), as.Date("2010-07-31")))
      stop("delay.date must be for the 2009-10 fishing season, ", 
           "meaning it must be between 2009-11-15 and 2010-07-31")
  }
  
  if (!is.null(closure.date)) { 
    if (length(closure.region) != 1 | is.null(closure.region)) 
      stop("If an early closure date is provided, ", 
           "closure.region must not be NULL and of length 1")
    
    if (!between(closure.date, as.Date("2009-11-15"), as.Date("2010-07-31")))
      stop("closure.date must be for the 2009-10 fishing season, ", 
           "meaning it must be between 2010-11-15 and 2010-07-31")
  }
  
  
  # Check region names, if applicable
  region.acc <- c("All", "CenCA", "NorCA", "BIA") #TODO: add "OR", "WA", CA-SCen", etc
  if (!all(names(x$Region) %in% region.acc))
    stop("All of x$Region must be one of:\n", 
         paste(region.acc, collapse = ", "))
  
  if (!is.null(delay.date))
    if (!(delay.region %in% region.acc))
      stop("Both delay.region and closure.region must either be NULL or one of:\n", 
           paste(region.acc, collapse = ", "))
  
  if (!is.null(closure.date))
    if (!(closure.region %in% region.acc))
      stop("Both delay.region and closure.region must either be NULL or one of:\n", 
           paste(region.acc, collapse = ", "))
  
  
  #----------------------------------------------------------------------------
  ### Initial processing
  
  # Extract 'constant' data (data that won't be changed)
  x.other <- x %>% 
    select(!!names.x.info) %>% 
    distinct()
  
  # Select for and do initial processing of effort data
  x.fish <- x %>% 
    select(!!names.x.fish) %>%
    mutate(season_date_st_min = as.Date(paste(substr(crab_year, 1, 4), 
                                              ifelse(Region == "CenCA", "11-15", "12-01"), 
                                              sep = "-")), 
           year = as.numeric(substr(year_month, 1, 4)), 
           date_record_orig = as.Date(day_of_year - 1, origin = as.Date(paste0(year, "-01-01"))), 
           decimal_record = ifelse(date_record_orig < season_date_st_min, 
                                   decimal_date(season_date_st_min), 
                                   decimal_date(date_record_orig)), 
           date_record = as.Date(
             round_date(date_decimal(decimal_record), unit = "day")), 
           year_month = func_year_mo(date_record)) %>% 
    select(-year, -day_of_year, -date_record_orig, -decimal_record)
  
  # For each crab season and Region, get end dates
  x.fish.end.summ <- x.fish %>% 
    group_by(crab_year, Region) %>% 
    summarise(season_date_end = max(date_record), 
              region_date_end = as.Date(paste0(
                substr(unique(crab_year), 6, 9), 
                ifelse(unique(Region) == "CenCA", "-07-15", "-07-31")
              ))
    ) %>% 
    ungroup()
  
  if (any(x.fish.end.summ$season_date_end > x.fish.end.summ$region_date_end))
    warning("Some season end dates come after the region end dates")
  
  
  #####
  # sum(x.fish$date_record != x.fish$date_record_orig) #1797
  # identical(
  #   which(x.fish$date_record_orig < x.fish$season_date_st_min), 
  #   which(x.fish$date_record != x.fish$date_record_orig)
  # ) #TRUE
  
  # sum(x.fish$date_record < x.fish$season_date_st_min) #1797
  # x.fish %>%
  #   group_by(crab_year, Region) %>%
  #   summarise(season_date_st_min = unique(season_date_st_min),
  #             date_record_min = min(date_record), 
  #             count_date_oob = sum(date_record < unique(season_date_st_min)))
  #####
  
  
  #----------------------------------------------------------------------------
  # Do delayed opening stuff
  if (is.null(delay.date)) {
    x.fish.delay <- x.fish
    
  } else {
    #------------------------------------------------------
    # Step 1) filter by region
    if (delay.region == "BIA") {
      stop("BIA delayed opening is not supported (yet?)")
      
    } else if (identical(delay.region, "All")) {
      x.d.fish.filter <- x.fish
      x.d.fish.nofilter <- NULL
      
    } else {
      x.d.fish.filter <- x.fish %>% filter(Region %in% delay.region)
      x.d.fish.nofilter <- x.fish %>% filter(!(Region %in% delay.region))
    } 
    
    if (nrow(x.d.fish.filter) == 0) 
      stop("Error in fishing data processing - delayed opening")
    
    
    #------------------------------------------------------
    # Step 2) determine the number of days each season would have been delayed
    season.mgmt.summ <- x.d.fish.filter %>% 
      mutate(mgmt_yr = if (year(delay.date) == 2009) substr(crab_year, 1, 4) else substr(crab_year, 6, 9), 
             season_open_mgmt = as.Date(paste(mgmt_yr, month(delay.date), day(delay.date), 
                                              sep = "-"))) %>% 
      group_by(crab_year, Region) %>% 
      summarise(season_date_st = min(date_record), #Ok since date_records altered above
                # season_date_end = max(date_record), 
                season_open_mgmt = unique(season_open_mgmt), 
                season_days_delayed = as.numeric(difftime(season_open_mgmt, season_date_st, 
                                                          units = "days"))) %>% 
      ungroup()
    
    
    #------------------------------------------------------
    # Step 3) shift dates/times as necessary
    x.fish.shifted <- delay_date_shift(
      left_join(x.d.fish.filter, season.mgmt.summ, by = c("crab_year", "Region")), 
      delay.method.shift
    )
    
    # Sanity check - all new date records are still in the proper crab year
    check.int <- interval(
      as.Date(paste0(substr(x.fish.shifted$crab_year, 1, 4), "-11-01")), 
      as.Date(paste0(substr(x.fish.shifted$crab_year, 6, 9), "-10-31"))
    )
    if (!all(x.fish.shifted$date_record_old %within% check.int)) 
      warning("Error in dealyed opening date shifting - shifted out of crab season")
    rm(check.int)
    
    
    #------------------------------------------------------
    # Step 4) redistribute effort using spatial or temporal fidelity
    x.fish.redist <- if (delay.method.fidelity == "temporal") {
      redist_temporal(x.fish.shifted, Num_DCRB_VMS_pings, "delay")
    } else if (delay.method.fidelity == "spatial") {
      x.fish.shifted %>% select(-starts_with("season_")) 
    } else {
      stop("Unrecognized value passed to delay.method.fidelity")
    } 
    
    x.fish.redist <- x.fish.redist %>% 
      # left_join(select(season.mgmt.summ, crab_year, Region, season_date_end), 
      #           by = c("crab_year", "Region")) %>% 
      # mutate(date_past_season_end = date_record > season_date_end) %>% 
      select(crab_year, GRID5KM_ID, Region, year_month, date_record, 
             BIA_bm_noNAs, BIA_mn_noNAs, BIA_bm_or_mn, 
             DCRB_lbs, DCRB_rev, Num_DCRB_VMS_pings, Num_DCRB_Vessels, 
             Num_Unique_DCRB_Vessels)
    # date_past_season_end)
    
    x.fish.delay <- x.d.fish.nofilter %>% 
      # mutate(date_past_season_end = FALSE) %>% 
      select(!!names(x.fish.redist)) %>% 
      bind_rows(x.fish.redist) %>% 
      arrange(crab_year, Region, day(date_record), GRID5KM_ID)
    
    rm(x.fish.shifted, season.mgmt.summ, x.fish.redist, 
       x.d.fish.filter, x.d.fish.nofilter)
  }
  
  
  #----------------------------------------------------------------------------
  # Do early closure stuff
  if (is.null(closure.date)) {
    x.fish.closure <- x.fish.delay
    
  } else {
    #------------------------------------------------------
    # Determine which records come after closure date
    x.fish.c1 <- x.fish.delay %>% 
      mutate(mgmt_yr = if (year(closure.date) == 2009) substr(crab_year, 1, 4) else substr(crab_year, 6, 9), 
             season_close_mgmt = as.Date(paste(mgmt_yr, month(closure.date), day(closure.date), 
                                               sep = "-")), 
             record_post_closure_date = date_record >= season_close_mgmt, 
             date_record_old = date_record, 
             year_month_old = year_month) %>% 
      select(-mgmt_yr)
    
    
    #------------------------------------------------------
    # Do region stuff..
    if (closure.region == "All") {
      if (closure.method == "temporal") {
        closure.method <- "remove"
        warning("closure.region is \"All\", and thus the \"Remove\"", 
                " closure method must be used")
      }
      x.fish.c2 <- x.fish.c1 %>% 
        mutate(record_closed = record_post_closure_date, 
               region_toshiftto = NA)
      
    } else if (closure.region == "BIA") {
      x.fish.c2 <- x.fish.c1 %>% 
        mutate(record_closed = BIA_bm_or_mn == "Inside BIA" & record_post_closure_date, 
               region_toshiftto = ifelse(record_closed, Region, NA))
      
    } else if (closure.region %in% c("CenCA", "NorCA")) {
      region.toshift <- ifelse(closure.region == "CenCA", "NorCA", "CenCA")
      x.fish.c2 <- x.fish.c1 %>% 
        mutate(record_closed = Region == closure.region & record_post_closure_date, 
               region_toshiftto = ifelse(record_closed, region.toshift, NA))
      rm(region.toshift)
      
    } else {
      stop("Unrecognized value passed to closure.region")
    }
    
    
    #------------------------------------------------------
    # Remove or redistribute closed effort in applicable region(s)
    if (closure.method == "remove") {
      x.fish.closure <- x.fish.c2 %>% 
        filter(!record_closed) %>% 
        # mutate(date_past_season_end = date_record > season_close_mgmt) %>% 
        select(-season_close_mgmt, -record_post_closure_date, 
               -date_record_old, -year_month_old, 
               -record_closed, -region_toshiftto)
      
    } else if (closure.method == "temporal") {
      # df.tmp <- x.fish.c1 %>% 
      #   select(crab_year, Region, season_close_mgmt) %>% 
      #   distinct()
      
      x.fish.closure <- x.fish.c2 %>% 
        mutate(record_toshift = record_closed, 
               record_base = !record_toshift & record_post_closure_date) %>% 
        redist_temporal(Num_DCRB_VMS_pings, "closure", closure.redist.percent) #%>% 
        # left_join(df.tmp, by = c("crab_year", "Region")) %>% 
        # mutate(date_past_season_end = date_record > season_close_mgmt) %>% 
        # select(-season_close_mgmt)
    }
    
    rm(x.fish.c1, x.fish.c2)
  }
  
  
  #----------------------------------------------------------------------------
  # Final checks and formatting
  x.fish.closure %>% 
    left_join(x.fish.end.summ, by = c("crab_year", "Region")) %>% 
    mutate(date_past_season_end = date_record > season_date_end, 
           date_past_region_end = date_record > region_date_end, 
           year = year(date_record), 
           month_as_numeric = month(date_record), 
           month = factor(month(date_record, label = TRUE, abbr = FALSE), 
                          levels = levels(x$month)), 
           day_of_year = yday(date_record)) %>% 
    left_join(x.other, by = c("crab_year", "GRID5KM_ID", "Region", "year_month"))
}


###############################################################################
###############################################################################
# Function for shifting date/time of effort and adding 'record_toshift' column
delay_date_shift <- function(y, delay.method.shift) {
  y <- y %>% 
    rename(year_month_old = year_month, date_record_old = date_record)
  
  #--------------------------------------------------------
  if (delay.method.shift == "lag") {
    y %>% 
      mutate(days_tolag = ifelse(season_days_delayed > 0, season_days_delayed, 0), 
             date_record = date_record_old + days(days_tolag), 
             year_month = func_year_mo(date_record))
    
    #------------------------------------------------------
  } else if (delay.method.shift == "pile") {
    y %>% 
      mutate(diff_days = as.numeric(difftime(date_record_old, season_open_mgmt, 
                                             units = "days")), 
             days_tolag = ifelse(diff_days > 0, 0, 
                                 ifelse(season_days_delayed > 0, 
                                        season_days_delayed, 0)), 
             
             date_record = date_record_old + days(days_tolag), 
             year_month = func_year_mo(date_record)) %>% 
      select(-diff_days)
    
    #------------------------------------------------------
  } else {
    stop("delay.method.shift is not either 'lag' or 'pile'")
  }
}


###############################################################################
# Temporal fidelity redistribution function
redist_temporal <- function(z, z.col, z.type, z.perc = 100) {
  ### Inputs
  # z: data frame; Contains columns described in z.cols.nec
  # z.col: symbol, column name to use for redistribution percentages
  # z.type: either "delay" or "closure"
  # z.perc: numeric; default is 100. Percentage multiplier for effort
  #   to be redistributed
  
  ### Output: 
  # A data frame with fishing effort data redistributed temporally  
  
  z.col <- enquo(z.col)
  
  #--------------------------------------------------------
  ### Input checks
  z.cols.nec <- c(
    "crab_year", "GRID5KM_ID", "Region", "date_record", "date_record_old", 
    "year_month_old", "year_month"
  )
  
  z.cols.dcrb <- c(
    "DCRB_lbs", "DCRB_rev", "Num_DCRB_VMS_pings", "Num_DCRB_Vessels", 
    "Num_Unique_DCRB_Vessels"
  )
  
  stopifnot(
    all(z.cols.nec %in% names(z)),
    all(z.cols.dcrb %in% names(z)), 
    z.type %in% c("delay", "closure"), 
    between(z.perc, 0, 100)
  )
  
  if (!(quo_name(z.col) %in% z.cols.dcrb))
    stop("The column to use for temporal redistribution (z.col) ",
         "must be one of the following:\n",
         paste(z.cols.dcrb, collapse = ", "))
  
  # Convert to decimal
  z.perc <- z.perc / 100
  
  
  #--------------------------------------------------------
  ### Pre-process - Extract necessary columns for calculations
  if (z.type == "closure") {
    z.closure.cols <- c("record_toshift", "region_toshiftto", "record_base")
    stopifnot(all(z.closure.cols %in% names(z)))
    
    z.use <- z %>% select(!!z.cols.nec, !!z.cols.dcrb, !!z.closure.cols)
    
  } else if (z.type == "delay") {
    z.delay.cols <- c("days_tolag", "season_open_mgmt")
    stopifnot(all(z.delay.cols %in% names(z)))
    
    z.use <- z %>% 
      select(!!z.cols.nec, !!z.cols.dcrb, !!z.delay.cols) %>%
      mutate(record_toshift = days_tolag > 0, 
             record_base = date_record_old >= season_open_mgmt, 
             region_toshiftto = Region) %>% 
      select(-!!z.delay.cols)
    
  } else {
    stop("Error in z.type")
  }
  
  
  #--------------------------------------------------------
  ### Process
  
  # Determine region-yms that have base effort
  z.use <- z.use %>% 
    mutate(id = paste(region_toshiftto, year_month, sep = "-"), 
           id_old = paste(Region, year_month_old, sep = "-"))
  
  z.id.base <- unique(filter(z.use, record_base)$id_old)
  
  # z.id.shift <- unique(filter(z.use, record_toshift)$id)
  # sum(!(z.id.shift %in% z.id.base))
  # z.id.shift[which(!(z.id.shift %in% z.id.base))]
  
  
  # 1) Get sums of effort to be redistributed, grouped by region and year-month,
  #   that has a base
  stopifnot(!any(is.na(filter(z.use, record_toshift)$region_toshiftto)))
  z.summ.toredistribute <- z.use %>% 
    filter(record_toshift, id %in% z.id.base) %>%
    group_by(region_toshiftto, year_month) %>% 
    summarise(DCRB_lbs_sum_toadd = sum(DCRB_lbs) * z.perc, 
              DCRB_rev_sum_toadd = sum(DCRB_rev) * z.perc, 
              Num_DCRB_VMS_pings_sum_toadd = sum(Num_DCRB_VMS_pings) * z.perc, 
              Num_DCRB_Vessels_sum_toadd = sum(Num_DCRB_Vessels) * z.perc, 
              Num_Unique_DCRB_Vessels_sum_toadd = sum(Num_Unique_DCRB_Vessels) * z.perc) %>%
    ungroup()
  
  # ...and get list to feed to tidyr::replace_na later
  list.nas.names <- names(select(z.summ.toredistribute, -region_toshiftto, -year_month))
  list.nas <- as.list(rep(0, length(list.nas.names))) %>%
    set_names(list.nas.names)
  
  # 2) Get effort that needs to be redistributed, but has no base for redistribution
  z.redist.nobase <- z.use %>% 
    filter(record_toshift, !(id %in% z.id.base)) %>% 
    mutate(DCRB_lbs = DCRB_lbs * z.perc, 
           DCRB_rev = DCRB_rev * z.perc, 
           Num_DCRB_VMS_pings = Num_DCRB_VMS_pings * z.perc, 
           Num_DCRB_Vessels = Num_DCRB_Vessels * z.perc, 
           Num_Unique_DCRB_Vessels = Num_Unique_DCRB_Vessels * z.perc)
  
  # 3) Get effort that is not being redistributed
  z.tostay <- z.use %>% filter(!record_toshift)
  
  
  # Get sums of z.col, grouped by region and year-month, to calculate percentages
  #   for redistributioin
  z.summ.col.old.sum <- z.use %>% 
    filter(record_base) %>% 
    group_by(Region, year_month_old) %>% 
    summarise(eff_sum = sum(!!z.col))
  
  # a) Join with z.col sums, and calculate percentages
  # b) Join with effort to redistribute, and redistribute
  # c) Variable cleanup
  z.redist.base <- z.use %>% 
    filter(record_base) %>% 
    left_join(z.summ.col.old.sum, by = c("Region", "year_month_old")) %>% 
    mutate(eff_percent = !!z.col / eff_sum) %>% 
    left_join(z.summ.toredistribute, 
              by = c("Region" = "region_toshiftto", 
                     "year_month_old" = "year_month")) %>% 
    replace_na(list.nas) %>% 
    mutate(DCRB_lbs = DCRB_lbs_sum_toadd * eff_percent,
           DCRB_rev = DCRB_rev_sum_toadd * eff_percent, 
           Num_DCRB_VMS_pings = Num_DCRB_VMS_pings_sum_toadd * eff_percent, 
           Num_DCRB_Vessels = Num_DCRB_Vessels_sum_toadd * eff_percent, 
           Num_Unique_DCRB_Vessels = Num_Unique_DCRB_Vessels_sum_toadd * eff_percent) %>% 
    select(-eff_sum, -eff_percent, -DCRB_lbs_sum_toadd, -DCRB_rev_sum_toadd, 
           -Num_DCRB_VMS_pings_sum_toadd, -Num_DCRB_Vessels_sum_toadd,
           -Num_Unique_DCRB_Vessels_sum_toadd)
  
  # Sanity check 1
  stopifnot(
    isTRUE(all.equal(sum(z.redist.base$DCRB_lbs), sum(z.summ.toredistribute$DCRB_lbs_sum_toadd))),
    isTRUE(all.equal(sum(z.redist.base$DCRB_rev), sum(z.summ.toredistribute$DCRB_rev_sum_toadd))),
    isTRUE(all.equal(sum(z.redist.base$Num_DCRB_VMS_pings), sum(z.summ.toredistribute$Num_DCRB_VMS_pings_sum_toadd))),
    isTRUE(all.equal(sum(z.redist.base$Num_DCRB_Vessels), sum(z.summ.toredistribute$Num_DCRB_Vessels_sum_toadd))),
    isTRUE(all.equal(sum(z.redist.base$Num_Unique_DCRB_Vessels), sum(z.summ.toredistribute$Num_Unique_DCRB_Vessels_sum_toadd)))
  )
  
  
  # a) Bind together 3 elements of 'new' effort (listed with 1,2,3 above)
  # b) Group by date and grid cell ID (other vars are there to keep info)
  z.new <- bind_rows(z.redist.base, z.tostay, z.redist.nobase) %>% 
    arrange(crab_year, Region, day(date_record), GRID5KM_ID) %>%
    group_by(crab_year, GRID5KM_ID, Region, year_month, date_record) %>%
    summarise(DCRB_lbs = sum(DCRB_lbs),
              DCRB_rev = sum(DCRB_rev),
              Num_DCRB_VMS_pings = sum(Num_DCRB_VMS_pings),
              Num_DCRB_Vessels = sum(Num_DCRB_Vessels),
              Num_Unique_DCRB_Vessels = sum(Num_Unique_DCRB_Vessels)) %>% 
    ungroup() %>% 
    left_join(distinct(select(z, GRID5KM_ID, BIA_bm_noNAs, BIA_mn_noNAs, BIA_bm_or_mn)), 
              by = c("GRID5KM_ID")) %>% 
    select(crab_year, GRID5KM_ID, Region, year_month, date_record, 
           BIA_bm_noNAs, BIA_mn_noNAs, BIA_bm_or_mn, 
           everything())
  
  
  # Sanity check 2: amount of effort in mataches amount of effort out
  func.sanity2 <- function(zzz, z.perc) {
    sum(z.new[[zzz]]) + 
      (1-z.perc) / z.perc * (sum(z.redist.base[[zzz]]) + sum(z.redist.nobase[[zzz]]))
  }
  stopifnot(
    isTRUE(all.equal(func.sanity2("DCRB_lbs", z.perc), sum(z$DCRB_lbs))),
    isTRUE(all.equal(func.sanity2("DCRB_rev", z.perc), sum(z$DCRB_rev))),
    isTRUE(all.equal(func.sanity2("Num_DCRB_VMS_pings", z.perc), sum(z$Num_DCRB_VMS_pings))),
    isTRUE(all.equal(func.sanity2("Num_DCRB_Vessels", z.perc), sum(z$Num_DCRB_Vessels))),
    isTRUE(all.equal(func.sanity2("Num_Unique_DCRB_Vessels", z.perc), sum(z$Num_Unique_DCRB_Vessels)))
  )
  
  z.new
}


###############################################################################
# Random helper functions
func_year_mo <- function(x) {
  stopifnot(inherits(x, "Date"))
  paste(lubridate::year(x), sprintf("%02d", lubridate::month(x)), sep = "_")
}

###############################################################################
