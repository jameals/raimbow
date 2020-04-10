# Input format for x: 'CA_DCRB_vms_fishing_daily_2009-2018_fishtix_humpback_blue_whales_grids.RDS'
mgmt_redistribute <- function(x, delay.date = NULL, closure.date = NULL, 
                              delay.region = NULL, closure.region = NULL, 
                              method.shift, method.fidelity) {
  ### Inputs
  # x: data.frame; from Jameal
  # delay.date: Date; date for which the fishery will open in 2009-10 crab season.
  #   If NULL, then there is no delayed opening
  # closure.date: Date; date for which the fishery will close in 2009-10 crab season
  #   If NULL, then there is no early (e.g. spring) closure
  # delay.region: character; one of NULL, "All", "CenCA", "BIA"
  # closure.region: character; see 'delay.region'
  # method.shift: character; either "pile" or "lag"
  # method.fideltiy: character; method of redistribution, 
  #   either "spatial" (fidelity) or "temporal" (fidelity)
  
  ### Output
  # TODO
  
  stopifnot(
    require(dplyr), 
    require(lubridate), 
    require(purrr)
  )
  
  
  #----------------------------------------------------------------------------
  # Input checks
  names.x.needed <- c("year", "crab_year", "Region")
  # names.x.needed <- c(
  #   "year", "crab_year", "year_month", "season", 
  #   "month", "month_as_numeric", "week_of_year", "day_of_year", 
  #   "GRID5KM_ID", "BAND_25KM", "BAND_50KM", "CA_OFFSHOR", "Region", 
  #   "BIA_mn_noNAs", "BIA_bm_noNAs", "BIA_bm_or_mn", 
  #   "DCRB_lbs", "DCRB_rev", "Num_DCRB_VMS_pings", "Num_DCRB_Vessels", "Num_Unique_DCRB_Vessels", 
  #   "Blue_occurrence_mean", "Blue_occurrence_sd", 
  #   "normalized_Blue_occurrence_mean", "H_Avg_Abund", "normalized_H_Avg_Abund", 
  #   "normalized_Num_DCRB_VMS_pings"
  # )
  
  region.acc <- c("All", "CenCA", "NorCA", "BIA") #TODO: add "OR", "WA", CA-SCen", etc
  
  stopifnot(
    inherits(x, "data.frame"), 
    all(names.x.needed %in% names(x)), 
    is.null(delay.date) | inherits(delay.date, "Date"), 
    is.null(closure.date) | inherits(closure.date, "Date"), 
    is.null(delay.region) | (delay.region %in% region.acc), 
    is.null(closure.region) | (closure.region %in% region.acc), 
    all(names(x$Region) %in% region.acc),
    is.null(method.shift) | (method.shift %in% c("pile", "lag")), 
    is.null(method.fidelity) | (method.fidelity %in% c("spatial", "temporal"))
  )
  
  # Check date values are for the 2009-10 fishing season
  if (!is.null(delay.date)) { 
    if (!between(delay.date, as.Date("2009-11-15"), as.Date("2010-10-31")))
      stop("delay.date must be for the 2009-10 fishing season, ", 
           "i.e. it must be between 2009-11-15 and 2010-10-31")
  }
  
  if (!is.null(closure.date)) { 
    if (!between(closure.date, as.Date("2009-11-15"), as.Date("2010-10-31")))
      stop("closure.date must be for the 2009-10 fishing season, ", 
           "i.e. it must be between 2010-11-15 and 2010-10-31")
  }
  
  
  # Check date-region interplay
  if (is.null(delay.date) & is.null(closure.date))
    stop("One of the date.closure input variables must not be NULL")
  
  if (!is.null(delay.date) & is.null(delay.region)) 
    stop("If there is an early closure date, ", 
         "delay.region must not be NULL")
  
  if (!is.null(closure.date) & is.null(closure.region)) 
    stop("If there is a late closure date, ", 
         "closure.region must not be NULL")
  
  
  #----------------------------------------------------------------------------
  # Process prep..
  
  # TODO: extract 'identifier' columns, which will be added back on at the end
  # TODO: extract whale information, which will be added back on at the end
  
  
  # x.other <- x.orig %>% select(-!!)
  
  x.fish <- x %>% 
    # select(fish stuff?) %>% 
    mutate(season_start_date = as.Date(paste(substr(crab_year, 1, 4), 
                                             ifelse(Region == "CenCA", "11-15", "12-01"), 
                                             sep = "-")), 
           date_record = as.Date(day_of_year - 1, origin = as.Date(paste0(year, "-01-01"))))
  
  # x.fish %>%
  #   filter(Region == "NorCA") %>%
  #   group_by(crab_year) %>%
  #   summarise(min(date_record))
  
  
  
  #----------------------------------------------------------------------------
  # Do delayed opening stuff
  browser()
  if (!is.null(delay.date)) {
    #------------------------------------------------------
    # Step 1) filter by region
    x.fish.filter <- if (delay.region == "BIA") {
      stop("BIA closure in early season is not supported yet")
    } else if (identical(delay.region, "All")) {
      x.fish
    } else {
      x.fish %>% filter(Region %in% delay.region)
    } 
    
    if (nrow(x.fish.filter) == 0) stop("Error in fishing data processing - delayed opening")
    
    
    #------------------------------------------------------
    # Step 2) determine the number of days each season would have been delayed
    delay.days.summ <- x.fish.filter %>% 
      mutate(mgmt_yr = if (year(delay.date) == 2009) substr(crab_year, 1, 4) else substr(crab_year, 6, 9),
             season_open_mgmt = as.Date(paste(mgmt_yr, month(delay.date), day(delay.date), 
                                              sep = "-"))) %>% 
      group_by(crab_year, Region) %>% 
      summarise(season_date_st = max(min(date_record), season_start_date), 
                season_open_mgmt = unique(season_open_mgmt), 
                season_days_delayed = as.numeric(difftime(season_open_mgmt, season_date_st, 
                                                          units = "days"))) %>% 
      ungroup()
    
    # Join season delay (in days), and ensure that fishing data is 
    #   (at the earliest) the season start date
    # TODO: Check about this with Jameal???
    x.fish.delay <- x.fish.filter %>% 
      left_join(delay.days.summ, by = c("crab_year", "Region")) %>% 
      # minimum 
      mutate(decimal_record = ifelse(date_record < season_date_st, 
                                     decimal_date(season_date_st), 
                                     decimal_date(date_record)), 
             date_record_old = as.Date(round_date(date_decimal(decimal_record),
                                                  unit = "day"))) %>% 
      rename(year_month_old = year_month) %>% 
      select(-decimal_record) 
    
    
    #------------------------------------------------------
    # Step 3) shift dates/times as necessary
    browser()
    x.fish.shifted <- effort_datetime_shift(x.fish.delay, method.shift) %>% 
      select(-season_start_date, -season_date_st, -season_open_mgmt,
             # -date_record_old, -year_mo_old,  
             -season_days_delayed)
    
    
    # Sanity check - all ew date records are still in the proper crab year
    check.int <- interval(
      as.Date(paste0(substr(x.fish.shifted$crab_year, 1, 4), "-11-01")), 
      as.Date(paste0(substr(x.fish.shifted$crab_year, 6, 9), "-10-31"))
    )
    if (!all(x.fish.shifted$date_record_old %within% check.int)) 
      stop("Error in date shifting - shifted out of crab season")
    rm(check.int)
    
  }
  
  
  #----------------------------------------------------------------------------
  # Do early closure stuff
  
  
  #----------------------------------------------------------------------------
  # Do spatial/temporal redistribution
  
  # # Redistribute fishing effort 
  # # TODO
  # if (method.fidelity == "temporal") {
  #   y.toshift.names <- names(y.toshift)
  #   
  #   d <- redistribute_temporal(y.toshift, Num_DCRB_VMS_pings)
  #   
  #   # y.out <- y.toshift %>% 
  #   #   select(-)
  # } else {
  #   
  # }
  
  
  #----------------------------------------------------------------------------
  # Calculate risk and aggregate by month
}




###############################################################################
# Functions for shifting date/time of effort

effort_datetime_shift <- function(y, method.shift) {
  #--------------------------------------------------------
  if (method.shift == "lag") {
    y %>% 
      mutate(days_tolag = ifelse(season_days_delayed > 0, season_days_delayed, 0), 
             date_record = date_record_old + days(days_tolag), 
             # TODO: ensure that date_record is at minimum season st date
             year_month = func_year_mo(date_record), 
             record_toshift = days_tolag > 0) %>% 
      select(-days_tolag)
    
    
    #------------------------------------------------------
  } else if (method.shift == "pile") {
    y %>% 
      mutate(diff_days = as.numeric(difftime(date_record_old, season_open_mgmt, 
                                             units = "days")), 
             days_tolag = ifelse(diff_days > 0, 0, 
                                 ifelse(season_days_delayed > 0, 
                                        season_days_delayed, 0)), 
             year_month = func_year_mo(date_record), 
             record_diff = difftime(date_record_old, date_record, unit = "days"), 
             record_toshift = record_diff != 0) %>% 
      select(-diff_days, -days_tolag, -decimal_record, -record_diff)
    
    # #####
    # y2 %>% 
    #   group_by(crab_year, Region) %>% 
    #   summarise(min(date_record), 
    #             min(season_date_st),
    #             min(date_record_old))
    # #####
    
    
    #------------------------------------------------------
  } else {
    stop("method.shift is not either 'lag' or 'pile'")
  }
}


###############################################################################
# Temporal fidelity redistribution function
redistribute_temporal <- function(z, z.col) {
  ### Input Reqs - column names of input data frame
  # GRID5KM_ID and Region: for defining redistribution areas/percentages
  # date_record: original ('actual') date of fishing effort
  # date_record_new: shifted date of fishing effort
  # z.col: symbol, column name to use for redistribution percentages
  
  ### Output: 
  # data frame with GRID5KM_ID, year_month_old, year_month (which is new year_month), 
  #   date_record_old, date_record (which is new date_record), 
  #   and DCRB info columns (with updated info)
  
  
  z.col <- enquo(z.col)
  # z.col <- "Num_DCRB_VMS_pings_sum"
  
  ### Input checks
  z.cols.nec <- c(
    "GRID5KM_ID", "Region", "year_month_old", "year_month"
  )
  
  z.cols.dcrb <- c(
    "DCRB_lbs", "DCRB_rev", "Num_DCRB_VMS_pings", "Num_DCRB_Vessels", 
    "Num_Unique_DCRB_Vessels"
  )
  
  stopifnot(
    quo_name(z.col) %in% z.cols.dcrb,
    all(z.cols.nec %in% names(z)),
    all(z.cols.dcrb %in% names(z))
  )
  
  ### Process
  z <- z %>% select(!!z.cols.nec, !!z.cols.dcrb) 
  
  z.old.eff.sum <- z %>% 
    group_by(Region, year_month_old) %>% 
    summarise(DCRB_lbs_sum = sum(DCRB_lbs), 
              DCRB_rev_sum = sum(DCRB_rev), 
              Num_DCRB_VMS_pings_sum = sum(Num_DCRB_VMS_pings), 
              Num_DCRB_Vessels_sum = sum(Num_DCRB_Vessels), 
              Num_Unique_DCRB_Vessels_sum = sum(Num_Unique_DCRB_Vessels), 
              eff_sum = sum(!!z.col)) %>%
    ungroup()
  
  z %>% 
    left_join(z.old.eff.sum, by = c("Region", "year_month_old")) %>% 
    mutate(eff_percent = Num_DCRB_VMS_pings / eff_sum, 
           DCRB_lbs_new = DCRB_lbs_sum * eff_percent,
           DCRB_rev_new = DCRB_rev_sum * eff_percent, 
           Num_DCRB_VMS_pings_new = Num_DCRB_VMS_pings_sum * eff_percent, 
           Num_DCRB_Vessels_new = Num_DCRB_Vessels_sum * eff_percent, 
           Num_Unique_DCRB_Vessels_new = Num_Unique_DCRB_Vessels_sum * eff_percent) %>% 
    select(GRID5KM_ID, Region, 
           year_month_old, year_month, 
           DCRB_lbs = DCRB_lbs_new, 
           DCRB_rev = DCRB_rev_new, 
           Num_DCRB_VMS_pings = Num_DCRB_VMS_pings_new,
           Num_DCRB_Vessels = Num_DCRB_Vessels_new, 
           Num_Unique_DCRB_Vessels = Num_Unique_DCRB_Vessels_new)
}


###############################################################################
# Random helpers
func_year_mo <- function(x) {
  stopifnot(
    inherits(x, "Date"), 
    require(lubridate)
  )
  paste(year(x), sprintf("%02d", month(x)), sep = "_")
}

###############################################################################
