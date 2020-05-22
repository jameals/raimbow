###############################################################################
# Input format for x: 'CA_DCRB_vms_fishing_daily_2009-2018_fishtix_humpback_blue_whales_grids.RDS'

# See 'Mgmt_testing_...' files for sample code

# Implement effect of management scenarios on fishing effort
# See 'Readme for Funcs_management_scenarios.docx' for pseudocode
effort_mgmt <- function(x, early.data.method = c("pile", "remove"), 
                        delay.date = NULL, 
                        delay.region = c("All", "CenCA", "NorCA"), 
                        delay.method = c("remove", "pile", "lag"), 
                        delay.method.fidelity = c("spatial", "temporal"), 
                        closure.date = NULL, 
                        closure.region = c("All", "CenCA", "NorCA", "BIA"), 
                        closure.method = c("remove", "temporal"), 
                        closure.redist.percent = 100, 
                        reduction.before.date = NULL, reduction.before.percent = 50, 
                        reduction.after.date = NULL, reduction.after.percent = 50) {
  
  # TODO: Allow for percent reduction (without fully closing areas) in both delayed opening and early closure scenarios
  #   Ideally this could be used in conjuncture with delayed opening and early closures
  # TODO: add "OR", "WA", CA-SCen", to regions
  # TODO: some way to define season_st_date_min in input, i.e. not hard-coded
  # TODO: how to determine 'natural season' start date?
  
  ### Inputs
  # x: data.frame; expected to has same format at data frame from Jameal
  # early.data.method: character; either "pile" or "remove". Represents what to
  #   to do with data that comes before minimum season start date, 
  #   e.g. data that comes before 15 Nov in Central CA
  # delay.date: Date; date for which the fishery will open in 2009-10 crab season.
  #   If NULL, then there is no delayed opening
  # delay.region: character; if not NULL, then one of "All", "CenCA", "BIA"
  # delay.method: character; if not NULL, either "remove", "pile", or "lag"
  # delay.method.fidelity: character; method of redistribution. 
  #   if used, either "spatial" (fidelity) or "temporal" (fidelity). 
  #   Ignored if delay.method = "remove"
  # closure.date: Date; date for which the fishery will close in 2009-10 crab season
  #   If NULL, then there is no early (e.g. spring) closure
  # closure.region: character; see 'delay.region'
  # closure.method: character; if not NULL, then either "remove" or "temporal (fidelity)
  # closure.redist.percent: numeric; default is 100. If used, 
  #   percentage of effort to redistribute that is kept
  # reduction.before.date: Date; if not NULL, then all effort values before this date
  #   are multiplied by (reduction.before.percent / 100)
  # reduction.before.percent: numeric; between 0 and 100. Ignored if reduction.before.date is NULL
  # reduction.after.date: Date; if not NULL, then all effort values after this date
  #   are multiplied by (reduction.after.percent / 100)
  # reduction.after.percent: numeric; between 0 and 100. Ignored if reduction.after.date is NULL
  
  
  ### Output
  # Data frame with simulated shifts in effort for a scenario and the following columns added:
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
    inherits(closure.redist.percent, c("numeric", "integer")), 
    dplyr::between(closure.redist.percent, 0, 100), 
    is.null(reduction.before.date) | inherits(reduction.before.date, "Date"), 
    is.null(reduction.after.date) | inherits(reduction.after.date, "Date"), 
    inherits(reduction.before.percent, c("numeric", "integer")), 
    inherits(reduction.after.percent, c("numeric", "integer")), 
    dplyr::between(reduction.before.percent, 0, 100), 
    dplyr::between(reduction.after.percent, 0, 100)
  )
  
  early.data.method <- match.arg(early.data.method)
  if (!is.null(delay.date)) delay.region <- match.arg(delay.region)
  if (!is.null(delay.date)) delay.method <- match.arg(delay.method)
  if (!is.null(delay.date)) delay.method.fidelity <- match.arg(delay.method.fidelity)
  if (!is.null(closure.date)) closure.region <- match.arg(closure.region)
  if (!is.null(closure.date)) closure.method <- match.arg(closure.method)
  
  
  # Data frame name checks
  names.x.fish <- c(
    "crab_year", "year_month", "day_of_year", "GRID5KM_ID", "Region", 
    "BIA_bm_noNAs", "BIA_mn_noNAs", "BIA_bm_or_mn", 
    "DCRB_lbs", "DCRB_rev", "Num_DCRB_VMS_pings", 
    "Num_DCRB_Vessels", "Num_Unique_DCRB_Vessels"
  )
  if (!all(names.x.fish %in% names(x)))
    stop("x does not contain all required columns:\n", 
         paste(names.x.fish, collapse = ", "))
  
  
  # Check dates - other checks are done by match.arg
  if (is.null(delay.date) & is.null(closure.date)) 
    message("Both delay.date and closure.date are NULL, ", 
            "and thus only 'early data' will be shifted")
  
  if (!is.null(delay.date) & !is.null(closure.date)) 
    if (delay.date >= closure.date)
      stop("delay.date must come before closure.date")
  
  
  # Check date values are for the 2009-10 fishing season
  if (!is.null(delay.date)) {
    if (!between(delay.date, as.Date("2009-11-01"), as.Date("2010-10-31")))
      stop("delay.date must be for the 2009-10 fishing season, ",
           "meaning it must be between 2009-11-01 (1 Nov 2009) ", 
           "and 2010-10-31 (31 October 2010)")
  }

  if (!is.null(closure.date)) {
    if (!between(closure.date, as.Date("2009-11-01"), as.Date("2010-10-31")))
      stop("closure.date must be for the 2009-10 fishing season, ",
           "meaning it must be between 2009-11-01 (1 Nov 2009) ", 
           "and 2010-10-31 (31 October 2010)")
  }
  
  
  #----------------------------------------------------------------------------
  ### Initial processing
  # Extract 'constant' data. Currently 'CA_OFFSHOR' and 'depth'
  names.constant <- c("CA_OFFSHOR", "depth")
  if (all(names.constant %in% names(x))) {
    x.other <- x %>%
      select(.data$GRID5KM_ID, !!names.constant) %>%
      distinct()
    
    if (any(duplicated(x.other$GRID5KM_ID)))
      stop("Error extracting constant data - duplicated grid cell IDs")
  }
  
  # Select for and do initial processing of effort data - shift or drop early data
  # browser() # baby steps for JS to understand function
  x.fish.pre <- x %>% 
    select(!!names.x.fish) %>%
    mutate(tmp = ifelse(Region == "CenCA", "-11-15", "-12-01"), # JS: we want to define this above so it is easy to modify
           season_date_st_min = as.Date(paste0(substr(crab_year, 1, 4), tmp)), 
           year = as.numeric(substr(year_month, 1, 4)), 
           date_record_orig = as.Date(
             day_of_year - 1, origin = as.Date(paste0(year, "-01-01")))) %>% 
    select(-.data$tmp)
  
  
  if (early.data.method == "pile") {
    x.fish <- x.fish.pre %>% 
      mutate(decimal_record = ifelse(date_record_orig < season_date_st_min, 
                                     decimal_date(season_date_st_min), 
                                     decimal_date(date_record_orig)), 
             date_record = as.Date(
               round_date(date_decimal(decimal_record), unit = "day")), 
             year_month = func_year_mo(date_record)) %>% 
      select(-decimal_record)
    
  } else if (early.data.method == "remove") {
    x.fish <- x.fish.pre %>% 
      filter(date_record_orig >= season_date_st_min) %>% 
      mutate(date_record = date_record_orig)
    
  } else {
    stop("Invalid argument passed to early.data.method")
  }
  
  x.fish <- x.fish %>% 
    select(-year, -day_of_year, -date_record_orig, -season_date_st_min)
  
  
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
    warning("In original data, some season end dates come after the region end dates")
  
  
  #####
  # # With pile method for early.data.method
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
             season_open_mgmt = as.Date(paste(mgmt_yr, lubridate::month(delay.date), day(delay.date), 
                                              sep = "-"))) %>% 
      group_by(crab_year, Region) %>% 
      summarise(season_date_st = min(date_record), #Ok since date_records altered above
                season_open_mgmt = unique(season_open_mgmt), 
                season_days_delayed = as.numeric(difftime(season_open_mgmt, season_date_st, 
                                                          units = "days"))) %>% 
      ungroup()
    
    
    #------------------------------------------------------
    # Step 3) shift dates/times as necessary, or remove records from "remove"
    x.fish.shifted <- delay_date_shift(
      left_join(x.d.fish.filter, season.mgmt.summ, by = c("crab_year", "Region")), 
      delay.method
    )
    
    
    #------------------------------------------------------
    # Step 4) redistribute effort using spatial or temporal fidelity
    x.fish.redist <- if (delay.method == "remove") {
      x.fish.shifted %>% select(-starts_with("season_")) 
    } else if (delay.method.fidelity == "temporal") {
      redistribute_temporal(x.fish.shifted, Num_DCRB_VMS_pings, "delay", delay.region)
    } else if (delay.method.fidelity == "spatial") {
      x.fish.shifted %>% select(-starts_with("season_")) 
    } else {
      stop("Unrecognized value passed to delay.method.fidelity")
    } 
    
    # select for instead of remove because redistribute_temporal removes some columns
    x.fish.redist <- x.fish.redist %>% 
      select(crab_year, year_month, date_record, GRID5KM_ID, Region, 
             BIA_bm_noNAs, BIA_mn_noNAs, BIA_bm_or_mn, 
             DCRB_lbs, DCRB_rev, Num_DCRB_VMS_pings, Num_DCRB_Vessels, 
             Num_Unique_DCRB_Vessels)
    
    # If else is in case region was "All"
    x.fish.delay <- if (is.null(x.d.fish.nofilter)) {
      x.fish.redist
    } else {
      x.d.fish.nofilter %>% 
        select(!!names(x.fish.redist)) %>% 
        bind_rows(x.fish.redist) %>% 
        arrange(crab_year, Region, day(date_record), GRID5KM_ID)
    }
    
    # Sanity check - all new date records are still in the proper crab year
    check.int <- interval(
      as.Date(paste0(substr(x.fish.delay$crab_year, 1, 4), "-11-01")), 
      as.Date(paste0(substr(x.fish.delay$crab_year, 6, 9), "-10-31"))
    )
    if (!all(x.fish.delay$date_record %within% check.int)) 
      warning("Error in delayed opening date shifting - shifted out of crab season")
    
    rm(check.int, x.fish.shifted, season.mgmt.summ, x.fish.redist, 
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
             season_close_mgmt = as.Date(paste(mgmt_yr, lubridate::month(closure.date), day(closure.date), 
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
        select(-season_close_mgmt, -record_post_closure_date, 
               -date_record_old, -year_month_old, 
               -record_closed, -region_toshiftto)
      
    } else if (closure.method == "temporal") {
      x.fish.closure <- x.fish.c2 %>% 
        mutate(record_toshift = record_closed, 
               record_base = !record_toshift & record_post_closure_date) %>% 
        redistribute_temporal(Num_DCRB_VMS_pings, "closure", closure.region, 
                              closure.redist.percent) # function below
    }
    
    rm(x.fish.c1, x.fish.c2)
  }
  
  
  #----------------------------------------------------------------------------
  # Final checks and formatting
  if (exists("x.other")) {
    x.fish.closure <- x.fish.closure %>% 
      left_join(x.other, by = "GRID5KM_ID") %>% 
      select(crab_year, year_month, date_record, 
             GRID5KM_ID, Region, CA_OFFSHOR, depth, everything())
  }
  
  x.out <- x.fish.closure %>% 
    # left_join(x.other, by = "GRID5KM_ID") %>% 
    # select(crab_year, GRID5KM_ID, Region, CA_OFFSHOR, everything()) %>% 
    select(crab_year, year_month, date_record, GRID5KM_ID, Region, 
           everything()) %>% #just in case
    left_join(x.fish.end.summ, by = c("crab_year", "Region")) %>% 
    mutate(date_past_season_end = date_record > season_date_end, 
           date_past_region_end = date_record > region_date_end, 
           year = year(date_record), 
           month_as_numeric = lubridate::month(date_record), 
           month = factor(lubridate::month(date_record, label = TRUE, abbr = FALSE), 
                          levels = levels(x$month)), 
           day_of_year = yday(date_record))
  
  if (nrow(x.out) != nrow(x.fish.closure))
    stop("Error - addition of extra rows when joining additional information")
  
  x.out
}


###############################################################################
###############################################################################
# Function for shifting date/time of effort
delay_date_shift <- function(y, delay.method) {
  if (delay.method == "remove") {
    y %>% filter(date_record >= season_open_mgmt)
    
  } else if (delay.method == "lag") {
    y %>% 
      # rename(year_month_old = year_month, date_record_old = date_record) %>% 
      mutate(year_month_old = year_month, 
             date_record_old = date_record, 
             days_tolag = ifelse(season_days_delayed > 0, season_days_delayed, 0), 
             date_record = date_record_old + days(days_tolag), 
             year_month = func_year_mo(date_record))
    
  } else if (delay.method == "pile") {
    y %>% 
      # rename(year_month_old = year_month, date_record_old = date_record) %>% 
      mutate(year_month_old = year_month, 
             date_record_old = date_record, 
             diff_days = as.numeric(difftime(date_record_old, season_open_mgmt, 
                                             units = "days")), 
             days_tolag = ifelse(diff_days > 0, 0, 
                                 ifelse(season_days_delayed > 0, 
                                        season_days_delayed, 0)), 
             
             date_record = date_record_old + days(days_tolag), 
             year_month = func_year_mo(date_record)) %>% 
      select(-diff_days)
    
  } else {
    stop("delay.method is not one of 'lag', 'pile', or 'remove'")
  }
}


###############################################################################
# Temporal fidelity redistribution function
redistribute_temporal <- function(z, z.col, z.type = c("delay", "closure"), 
                                  z.reg = c("CenCA", "NorCA", "BIA", "All"), 
                                  z.perc = 100) {
  ### Inputs
  # z: data frame; Contains columns described in z.cols.nec
  # z.col: symbol, column name to use for redistribution percentages
  # z.type: character; either "delay" or "closure"
  # z.reg: character; region that effort is being shifted out of
  # z.perc: numeric; default is 100. Percentage multiplier for effort
  #   to be redistributed
  
  ### Output: 
  # A data frame with fishing effort data redistributed temporally  
  
  z.col <- enquo(z.col)
  
  #--------------------------------------------------------
  ### Input checks
  z.type <- match.arg(z.type)
  z.reg <- match.arg(z.reg)
  
  # Necessary column names
  z.cols.nec <- c(
    "crab_year", "GRID5KM_ID", "Region", "date_record", "year_month"
  )
  z.cols.nec2 <- c("date_record_old", "year_month_old") #columns not included in the output
  
  z.cols.dcrb <- c(
    "DCRB_lbs", "DCRB_rev", "Num_DCRB_VMS_pings", "Num_DCRB_Vessels", 
    "Num_Unique_DCRB_Vessels"
  )
  
  z.cols.other <- c("BIA_bm_noNAs", "BIA_mn_noNAs", "BIA_bm_or_mn")
  
  stopifnot(
    all(c(z.cols.nec, z.cols.nec2, z.cols.dcrb, z.cols.other) %in% names(z)),
    # z.type %in% c("delay", "closure"), 
    # z.reg %in% c("CenCA", "NorCA", "BIA", "All"), 
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
    
    z.use <- z %>% select(!!z.cols.nec, !!z.cols.nec2, !!z.cols.dcrb, !!z.closure.cols)
    
  } else if (z.type == "delay") {
    z.delay.cols <- c("days_tolag", "season_open_mgmt")
    stopifnot(all(z.delay.cols %in% names(z)))
    
    z.use <- z %>% 
      select(!!z.cols.nec, !!z.cols.nec2, !!z.cols.dcrb, !!z.delay.cols) %>%
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
  
  if (z.reg == "BIA" & nrow(z.redist.nobase) > 0)
    warning("Some effort being redistributed out of the BIAs does not have ", 
            "a base for redistribution - ", 
            "this will result in an incorrect output file")
  
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
  # c) Ensure that order of (remaining) columns is the same as the input
  z.new.names <- base::intersect(names(z), c(z.cols.nec, z.cols.dcrb, z.cols.other))
  z.new <- bind_rows(z.redist.base, z.tostay, z.redist.nobase) %>% 
    arrange(crab_year, Region, day(date_record), GRID5KM_ID) %>%
    group_by(crab_year, GRID5KM_ID, Region, year_month, date_record) %>%
    summarise(DCRB_lbs = sum(DCRB_lbs),
              DCRB_rev = sum(DCRB_rev),
              Num_DCRB_VMS_pings = sum(Num_DCRB_VMS_pings),
              Num_DCRB_Vessels = sum(Num_DCRB_Vessels),
              Num_Unique_DCRB_Vessels = sum(Num_Unique_DCRB_Vessels)) %>% 
    ungroup() %>% 
    left_join(distinct(select(z, GRID5KM_ID, !!z.cols.other)), 
              by = c("GRID5KM_ID")) %>% 
    select(!!z.new.names)
  
  
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
