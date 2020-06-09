###############################################################################
# Input format for x: 'CA_DCRB_vms_fishing_daily_2009-2018_fishtix_humpback_blue_whales_grids.RDS'

# See 'Mgmt_testing_...' files for sample code

# Implement effect of management scenarios on fishing effort
# See 'Readme for Funcs_management_scenarios.docx' for pseudocode
# Note that this function does NOT include normalized effort in the output
effort_mgmt <- function(x, season.st.key = NULL, preseason.days = 3, 
                        season.st.backstop = NULL, 
                        early.data.method = c("pile", "remove"), 
                        delay.date = NULL, 
                        delay.region = c("All", "CenCA", "NorCA", "OR", "WA"), 
                        delay.method = c("remove", "pile", "lag", "depth"), 
                        delay.method.fidelity = c("spatial", "temporal"), 
                        closure.date = NULL, 
                        closure.region = c("All", "CenCA", "NorCA", "BIA", "OR", "WA"), 
                        closure.method = c("remove", "temporal", "depth"), 
                        closure.redist.percent = 100, 
                        depth.shallow = NULL, depth.deep = NULL, 
                        reduction.before.date = NULL, 
                        reduction.before.percent = 50, 
                        reduction.before.region = c("All", "CenCA", "NorCA", "BIA", "OR", "WA"), 
                        reduction.after.date = NULL, 
                        reduction.after.percent = 50, 
                        reduction.after.region = c("All", "CenCA", "NorCA", "BIA", "OR", "WA")) {
  
  ### Inputs
  # x: data.frame; expected to have same format at data frame from Jameal
  # preseason.days: numeric; number of days before the provided season start date
  #   definied by season.st.key for which effort will be kept, i.e. not affected by early.data.method.
  #   For instance, is 3, than all data up to 3 days before the season start is kept, 
  #   and not processed by early.data.method
  # season.st.key: data frame with 3 columns; specifies the season start dates for each region. 
  #   The columns must be (in order): crab year, region, and start date. Names don't matter
  #   Data before these dates are handled as specified in early.data.method
  #   If NULL, then the season start dates are determined as 
  #   the first day with histroical effort (from x) in that region for that crab year
  # season.st.backstop: data frame with the same format as season.st.key.
  #   This data serves as a 'backstop' minimum season start date. 
  #   A message is printed if any dates from season.st.key are before the corresponding backstop date, 
  #   and the corresponding backstop date is used in place of the season.st.key date.
  #   If NULL, these minimum values default to: Nov 10 for CenCA, and Nov 26 for NorCA.
  # early.data.method: character; either "pile" or "remove". Represents what to
  #   to do with data that comes before minimum season start date, 
  #   specified by season.st.key and season.st.backstop
  # delay.date: Date; date for which the fishery will open in 2009-10 crab season.
  #   If NULL, then there is no delayed opening and 
  #   other delay.. arguments are ignored
  # delay.region: character; one of options listed above. Ignored if delay.date is NULL
  # delay.method: character; one of options listed above. Ignored if delay.date is NULL
  # delay.method.fidelity: one of options listed above. Ignored (only) if delay.date is NULL 
  # closure.date: Date; date for which the fishery will close in 2009-10 crab season
  #   If NULL, then there is no early (e.g. spring) closure and 
  #   other 'closure...' arguments are ignored
  # closure.region: character; one of options listed above. Ignored if closure.date is NULL
  # closure.method: character; one of options listed above. Ignored if closure.date is NULL
  # closure.redist.percent: numeric; default is 100. 
  #   Ignored if closure.method is not "temporal". Otherwise, 
  #   values being redistributed are multiplied by (closure.redist.percent / 100) 
  # depth.shallow and depth.deep: numerics; if either delay.method or closure.method is "depth", 
  #   then 1) both values cannot be NULL and 2) all effort between these values 
  #   (code: `between(depth, depth.deep, depth.shallow)`) will be KEPT
  #   If one value is NULL, then that value is 'ignored', 
  #   i.e it is set as the applicable min/max extreme from the data
  #   These values are both in meters, as is the depth for the fishing effort data.
  # reduction.before.date: Date; if not NULL, then all effort values before this date
  #   are multiplied by (1 - (reduction.before.percent / 100))
  # reduction.before.percent: numeric; between 0 and 100. Ignored if reduction.before.date is NULL
  # reduction.before.region: character; one of options listed above. 
  #   Ignored if reduction.before.date is NULL
  # reduction.after.date: Date; if not NULL, then all effort values after this date
  #   are multiplied by (1- (reduction.after.percent / 100))
  # reduction.after.percent: numeric; between 0 and 100. Ignored if reduction.after.date is NULL
  # reduction.after.region: character; one of options listed above. 
  #   Ignored if reduction.after.date is NULL
  
  
  ### Output
  # Data frame with simulated shifts in effort for a scenario and the following columns added:
  #   date_record: class Date, date of effort
  #   date_past_season_end: logical; indicates if date_record is past the 
  #     original end date for this Region in this crab year
  #   date_past_region_end: logical; indicates if date_record is past the 
  #     lawful end date (July 15 for central CA and July 31 otherwise) 
  #     for this Region in this crab year
  
  #browser()
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
    "season.st.key must be NULL or a data frame with 3 columns" = 
      is.null(season.st.key) | inherits(season.st.key, "data.frame") & ncol(season.st.key) == 3, 
    inherits(preseason.days, c("numeric", "integer")), 
    preseason.days >= 0,
    "season.st.backstop must be NULL or a data frame with 3 columns" = 
      is.null(season.st.backstop) | inherits(season.st.backstop, "data.frame") & ncol(season.st.backstop) == 3, 
    "delay.date must be NULL or a Date object of length 1" = 
      is.null(delay.date) | (inherits(delay.date, "Date") & length(delay.date) == 1), 
    "closure.date must be NULL or a Date object of length 1" = 
      is.null(closure.date) | (inherits(closure.date, "Date") & length(closure.date) == 1), 
    inherits(closure.redist.percent, c("numeric", "integer")), 
    "closure.redist.percent must a numeric of length 1 between 0 and 100" = 
      length(closure.redist.percent) == 1 & dplyr::between(closure.redist.percent, 0, 100), 
    "reduction.before.date must be NULL or a Date object of length 1" = 
      is.null(reduction.before.date) | (inherits(reduction.before.date, "Date") & length(reduction.before.date) == 1), 
    "reduction.after.date must be NULL or a Date object of length 1" = 
      is.null(reduction.after.date) | (inherits(reduction.after.date, "Date") & length(reduction.after.date) == 1), 
    inherits(reduction.before.percent, c("numeric", "integer")), 
    inherits(reduction.after.percent, c("numeric", "integer")), 
    "reduction.before.percent must a numeric of length 1 between 0 and 100" = 
      length(reduction.before.percent) == 1 & dplyr::between(reduction.before.percent, 0, 100), 
    "reduction.after.percent must a numeric of length 1 between 0 and 100" = 
      length(reduction.after.percent) == 1 & dplyr::between(reduction.after.percent, 0, 100), 
    is.null(depth.shallow) | inherits(depth.shallow, c("integer", "numeric")), 
    is.null(depth.deep) | inherits(depth.deep, c("integer", "numeric"))
  )
  
  early.data.method <- match.arg(early.data.method)
  if (!is.null(delay.date)) delay.region <- match.arg(delay.region, several.ok = TRUE)
  if (!is.null(delay.date)) delay.method <- match.arg(delay.method)
  if (!is.null(delay.date)) delay.method.fidelity <- match.arg(delay.method.fidelity)
  if (!is.null(closure.date)) closure.region <- match.arg(closure.region, several.ok = TRUE)
  if (!is.null(closure.date)) closure.method <- match.arg(closure.method)
  if (!is.null(reduction.before.date)) 
    reduction.before.region <- match.arg(reduction.before.region, several.ok = TRUE)
  if (!is.null(reduction.after.date)) 
    reduction.after.region <- match.arg(reduction.after.region, several.ok = TRUE)
  
  
  # Data frame name checks
  names.x.fish <- c(
    "crab_year", "year_month", "day_of_year", "GRID5KM_ID", "Region", "depth", 
    "BIA_bm_noNAs", "BIA_mn_noNAs", "BIA_bm_or_mn", 
    "DCRB_lbs", "DCRB_rev", "Num_DCRB_VMS_pings", 
    "Num_DCRB_Vessels", "Num_Unique_DCRB_Vessels"
  )
  if (!all(names.x.fish %in% names(x)))
    stop("x does not contain all required columns:\n", 
         paste(names.x.fish, collapse = ", "))
  
  eff.regions <- c("CenCA", "NorCA", "OR", "WA")
  if (!all(x$Region %in% eff.regions))
    stop("All effort data must be in the following regions\n:", 
         paste(eff.regions, collapse = ", "))
  
  if (identical(delay.method, "depth") | identical(closure.method, "depth")) {
    if (is.null(depth.shallow)) depth.shallow <- max(x$depth)
    if (is.null(depth.deep)) depth.deep <- min(x$depth)
    if (!(depth.shallow <= 0 & depth.deep < depth.shallow))
      stop("depth.shallow and depth.deep must be less than 0,", 
           "and depth.deep must be less than depth.shallow", 
           "(i.e. have a greater absolute value)")
    
    if (!any(x$depth < depth.shallow))
      warning("No depth values will be removed with the given depth.shallow value", 
              immediate. = TRUE)
  }
  
  
  # Check that reduction and non-depth methods are not being used at the same time  
  if ((!is.null(delay.date) & !identical(delay.method, "depth")) & !is.null(reduction.before.date))
    stop("Effort reduction before some date and a delayed opening cannot be used ", 
         "simultaneously unless delay.method is 'depth'")
  
  if ((!is.null(closure.date) & !identical(closure.method, "depth")) & !is.null(reduction.after.date))
    stop("Effort reduction after some date and an early closure cannot be used ", 
         "simultaneously unless closure.method is 'depth'")
  
  
  # Checks delay/closure dates relative to each other
  if (is.null(delay.date) & is.null(closure.date) & is.null(reduction.before.date) & 
      is.null(reduction.after.date)) 
    message("All of delay.date, closure.date, reduction.before.date, ", 
            "and reduction.after.date are NULL, ", 
            "and thus only 'early data' will be shifted")
  
  if (!is.null(delay.date) & !is.null(closure.date)) 
    if (delay.date >= closure.date)
      stop("delay.date must come before closure.date")
  
  # Checks that non-NULL dates are within the correct ranges
  date.message.common <- paste(
    "must be for the 2009-10 fishing season,",
    "meaning it must be between 2009-11-01 (1 Nov 2009)", 
    "and 2010-10-31 (31 October 2010)"
  )
  
  if (!is.null(delay.date))
    if (!between(delay.date, as.Date("2009-11-01"), as.Date("2010-10-31")))
      stop("delay.date ", date.message.common)
  
  if (!is.null(closure.date))
    if (!between(closure.date, as.Date("2009-11-01"), as.Date("2010-10-31")))
      stop("closure.date ", date.message.common)
  
  if (!is.null(reduction.before.date))
    if (!between(reduction.before.date, as.Date("2009-11-01"), as.Date("2010-10-31")))
      stop("reduction.before.date ", date.message.common)
  
  if (!is.null(reduction.after.date))
    if (!between(reduction.after.date, as.Date("2009-11-01"), as.Date("2010-10-31")))
      stop("reduction.after.date ", date.message.common)
  
  rm(date.message.common)
  
  #browser()
  #----------------------------------------------------------------------------
  # Initial processing - extract 'constant' data
  # Currently 'CA_OFFSHOR' - depth required due to depth restriction method
  names.constant <- c("CA_OFFSHOR")
  if (all(names.constant %in% names(x))) {
    x.other <- x %>%
      select(.data$GRID5KM_ID, !!names.constant) %>%
      distinct()
    
    if (any(duplicated(x.other$GRID5KM_ID)))
      stop("Error extracting constant data - duplicated grid cell IDs")
  }
  
  
  #----------------------------------------------------------------------------
  # Get season start dates, adjust early data as necessary
  
  x.crabyear <- sort(unique(x$crab_year))
  x.region <- sort(unique(x$Region))
  
  ### Create date column for fishing effort
  x.fish.pre <- x %>% 
    select(!!names.x.fish) %>%
    mutate(yr_origin = make_date(substr(.data$year_month, 1, 4), 1, 1), 
           #`day_of_year - 1` because origin is considered day 1, not day 0
           date_record = as.Date(.data$day_of_year - 1, origin = yr_origin)) %>% 
    select(-.data$yr_origin)
  
  
  ### Process season start date key
  if (is.null(season.st.key)) {
    # Default is minimum date (by crab year and Region) in fishing data
    #   Here we're already at the min date, so subtracting days is extra
    season.st.key <- x.fish.pre %>% 
      group_by(crab_year, Region) %>% 
      summarise(season_st_date_key = min(date_record) - lubridate::days(preseason.days),
                .groups = "drop")
    
  } else {
    stopifnot(
      "All of the crab years in x must be in the first column of season.st.key" = 
        all(x.crabyear %in% season.st.key[[1]]), 
      "All of the regions in x must be in the second column of season.st.key" = 
        all(x.region %in% season.st.key[[2]]), 
      "The third column of season.st.key must be of class Date" = 
        inherits(season.st.key[[3]], "Date")
    )
    season.st.key <- season.st.key %>% 
      select(crab_year = 1, Region = 2, season_st_date_key = 3) %>% 
      filter(crab_year %in% x.crabyear, Region %in% x.region) %>% 
      mutate(season_st_date_key = season_st_date_key - lubridate::days(preseason.days)) # this is the source of the error msg
  }
  
  
  ### Process season start backstop key
  if (is.null(season.st.backstop)) {
    # Default is Nov 10 for CenCA, and Nov 26 otherwise
    season.st.backstop <- data.frame(expand.grid(crab_year = x.crabyear, Region = x.region, 
                                                 stringsAsFactors = FALSE)) %>% 
      mutate(season_st_yr = as.numeric(substr(.data$crab_year, 1, 4)), 
             season_st_day = ifelse(.data$Region == "CenCA", 10, 26), 
             season_st_date_min = make_date(season_st_yr, 11, season_st_day)) %>% 
      select(crab_year, Region, season_st_date_min)
    
  } else {
    stopifnot(
      "All of the crab years in x must be in the first column of season.st.backstop" = 
        all(x.crabyear %in% season.st.backstop[[1]]), 
      "All of the regions in x must be in the second column of season.st.backstop" = 
        all(x.region %in% season.st.backstop[[2]]), 
      "The third column of season.st.backstop must be of class Date" = 
        inherits(season.st.backstop[[3]], "Date")
    )
    season.st.backstop <- season.st.backstop %>% 
      select(crab_year = 1, Region = 2, season_st_date_min = 3) %>% 
      filter(crab_year %in% x.crabyear, Region %in% x.region)
  }
  
  
  ### Chose maximum date of season.st.key and season.st.backstop to be the used season start date
  stopifnot(nrow(season.st.key) == nrow(season.st.backstop))
  season.st <- season.st.key %>% 
    left_join(season.st.backstop, by = c("crab_year", "Region")) %>% 
    mutate(season_date_st = pmax(.data$season_st_date_key, .data$season_st_date_min)) %>% 
    select(crab_year, Region, season_date_st)
  
  
  ### Pile or remove early data
  x.fish <- if (early.data.method == "pile") {
    x.fish.pre %>% 
      left_join(season.st, by = c("crab_year", "Region")) %>% 
      mutate(date_record = pmax(date_record, season_date_st), 
             year_month = func_year_mo(date_record)) %>% 
      select(-day_of_year)
    
  } else if (early.data.method == "remove") {
    x.fish.pre %>% 
      left_join(season.st, by = c("crab_year", "Region")) %>% 
      filter(date_record >= season_date_st) %>% 
      select(-day_of_year)
    
  } else {
    stop("Invalid argument passed to early.data.method")
  }
  rm(x.fish.pre)
  
  
  # For each crab season and Region, get end dates. Joined to df at the end
  x.fish.end.summ <- x.fish %>% 
    group_by(crab_year, Region) %>% 
    summarise(season_date_end = max(date_record), 
              region_date_end = make_date(
                substr(unique(crab_year), 6, 9), 7 ,
                ifelse(unique(Region) == "CenCA", 15, 31)), 
              .groups = "drop")
  
  if (any(x.fish.end.summ$season_date_end > x.fish.end.summ$region_date_end))
    warning("In original data, some season end dates come after the region end dates", 
            immediate. = TRUE)
  
  # Sanity check
  stopifnot(
    identical(c("crab_year", "year_month", "GRID5KM_ID", "Region", "depth", 
                "BIA_bm_noNAs", "BIA_mn_noNAs", "BIA_bm_or_mn", "DCRB_lbs", 
                "DCRB_rev", "Num_DCRB_VMS_pings", "Num_DCRB_Vessels", 
                "Num_Unique_DCRB_Vessels", "date_record", "season_date_st"), 
              names(x.fish))
  )
  
  
  #----------------------------------------------------------------------------
  # Do percent reduction - early season
  if (is.null(reduction.before.date)) {
    x.fish.delay.reduct <- x.fish
    
  } else {
    x.fish.delay.reduct <- x.fish %>% 
      mutate(reduce_yr = func_year_extract(reduction.before.date, crab_year), 
             reduce_mgmt = make_date(reduce_yr, lubridate::month(reduction.before.date), 
                                     day(reduction.before.date)), 
             reduce_date = date_record < reduce_mgmt)
    
    if ("All" %in% reduction.before.region) {
      if (!identical(reduction.before.region, "All"))
        stop("You cannot use 'All' in conjunction with other regions in reduction.before.region")
      
      x.fish.delay.reduct <- x.fish.delay.reduct %>% 
        mutate(reduce_region = TRUE)
      
    } else {
      x.fish.delay.reduct <- x.fish.delay.reduct %>% 
        mutate(reduce_region = Region %in% reduction.before.region)
    }
    
    x.fish.delay.reduct <- x.fish.delay.reduct %>% 
      mutate(rec = reduce_region & reduce_date, 
             rec_dec = 1 - (reduction.before.percent / 100), 
             DCRB_lbs = ifelse(rec, DCRB_lbs * rec_dec, DCRB_lbs), 
             DCRB_rev = ifelse(rec, DCRB_rev * rec_dec, DCRB_rev), 
             Num_DCRB_VMS_pings = ifelse(rec, Num_DCRB_VMS_pings * rec_dec, Num_DCRB_VMS_pings), 
             Num_DCRB_Vessels = ifelse(rec, Num_DCRB_Vessels * rec_dec, Num_DCRB_Vessels), 
             Num_Unique_DCRB_Vessels = ifelse(rec, Num_Unique_DCRB_Vessels * rec_dec, Num_Unique_DCRB_Vessels)) %>% 
      select(-reduce_yr, -reduce_mgmt, -reduce_date, -reduce_region, -rec, -rec_dec)
  }
  
  #----------------------------------------------------------------------------
  # Do delayed opening stuff
  if (is.null(delay.date)) {
    x.fish.delay <- x.fish.delay.reduct %>% 
      select(-season_date_st) %>% 
      select(crab_year, year_month, date_record, everything())
    
  } else {
    #------------------------------------------------------
    # Step 1) filter by region
    if ("BIA" %in% delay.region) {
      stop("A delayed opening in BIAs is not supported")
      
    } else if ("All" %in% delay.region) {
      if (!identical(delay.region, "All"))
        stop("You cannot use 'All' in conjunction with other regions in delay.region")
      
      x.d.fish.filter <- x.fish.delay.reduct
      x.d.fish.nofilter <- NULL
      
    } else {
      x.d.fish.filter <- x.fish.delay.reduct %>% filter(Region %in% delay.region)
      x.d.fish.nofilter <- x.fish.delay.reduct %>% filter(!(Region %in% delay.region))
    } 
    
    if (nrow(x.d.fish.filter) == 0) 
      stop("Error in fishing data processing - delayed opening")
    
    
    #------------------------------------------------------
    # Step 2) determine the number of days each season would have been delayed
    #   Use already-added 'season_date_st'
    season.mgmt.summ <- x.d.fish.filter %>% 
      mutate(mgmt_yr = func_year_extract(delay.date, crab_year), 
             # if (year(delay.date) == 2009) substr(crab_year, 1, 4) else substr(crab_year, 6, 9), 
             season_open_mgmt = make_date(mgmt_yr, lubridate::month(delay.date), 
                                          day(delay.date))) %>% 
      group_by(crab_year, Region) %>% 
      summarise(season_open_mgmt = unique(season_open_mgmt), 
                #season_date_st is already in x.d.fish.filter
                season_days_delayed = as.numeric(
                  difftime(season_open_mgmt, unique(season_date_st), units = "days")
                ), 
                .groups = "drop")
    
    x.d.fish.filter.mgmt <- x.d.fish.filter %>% 
      left_join(season.mgmt.summ, by = c("crab_year", "Region"))
    
    
    #------------------------------------------------------
    # Step 3) shift dates/times as necessary, remove records from "remove", 
    #   or keep records either after season open date or within the depth restrictions
    x.fish.shifted <- if (delay.method == "remove") {
      x.d.fish.filter.mgmt %>% 
        filter(date_record >= season_open_mgmt)
      
    } else if (delay.method == "depth") {
      x.d.fish.filter.mgmt %>% 
        filter((date_record >= season_open_mgmt) | between(.data$depth, depth.deep, depth.shallow))
      
    } else {
      delay_date_shift(x.d.fish.filter.mgmt, delay.method)
    }
    
    
    #------------------------------------------------------
    # Step 4) redistribute effort using spatial or temporal fidelity
    x.fish.redist <- if (delay.method %in% c("remove", "depth")) {
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
             depth, BIA_bm_noNAs, BIA_mn_noNAs, BIA_bm_or_mn, 
             DCRB_lbs, DCRB_rev, Num_DCRB_VMS_pings, Num_DCRB_Vessels, 
             Num_Unique_DCRB_Vessels)
    
    # If-else is in case region was "All"
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
      make_date(substr(x.fish.delay$crab_year, 1, 4), 11, 1), 
      make_date(substr(x.fish.delay$crab_year, 6, 9), 10, 31)
    )
    if (!all(x.fish.delay$date_record %within% check.int)) 
      warning("Error in delayed opening date shifting - shifted out of crab season")
    
    rm(check.int, x.fish.shifted, season.mgmt.summ, x.fish.redist, 
       x.d.fish.filter.mgmt, x.d.fish.filter, x.d.fish.nofilter)
  }
  
  
  #----------------------------------------------------------------------------
  # Do percent reduction - late season
  if (is.null(reduction.after.date)) {
    x.fish.closure.reduct <- x.fish.delay
    
  } else {
    x.fish.closure.reduct <- x.fish.delay %>% 
      mutate(reduce_yr = func_year_extract(reduction.after.date, crab_year), 
             # if (year(reduction.after.date) == 2009) substr(crab_year, 1, 4) else substr(crab_year, 6, 9), 
             reduce_mgmt = make_date(reduce_yr, lubridate::month(reduction.after.date), 
                                     day(reduction.after.date)), 
             reduce_date = date_record >= reduce_mgmt)
    
    if ("All" %in% reduction.after.region) {
      if (length(reduction.after.region) > 1)
        stop("You cannot use 'All' in conjunction with other regions in reduction.after.region")
      
      x.fish.closure.reduct <- x.fish.closure.reduct %>% 
        mutate(reduce_region = TRUE)
      
    } else {
      x.fish.closure.reduct <- x.fish.closure.reduct %>% 
        mutate(reduce_region = Region %in% reduction.after.region)
    }
    
    x.fish.closure.reduct <- x.fish.closure.reduct %>% 
      mutate(rec = reduce_region & reduce_date, 
             rec_dec = 1 - (reduction.after.percent / 100), #get decimal value
             DCRB_lbs = ifelse(rec, DCRB_lbs * rec_dec, DCRB_lbs), 
             DCRB_rev = ifelse(rec, DCRB_rev * rec_dec, DCRB_rev), 
             Num_DCRB_VMS_pings = ifelse(rec, Num_DCRB_VMS_pings * rec_dec, Num_DCRB_VMS_pings), 
             Num_DCRB_Vessels = ifelse(rec, Num_DCRB_Vessels * rec_dec, Num_DCRB_Vessels), 
             Num_Unique_DCRB_Vessels = ifelse(rec, Num_Unique_DCRB_Vessels * rec_dec, Num_Unique_DCRB_Vessels)) %>% 
      select(-reduce_yr, -reduce_mgmt, -reduce_date, -reduce_region, -rec, -rec_dec)
  }
  
  
  #----------------------------------------------------------------------------
  # Do early closure stuff
  if (is.null(closure.date)) {
    x.fish.closure <- x.fish.closure.reduct
    
  } else {
    #------------------------------------------------------
    # Determine which records come after closure date
    x.fish.c1 <- x.fish.closure.reduct %>% 
      mutate(mgmt_yr = func_year_extract(closure.date, crab_year), 
             # if (year(closure.date) == 2009) substr(crab_year, 1, 4) else substr(crab_year, 6, 9), 
             season_close_mgmt = make_date(mgmt_yr, lubridate::month(closure.date), day(closure.date)), 
             record_post_closure_date = date_record >= season_close_mgmt) %>% 
      select(-mgmt_yr)
    
    #browser()
    #------------------------------------------------------
    # Do region-specific stuff
    #   identical() ensures that closure.region is of length 1
    if (identical(closure.region, "BIA")) { 
      # If BIAs, temporally redistribute effort to CA region that the cell is in
      x.fish.c2 <- x.fish.c1 %>% 
        mutate(record_closed = BIA_bm_or_mn == "Inside BIA" & record_post_closure_date, 
               region_toshiftto = ifelse(record_closed, Region, NA))
      
    } else if (identical(closure.region, "CenCA") | identical(closure.region, "NorCA")) {
      # If one of CenCA or NorCA, prep to shift to the other region
      #   region.toshift will be ignored below if closure.method == "remove:
      region.toshift <- ifelse(closure.region == "CenCA", "NorCA", "CenCA")
      x.fish.c2 <- x.fish.c1 %>% 
        mutate(record_closed = Region == closure.region & record_post_closure_date, 
               region_toshiftto = ifelse(record_closed, region.toshift, NA))
      rm(region.toshift)
      
    } else {
      # Otherwise can only remove, but from any combo of regions
      if (closure.method == "temporal")
        stop("closure.region is not length one and one of 'BIA', 'CenCA', or 'NorCA'. ", 
             "You must use either the 'remove' or 'depth' closure method")
      
      if ("All" %in% closure.region) {
        # If All, check that no other regions are provided and then remove everything
        if (length(closure.region) > 1)
          stop("You cannot use 'All' in conjunction with other regions in closure.region")
        
        x.fish.c2 <- x.fish.c1 %>% 
          mutate(record_closed = record_post_closure_date, 
                 region_toshiftto = NA)
        
      } else {
        # If not All, then do region-specific closure
        x.fish.c2 <- x.fish.c1 %>% 
          mutate(record_closed = ifelse(Region %in% closure.region, record_post_closure_date, FALSE), 
                 region_toshiftto = NA)
      }
    }
    
    #------------------------------------------------------
    # Remove or redistribute closed effort in applicable region(s)
    x.fish.closure <- if (closure.method == "remove") {
      x.fish.c2 %>% 
        filter(!record_closed) %>% 
        select(-season_close_mgmt, -record_post_closure_date, 
               -record_closed, -region_toshiftto)
      
    } else if (closure.method == "depth") {
      # Keep effort not in closed time OR that is in accepted depth band
      x.fish.c2 %>% 
        filter(!record_closed | between(.data$depth, depth.deep, depth.shallow)) %>% 
        select(-season_close_mgmt, -record_post_closure_date, 
               -record_closed, -region_toshiftto)
      
    } else if (closure.method == "temporal") {
      x.fish.c2 %>% 
        mutate(record_toshift = record_closed, 
               record_base = !record_toshift & record_post_closure_date, 
               date_record_old = date_record, 
               year_month_old = year_month) %>% 
        redistribute_temporal(Num_DCRB_VMS_pings, "closure", closure.region, 
                              closure.redist.percent)# function below
      
    } else {
      stop("Invalid closure.method argument")
    }
    
    rm(x.fish.c1, x.fish.c2)
  }
  
  
  #----------------------------------------------------------------------------
  # Final checks and formatting
  if (exists("x.other")) { #it always should exist, but just in case
    x.fish.closure <- x.fish.closure %>% 
      left_join(x.other, by = "GRID5KM_ID") %>% 
      select(crab_year, year_month, date_record, 
             GRID5KM_ID, Region, CA_OFFSHOR, depth, everything())
  }
  
  x.out <- x.fish.closure %>% 
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
delay_date_shift <- function(y, delay.method = c("lag", "pile")) {
  delay.method <- match.arg(delay.method)
  
  if (delay.method == "lag") {
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
    stop("delay.method is not one of 'lag' or 'pile'")
  }
}


###############################################################################
# Temporal fidelity redistribution function
redistribute_temporal <- function(z, z.col, z.type = c("delay", "closure"), 
                                  z.reg = c("All", "CenCA", "NorCA", "BIA", "OR", "WA"), 
                                  z.perc = 100) {
  ### Inputs
  # z: data frame; Contains columns described in z.cols.nec
  # z.col: symbol, column name to use for redistribution percentages
  # z.type: character; either "delay" or "closure"
  # z.reg: character; passed from delay.region or closure.region
  # z.perc: numeric; default is 100. Percentage multiplier for effort
  #   to be redistributed
  
  ### Output: 
  # A data frame with fishing effort data redistributed temporally  
  
  z.col <- enquo(z.col)
  
  #--------------------------------------------------------
  ### Input checks
  z.type <- match.arg(z.type)
  z.reg <- match.arg(z.reg, several.ok = TRUE)
  if (z.type == "closure")
    if (!(z.reg %in% c("CenCA", "NorCA", "BIA")) & length(z.reg) == 1)
      stop("If z.type is closure, then z.reg must be BIA, CenCA, or NorCA")
  
  
  # Necessary column names
  z.cols.nec <- c(
    "crab_year", "GRID5KM_ID", "Region", "date_record", "year_month"
  )
  z.cols.nec2 <- c("date_record_old", "year_month_old") #columns not included in the output
  
  z.cols.dcrb <- c(
    "DCRB_lbs", "DCRB_rev", "Num_DCRB_VMS_pings", "Num_DCRB_Vessels", 
    "Num_Unique_DCRB_Vessels"
  )
  
  z.cols.other <- c("depth", "BIA_bm_noNAs", "BIA_mn_noNAs", "BIA_bm_or_mn")
  
  stopifnot(
    all(c(z.cols.nec, z.cols.nec2, z.cols.dcrb, z.cols.other) %in% names(z)),
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
  
  
  # 1) Get sums of effort to be redistributed, grouped by region and year-month, that has a base
  stopifnot(!any(is.na(filter(z.use, record_toshift)$region_toshiftto)))
  z.summ.toredistribute <- z.use %>% 
    filter(record_toshift, id %in% z.id.base) %>%
    group_by(region_toshiftto, year_month) %>% 
    summarise(DCRB_lbs_sum_toadd = sum(DCRB_lbs) * z.perc, 
              DCRB_rev_sum_toadd = sum(DCRB_rev) * z.perc, 
              Num_DCRB_VMS_pings_sum_toadd = sum(Num_DCRB_VMS_pings) * z.perc, 
              Num_DCRB_Vessels_sum_toadd = sum(Num_DCRB_Vessels) * z.perc, 
              Num_Unique_DCRB_Vessels_sum_toadd = sum(Num_Unique_DCRB_Vessels) * z.perc, 
              .groups = "drop") #%>%
  # ungroup()
  
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
    summarise(eff_sum = sum(!!z.col), .groups = "drop")
  
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
              Num_Unique_DCRB_Vessels = sum(Num_Unique_DCRB_Vessels), 
              .groups = "drop") %>% 
    # ungroup() %>% 
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
func_year_mo <- function(i) {
  stopifnot(inherits(i, "Date"))
  paste(lubridate::year(i), sprintf("%02d", lubridate::month(i)), sep = "_")
}

func_year_extract <- function (i, j) {
  stopifnot(inherits(i, "Date"), nchar(j) == 9)
  if (lubridate::year(i) == 2009) substr(j, 1, 4) else substr(j, 6, 9)
}

###############################################################################
