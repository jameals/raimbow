# Function for calculating risk after shifting effort based on management scenarios

risk_mgmt <- function(x, x.col, y = NULL, ym.min = "2009_11", ym.max = "2018_06") {
  ### Inputs
  # x: data frame; e.g. output of effort_mgmt()
  # x.col: symbol (i.e. column name without quotes); 
  #   fishing metric (column of x) that will be used to calculate risk
  # y: data frame with whale predictions. If not NULL, this data frame 
  #   will be joined with x by "GRID5KM_ID" and "year_month". 
  #   If not NULL, exepceted columns will be selected (see names.y)
  # ym.min: character; minimum year_month value for which to calculate risk
  # ym.min: character; maximum year_month value for which to calculate risk
  
  ### Output
  # Data frame with effort data and risk values grouped by year_month/Region 
  #   and summed. 
  # Whale predictions are not included because this function does not use 
  #   complete() to fill out year_months and grid cells, and thus the 
  #   whale predictions would not be comparable
  
  
  x.col <- enquo(x.col)
  
  
  #----------------------------------------------------------------------------
  ### Input checks
  stopifnot(
    require(dplyr), 
    require(lubridate), 
    require(tidyr)
  )
  
  
  # Check that x has required columns
  names.x.info <- c("GRID5KM_ID", "Region", "year_month")
  names.x.fish <- c(
    "DCRB_lbs", "DCRB_rev", "Num_DCRB_VMS_pings", 
    "Num_DCRB_Vessels", "Num_Unique_DCRB_Vessels"
  )
  if (!all(c(names.x.info, names.x.fish) %in% names(x)))
    stop("x must contain the following columns:\n", 
         paste(c(names.x.info, names.x.fish), collapse = ", "))
  
  
  # Check fishing effort column names
  if (!(quo_name(x.col) %in% names.x.fish)) 
    stop("x.col must be one of the following:\n", 
         paste(names.x.fish, collapse = ", "))
  
  
  # Check and prep whale preds
  names.y <- c(
    "GRID5KM_ID", "year_month", "Blue_occurrence_mean", "Blue_occurrence_se", 
    "Humpback_dens_mean", "Humpback_dens_se"
  )
  if (is.null(y)) {
    if (!all(names.y %in% names(x)))
      stop("Since y is NULL, x must contain all of the following columns:\n", 
           paste(names.y, collapse = ", "))
    
    y <- x %>% select(!!names.y)
    if (any(is.na(y$Blue_occurrence_mean) | is.na(y$Humpback_dens_mean)))
      warning("Some of the blue/humpback prediction values in x are NA; ", 
              "is this expected?", 
              immediate. = TRUE)
    
  } else {
    if (!all(names.y %in% names(y)))
      stop("y must contain all of the following columns:\n", 
           paste(names.y, collapse = ", "))
    
    y <- y %>% select(!!names.y)
  }
  
  # Test that grid cells only are assigned to one region
  test.df <- x %>% 
    group_by(GRID5KM_ID) %>% 
    summarise(reg_count = length(unique(Region)))
  if (!all(test.df$reg_count == 1)) {
    grid.mult <- test.df$GRID5KM_ID[test.df$reg_count > 1]
    stop("The following grid cell(s) in x are assigned to multiple regions\n:", 
         paste(grid.mult, collapse = ", "))
  }
  rm(test.df)
  
  #browser()
  #----------------------------------------------------------------------------
  ### Processing
  
  # Summarize whale preds by year_month and Region
  # y.summ <- y %>% 
  #   group_by(year_month, Region) %>% 
  #   summarise(Blue_occurrence_mean = sum(Blue_occurrence_mean, na.rm = TRUE), 
  #             Humpback_dens_mean = sum(Humpback_dens_mean, na.rm = TRUE))
  
  # Get all year_months for which to calculate risk
  ym.seq.date <- seq(
    as.Date(paste0(ym.min, "_01"), format = "%Y_%m_%d"), 
    as.Date(paste0(ym.max, "_01"), format = "%Y_%m_%d"), 
    by = "month"
  )
  ym.seq <- paste(year(ym.seq.date), sprintf("%02d", month(ym.seq.date)), 
                  sep = "_")
  rm(ym.seq.date)
  
  
  # Summarize fishing data by year_month and grid cell
  if (any(!(x$year_month %in% ym.seq)))
    warning("Some records in x have year_months that are outside of the ", 
            "range provided via ym.min and ym.max", 
            immediate. = TRUE)
  
  x.ym <- x %>% 
    filter(year_month %in% ym.seq) %>% 
    group_by(year_month, GRID5KM_ID, Region) %>% 
    summarise(effort_val = sum(!!x.col), 
              DCRB_lbs = sum(DCRB_lbs), 
              DCRB_rev = sum(DCRB_rev), 
              Num_DCRB_VMS_pings = sum(Num_DCRB_VMS_pings), 
              Num_DCRB_Vessels = sum(Num_DCRB_Vessels), 
              Num_Unique_DCRB_Vessels = sum(Num_Unique_DCRB_Vessels)) %>% 
    ungroup()
  # complete(year_month = ym.seq, GRID5KM_ID, 
  #          fill = list(effort_val = 0, DCRB_lbs = 0, DCRB_rev = 0, 
  #                      Num_DCRB_VMS_pings = 0, Num_DCRB_Vessels = 0, 
  #                      Num_Unique_DCRB_Vessels = 0)) %>% 
  # arrange(year_month, GRID5KM_ID)
  
  
  # Add in whale predictions and calculate risk
  x.ym.risk <- x.ym %>% 
    left_join(y, by = c("GRID5KM_ID", "year_month")) %>%
    mutate(risk_humpback = effort_val * Humpback_dens_mean, 
           risk_blue = effort_val * Blue_occurrence_mean)
  
  # Print messages if any whale predictions are NA
  if (any(is.na(x.ym.risk$risk_humpback)))
    message("Note that the following grid cells have NA humpback predictions ", 
            "for at least some year_month/grid cell combos with fishing effort:\n", 
            paste(sort(unique(x.ym.risk$GRID5KM_ID[is.na(x.ym.risk$Humpback_dens_mean)])), 
                  collapse = ", "))
  
  if (any(is.na(x.ym.risk$risk_blue)))
    message("Note that the following grid cells have NA blue whale predictions ", 
            "for at least some year_month/grid cell combos with fishing effort:\n", 
            paste(sort(unique(x.ym.risk$GRID5KM_ID[is.na(x.ym.risk$Blue_occurrence_mean)])), 
                  collapse = ", "))
  
  # Summarize risk by year_month and Region
  #   To summarize whale preds, would need to complete() grid cells and year_months
  if (any(is.na(x.ym.risk$Region))) 
    stop("NA regions - error in effort processing")
  
  risk.summ <- x.ym.risk %>% 
    group_by(year_month, Region) %>% 
    summarise_at(vars(DCRB_lbs:Num_Unique_DCRB_Vessels, risk_humpback:risk_blue), 
                 sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    # complete(year_month = ym.seq, 
    #          fill = list(DCRB_lbs = 0, DCRB_rev = 0, Num_DCRB_VMS_pings = 0, 
    #                      Num_DCRB_Vessels = 0, Num_Unique_DCRB_Vessels = 0, 
    #                      risk_humpback = 0, risk_blue = 0)) %>% 
    # arrange(year_month) %>% 
    # left_join(y.summ, by = "year_month") %>% 
    mutate(crab_year = func_crab_year(year_month)) %>% 
    select(year_month, crab_year, everything())
  
  
  risk.summ
}


###############################################################################
# Helper functions

### Function to calculate crab year
func_crab_year <- function(z) {
  # z: character; year_month
  
  stopifnot(require(lubridate))
  z.yr <- as.numeric(substr(z, 1, 4))
  z.mon <- as.numeric(substr(z, 6, 7))
  
  ifelse(
    z.mon %in% 11:12, 
    paste(z.yr, z.yr + 1, sep = "_"), paste(z.yr - 1, z.yr, sep = "_")
  )
}


###############################################################################
