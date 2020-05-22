# This file contains two functions for calculating risk after shifting effort based on management scenarios
#   risk_mgmt: calculate risk on a grid cell/year-month basis. Does not group and summarize
#   risk_mgmt_summ: takes output of risk_mgmt(), and summarizes (sums) based on 
#     specified 'summary.level' argument, i.e. by in/out of BIA or Region

risk_mgmt <- function(x, x.col, y, risk.unit = c("orig", "dens"), area.key, 
                      ym.min = "2009_11", ym.max = "2018_06") { #will need to update ym.max to 2019_07
  ### Inputs
  # x: data frame; e.g. output of effort_mgmt()
  # x.col: symbol (i.e. column name without quotes); 
  #   fishing metric (column of x) that will be used to calculate risk
  # y: data frame with whale predictions; whale prediction units are expected to be 
  #   probability of occurrence and abundance for blues and humpbacks, 
  #   respectively (see names.y object for required names).
  #   This data frame will be joined with x by "GRID5KM_ID" and "year_month". 
  # risk.unit: character; either "orig" or "dens". 
  #   "dens" means that risk values will be divided by the grid cell area
  # area.key: data.frame; key for grid cell ID and grid cell area (with land removed)
  #   Used to calculate 'risk density'
  # ym.min: character; minimum year_month value for which to calculate risk
  # ym.min: character; maximum year_month value for which to calculate risk
  
  ### Output
  # Data frame with risk values calculated by year_month/Region
  #   Default humpback units: risk is whale abundance * effort_val, eg [pings * whales]
  #   Default blue units:  risk is whale occurrence * effort_val, eg [pings * probability of occurrence].
  #   Both of these units will be divided by km^2 if 'risk.unit = "dens"'
  # Whale predictions are not included because this function does not use 
  #   complete() to fill out year_months and grid cells, and thus the 
  #   whale predictions would not be comparable. If we want whale predictions in the output table
  #   we should 1) determine all of the grid cells that have effort in them in any year/month, 
  #   2) filter the whale preds for those grid cells, and then 3) group and summarize the filtered whale preds.
  
  
  x.col <- enquo(x.col)
  
  
  #----------------------------------------------------------------------------
  ### Input checks
  stopifnot(
    require(dplyr), 
    require(lubridate), 
    require(tidyr), 
    inherits(x, "data.frame"), 
    inherits(y, "data.frame"), 
    inherits(area.key, "data.frame")
  )
  
  risk.unit <- match.arg(risk.unit)
  
  
  # Check that x has required columns
  names.x.info <- c("GRID5KM_ID", "Region", "year_month")
  names.x.other <- c("CA_OFFSHOR", "BIA_bm_noNAs", "BIA_mn_noNAs", "BIA_bm_or_mn")
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
    "GRID5KM_ID", "year_month", #"area_km_lno", 
    "Blue_occurrence_mean", "Blue_occurrence_se", 
    "Humpback_abund_mean", "Humpback_abund_se", 
    "normalized_humpback", "normalized_blue" 
  )
  # if (is.null(y)) {
  #   if (!all(names.y %in% names(x)))
  #     stop("Since y is NULL, x must contain all of the following columns:\n", 
  #          paste(names.y, collapse = ", "))
  #   
  #   y <- x %>% select(!!names.y)
  #   if (any(is.na(y$Blue_occurrence_mean) | is.na(y$Humpback_dens_mean)))
  #     warning("Some of the blue/humpback prediction values in x are NA; ", 
  #             "is this expected?", 
  #             immediate. = TRUE)
  #   
  # } else {
  if (!all(names.y %in% names(y)))
    stop("y must contain all of the following columns:\n", 
         paste(names.y, collapse = ", "))
  
  y <- y %>% select(!!names.y)
  # }
  
  # Check area.key
  names.area.key <- c("GRID5KM_ID", "area_km_lno")
  if (!identical(names(area.key), names.area.key))
    stop("The names of area.key must be: ", 
         paste(names.area.key, collapse = ", "))
  
  
  #----------------------------------------------------------------------------
  ### Processing
  
  # Extract ;'other' data to be joined back in at the end
  x.other <- x %>% 
    select(GRID5KM_ID, !!names.x.other) %>% 
    distinct()
  
  
  # Get all year_months for which to calculate risk
  ym.seq.date <- seq(
    as.Date(paste0(ym.min, "_01"), format = "%Y_%m_%d"), 
    as.Date(paste0(ym.max, "_01"), format = "%Y_%m_%d"), 
    by = "month"
  )
  ym.seq <- paste(year(ym.seq.date), sprintf("%02d", month(ym.seq.date)), 
                  sep = "_")
  rm(ym.seq.date)
  
  
  # Summarize fishing data by year_month and grid cell, and join with identifiers
  if (any(!(x$year_month %in% ym.seq)))
    warning(paste("There are", sum(!(x$year_month %in% ym.seq))), 
            " records in x with the following year_months, which are ", 
            "outside of the range provided via ym.min and ym.max:\n", 
            paste(setdiff(sort(unique(x$year_month)), ym.seq), collapse = ", "), 
            immediate. = TRUE)
  
  x.ym <- x %>% 
    left_join(area.key, by = "GRID5KM_ID") %>%
    filter(year_month %in% ym.seq) %>% 
    group_by(year_month, GRID5KM_ID) %>% 
    summarise(Region = unique(Region), 
              area_km_lno = unique(area_km_lno), 
              effort_val = sum(!!x.col), 
              DCRB_lbs = sum(DCRB_lbs), 
              DCRB_rev = sum(DCRB_rev), 
              Num_DCRB_VMS_pings = sum(Num_DCRB_VMS_pings), 
              Num_DCRB_Vessels = sum(Num_DCRB_Vessels), 
              Num_Unique_DCRB_Vessels = sum(Num_Unique_DCRB_Vessels)) %>% 
    ungroup() %>% 
    left_join(x.other, by = "GRID5KM_ID")
  
  
  #browser()
  # Add in whale predictions, calculate normalzied effort, and calculate risk
  # TODO: should we calculate normalized whale values in here too for consistency?
  x.ym.risk <- x.ym %>% 
    left_join(y, by = c("GRID5KM_ID", "year_month")) %>%
    mutate(normalized_effort = as.vector(scale(effort_val, 
                                               center = min(effort_val, na.rm=TRUE), 
                                               scale = diff(range(effort_val, na.rm=TRUE)))),
           # normalized_humpback = as.vector(
           #   scale(
           #     Humpback_dens_mean,center=min(Humpback_dens_mean, na.rm=TRUE),scale=diff(range(Humpback_dens_mean, na.rm=TRUE))
           #   )
           # ),
           # normalized_blue = as.vector(
           #   scale(
           #     Blue_occurrence_mean,center=min(Blue_occurrence_mean, na.rm=TRUE),scale=diff(range(Blue_occurrence_mean, na.rm=TRUE))
           #   )
           # ),    
           risk_humpback = effort_val * Humpback_abund_mean, 
           risk_blue = effort_val * Blue_occurrence_mean,
           n_risk_humpback = normalized_effort * normalized_humpback, 
           n_risk_blue = normalized_effort * normalized_blue)
  
  if (risk.unit == "dens") {
    x.ym.risk <- x.ym.risk %>% 
      mutate(risk_humpback = risk_humpback / area_km_lno, 
             risk_blue = risk_blue / area_km_lno,
             n_risk_humpback = n_risk_humpback / area_km_lno, 
             n_risk_blue = n_risk_blue / area_km_lno)
  }
  
  # option to write out 5km grid cell risk values for each year_mo
  #write_rds(x.ym.risk, paste0("/Users/jameal.samhouri/Documents/RAIMBOW/Processed Data/Samhouri et al. whales risk/Output_Data/scenario_output_dataframes/status_quo_risk_",today(),".rds"))
  
  # # Should not be necessary given that whale values have to be passed in separately
  # # Print messages if any whale predictions are NA
  # if (any(is.na(x.ym.risk$risk_humpback)))
  #   message("Note that the following grid cells have NA humpback predictions ", 
  #           "for at least some year_month/grid cell combos with fishing effort:\n", 
  #           paste(sort(unique(x.ym.risk$GRID5KM_ID[is.na(x.ym.risk$Humpback_abund_mean)])), 
  #                 collapse = ", "))
  # 
  # if (any(is.na(x.ym.risk$risk_blue)))
  #   message("Note that the following grid cells have NA blue whale predictions ", 
  #           "for at least some year_month/grid cell combos with fishing effort:\n", 
  #           paste(sort(unique(x.ym.risk$GRID5KM_ID[is.na(x.ym.risk$Blue_occurrence_mean)])), 
  #                 collapse = ", "))
  
  x.ym.risk
}




###############################################################################
risk_mgmt_summ <- function(x, summary.level = c("Region", "BIA")) {
  ### Inputs
  # x: output data frame from risk_summ()
  # summary.level: character; one of Region or BIA. Indicates how 
  #   risk and effort are summarized, i.e. by Region or inside/outside of BIAs
  #   TODO: should BIA option make things grouped by Region AND in/out of BIA instead of just in/out of BIA?
  
  #----------------------------------------------------------------------------
  summary.level <- match.arg(summary.level)
  
  # Prep and Error checks
  names.x <- c(
    "year_month", "GRID5KM_ID", "Region", "area_km_lno", "effort_val", 
    "DCRB_lbs", "DCRB_rev", "Num_DCRB_VMS_pings", "Num_DCRB_Vessels", "Num_Unique_DCRB_Vessels", 
    "CA_OFFSHOR", "BIA_bm_noNAs", "BIA_mn_noNAs", "BIA_bm_or_mn", 
    "Blue_occurrence_mean", "Blue_occurrence_se", "Humpback_abund_mean", "Humpback_abund_se", 
    "normalized_humpback", "normalized_blue", "normalized_effort", 
    "risk_humpback", "risk_blue", "n_risk_humpback", "n_risk_blue"
  )
  if (!identical(names(x), names.x))
    stop("The names of x must be as follows; is x the output of risk_mgmt()?\n", 
         "Column names: ", paste(names.x, collapse = ", "))
  
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
  
  # No Region values are NA
  if (any(is.na(x$Region))) 
    stop("NA regions - error in effort processing")
  
  #----------------------------------------------------------------------------
  # Summarize risk by year_month and Region/BIA. Whale preds are separate
  #   To summarize whale preds, would need to complete() grid cells and year_months
  risk.summ <-  if (summary.level == "Region") {
    x %>% group_by(year_month, Region)
  } else if (summary.level == "BIA") {
    x %>% group_by(year_month, BIA_bm_or_mn)
  }
  
  risk.summ %>% 
    summarise_at(vars(DCRB_lbs:Num_Unique_DCRB_Vessels, risk_humpback:n_risk_blue), 
                 sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    mutate(crab_year = func_crab_year(year_month)) %>% 
    select(year_month, crab_year, everything())
}




###############################################################################
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
