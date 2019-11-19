# RAIMBOW pre-processing function: whale predictions
# Aggregate raw (bidaily) predictions to desired level
#   Supported levels: biweekly, monthly, days (e.g. 7 days)
# If we anticipate any predictions coming in as shapefiles, 
#   we could change to S3 method

# By Sam Woodman, May 2019


###############################################################################
whalepreds_aggregate <- function(
  x, x.cols, x.col.idx, aggr.level = NULL, range.dates = NULL, se.calc = FALSE) 
{
  ### Inputs:
  # x: data frame with predictions
  # x.cols: column names or indices that contain prediction values
  #   Data column names of x must contain year/month/day as: 
  #   'yyyy.mm.dd' or 'yyyy_mm_dd'.
  #   Column not specified will be passed to output data frame
  # x.col.idx: indices of characters in column names that specfiy the date;
  #   must be either of length 8 or 10 
  # aggr.level: aggregation level; either "biweekly" , "monthly", or "#day" 
  #   (e.g. "5day"). Ignored if range.dates is not NULL
  # range.dates: object of class Date; custom date range into which to 
  #   aggregate predictions. Interval evaluation is [ , )
  #   If NULL, aggr.level must be specified
  # se.calc: logical indicating whether SE values should be calculated from
  #   aggregated values
  
  ### Output: data frame of aggregated/summarized whale predictions.
  
  #----------------------------------------------------------------------------
  # Helper functions
  
  # browser()
  if (!exists("whalepreds_aggregate_dates")) {
    stop("Please load the 'whalepreds_aggregate_dates' function by sourcing ", 
         "Whalepreds_aggregate_dates.R")
  }
  
  ### Based on esdm_weighted_var_amv
  raimbow_se <- function(x) {
    ### Inputs:
    # x: numeric vector of values that were used to calculate mean
    
    x.mean <- mean(x, na.rm = TRUE)
    
    if (sum(!is.na(x)) == 0) {
      NA
    } else {
      sqrt(sum((x - x.mean)^2, na.rm = TRUE) / length(x))
    }
  }
  
  # ### For use within pmap() call
  # raimbow_se_pmap <- function(...) {
  #   ### Inputs:
  #   # input from pmpa
  # 
  #   x <- unlist(list(...))
  #   x.mean <- mean(x, na.rm = TRUE)
  #   
  #   if (sum(!is.na(x)) == 0) {
  #     NA
  #   } else {
  #     sqrt(sum((x - x.mean)^2, na.rm = TRUE) / (length(x) - 1))
  #   }
  # }
  
  #----------------------------------------------------------------------------
  ### Process and check inputs
  stopifnot(
    inherits(x, "data.frame"), 
    inherits(x.cols, c("character", "integer", "numeric")), 
    inherits(x.col.idx, c("integer", "numeric")), 
    inherits(se.calc, "logical")
  )
  
  if (!inherits(x.cols, "character")) x.cols <- names(x)[x.cols]
  stopifnot(all(x.cols %in% names(x)))
  
  #----------------------------------------------------------------------------
  ### Extract data columns, dates, and date intervals
  x.other <- x %>% dplyr::select(-!!x.cols)
  x.data <- x %>% 
    dplyr::select(!!x.cols) %>% 
    set_names(substr(names(.), min(x.col.idx), max(x.col.idx)))
  
  
  date.list <- whalepreds_aggregate_dates(
    names(x.data), seq_along(x.col.idx), aggr.level, range.dates
  )
  aggr.level  <- date.list$aggr.level
  cols.dates  <- date.list$cols.dates
  range.dates <- date.list$range.dates
  
  
  #----------------------------------------------------------------------------
  ### Map columns to date intervals
  x.key.summ <- data.frame(x_cols = x.cols, stringsAsFactors = FALSE) %>% 
    mutate(x_data_cols = names(x.data),
           cols_interval = findInterval(
             cols.dates, range.dates, left.open = FALSE, rightmost.closed = FALSE
           )) %>% 
    group_by(cols_interval) %>% 
    summarise(x_data_cols_list = list(x_data_cols)) %>%
    filter(between(cols_interval, 1, length(range.dates) - 1)) %>% 
    mutate(range_beg = gsub("-", "_", range.dates[cols_interval])) %>% 
    dplyr::select(cols_interval, range_beg, x_data_cols_list)
  
  
  if (nrow(x.key.summ) < (length(range.dates) - 1)) {
    d <- data.frame(begdate = head(range.dates, -1), 
                    enddate = tail(range.dates - 1, -1)) %>% 
      mutate(coldate_count = pmap_dbl(
        list(begdate, enddate), function(i, j, k) sum(between(k, i, j)), k = cols.dates)
      )
    
    warning("The intervals beginning with the following dates ", 
            "do not contain any column dates, ", 
            "and thus will not be included in the final output:\n", 
            paste(d$begdate[d$coldate_count == 0], collapse = ", "))
  }
  
  #----------------------------------------------------------------------------
  # Aggregate/average columns and return output
  
  ### Calculate mean prediction value
  data.summ <- data.frame(apply(x.key.summ, 1, function(i, j) {
    rowMeans(j[,  i[["x_data_cols_list"]]], na.rm = TRUE)
  }, j = x.data)) %>%
    set_names(paste("Avg", aggr.level, x.key.summ$range_beg, sep = "_"))
  
  ### Calculate SE of prediction values
  if (se.calc) {
    data.summ.se <- data.frame(apply(x.key.summ, 1, function(i, j) {
      apply(j[,  i[["x_data_cols_list"]]], 1, raimbow_se)
    }, j = x.data)) %>% 
      set_names(paste("SE", aggr.level, x.key.summ$range_beg, sep = "_"))
    
  } else {
    data.summ.se <- NULL
  }
  
  ### Return
  bind_cols(x.other, data.summ, data.summ.se)
}

###############################################################################
