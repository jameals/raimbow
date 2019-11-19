# Generate range of dates into whcih to aggregate predictions
# Originally simply part of whalepreds_aggregate(); 
#   made into own function to allow for aggregating dates from filenames 
#   (e.g. for aggregating Briana's raster predictions)


whalepreds_aggregate_dates <- function(x.names, x.col.idx, aggr.level = NULL, range.dates = NULL) {
  ### Inputs
  # x.names: character; vector of dates as characters
  # x.col.idx: indices of characters in column names that specfiy the date;
  #   must be either of length 8 or 10 
  # aggr.level: aggregation level; either "biweekly" , "monthly", or "#day" 
  #   (e.g. "5day"). Ignored if range.dates is not NULL
  # range.dates: object of class Date; custom date range into which to 
  #   aggregate predictions. Interval evaluation is [ , )
  #   If NULL, aggr.level must be specified
  
  ### Output
  # List of:
  #   aggr.level: character; aggregation level (user, monthly, #day)
  #   cols.dates: Date; dates from each string in x.names
  #   range.dates: Date; vector of dates that define the intervals into which 
  #     data will be aggregated; interval evaluation is assumed to be [ , )
  
  
  #----------------------------------------------------------------------------
  stopifnot(
    require(dplyr), 
    require(lubridate), 
    inherits(x.col.idx, c("integer", "numeric"))
  )
  
  x.names <- substr(x.names, min(x.col.idx), max(x.col.idx))
  
  
  #----------------------------------------------------------------------------
  ### Prep: get dates from column names
  # Are dates separated by '.' or '_'?
  if (all(grepl("[.]", x.names))) {
    tmp <- "."
  } else if (all(grepl("_", x.names))) {
    tmp <- "_"
  } else if (all(grepl("-", x.names))) {
    tmp <- "-"
  } else {
    stop("The dates in the column names must all be separated by '.', '_', or '-")
  }
  
  # Are dates 8 digit or 10 digit?
  if (length(x.col.idx) == 8) {
    cols.dates <- as.Date(x.names, format = paste0("%y", tmp, "%m", tmp, "%d"))
    
  } else if (length(x.col.idx) == 10) {
    cols.dates <- as.Date(x.names, format = paste0("%Y", tmp, "%m", tmp, "%d"))
    
  } else {
    stop("x.col.idx must be of length 8 (e.g. \"05_01_01\" or \"05.01.01\") ", 
         "or 10 (e.g. \"2005_01_01\" or \"2005.01.01\")")
  }
  rm(tmp)
  
  
  #----------------------------------------------------------------------------
  ### Prep: generate dates to define specfied intervals
  if (is.null(range.dates)) {
    # Date range NOT is manually specified
    stopifnot(
      aggr.level %in% c("monthly") | grepl("day", aggr.level)
    )
    
    if (aggr.level == "monthly") {
      range.dates <- seq(from = min(cols.dates), to = max(cols.dates), by = "1 month")
      range.dates <- c(range.dates, max(range.dates) %m+% months(1))
      
    } else if (grepl("day", aggr.level)) {
      day.int <- as.numeric(strsplit(aggr.level, "day")[[1]][1])
      if (!inherits(day.int, "numeric")) {
        stop("Please ensure aggr.level is either \"monthly\", \"biweekly\", ", 
             "or in the form '#day', e.g. \"10day\"")
      }
      range.dates <- seq(from = min(cols.dates), to = max(cols.dates), by = paste(day.int, "day"))
      range.dates <- c(range.dates, max(range.dates) %m+% days(day.int))
      
    } else {
      stop("Error in aggr.level argument")
    }
    
    cols.interval <- findInterval(
      cols.dates, range.dates, left.open = FALSE, rightmost.closed = FALSE
    )
    stopifnot(
      all(between(cols.interval, 1, length(range.dates) - 1))
    )
    rm(cols.interval)
    
  } else {
    # Date range is manually specified
    stopifnot(inherits(range.dates, "Date"))
    
    date.between <- between(cols.dates, min(range.dates), max(range.dates) - 1)
    aggr.level <- "user"
    
    if (!any(date.between)) {
      stop("None of the column dates fall within an interval defined by range.dates")
    } else if (!all(date.between)) {
      warning("Not all column dates fall within an interval defined by range.dates")
    }
  }
  
  
  #----------------------------------------------------------------------------
  list(
    aggr.level = aggr.level, cols.dates = cols.dates, 
    range.dates = range.dates
  )
}
