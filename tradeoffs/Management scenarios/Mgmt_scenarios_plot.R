# Function for plotting the number of effort records per day
effort_plot_time <- function(x) {
  stopifnot(require(ggplot2), require(lubridate))
  
  if (!("date_record" %in% names(x))) {
    x <- x %>% 
      mutate(season_date_st_min = as.Date(paste(substr(crab_year, 1, 4), 
                                                ifelse(Region == "CenCA", "11-15", "12-01"), 
                                                sep = "-")), 
             date_record_orig = as.Date(day_of_year - 1, 
                                        origin = as.Date(paste0(as.numeric(substr(year_month, 1, 4)), "-01-01"))), 
             decimal_record = ifelse(date_record_orig < season_date_st_min, 
                                     decimal_date(season_date_st_min), 
                                     decimal_date(date_record_orig)), 
             date_record = as.Date(
               round_date(date_decimal(decimal_record), unit = "day")), 
             year_month = paste(lubridate::year(date_record), 
                                sprintf("%02d", lubridate::month(date_record)), 
                                sep = "_")) %>% 
      select(-season_date_st_min, -date_record_orig, -decimal_record)
  }
  
  x.summ <- x %>% 
    group_by(crab_year, Region, date_record) %>% 
    summarise(count = n())
  
  ggplot(x.summ, aes(date_record, count, colour = Region, fill = Region, group = Region)) +
    geom_point() + 
    facet_wrap(vars(crab_year), nrow = 3, scales = "free_x") +
    ggtitle("Number of effort records per day (aka number of grid cells with effort)")
}


effort_plot_time(x.orig)
ggsave("../raimbow-local/Plots/DC_Effort_daily.png", width = 12, height = 8)
