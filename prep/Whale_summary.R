# Script to select whale preds from grid cells with fishing effort, 
#   and group by region

#------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(lubridate)


source("User_script_local.R")
if (user == "JS") {
  
} else if (user == "SMW") {
  path.key.region <- "../raimbow-local/RDATA_files/Grid5km_key_region.rds"
  path.grid.studyarea <- "../raimbow-local/RDATA_files/Grid5km_studyarea.rds"
  path.hump <- "../raimbow-local/Outputs/Humpback_5km_long_monthly.rds"
  path.blue <- "../raimbow-local/Outputs/BlueWhale_5km_long_monthly.rds"
  
} else {
  stop("Invlaid user")
}


#------------------------------------------------------------------------------
key.region <- readRDS(path.key.region)
grid.studyarea <- readRDS(path.grid.studyarea)


x.hump <- readRDS(path.hump) %>%
  filter(GRID5KM_ID %in% grid.studyarea) %>% 
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  left_join(key.region, by = "GRID5KM_ID") %>% 
  select(GRID5KM_ID, year_month, Humpback_dens_mean, Humpback_dens_se, Region)

x.blue <- readRDS(path.blue) %>%
  filter(GRID5KM_ID %in% grid.studyarea) %>% 
  mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
  left_join(key.region, by = "GRID5KM_ID") %>% 
  select(GRID5KM_ID, year_month, Blue_occurrence_mean, Blue_occurrence_se, Region)


#------------------------------------------------------------------------------
# How to carry along uncertainty.?
hump.summ <- x.hump %>% 
  group_by(Region, year_month) %>% 
  summarise(Humpback_dens_sum = sum(Humpback_dens_mean, na.rm = TRUE), 
            count = n())

blue.summ <- x.blue %>% 
  group_by(Region, year_month) %>% 
  summarise(Blue_occurrence_sum = sum(Blue_occurrence_mean, na.rm = TRUE), 
            count = n())


table(hump.summ$count, hump.summ$Region)
table(blue.summ$count, blue.summ$Region)


ggplot(hump.summ, aes(x = year_month, y = Humpback_dens_sum, color = Region, group = Region)) + 
  geom_point() + 
  geom_line()

#------------------------------------------------------------------------------
