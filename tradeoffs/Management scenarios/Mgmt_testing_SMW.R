library(dplyr)
library(lubridate)
library(sf)

# https://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string
simpleCap <- function(x) {
  s <- sapply(strsplit(x, " "), function(i) i[[1]])
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}


###############################################################################
# x.hump <- readRDS("C:/SMW/RAIMBOW/raimbow-local/Outputs/Humpback_5km_long_monthly.rds") %>%
#   mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
#   select(GRID5KM_ID, year_month, Humpback_dens_mean, Humpback_dens_se)
# 
# x.blue <- readRDS("C:/SMW/RAIMBOW/raimbow-local/Outputs/BlueWhale_5km_long_monthly.rds") %>%
#   mutate(year_month = paste(year(date), sprintf("%02d", month(date)), sep = "_")) %>%
#   select(GRID5KM_ID, year_month, Blue_occurrence_mean, Blue_occurrence_se)

x.orig.noinfo <- readRDS("C:/SMW/RAIMBOW/raimbow-local/Data/fishDataCA/CA_DCRB_vms_fishing_daily_2009-2019_all_vessels.RDS") %>%
  select(-contains("risk"), -contains("H_Avg_Abund"), -contains("Blue_"), 
         -Region, -CA_OFFSHOR)

grid.key <- readRDS("C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid5km_key.rds") %>% 
  select(-region_ts)

x.orig <- x.orig.noinfo %>% 
  left_join(grid.key, by = "GRID5KM_ID")
stopifnot(nrow(grid.key) == nrow(distinct(select(x.orig, GRID5KM_ID, Region, CA_OFFSHOR))))

x.whale <- readRDS("C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid5km_whale.rds") %>% 
  select(-area_km_lno) #area.key object takes care of this in risk_mgmt()

# d.join <- left_join(x.orig, x.whale)
# length(table(d.join$GRID5KM_ID[is.na(d.join$Blue_occurrence_mean)])) #24, as expected
# length(table(d.join$GRID5KM_ID[is.na(d.join$Humpback_abund_mean)])) #3, as expected


# Formatting for effort_mgmt()
season.st.date.key <- readRDS("C:/SMW/RAIMBOW/raimbow-local/RDATA_files/start_dates_by_CA_region.rds") %>% 
  mutate(crab_year = gsub("-", "_", .data$crab_season), 
         Region = unname(sapply(CA_region, simpleCap))) %>% 
  select(crab_year, Region, start_of_season_oneperc)



# x.exp <- x.orig %>% 
#   filter(year_month == "2010_04") %>% 
#   slice(1:20) %>% 
#   mutate(depth = seq(-5, -100, by = -5))


###############################################################################
### Shift/redistribute effort as specified
source("tradeoffs/Management scenarios/Mgmt_scenarios_shift_effort.R")
d <- effort_mgmt(
  x = x.orig, 
  season.st.key = season.st.date.key, 
  preseason.days = 3, 
  # season.st.backstop = NULL,
  early.data.method = "remove", 
  delay.date = NULL, #as.Date("2010-01-01"),
  delay.region = "CenCA",
  delay.method = "depth",
  delay.method.fidelity = NULL, #"temporal",
  closure.date = as.Date("2010-04-01"),
  closure.region = c("All"),
  closure.method = "depth+temporal",
  closure.redist.percent = 100,
  depth.shallow = 0, depth.deep = -100,
  # reduction.before.date = as.Date("2009-12-15"),
  # reduction.before.percent = 50,
  # reduction.before.region = "All",
  reduction.after.date = as.Date("2010-04-01"),
  reduction.after.percent = 50,
  reduction.after.region = "CenCA", 
  reduction.after.redist = TRUE, 
  reduction.after.redist.percent = 10
)

x.summ <- x.orig %>% 
  group_by(year_month, Region) %>% 
  summarise(across(DCRB_lbs:Num_Unique_DCRB_Vessels, sum, na.rm = TRUE), 
            .groups = "drop") %>% 
  mutate(year = as.numeric(substr(year_month, 1, 4)))
d.summ <- d %>% 
  group_by(year_month, Region) %>% 
  summarise(across(DCRB_lbs:Num_Unique_DCRB_Vessels, sum, na.rm = TRUE, .names = "x_{.col}"), 
            .groups = "drop") %>% 
  mutate(year = as.numeric(substr(year_month, 1, 4)))

xd <- left_join(x.summ, d.summ)



###############################################################################
# ### Shift/redistribute effort as specified
# source("tradeoffs/Management scenarios/Mgmt_scenarios_shift_effort.R")
# d <- effort_mgmt(
#   x = x.orig, #x.exp,
#   season.st.key = season.st.date.key, 
#   preseason.days = 3, 
#   # season.st.backstop = NULL,
#   early.data.method = "remove", 
#   delay.date = as.Date("2010-01-01"),
#   delay.region = "CenCA",
#   delay.method = "depth",
#   delay.method.fidelity = NULL, #"temporal",
#   closure.date = as.Date("2010-04-01"),
#   closure.region = c("All"),
#   closure.method = "depth",
#   closure.redist.percent = 50,
#   depth.shallow = 0, depth.deep = -100,
#   # reduction.before.date = as.Date("2009-12-15"),
#   # reduction.before.percent = 50,
#   # reduction.before.region = "All",
#   # reduction.after.date = as.Date("2010-04-01"),
#   # reduction.after.percent = 50,
#   # reduction.after.region = "All"
# )
# 
# # depth+temporal sanity testing
# sum(d$Num_DCRB_VMS_pings)
# x.orig.depth.logical <- !between(x.orig$depth, -100, 0) & substr(x.orig$year_month, 6, 7) %in% sprintf("%02d", 4:10) #& x.orig$Region == "NorCA"
# sum(d2$Num_DCRB_VMS_pings) - 0.5 * sum(x.orig$Num_DCRB_VMS_pings[x.orig.depth.logical])
# 
# d.summ <- d %>% 
#   group_by(year_month, date_record, GRID5KM_ID) %>% 
#   summarise(count = n(), 
#             Num_DCRB_VMS_pings = sum(Num_DCRB_VMS_pings))
# d2 <- d2 %>% arrange(year_month, date_record, GRID5KM_ID)

###############################################################################



# Look at date distribution by Region
d.summ.date <- d %>% group_by(crab_year, Region) %>% summarise(min(date_record), max(date_record))

# Look at depth distribution by year-month
d.summ.depth <- d %>% group_by(crab_year, year_month) %>% summarise(min(date_record), max(date_record), min(depth), max(depth))



### Check that effort values are the not shifting anything
d.noshift <- effort_mgmt(
  x = x.orig, season.st.key = season.st.date.key, preseason.days = 3, 
  early.data.method = "pile"
)
all.equal(x.orig %>% select(DCRB_lbs:Num_Unique_DCRB_Vessels), 
          d.noshift %>% select(DCRB_lbs:Num_Unique_DCRB_Vessels))


# s0 <- x.orig %>% group_by(year_month, GRID5KM_ID) %>% 
#   summarise(ping_sum = sum(Num_DCRB_VMS_pings))
# s1 <- d.noshift %>% group_by(year_month, GRID5KM_ID) %>% 
#   summarise(ping_sum = sum(Num_DCRB_VMS_pings))
# s2 <- effort_mgmt(
#   x = x.orig, season.st.key = season.st.date.key, preseason.days = 3, 
#   early.data.method = "remove"
# ) %>%
#   group_by(year_month, GRID5KM_ID) %>% 
#   summarise(ping_sum = sum(Num_DCRB_VMS_pings))
# s2b <- effort_mgmt(
#   x = x.orig, season.st.key = NULL, preseason.days = 3, 
#   early.data.method = "remove"
# ) %>%
#   group_by(year_month, GRID5KM_ID) %>% 
#   summarise(ping_sum = sum(Num_DCRB_VMS_pings))
# s2c <- effort_mgmt(
#   x = x.orig, season.st.key = season.st.date.key, preseason.days = 100, 
#   early.data.method = "remove"
# ) %>%
#   group_by(year_month, GRID5KM_ID) %>% 
#   summarise(ping_sum = sum(Num_DCRB_VMS_pings))
# 
# max(s0$ping_sum)
# max(s1$ping_sum)
# max(s2$ping_sum)
# max(s2b$ping_sum)
# max(s2c$ping_sum)
# 
# sum(s0$ping_sum)
# sum(s1$ping_sum)
# sum(s2$ping_sum)
# sum(s2b$ping_sum)
# sum(s2c$ping_sum)



###############################################################################
# Load and prep grid cell - area key
grid.5km.lno <- readRDS("C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid_5km_landerased.rds")
area.key <- grid.5km.lno %>% 
  st_drop_geometry() %>% 
  select(GRID5KM_ID, area_km_lno) %>% 
  distinct()

# Load saved scale vars
ca.effort.scale.vals.list <- readRDS("C:/SMW/RAIMBOW/raimbow/grid-prep/CA_fishing_metrics_range_2009_2019.rds")


### Calculate and summarize risk
source("tradeoffs/Management scenarios/Mgmt_scenarios_risk.R")
d.risk <- risk_mgmt(
  x = d, x.col = Num_DCRB_VMS_pings, y = x.whale, 
  risk.unit = "dens", area.key = area.key, scale.list = ca.effort.scale.vals.list
)
d.risk.summ <- risk_mgmt_summ(d.risk, summary.level = "BIA")



###############################################################################
source("tradeoffs/Management scenarios/make_scenarios_table.R")
scenario_table[1, ]
source("tradeoffs/Management scenarios/Mgmt_scenarios_shift_effort.R")

# for(i in 1:nrow(scenario_table)) {
scenario.output.list <- lapply(1:nrow(scenario_table), function(i, scenario_table) {
  print(i)
  # browser()
  
  # i=1 # testing. breaks because "At least one of delay.date or closure.date must not be NULL"
  # i=2 # testing. breaks because when switch() is used and does not return NULL, it returns nothing
  scenario.output.df.noinfo <- effort_mgmt(
    x = x.orig.noinfo,
    
    early.data.method = scenario_table$early.data.method[i], 
    
    delay.date = if (scenario_table$delay.date[i] == "NULL") {
      NULL
    } else {
      as.Date(scenario_table$delay.date[i])
    },
    
    delay.region = if (scenario_table$delay.region[i] == "NULL") NULL else scenario_table$delay.region[i],
    
    delay.method.shift = scenario_table$delay.method.shift[i],
    
    delay.method.fidelity = scenario_table$delay.method.fidelity[i],
    
    closure.date = if (scenario_table$closure.date[i] == "NULL") NULL else as.Date(scenario_table$closure.date[i]),
    
    closure.region = if (scenario_table$closure.region[i] == "NULL") NULL else scenario_table$closure.region[i],
    
    closure.method = scenario_table$closure.method[i],
    
    closure.redist.percent = scenario_table$closure.redist.percent[i]
  )
  
}, scenario_table = scenario_table)




