# Overlay 3km Mn predictions, but the file from Karin is an rds file 
#   with the values already aggregated (averaged) by month

###############################################################################
library(dplyr)
library(eSDM)
library(lubridate)
library(purrr)
library(readr)
library(sf)
library(tidyr)

sf_use_s2(FALSE)

# source(here::here("whalepreds_aggregate", "Whalepreds_aggregate.R"), local = TRUE, echo = FALSE)
source(here::here("User_script_local.R"), local = TRUE, echo = FALSE)

if (user == "JS") {
  
} else if (user == "SMW") {
  path.mn.preds <- "C:/SMW/RAIMBOW/raimbow-local/Data/Humpback 3km models/Mn_3km_2005-01-01to2020-09-29_monthly.rds"
  path.grid.5km.lno <- "C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid_5km_landerased.rds"
  
  # file.out.all.csv <- "C:/SMW/RAIMBOW/raimbow-local/Outputs/Humpback_5km_wide_bidaily_dens_allyrs.csv"
  # file.out.csv <- "C:/SMW/RAIMBOW/raimbow-local/Outputs/Humpback_5km_wide_bidaily_dens.csv"
  # file.out.rds <- "C:/SMW/RAIMBOW/raimbow-local/Outputs/Humpback_5km_wide_bidaily_dens.rds"
  file.out <- "C:/SMW/RAIMBOW/raimbow-local/Outputs/Humpback_5km_long_monthly.rds"
}


###############################################################################
# Remove grid cells that always have NA predictions
x <- readRDS(path.mn.preds)
x.nona <- x %>% 
  rowwise() %>% 
  filter(!all(is.na(c_across(Avg_monthly_2005_01_01:Avg_monthly_2020_09_01)))) %>% 
  ungroup() %>% 
  select(mlon, mlat, starts_with("Avg_m")) %>% 
  eSDM::pts2poly_centroids(0.027 / 2, crs = 4326, agr = "constant")



###############################################################################
# Overlay

### Land-erased polygon
grid.5km.lno <- readRDS(path.grid.5km.lno)


### Overlay and sanity checks
tmp.over <- overlay_sdm( # <10 min with sf_use_s2(FALSE); otherwise hours
  st_geometry(grid.5km.lno), x.nona, 1:(ncol(x.nona) - 1), 
  overlap.perc = 0
)

# save.image("C:/SMW/RAIMBOW/raimbow-local/RDATA_files/save_image_nos2.rdata")
# load("C:/SMW/RAIMBOW/raimbow-local/RDATA_files/save_image_nos2.rdata")

mn.preds.lno.5kmover <- tmp.over %>%
  mutate(GRID5KM_ID = grid.5km.lno$GRID5KM_ID,
         area_km_lno = as.numeric(units::set_units(st_area(geometry), "km^2"))) %>%
  select(GRID5KM_ID, area_km_lno, starts_with("Avg_Monthly"))

# mn.over.nona <- mn.preds.lno.5kmover %>% 
#   rowwise() %>% 
#   filter(!all(is.na(c_across(Avg_monthly_2005_01_01:Avg_monthly_2020_09_01)))) %>% 
#   ungroup()

# ### Sanity checks
# # Sanity check 0
# identical(st_geometry(mn.preds.lno.5kmover), st_geometry(grid.5km.lno))
# 
# # Sanity check 1
# column.name <- "Avg_monthly_2018_08_01"
# # eSDM::model_abundance(x, column.name)
# eSDM::model_abundance(x.nona, column.name)
# eSDM::model_abundance(mn.preds.lno.5kmover, column.name)
# # eSDM::model_abundance(mn.over.nona, column.name)
# sum(mn.preds.lno.5kmover[[column.name]] * mn.preds.lno.5kmover$area_km_lno, na.rm = TRUE)
# # sum(mn.over.nona[[column.name]] * mn.over.nona$area_km_lno, na.rm = TRUE)
# 
# # Sanity check 2
# plot(x.nona[column.name], axes = T, border = NA)
# plot(mn.preds.lno.5kmover[column.name], axes = T, border = NA)
# # plot(mn.over.nona[column.name], axes = T, border = NA)




###############################################################################
# Make data long, and save
mn.proc.long <- mn.preds.lno.5kmover %>%
  st_drop_geometry() %>%
  pivot_longer(cols = starts_with("Avg_monthly_"),
               names_to = "key",  values_to = "Humpback_dens_mean") %>%
  filter(!is.na(Humpback_dens_mean)) %>% 
  mutate(date = ymd(substr(key, 13, 22)),
         Humpback_dens_se = NA_real_) %>%
  select(GRID5KM_ID, area_km_lno, date, Humpback_dens_mean, Humpback_dens_se) %>% 
  arrange(date, GRID5KM_ID)

saveRDS(mn.proc.long, file = file.out)


# # Sanityyyy
# d <- readRDS("C:/SMW/RAIMBOW/raimbow-local/Outputs/Humpback_5km_long_monthly - Copy.rds")
# z <- full_join(d, mn.proc.long, by = c("date", "GRID5KM_ID"))
# z.1 <- z %>% filter(area_km_lno.x != area_km_lno.y)
# z.2 <- z %>% filter(abs(Humpback_dens_mean.x - Humpback_dens_mean.y) > 0.001)
