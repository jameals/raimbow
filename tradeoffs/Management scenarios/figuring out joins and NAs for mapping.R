# has the fishing season months, missing zeroes for some grid cells in some yr mths
df_fishing <- tibble(
     yr_mth = c(1:2,2),
     grid = c("A","B","A"),
     #item_name = c("a", "b", "b"),
     pings = 10:12
#     value2 = 4:6
)
df_fishing

# insert zero pings where nec
df_fishing <- df_fishing %>% 
  complete(yr_mth, nesting(grid), fill = list(pings = 0))

# has all the right grid cells, but extra yr mths
df_whales <- tibble(
  yr_mth = c(1:3, 1:3, 1:3),
  grid = c("A","A","A","B","B","B","C","C","C"),
  #item_name = c("a", "b", "b"),
  humps = c(7:9, 70:72, 80:82),
  blues = c(11:13, 110:112, 50:52)
)
df_whales

# join df's so we have all the grid cells from df_whales, but only the yr_mths from df_fishing. so we want grids A:C but only yr_mths 1:2

df_whales %>% full_join(df_fishing, by = c("grid", "yr_mth")) %>%
  filter(!(is.na(pings)) | yr_mth %in% unique(df_fishing$yr_mth)) %>% # if pings = NA and yr_mth is in df_fishing, drop
  replace_na(list(pings=0))

#############


merge(df_whales, df_fishing, c("grid", "yr_mth"), all.x = F, all.y = T)
merge(df_whales, df_fishing, c("grid", "yr_mth"), all.x = T, all.y = F)

df_whales %>% inner_join(df_fishing, by = c("grid", "yr_mth"))
df_fishing %>% inner_join(df_whales, by = c("grid", "yr_mth"))

df_whales %>% left_join(df_fishing, by = c("grid", "yr_mth")) %>%
  filter(yr_mth %in% df_fishing)
df_fishing %>% left_join(df_whales, by = c("grid", "yr_mth"))

df_whales %>% right_join(df_fishing, by = c("grid", "yr_mth"))
df_fishing %>% right_join(df_whales, by = c("grid", "yr_mth"))

df_whales %>% right_join(df_fishing, by = c("grid"))
df_whales %>% right_join(df_fishing, by = c("yr_mth"))
df_fishing %>% right_join(df_whales, by = c("grid")) 
df_fishing %>% right_join(df_whales, by = c("yr_mth"))




df_whales %>% anti_join(df_fishing, by = c("grid", "yr_mth"))
df_fishing %>% anti_join(df_whales, by = c("grid", "yr_mth"))

df_whales %>% semi_join(df_fishing, by = c("grid", "yr_mth"))
df_fishing %>% semi_join(df_whales, by = c("grid", "yr_mth"))



#############

### Examine and plot the grid cells that have non-NA fishing values but NA whale preds
sum(!(grid.studyarea.id %in% x.whale))
sum(!(grid.studyarea.id %in% risk_out_sq_complete))

whale.id <- sort(unique(x.whale$GRID5KM_ID))
fish.id <- sort(unique(risk_out_sq_complete$GRID5KM_ID))

z.whale <- grid.studyarea.id[(!(grid.studyarea.id %in% whale.id))]
z.fish <- grid.studyarea.id[(!(grid.studyarea.id %in% fish.id))]

whale.id[which(whale.id %in% fish.id == FALSE)]
fish.id[which(fish.id %in% whale.id == FALSE)]

# these are grids that are in the original daily fishing data not in the whale and fishing data
z.whale.fish <- grid.studyarea.id[(!(whale.id %in% fish.id))]
z.fish.whale <- grid.studyarea.id[(!(fish.id %in% whale.id))]

grid.5km.na.whale <- grid.5km %>% filter(GRID5KM_ID %in% c(z.whale))
grid.5km.na.fish <- grid.5km %>% filter(GRID5KM_ID %in% c(z.fish))
grid.5km.fishnona <- grid.5km %>% filter(GRID5KM_ID %in% c(grid.studyarea.id))

rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")), 
  ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>% 
    filter(admin %in% c("Canada", "Mexico")) %>% 
    st_geometry()
)

# Could save these using png() and dev.off()
# Plot - grid is blue, grid with non-NA fishing and NA whale is red
plot(st_geometry(grid.5km), axes = TRUE, border = NA, col = "blue")
plot(st_geometry(grid.5km.na.whale), add = TRUE, border = NA, col = "red")
plot(st_geometry(grid.5km.na.fish), add = TRUE, border = NA, col = "black")
plot(rmap.base, add = TRUE, border = "tan", col = NA)

# Plot - grid is blue, grid with non-NA fishing is green
plot(st_geometry(grid.5km), axes = TRUE, border = NA, col = "blue")
plot(st_geometry(grid.5km.fishnona), add = TRUE, border = NA, col = "green")
plot(rmap.base, add = TRUE, border = "tan", col = NA)




