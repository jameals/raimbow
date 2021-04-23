###############################################################################
bm.q.month <- x.month %>% 
  group_by(month, Region) %>% 
  summarise(quibble2(Blue_occurrence_mean), 
            .groups = "drop") %>% 
  mutate(Blue_occurrence_mean = unname(Blue_occurrence_mean)) %>% 
  tidyr::pivot_wider(names_from = Blue_occurrence_mean_q, names_prefix = "bm_occ_", 
                     values_from = Blue_occurrence_mean)

mn.q.month <- x.month %>% 
  group_by(month, Region) %>% 
  summarise(quibble2(Humpback_dens), 
            .groups = "drop") %>% 
  mutate(Humpback_dens = unname(Humpback_dens)) %>% 
  tidyr::pivot_wider(names_from = Humpback_dens_q, names_prefix = "mn_dens_", 
                     values_from = Humpback_dens)


bm.q.ca.month <- x.month %>% 
  group_by(month) %>% 
  summarise(quibble2(Blue_occurrence_mean), 
            .groups = "drop") %>% 
  mutate(Blue_occurrence_mean = unname(Blue_occurrence_mean)) %>% 
  tidyr::pivot_wider(names_from = Blue_occurrence_mean_q, names_prefix = "bm_occ_", 
                     values_from = Blue_occurrence_mean)

mn.q.ca.month <- x.month %>% 
  group_by(month) %>% 
  summarise(quibble2(Humpback_dens), 
            .groups = "drop") %>% 
  mutate(Humpback_dens = unname(Humpback_dens)) %>% 
  tidyr::pivot_wider(names_from = Humpback_dens_q, names_prefix = "mn_dens_", 
                     values_from = Humpback_dens)



###############################################################################
x.summ.month <- x.month %>%   
  group_by(month, Region) %>%
  summarise(bm_occ_mean = mean(Blue_occurrence_mean, na.rm = TRUE),
            mn_dens_mean = mean(Humpback_dens, na.rm = TRUE),
            bm_normalized_med = median(normalized_blue, na.rm = TRUE),
            mn_normalized_med = median(normalized_humpback, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(bm.q.month, by = c("month", "Region")) %>%
  left_join(mn.q.month, by = c("month", "Region")) %>% 
  mutate(month = factor(month.abb[month], levels = month.abb))

x.summ.ca.month <- x.month %>%   
  group_by(month) %>%
  summarise(bm_occ_mean = mean(Blue_occurrence_mean, na.rm = TRUE),
            mn_dens_mean = mean(Humpback_dens, na.rm = TRUE),
            bm_normalized_med = median(normalized_blue, na.rm = TRUE),
            mn_normalized_med = median(normalized_humpback, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(bm.q.ca.month, by = c("month")) %>%
  left_join(mn.q.ca.month, by = c("month")) %>% 
  mutate(Region = "CA", 
         month = factor(month.abb[month], levels = month.abb))