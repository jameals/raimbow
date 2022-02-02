###############################################################################
# Sum risk after filtering for desired months
mgmt_risk_sum <- function(x1, x2, mon.keep, wa.flag = TRUE) {
  tmp <- x1 %>% 
    filter(mon %in% mon.keep) %>% 
    group_by(region, fish_season) %>% 
    summarise(risk_total_season = sum(risk_sum_total), 
              risk_dens_season = sum(risk_sum_dens), 
              .groups = "drop") %>% 
    mutate(risk_total_ratio = risk_total_season / x2$risk_total_season, 
           risk_dens_ratio = risk_dens_season / x2$risk_dens_season, 
           risk_total_diff = risk_total_season - x2$risk_total_season, 
           risk_dens_diff = risk_dens_season - x2$risk_dens_season)
  
  if (wa.flag) {
    tmp
  } else {
    filter(tmp, region != "WA")
  }
}

# Plot of percentage of risk remaining for the season after applying scenario
mgmt_plot_perc <- function(x1, x2, x.title, x.xlab, x.ylab) {
  ggplot(x1, aes(fish_season, risk_dens_ratio, colour = region, group = region)) + 
    geom_point() + 
    geom_path() + 
    scale_colour_brewer(palette = "Set1", name = "Region", drop = FALSE) + 
    ggtitle(x.title) + 
    xlab(x.xlab) + 
    ylab(x.ylab) + 
    coord_cartesian(ylim = c(0, 1)) +
    scale_x_continuous(breaks = seq_along(unique(x2$fish_season)), 
                       labels = unique(x2$yr))
}


# Plot of risk removed by management scenario
mgmt_plot_diff <- function(x1, x2, x.title, x.xlab, x.ylab) {
  ggplot(x1, aes(fish_season, risk_dens_diff, colour = region, group = region)) + 
    geom_point() + 
    geom_path() + 
    scale_colour_brewer(palette = "Set1", name = "Region", drop = FALSE) + 
    ggtitle(x.title) + 
    xlab(x.xlab) + 
    ylab(x.ylab) + 
    coord_cartesian(ylim = c(-1, 0)) +
    scale_x_continuous(breaks = seq_along(unique(x2$fish_season)), 
                       labels = unique(x2$yr))
}


# Plot of summed risk density for the year with and without management scenario
mgmt_plot_bar <- function(x1, x2, v1, x.title, x.ylab) {
  v1 <- enquo(v1)
  
  x.new <- bind_rows(
    x2 %>% select(region, fish_season, risk_total_season, risk_dens_season), 
    x1 %>% select(region, fish_season, risk_total_season, risk_dens_season)
  ) %>% 
    mutate(orig = factor(c(rep("Original", nrow(x.orig)), rep("Closure", nrow(x1)))))
  
  
  ggplot(x.new, aes(fish_season, !!v1, fill = orig)) +
    geom_col(position = "dodge") +
    facet_wrap(facets = vars(region), nrow = 2, drop = TRUE) +
    guides(fill = guide_legend(title = "Scenario")) +
    ggtitle(x.title) +
    xlab(xlab.all) +
    ylab(x.ylab) +
    scale_x_continuous(breaks = seq_along(unique(x2$fish_season)),
                       labels = unique(x2$yr)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.4))
}

# Plot of summed risk for the year without management scenario, and with orig value as dot above
mgmt_plot_bardot <- function(x1, x2, v1, x.title, x.ylab, wa.flag = TRUE) {
  v1 <- enquo(v1)
  
  if (!wa.flag) x2 <- x2 %>% filter(region != "WA")
  
  ggplot() +
    geom_col(data = x1, aes(fish_season, !!v1, fill = "mgmt")) +
    geom_point(data = x2, aes(fish_season, !!v1, colour = "orig")) + 
    facet_wrap(facets = vars(region), nrow = 2, drop = TRUE) +
    scale_fill_manual(name = "Risk", 
                      values = c('mgmt' = hue_pal()(1)), 
                      labels = "Management") + 
    scale_color_manual(name = "", 
                       values = c('orig' = 'black'), 
                       labels = "Original") + 
    ggtitle(x.title) +
    xlab(xlab.all) +
    ylab(x.ylab) +
    scale_x_continuous(breaks = seq_along(unique(x2$fish_season)),
                       labels = unique(x2$yr)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.4))
}


###############################################################################
# Function for 
mgmt_disp <- function(x, mon.cut, mon.dist, grid.region.summ, wa.flag = TRUE) {
  # Area of each region
  reg.areas <- x %>% 
    filter(!duplicated(GRID5KM_ID)) %>% 
    group_by(region) %>% 
    summarize(area_sum = sum(area_km_lno), 
              .groups = "drop")
  
  # Effort, summed by region and fishing season, to be redistributed
  eff.toredist <- x %>% 
    filter(mon %in% mon.cut) %>% 
    group_by(region, fish_season) %>% 
    summarise(yr_min = min(yr), 
              eff = sum(vms_pings, na.rm = TRUE), 
              .groups = "drop")
  
  # Total effort, by region and fishing season, in month from which to get redistribution proportions
  eff.redist.total <- x %>%
    filter(mon == mon.dist) %>%
    group_by(region, fish_season) %>%
    summarise(eff_sum_region = sum(vms_pings, na.rm = TRUE), 
              .groups = "drop")
  
  # 
  stopifnot(length(mon.dist) == 1)
  eff.redist.perc <- x %>% 
    filter(mon == mon.dist) %>% 
    left_join(eff.redist.total) %>% 
    mutate(eff_redist_perc = vms_pings / eff_sum_region) %>% 
    left_join(grid.region.summ) %>% 
    mutate(eff_redist_perc2 = ifelse(is.na(eff_redist_perc), 1/count, eff_redist_perc), 
           region = factor(region, levels = c("WA", "OR", "CA-N", "CA-Cen", "CA-SCen")))
  
  #
  eff.toadd <- eff.redist.perc %>% 
    left_join(eff.toredist) %>% 
    mutate(eff_toadd = eff * eff_redist_perc2, 
           vms_pings = vms_pings + eff_toadd) %>% 
    select(GRID5KM_ID, region, area_km_lno, fish_season, yr, mon, 
           mn_dens, vms_pings, eff_toadd)
  stopifnot(
    isTRUE(all.equal(sum(eff.toadd$eff_toadd), sum(eff.toredist$eff))), 
    sum(x$mon == 1) == nrow(eff.toadd)
  )
  
  # Summarize 'new' risk data for management plot
  tmp <- x %>% 
    filter(!(mon %in% c(mon.cut, mon.dist))) %>% 
    bind_rows(select(eff.toadd, -eff_toadd)) %>% 
    arrange(region, fish_season, GRID5KM_ID) %>% 
    mutate(risk_sum_total = mn_dens * vms_pings * area_km_lno) %>% 
    group_by(region, fish_season) %>% 
    summarise(yr = min(yr), 
              area_sum = sum(area_km_lno[!duplicated(GRID5KM_ID)]), 
              risk_total_season = sum(risk_sum_total), 
              risk_dens_season = risk_total_season / area_sum, 
              .groups = "drop")
  
  
  if (wa.flag) {
    tmp
  } else {
    filter(tmp, region != "WA")
  }
}

###############################################################################
