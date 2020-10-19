# Code to generate specific time series plots for KAF Entanglement Science Workshop presentation
#   'Objects are from 3_Whale_risk_timeseries.Rmd' - most of this file must be run first


#------------------------------------------------------------------------------
raimbow_ggplot_esw <- function(obj.df, y, plot.main = NULL, y.lab = NULL, filter.reg.idx) {
  ### Inputs
  # obj.df: summary of risk, etc, by month and region
  # y: unquoted name of value to be plotted
  # wa.flag: logical indicating whether WA values should be plotted
  # filter.reg.idx: index number indicating regions to be filtered for
  
  y <- enquo(y)
  # x <- enquo(x)
  # region <- enquo(region)
  # dc.season = enquo(dc.season)
  
  x.max <- length(unique(obj.df$ym))
  x.lab.idx <- seq(1, to = x.max, by = 3)
  x.lab <- sort(unique(obj.df$ym))[x.lab.idx]
  vert.lines <- seq(0.5, to = x.max, by = 12)
  
  
  
  
  reg.vals <- levels(obj.df$region)[filter.reg.idx]
  cols.vals <- RColorBrewer::brewer.pal(5, "Set1")[filter.reg.idx]
  
  obj.df <- obj.df %>%
    filter(yr < 2014, !(yr == 2013 & mon %in% c(12)), 
           region %in% reg.vals) 
  y.max <- max(select(obj.df, !!y))
  
  # if (!wa.flag) {
  #   obj.df <- filter(obj.df, region != "WA")
  # }
  
  # if (!is.null(filter.reg.val)) {
  #   obj.df <- obj.df %>% filter(region %in% filter.reg.val)
  # }
  
  obj.df %>% 
    mutate(DC_season = factor(1)) %>% 
    ggplot(aes(ym, !!y, colour = region, group = region, linetype = DC_season)) + 
    geom_point() + 
    geom_path() + 
    geom_vline(xintercept = vert.lines, col = "black", lwd = 0.35) +
    
    scale_colour_manual(values = cols.vals, drop = TRUE, name = "Region") +
    # scale_colour_brewer(palette = "Set1", name = "Region", drop = FALSE) +
    guides(linetype = guide_legend(title = "DC season", label = FALSE, 
                                   override.aes = list(colour = "black"))) + 
    
    # coord_cartesian(xlim = c(-1, x.max)) + 
    scale_x_discrete(breaks = x.lab) + 
    xlab (NULL) + 
    ylab(y.lab) + 
    ylim(0, y.max) + 
    ggtitle(plot.main) + 
    # theme_classic() +
    theme(plot.title = element_text(hjust = 0.5), 
          axis.text.x = element_text(angle = 90, vjust = 0.4), 
          legend.justification = "top")
}


### State-specific plots of Mn abundance for first two years
p1 <- raimbow_ggplot_esw(
  all.df.summ, mn_sum_dens, plot.main = "Humpback whales - Washington",
  y.lab = "Whales / km2", filter.reg.idx = 1
)

p2 <- raimbow_ggplot_esw(
  all.df.summ, mn_sum_dens, plot.main = "Humpback whales - Oregon",
  y.lab = "Whales / km2", filter.reg.idx = 2
)

p3 <- raimbow_ggplot_esw(
  all.df.summ, mn_sum_dens, plot.main = "Humpback whales - California",
  y.lab = "Whales / km2", filter.reg.idx = c(3, 4, 5)
)

p123 <- grid.arrange(p1, p2, p3, nrow = 3)

ggsave(filename = "Mn_timeseries_ESW.png", p123, path = "C:/SMW/RAIMBOW/raimbow-local/Plots", 
       height = 7, width = 7)


#------------------------------------------------------------------------------
raimbow_ggplot_esw2 <- function(obj.df, y, plot.main = NULL, y.lab = NULL, 
                                wa.flag = TRUE) {
  ### Inputs
  # obj.df: summary of risk, etc, by month and region
  # y: unquoted name of value to be plotted
  # wa.flag: logical indicating whether WA values should be plotted
  
  y <- enquo(y)
  # x <- enquo(x)
  # region <- enquo(region)
  # dc.season = enquo(dc.season)
  
  x.max <- length(unique(obj.df$ym))
  x.lab.idx <- seq(1, to = x.max, by = 3)
  x.lab <- sort(unique(obj.df$ym))[x.lab.idx]
  vert.lines <- seq(0.5, to = x.max, by = 12)
  
  if (!wa.flag) {
    obj.df <- filter(obj.df, region != "WA")
  }
  
  obj.df %>% 
    mutate(DC_season = factor(1)) %>%
    ggplot(aes(ym, !!y, colour = region, group = region, linetype = DC_season)) + 
    geom_point() + 
    geom_path() + 
    geom_vline(xintercept = vert.lines, col = "black", lwd = 0.35) +
    
    scale_colour_brewer(palette = "Set1", name = "Region", drop = FALSE) +
    guides(linetype = guide_legend(title = "DC season", label = FALSE, 
                                   override.aes = list(colour = "black"))) + 
    
    coord_cartesian(xlim = c(-1, x.max)) + 
    scale_x_discrete(breaks = x.lab) + 
    xlab (NULL) + 
    ylab(y.lab) + 
    ggtitle(plot.main) + 
    # theme_classic() +
    theme(plot.title = element_text(hjust = 0.5), 
          axis.text.x = element_text(angle = 90, vjust = 0.4), 
          legend.position = c("left"), 
          legend.justification = c("top"))
}


### Other plot
p.mn <- raimbow_ggplot_esw2(
  all.df.summ, mn_sum_dens, plot.main = "Humpback whales", 
  y.lab = "Whales / km2", wa.flag = TRUE
)
p.mn
ggsave(filename = "Mn_timeseries_ESW_all.png", p.mn, path = "C:/SMW/RAIMBOW/raimbow-local/Plots", 
       height = 3.4, width = 10)

#------------------------------------------------------------------------------
