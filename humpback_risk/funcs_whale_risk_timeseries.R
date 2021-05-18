# Functions used throughout the raimbow-whaleRisk directory

###############################################################################
# Generate ggplot2 time series plot
raimbow_ggplot <- function(obj.df, y, plot.main = NULL, y.lab = NULL, 
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
          legend.justification = "top")
}


# Generate ggplot2 time series plot, when regions are CA counties
raimbow_ggplot_region <- function(obj.df, y, plot.main = NULL, y.lab = NULL) {
  y <- enquo(y)
  # x <- enquo(x)
  # region <- enquo(region)
  # dc.season = enquo(dc.season)
  
  x.max <- length(unique(obj.df$ym))
  x.lab.idx <- seq(1, to = x.max, by = 3)
  x.lab <- sort(unique(obj.df$ym))[x.lab.idx]
  vert.lines <- seq(0.5, to = x.max, by = 12)
  
  obj.df %>% 
    mutate(DC_season = factor(1)) %>%
    ggplot(aes(ym, !!y, colour = region, group = region, linetype = DC_season)) + 
    geom_point() + 
    geom_path() + 
    geom_vline(xintercept = vert.lines, col = "black", lwd = 0.35) +
    
    # scale_colour_brewer(palette = "Set1", name = "Region") +
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
          legend.justification = "top")
}
###############################################################################
