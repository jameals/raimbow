# Helper functions 'Whale_risk_monthly_summ.R'; for processing and plotting


###############################################################################
# Functions

### Process and plot 3 row graphs for risk, humpbacks and fishing (x, y, z)
risksum_plot_3row <- function(x, y, z, df.key, reg.curr, plot.main, par.flag) {
  ### Inputs:
  # x:         data frame; risk values, NAs removed
  # y:         data frame; humpback abundance values
  # z:         data frame; fishing data values
  # df.key:    df.key object created early in 'Whale_risk_monthly_summ.R'
  # reg.curr:  vector of 2 numbers; latitude boundaries of current region
  # plot.main: character; title of plot, i.e. region name
  # par.flag:  logical; if true, make 3-row plot for risk, Mn, fishing
  
  x.summ <- risksum_summ(x, df.key, reg.curr, "risk_sum")
  y.summ <- risksum_summ(y, df.key, reg.curr, "humpback_sum")
  z.summ <- risksum_summ(z, df.key, reg.curr, "fish_sum")
  
  # Plot summarized risk, humpback, and fish
  if (par.flag) opar <- par(mfrow=c(3,1))
  
  risksum_plot_individual(x.summ, "risk_sum", "Risk sum", plot.main = plot.main)
  risksum_plot_individual(y.summ, "humpback_sum", "Humpback sum")
  risksum_plot_individual(z.summ, "fish_sum", "Fish sum")
  
  if (par.flag) par(opar) #Reset graphical parameters, e.g. mfrow
}


### Filter by latitude and summarize data
risksum_summ <- function(
  df.all, df.key, reg.curr, col.name, col.rm = 1:4, area.flag = FALSE, 
  area.flag2 = FALSE, area.flag.print = FALSE, area.name = NULL)
{
  ### Inputs 
  # df.all:    data.frame; current df of values to filter and summarize
  # df.key:    df.key object created early in 'Whale_risk_monthly_summ.R'
  # reg.curr:  vector of 2 numbers; latitude boundaries of current region
  # col.name:  character; name of column of summarized data
  # col.rm:    column indices to remove from df.all before summarizing
  # area.flag: logical; divide summed values by sum of areas for any cells in
  #   timeseries with a non-NA value
  # area.flag2: logical; divide summed values by sum of areas for cells
  #   with non-NA values for that specific month
  # area.flag.print: logical; if true, print the area covered by non-NA 
  #   cells in df.all
  
  stopifnot(
    (ncol(df.all) - length(col.rm)) == nrow(df.key)
  )
  
  # if (any(is.na(df.all))) warning("Some risk values are NA")
  
  df.filt <- df.all %>% 
    filter(LATITUDE > reg.curr[1], LATITUDE <= reg.curr[2])
  
  if (area.flag) {
    area.sum <- sum(df.filt$area_km_lno, na.rm = TRUE)
    df.filt <- select(df.filt, -col.rm) / area.sum
    
    if (area.flag.print) 
      print(paste0(area.name, " - ", round(area.sum, 2), "km2"))
    
  } else if (area.flag2) {
    area2.func <- function(i, j) {i / sum(j[!is.na(i)])}
    df.filt.area <- df.filt$area_km_lno
    df.filt <- df.filt %>% 
      select(-col.rm) %>% 
      mutate_all(area2.func, j = df.filt.area)
  } else {
    df.filt <- select(df.filt, -col.rm)
  }
  
  
  data.frame(
    ym_num = seq_len(nrow(df.key)), 
    ym = paste(df.key$year, df.key$month, sep = "-"), 
    tmp_sum = apply(df.filt, 2, sum, na.rm = TRUE), 
    stringsAsFactors = FALSE
  ) %>% 
    set_names(c("ym_num", "ym", col.name))
}


### Plot individual graphs
risksum_plot_individual <- function(
  df.sum, col.name, ylab, plot.main = NA)
{
  df.nrow <- nrow(df.sum)
  axis.idx = seq(from = 1, to = df.nrow, by = 3)
  
  plot(df.sum[, col.name], axes = FALSE, ylab = ylab, xlab = NA, main = plot.main)
  axis(2)
  axis(1, at = (1:df.nrow)[axis.idx], labels = df.sum$ym[axis.idx], las = 3, cex.axis = 1)
  abline(v = seq(from = 4.5, to = df.nrow, by = 12), col = "grey")
  # abline(v = 36.5, col = "red")
  graphics::box()
}


###############################################################################
### Helper plotting function 1
risksum_plot_allin1_plot <- function(
  obj.list, col.name, plot.main, list.col, thirdly.flag, ...) 
{
  x.max.curr <- nrow(obj.list[[1]])
  y.max.curr <- max(sapply(obj.list, function(i) max(i[, col.name], na.rm = TRUE)))
  axis.idx = seq(from = 1, to = x.max.curr, by = 3)
  axis.idx.season <- which(substr(obj.list[[1]]$ym, 6, 7) == "11") - 0.5
  
  plot.new()
  plot.window(xlim = c(0, x.max.curr*1.15), ylim = c(0, 1.05*y.max.curr))
  title(main = plot.main, ...)
  for (i in seq_along(obj.list)) {
    points(x = seq_len(x.max.curr), y = obj.list[[i]][, col.name], col = list.col[i], pch = 19)
    lines(x = seq_len(x.max.curr), y = obj.list[[i]][, col.name], col = list.col[i], lty = 1)
  }
  
  axis(2)
  rug(x = seq_len(x.max.curr), ticksize = -0.02)
  axis(1, at = seq_len(x.max.curr)[axis.idx], labels = obj.list[[1]]$ym[axis.idx], las = 3, cex.axis = 1)
  abline(v = axis.idx.season, col = "grey")
  if (thirdly.flag) abline(v = 36.5, col = "black")
  graphics::box()
}

### helper plotting function 2, legend
risksum_plot_allin1_plotlegend <- function(leg.txt, leg.col, thirdly.flag) {
  d <- length(leg.txt)
  stopifnot(d == length(leg.col))
  
  leg.txt <- c(leg.txt, "DC season")
  leg.col <- c(leg.col, "grey")
  leg.pch <- c(rep(19, d), NA)
  leg.lty <- rep(1, d+1)
  
  if (thirdly.flag) {
    leg.txt <- c(leg.txt, "Jun-Jul, 2013")
    leg.col <- c(leg.col, "black")
    leg.pch <- c(leg.pch, NA)
    leg.lty <- c(leg.lty, 1)
  }
  
  legend("topright", legend = leg.txt, col = leg.col, pch = leg.pch, lty = leg.lty)
}


### Plot sums for all regions on one plot
risksum_plot_allin1 <- function(
  x, y, z, df.key, reg.list, col.pal, plot.main.list, area.flag, area.flag2, 
  par.flag, thirdly.flag, x.ylab = NULL, y.ylab = NULL, z.ylab = NULL) 
{
  
  x.list <- mapply(function(i, j) {
    risksum_summ(x, df.key, i, "risk_sum", area.flag = area.flag, area.flag2 = area.flag2, 
                 area.flag.print = area.flag, area.name = j)
  }, i = reg.list, j = names(reg.list), SIMPLIFY = FALSE)
  y.list <- lapply(reg.list, function(i) {
    risksum_summ(y, df.key, i, "humpback_sum", area.flag = area.flag, area.flag2 = area.flag2, 
                 area.flag.print = FALSE)
  })
  z.list <- lapply(reg.list, function(i) {
    risksum_summ(z, df.key, i, "fish_sum", area.flag = area.flag, area.flag2 = area.flag2, 
                 area.flag.print = FALSE)
  })
  
  
  if (par.flag) opar <- par(mfrow=c(3,1))
  list.col <- RColorBrewer::brewer.pal(length(reg.list), col.pal)
  
  risksum_plot_allin1_plot(x.list, "risk_sum", plot.main.list[1], list.col, thirdly.flag, ylab = x.ylab)
  risksum_plot_allin1_plotlegend(names(reg.list), list.col, thirdly.flag)
  
  risksum_plot_allin1_plot(y.list, "humpback_sum", plot.main.list[2], list.col, thirdly.flag, ylab = y.ylab)
  risksum_plot_allin1_plotlegend(names(reg.list), list.col, thirdly.flag)
  
  risksum_plot_allin1_plot(z.list, "fish_sum", plot.main.list[3], list.col, thirdly.flag, ylab = z.ylab)
  risksum_plot_allin1_plotlegend(names(reg.list), list.col, thirdly.flag)
  
  if (par.flag) par(opar) #Reset graphical parameters, e.g. mfrow
}

###############################################################################
