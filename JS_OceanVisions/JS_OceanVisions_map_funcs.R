# Functions for creating maps for Jameal's Ocean Visions presentation
# April 2019


###############################################################################
# Calculate break points for plots
breaks_calc <- function(x, breaks = c(seq(0.4, 0.05, by = -0.05), 0.02)) {
  x <- stats::na.omit(x)
  x <- sort(x, decreasing = TRUE)
  
  c(-Inf, x[ceiling(breaks * length(x))], Inf)
}


###############################################################################
plot_ov_bia <- function(y.curr, y.curr.col, map.base.curr, plot.main, 
                        b.val.type, col.pal, bia.curr = NULL, bia.leg.txt, 
                        cdfw.offshore = FALSE) {
  ### Inputs:
  # y.curr: sf object (crs must be 4326) with 3 data columns: 
  #   1) B_or_A_April1 (before or after April 1), 
  #   2) Vessel_Size (vessel size), and
  #   3) Fishing data that will be used as proxy for fishing effort to calculate risk
  # y.curr.col: Column name of 'Fishing data' column of y.curr
  # map.base.curr: Base map to use in plot
  # plot.main: Title of plot
  # b.val.type: Must be one of 'perc', 'num', or 'num0'; indicates plot value 
  #   type (units). 'num0' is numeric scale with 0's kept separate
  # col.pal: Color palette
  # bia.curr: sf or sfc object of BIA polygons; will be plotted with red border
  # bia.leg.txt: Text about bia.curr to be included in plot legend
  # cdfw.offshore: Logical for plotting CA Dept of F&W offshore regions
  
  #----------------------------------------------------------------------------
  # Prep
  stopifnot(identical(st_crs(y.curr), st_crs(4326)))
  
  y.curr.df <- st_set_geometry(y.curr, NULL)
  val.len <- length(col.pal) + 1
  
  if (b.val.type == "perc") {
    # b.val calculate below for each plot
    b.perc <- c(0.40, 0.30, 0.20, 0.10, 0.05)
    leg.txt <- c(
      "Lowest 60%", "20 - 40%", "20 - 30%", "10 - 20%", "5 - 10%", "Highest 5%"
    )
    
  } else if (b.val.type == "num") {
    b.val <- seq(
      min(y.curr.df[, y.curr.col]), max(y.curr.df[, y.curr.col]), 
      length.out = val.len
    )
    b.val.txt <- format(round(b.val, 0), justify = "right") #round(b.val, 5)
    leg.txt <- paste(head(b.val.txt, -1), tail(b.val.txt, -1), sep = " - ")
    
  } else if (b.val.type == "num0") {
    b.val <- c(
      0, 1e-06, 
      tail(seq(
        min(y.curr.df[, y.curr.col]), max(y.curr.df[, y.curr.col]), 
        length.out = val.len - 1
      ), -1)
    )
    b.val.txt <- c(
      "0", format(round(tail(b.val, -2), 0), justify = "right")
    )
    leg.txt <- c(
      "0", 
      paste(head(b.val.txt, -1), tail(b.val.txt, -1), sep = " - ")
    )
    
  } else {
    stop("Invalid 'b.val.type' value. Must be one of \"perc\", \"num\", or \"num0\"")
  }
  
  
  
  #----------------------------------------------------------------------------
  # Plot
  # for vessels < 45 ft on and after April 1
  date.val <- c("pre", "post", "pre", "post")
  date.txt <- rep(c("before April 1", "on and after April 1"), 2)
  size.val <- c("small", "small", "large", "large")
  size.txt <- c(rep("for vessels < 45 ft", 2), rep("for vessels >= 45 ft", 2))
  
  layout(matrix(1:4, 2, byrow = TRUE))
  for (i in 1:4) {
    y2.curr <- y.curr %>% 
      filter(B_or_A_April1 == date.val[i], 
             Vessel_Size == size.val[i])
    
    if (b.val.type == "perc") {
      b.val <- breaks_calc(
        st_set_geometry(y2.curr, NULL)[, y.curr.col], breaks = b.perc
      )
    }
    
    plot(
      y2.curr[y.curr.col], axes = TRUE, 
      main = paste(plot.main.curr, size.txt[i], date.txt[i]), cex.main = 0.9, 
      breaks = b.val, pal = col.pal, border = NA, 
      xlim = c(-126, -118), ylim = c(33, 43), key.pos = NULL, reset = FALSE
    )
    
    if (cdfw.offshore) {
      abline(h = (42), lty = 2, col = "black")
      abline(h = (38 + 5/6), lty = 2, col = "black")
      abline(h = (34.5), lty = 2, col = "black")
      text(-126.3, 39.5, "NorCA", pos = 4)
      text(-126.3, 37, "CenCA", pos = 4)
    }
    
    plot(map.base.curr, add = TRUE, border = "black", col = "tan", lwd = 0.2)
    
    legend("topright", legend = rev(leg.txt), col = rev(col.pal), 
           pch = 15, pt.cex = 2)
    
    
    if (!is.null(bia.curr) & cdfw.offshore) {
      plot(st_geometry(bia.curr), add = T, border = "red")
      
      legend(x = par("usr")[2], y = 40, legend = c(bia.leg.txt, "Offshore regions"), 
             col = c("red", "black"), pch = c(0, NA), lty = c(NA, 2), pt.cex = 2, 
             xjust = 1, yjust = 1)
      
    } else if (cdfw.offshore) {
      legend(x = par("usr")[2], y = 40, legend = c("Offshore regions"), 
             col = c("black"), pch = c(NA), lty = c(2), pt.cex = 2, 
             xjust = 1, yjust = 1)
      
    } else if (!is.null(bia.curr)) {
      plot(st_geometry(bia.curr), add = T, border = "red")
      legend(x = par("usr")[2], y = 40, legend = c(bia.leg.txt), 
             col = c("red"), pch = c(0), lty = c(NA), pt.cex = 2, 
             xjust = 1, yjust = 1)
    }
  }
  
  
  #----------------------------------------------------------------------------
  # if (len_km_plot) {
  #   txt.labs <- c(
  #     paste("Total trackline length (km):", round(sum(y.curr$len_km), 1)),
  #     paste("Unique MMSI count:", length(unique(y.curr$MMSI)))
  #   )
  #   text(c(-122, -122), c(45, 43.5), labels = txt.labs, pos = 4, cex = txt.cex)
  #   
  # } else {
  #   txt.labs <- paste("Unique MMSI count:", length(unique(y.curr$MMSI)))
  #   text(-122, 45, labels = txt.labs, pos = 4, cex = txt.cex)
  # }
  # 
  # if(!is.null(socal.count)) {
  #   txt.lab.socal <- paste("Unique MMSI count (S of 40N):", socal.count)
  #   text(-122, 42, labels = txt.lab.socal, pos = 4, cex = txt.cex)
  #   
  # }
}

###############################################################################
