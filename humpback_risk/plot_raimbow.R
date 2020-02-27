# Functions to plot objects for RAIMBOW work

###############################################################################
# plot_raimbow
# Assumes objects have GRID5KM_ID column

#------------------------------------------------------------------------------
plot_raimbow <- function(x, x.col, grid.sf = NULL, map.base = NULL, 
                         map.b1 = NULL, map.b2 = NULL, 
                         col.breaks, col.pal, 
                         key.pos = NULL, reset = FALSE, ...) 
  UseMethod("plot_raimbow")
### Inputs: 
# x: data frame or sf object
# grid.sf


#------------------------------------------------------------------------------
plot_raimbow.data.frame <- function(x, x.col, grid.sf, map.base, map.b1, map.b2, 
                                    col.breaks, col.pal, 
                                    key.pos, reset, ...) {
  if (is.null(grid.sf)) stop("grid.sf cannot be NULL if x is a data frame")
  stopifnot(
    "GRID5KM_ID" %in% names(x), 
    "GRID5KM_ID" %in% names(grid.sf)
  )
  
  tmp <- x %>% 
    left_join(grid.sf, by = "GRID5KM_ID") %>% 
    st_sf(agr = "constant")
  
  plot_raimbow(tmp, x.col, NULL, map.base, map.b1, map.b2, col.breaks, col.pal, 
               key.pos, reset, ...)
}


#------------------------------------------------------------------------------
plot_raimbow.sf <- function(x, x.col, grid.sf, map.base, map.b1, map.b2, 
                            col.breaks, col.pal, key.pos, reset, ...) {
  stopifnot(require(maps))
  if (is.null(map.base))
    map.base <- st_geometry(st_as_sf(maps::map('world', plot = FALSE, fill = TRUE)))
  
  lst <- list(...)
  
  plot(x[x.col], axes = TRUE, border = NA, 
       breaks = col.breaks, pal = col.pal, 
       key.pos = NULL, reset = FALSE, 
       ...)
  plot(map.base, add = TRUE, col = "tan", border = NA)
  if (!is.null(map.b1)) plot(map.b1, add = TRUE, col = "grey", lwd = 0.5)
  if (!is.null(map.b2)) plot(map.b2, add = TRUE, col = "black", lwd = 0.5)
  graphics::box()
  
  if ("xaxt" %in% names(lst)) if (lst$xaxt == "n") .degAxis(1, at = c(-125, -120))
  if ("yaxt" %in% names(lst)) if (lst$yaxt == "n") .degAxis(2)
}


###############################################################################
# Create legend from break points
legend_raimbow <- function(col.breaks, sprintf.code = "%0.2f", 
                           leg.add = NULL, ...) {
  ### Inputs:
  # col.breaks: numeric; vector of break point values
  # sprintf.code: character or NULL; fmt argument of sprintf
  # bathy.flag: character; text to append to legend text
  # ... Arguments passed to legend() call
  # Note that the palette must be passed to either col or fill
  
  if (is.null(sprintf.code)) {
    leg.txt <- rev(paste(
      head(col.breaks, -1), tail(col.breaks, -1), 
      sep = " - "
    ))
  } else {
    leg.txt <- rev(paste(
      sprintf(sprintf.code, head(col.breaks, -1)), 
      sprintf(sprintf.code, tail(col.breaks, -1)), 
      sep = " - "
    ))
  }
  
  
  legend("topright", legend = c(leg.txt, leg.add), ...)
}

# Default legend for contour lines
legend.raimbow.bathy <- function(...) {
  legend(
    "right", title = "Isobaths", legend = c("100m", "200m"), 
    col = c("grey", "black"), pch = NA, lty = 1, lwd = 2, cex = 1, 
    ...
  )
}

###############################################################################
