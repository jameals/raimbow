# Functions to plot objects for RAIMBOW work

###############################################################################
# plot_raimbow
# Assumes objects have GRID5KM_ID column

#------------------------------------------------------------------------------
plot_raimbow <- function(x, x.col, grid.sf = NULL, map.base = NULL, 
                         col.breaks, col.pal, 
                         key.pos = NULL, reset = FALSE, ...) 
  UseMethod("plot_raimbow")
### Inputs: 
# x: data frame or sf object
# grid.sf


#------------------------------------------------------------------------------
plot_raimbow.data.frame <- function(x, x.col, grid.sf, map.base, 
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
  
  plot_raimbow(tmp, x.col, NULL, map.base, col.breaks, col.pal, 
               key.pos, reset, ...)
}


#------------------------------------------------------------------------------
plot_raimbow.sf <- function(x, x.col, grid.sf, map.base, col.breaks, col.pal, 
                            key.pos, reset, ...) {
  stopifnot(require(maps))
  if (is.null(map.base))
    map.base <- st_geometry(st_as_sf(maps::map('world', plot = FALSE, fill = TRUE)))
  
  lst <- list(...)

  plot(x[x.col], axes = TRUE, border = NA, 
       breaks = col.breaks, pal = col.pal, 
       key.pos = NULL, reset = FALSE, 
       ...)
  plot(map.base, add = TRUE, col = "tan", border = "black")
  graphics::box()
  
  # browser()
  if ("xaxt" %in% names(lst)) if (lst$xaxt == "n") .degAxis(1, at = c(-125, -120))
  if ("yaxt" %in% names(lst)) if (lst$yaxt == "n") .degAxis(2)
  
  # plot(x[x.col], main = plot.title, axes = TRUE, xlim = xlim, ylim = ylim, 
  #      breaks = col.breaks, pal = col.pal, 
  #      cex.main = 0.9, cex.axis = 0.75, xlab = "FALSE", 
  #      key.length = 1, key.pos = legend.pos, reset = FALSE
  #     )
}


###############################################################################
# Create legend from break points
legend_raimbow <- function(col.breaks, sprintf.code = "%0.2f", ...) {
  ### Inputs:
  # col.breaks: numeric; vector of break point values
  # sprintf.code: character or NULL; fmt argument of sprintf
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
  
  
  legend("topright", legend = leg.txt, ...)
}

###############################################################################
