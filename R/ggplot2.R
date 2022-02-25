# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------


#' Plot theme to mimic the look of SPSS graphs
#'
#' Complete theme that controls all non-data display of a plot to mimic the
#' look of SPSS graphs.  Use \code{\link[ggplot2]{theme}} after
#' \code{theme_SPSS} to further tweak the display.
#'
#' @param base_size  an integer giving the base font size in pts.
#' @param base_family  a character string giving the base font family.
#' @param base_line_size  base size for line elements.
#' @param base_rect_size  base size for borders of rectangle elements.
#' @param version  a character string specifying whether to mimic the look
#' of recent SPSS versions (\code{"modern"}) or older versions (<24;
#' \code{"legacy"}).
#' @param scales,scale.x,scale.y  a character string specifying whether both
#' or each of the axes are expected to be continuous (\code{"continuous"}) or
#' discrete (\code{"discrete"}).  Note that this only controls the appearance
#' of the tick labels on the axis, as the theme has no control over the
#' information that is displayed in the plot.  SPSS displays larger tick
#' labels a for discrete axis than for a continuous axis, hence specifying
#' this information in the theme adjusts the tick label size accordingly.
#' The default (\code{NULL}) means that the tick labels are of the same size
#' as for a continuous axis.
#'
#' @examples
#' # data to be plotted
#' df <- data.frame(x = 1:30, y = 0)
#'
#' # initialize plot
#' p <- ggplot(aes(x = x, y = y, fill = factor(x)), data = df) +
#'   geom_point(shape = 21, size = 3, show.legend = FALSE) +
#'   theme_SPSS()
#'
#' # colors of modern SPSS versions
#' p + theme_SPSS() + scale_fill_SPSS()
#'
#' # colors of legacy SPSS versions
#' p + theme_SPSS(version = "legacy") +
#'   scale_fill_SPSS(version = "legacy")
#'
#' @keywords aplot
#'
#' @import ggplot2
#' @export

theme_SPSS <- function(base_size = 12, base_family = "",
                       base_line_size = 0.5,
                       base_rect_size = 0.5,
                       version = r2spssOptions$get("version"),
                       scales = NULL, scale.x = scales, scale.y = scales) {
  # initializations
  version <- match.arg(version, choices = getVersionValues())
  scales <- match.arg(scales, choices = getScaleValues())
  scale.x <- match.arg(scale.x, choices = getScaleValues())
  scale.y <- match.arg(scale.y, choices = getScaleValues())
  # useful measurements
  half_line <- base_size / 2
  quarter_line <- half_line / 2
  tiny <- 0.8   # multiplication factor to make some elements tiny
  small <- 0.9  # multiplication factor to make some elements smaller
  large <- 1.2  # multiplication factor to make some elements larger
  # define some elements to be used in the theme
  if (version == "legacy") {
    ticks_length <- unit(half_line, "pt")
    axis_text_margin <- tiny * quarter_line
    panel_background <- element_rect(fill = "#F0F0F0", color = NA)
    panel_border <- element_rect(fill = NA, color = "black")
    grid_lines <- element_blank()
  } else {
    ticks_length <- unit(0, "pt")
    axis_text_margin <- quarter_line
    panel_background <- element_rect(fill = "white", color = NA)
    panel_border <- element_blank()
    grid_lines <- element_line(color = "#AEAEAE")
  }

  # define theme
  theme(
    # basic elements
    line = element_line(color = "black", size = base_line_size,
                        linetype = "solid", lineend = "butt"),
    rect = element_rect(fill = "white",  color = "black",
                        size = base_rect_size, linetype = "solid"),
    text = element_text(family = base_family, face = "plain", color = "black",
                        size = base_size, hjust = 0.5, vjust = 0.5, angle = 0,
                        lineheight = 0.9, margin = margin(), debug = FALSE),
    title = element_text(face = "bold"),
    aspect.ratio = NULL,
    # axis labels
    # axis.title = element_text(face = "bold"),
    axis.title = NULL,
    axis.title.x = element_text(vjust = 1,
                                margin = margin(t = half_line)),
    axis.title.y = element_text(vjust = 1,
                                angle = 90,
                                margin = margin(r = half_line)),
    axis.title.x.top = element_text(vjust = 0,
                                    margin = margin(b = half_line)),
    axis.title.x.bottom = NULL,
    axis.title.y.left = NULL,
    axis.title.y.right = element_text(vjust = 0,
                                      angle = -90,
                                      margin = margin(l = half_line)),
    # axis tick labels
    axis.text = element_text(color = "black",
                             size = if (scales != "discrete") rel(tiny)),
    axis.text.x = element_text(size = if (scale.x == "discrete") base_size,
                               vjust = 1,
                               margin = margin(t = axis_text_margin)),
    axis.text.y = element_text(size = if (scale.y == "discrete") base_size,
                               hjust = 1,
                               margin = margin(r = axis_text_margin)),
    axis.text.x.top = element_text(vjust = 0,
                                   margin = margin(b = axis_text_margin)),
    axis.text.x.bottom = NULL,
    axis.text.y.left = NULL,
    axis.text.y.right = element_text(hjust = 0,
                                     margin = margin(l = axis_text_margin)),
    # axis tick marks
    axis.ticks = NULL,
    axis.ticks.x = NULL,
    axis.ticks.x.top = NULL,
    axis.ticks.x.bottom = NULL,
    axis.ticks.y = NULL,
    axis.ticks.y.left = NULL,
    axis.ticks.y.right = NULL,
    # length of axis tick marks
    axis.ticks.length = ticks_length,
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    # axis line
    axis.line = if (version == "legacy") element_blank(),
    axis.line.x = NULL,
    axis.line.x.top = if (version == "modern") element_blank(),
    axis.line.x.bottom = if (version == "modern") element_line(),
    axis.line.y = NULL,
    axis.line.y.left = if (version == "modern") element_line(),
    axis.line.y.right = if (version == "modern") element_blank(),
    # legend
    legend.background = element_rect(color = NA),  # suppress border
    legend.spacing = unit(base_size, "pt"),        # space between legends
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    # legend keys
    legend.key = element_rect(color = NA),
    legend.key.size = NULL,
    legend.key.height = unit(base_size, "pt"),
    legend.key.width = unit(2 * base_size, "pt"),
    # legend text
    legend.text = element_text(size = rel(small)),
    legend.text.align = NULL,
    # legend title
    # legend.title = element_blank(),  # suppress legend titles
    legend.title = NULL,
    legend.title.align = NULL,
    # legend position
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "top",
    # arrangement of muliple legends
    legend.box = NULL,
    legend.box.just = NULL,
    legend.box.margin = margin(),
    legend.box.background = element_blank(),
    legend.box.spacing = unit(base_size, "pt"),
    # plotting area
    panel.background = panel_background,
    panel.border = panel_border,
    # space between panels
    panel.spacing = unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    # grid lines
    panel.grid = NULL,
    panel.grid.major = NULL,
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = grid_lines,
    panel.grid.minor.x = NULL,
    panel.grid.minor.y = NULL,
    # plot panel and grid lines over data layers?
    panel.ontop = FALSE,
    # background of the entire plot
    plot.background = element_rect(fill = "white", color = "white"),
    # plot titles
    plot.title = element_text(size = rel(large), vjust = 1,
                              margin = margin(b = large * half_line)),
    plot.subtitle = element_text(size = rel(large), vjust = 1,
                                 margin = margin(b = large * half_line)),
    plot.title.position = "plot",
    # plot caption
    plot.caption = element_text(face = "plain", size = rel(small), vjust = 1,
                                margin = margin(t = large * half_line)),
    plot.caption.position = "plot",
    # plot tag
    plot.tag = element_text(face = "plain", size = rel(large)),
    plot.tag.position = "topleft",
    # plot margins
    plot.margin = margin(half_line, half_line, half_line, half_line),
    # facet labels (not applicable, but we should to set this anyway)
    strip.background = element_rect(fill = "#E4E4E4"),
    strip.background.x = NULL,
    strip.background.y = NULL,
    # facet label placement with respect to axis
    strip.placement = "inside",
    # space between facet labels and axes when facet labels are switched
    strip.switch.pad.grid = unit(quarter_line, "pt"),
    strip.switch.pad.wrap = unit(quarter_line, "pt"),
    # indicate that this is a complete theme
    complete = TRUE
  )
}


#' SPSS Color Palette and Color Scales
#'
#' Color palette used by SPSS, and discrete color scales to be used in plots
#' (e.g., for multiple lines in a plot) to mimic the look of SPSS graphs.
#'
#' @param n  an integer giving the number of colors to select from the palette.
#' If \code{NULL} (the default), all colors of the palette are returned.
#' @param version  a character string specifying whether to use the color
#' palette of recent SPSS versions (\code{"modern"}) or older versions (<24;
#' \code{"legacy"}).
#'
#' @return  \code{paletteSPSS} returns a character vector specifying up to 30
#' colors as used by SPSS.
#'
#' @author Andreas Alfons
#'
#' @examples
#' # data to be plotted
#' df <- data.frame(x = 1:30, y = 0)
#'
#' # initialize plot
#' p <- ggplot(aes(x = x, y = y, fill = factor(x)), data = df) +
#'   geom_point(shape = 21, size = 3, show.legend = FALSE) +
#'   theme_SPSS()
#'
#' # colors of modern SPSS versions
#' p + theme_SPSS() + scale_fill_SPSS()
#'
#' # colors of legacy SPSS versions
#' p + theme_SPSS(version = "legacy") +
#'   scale_fill_SPSS(version = "legacy")
#'
#' @keywords color
#'
#' @importFrom grDevices rgb
#' @export

paletteSPSS <- function(n = NULL, version = r2spssOptions$get("version")) {
  # initializations
  version <- match.arg(version, choices = getVersionValues())
  # define red, green and blue vectors
  if (version == "legacy") {
    red <-   c( 62,  46, 211, 121, 251, 239,  72, 204, 122,  10, 248, 221,  26, 204, 187, 153,   0, 182, 255, 121, 112,  51, 172, 162,  93, 228,  39, 184, 102,  13)
    green <- c( 88, 184, 206,  40, 248,  51, 194, 204, 170,  86, 152, 186,  95, 255,  63, 153,   0, 231, 255, 122, 220,  51, 208,  22,  97, 228, 139, 155, 102, 141)
    blue <-  c(172,  72, 151, 125, 115,  56, 197, 204, 213,  44,  29, 241, 118, 204, 127, 153,   0, 232, 255, 167, 132,  51, 238,  25, 255, 228, 172, 201, 102,  70)
  } else {
    red <-   c( 17,   0, 159, 250,  87,  25,   0, 238, 178,   0,   1, 138, 165, 236,  69,  92, 208, 204, 225, 237,  28,  92, 225,   9,  90, 155, 207, 150,  63, 105)
    green <- c(146,  93,  24,  77,   4, 128,  45,  83, 134, 157,  39,  56, 110, 230,  70, 202,  83, 127, 188,  75, 205, 113, 139,  38, 100,   0, 172, 145, 235,  41)
    blue <-  c(232,  93,  83,  86,   8,  56, 156, 139,   0, 154,  73,   0, 255, 208,  71, 136,  52, 228,  29,  75, 205,  72,  14, 114,  94,   0, 227, 145, 124, 196)
  }
  # check number of colors to return
  max <- length(red)
  if (is.numeric(n) && length(n) > 0) {
    n <- n[1]
    if (n < 1) {
      n <- 1
      warning("must request at least one color; returning the first color",
              call. = FALSE)
    }
    if (n <= max) {
      keep <- seq_len(n)
      red <- red[keep]
      green <- green[keep]
      blue <- blue[keep]
    } else {
      warning("only ", max, " colors available; returning those colors",
              call. = FALSE)
    }
  }
  # return colors
  rgb(red, green, blue, maxColorValue = 255)
}


#' @rdname paletteSPSS
#'
#' @param direction  an integer giving the direction to travel through the
#' palette.  Possible values are 1 for forward (the default) and -1 for
#' backward.
#'
#' @return  \code{SPSS_pal} returns a function that generates colors from the
#' specified SPSS color palette, in the specified direction.  It is mainly
#' used internally by the discrete color scales.
#'
#' @export

SPSS_pal <- function(version = r2spssOptions$get("version"), direction = 1) {
  # initializations
  version <- match.arg(version, choices = getVersionValues())
  # return function to be used for discrete color scales in ggolot2
  function(n) {
    pal <- suppressWarnings(paletteSPSS(n, version))
    if (direction == -1) pal <- rev(pal)
    pal
  }
}


#' @rdname paletteSPSS
#'
#' @param \dots  additional arguments to be passed to
#' \code{\link[ggplot2]{discrete_scale}}.
#' @param aesthetics  a character string or vector listing the names of the
#' aesthetics with which the scale works.  For example, color settings can be
#' applied to the \code{color} and \code{fill} aesthetics by supplying
#' \code{c("color", "fill")}.
#'
#' @return  \code{scale_color_SPSS}, \code{scale_colour_SPSS}, and
#' \code{scale_fill_SPSS} return a discrete color scale to be added to plots.
#'
#' @export

scale_color_SPSS <- function(..., version = r2spssOptions$get("version"),
                             direction = 1, aesthetics = "color") {
  discrete_scale(aesthetics, "SPSS", SPSS_pal(version, direction), ...)
}


#' @rdname paletteSPSS
#' @export

scale_colour_SPSS <- function(..., version = r2spssOptions$get("version"),
                              direction = 1, aesthetics = "colour") {
  discrete_scale(aesthetics, "SPSS", SPSS_pal(version, direction), ...)
}


#' @rdname paletteSPSS
#' @export

scale_fill_SPSS <- function(..., version = r2spssOptions$get("version"),
                            direction = 1, aesthetics = "fill") {
  discrete_scale(aesthetics, "SPSS", SPSS_pal(version, direction), ...)
}


#' Format axis tick labels similar to SPSS
#'
#' Format axis tick labels in a similar manner to SPSS to mimic the look of
#' SPSS graphs.
#'
#' \code{numberSPSS} is a wrapper for \code{\link[scales]{number}} that by
#' default does not put a separator every 3 digits so separate thousands.  It
#' mainly exists to prevent scientific notation in axis tick labels, hence it
#' is typically supplied as the \code{labels} argument of
#' \code{\link[ggplot2]{scale_x_continuous}} or
#' \code{\link[ggplot2]{scale_y_continuous}}.
#'
#' \code{substrSPSS} is a wrapper for \code{\link{substr}} to cut character
#' strings by default to the first 8 characters, which is SPSS behavior for
#' the tick labels of a discrete axis in some (but not all) plots.  It is
#' typically supplied as the  \code{labels} argument of
#' \code{\link[ggplot2]{scale_x_discrete}} or
#' \code{\link[ggplot2]{scale_y_discrete}}.
#'
#' @name labels_SPSS
#'
#' @param x  for \code{numberSPSS}, a numeric vector to format.  For
#' \code{substrSPSS}, a vector of character strings to be cut.
#' @param big.mark  a character string to be inserted every 3 digits to
#' separate thousands.  The default is an empty string for no separation.
#' @param \dots  additional arguments to be passed to
#' \code{\link[scales]{number}}.
#' @param start,stop  integers giving the first and last character to remain in
#' the cut string, respectively.  The default is to cut strings to the first 8
#' characters.
#'
#' @return  A character vector of the same length as \code{x}
#'

NULL


#' @rdname labels_SPSS
#' @importFrom scales number
#' @export

numberSPSS <- function(x, big.mark = "", ...) {
  scales::number(x, big.mark = big.mark, ...)
}


#' @rdname labels_SPSS
#' @export

substrSPSS <- function(x, start = 1, stop = 8) {
  substr(x, start = start, stop = stop)
}


# internal function similar to standardise_aes_names() but directly returning
# list of arguments with standardized names
standardize_args <- function(args, replace = NULL) {
  # obtain argument names
  argument_names <- names(args)
  # default mapping of names (frequently used graphical parameters)
  if (is.null(replace)) replace <- get_aes_mapping()
  # replace names if necessary
  replace <- replace[names(replace) %in% argument_names]
  if (length(replace) > 0) {
    argument_names[match(names(replace), argument_names)] <- replace
    names(args) <- argument_names
  }
  # return list of arguments with standardized names
  args
}

# internal function to obtain mapping of graphical parameters to aesthetics
get_aes_mapping <- function() {
  c(col = "color", colour = "color", bg = "fill", fg = "color",
    pch = "shape", cex = "size", lty = "linetype", lwd = "size",
    srt = "angle", adj = "hjust", min = "ymin", max = "ymax")
}

# internal function to obtain mapping of aesthetics to graphical parameters
get_par_mapping <- function(which = "point") {
  c(color = "col", colour = "col", fill = "bg",
    shape = "pch", size = "cex", title = "main")
}
