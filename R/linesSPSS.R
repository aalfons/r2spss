# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' @import ggplot2
#' @export

## Note:
## Plot functions in \pkg{r2spss} modify the axes such that they mimic the
## appearance of SPSS.  It is therefore not expected that the user adds
## \code{scale_x_XXX()} or \code{scale_y_XXX()} to modify the axes, and doing
## so may have unwanted side effects on the plot.  Customization of the axes
## should instead be done via arguments of the plot function.

lineplotSPSS <- function(data, variables, index = NULL,
                         version = r2spssOptions$get("version"),
                         expand = expansion(mult = 0.05, add = 0.6),
                         ...) {
  # initializations
  data <- as.data.frame(data)
  n <- nrow(data)
  variables <- as.character(variables)
  if (length(variables) == 0) stop("a variable to display must be specified")
  # check index to plot on x-axis
  if (is.null(index) || length(index) == 0) {
    xval <- seq_len(n)
    xlab <- "Case Number"
  } else {
    index <- as.character(index)
    xval <- data[, index[1]]
    xlab <- index[1]
  }
  # check which SPSS version to mimic
  version <- match.arg(version, choices = getVersionValues())
  # create plot
  if (length(variables) == 1) {
    # create new data frame since index may not be a variable but case numbers
    data <- data.frame(x = xval, y = data[, variables])
    # define aesthetic mapping and initialize plot
    mapping <- aes_string(x = "x", y = "y", group = 1)
    p <- ggplot() +
      geom_line(mapping, data = data, color = "black", size = 1)
    # define default y-axis label
    ylab <- variables
  } else {
    # restructure data into long format with additional grouping variable
    list <- lapply(variables, function(variable) {
      data.frame(x = xval, y = data[, variable], group = variable,
                 stringsAsFactors = TRUE)
    })
    data <- do.call(rbind, list)
    # define aesthetic mapping and initialize plot
    mapping <- aes_string(x = "x", y = "y", color = "group", group = "group")
    p <- ggplot() +
      geom_line(mapping, data = data) +
      scale_color_SPSS()
    # define default y-axis label
    ylab <- "Value"
  }
  # extract scales of axes
  scales <- extract_scales(p, expand = expand)
  # finalize plot
  p <- p +
    geom_hline(yintercept = scales$y$limits[1]) +
    geom_vline(xintercept = scales$x$limits[1]) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold"),
          axis.ticks.length = unit(0, "points"),
          legend.title = element_blank(),
          legend.position = "right",
          legend.justification = "top",
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "#AEAEAE"),
          panel.grid.minor = element_blank())
  if (scales$x$discrete) {
    p <- p +
      theme(axis.text.x = element_text(size = 11)) +
      expand_limits(x = scales$x$limits) +
      scale_x_discrete(expand = expansion(add = 0))
  } else {
    p <- p +
      scale_x_continuous(labels = numberSPSS, limits = scales$x$limits,
                         expand = expansion(mult = 0))
  }
  if (scales$y$discrete) {
    p <- p +
      theme(axis.text.y = element_text(size = 11)) +
      expand_limits(y = scales$y$limits) +
      scale_y_discrete(expand = expansion(add = 0))
  } else {
    p <- p +
      scale_y_continuous(labels = numberSPSS, limits = scales$y$limits,
                         expand = expansion(mult = 0))
  }
  p <- p +
    labs(x = xlab, y = ylab)
  # return plot
  p
}


# internal function to extract information on scales from "ggplot" object
extract_scales <- function(plot, i = 1L,
                           expand = expansion(mult = 0.05, add = 0.6)) {
  # extract complete information on scales
  scales <- layer_scales(plot)
  # loop over axes to extract breaks, labels, and limits
  lapply(scales, function(scale) {
    # check if axis is discrete
    discrete <- scale$is_discrete()
    if (discrete) {
      # # extract breaks and labels
      # breaks <- scale$get_breaks()
      # labels <- scale$get_labels()
      # let ggplot handle breaks and labels
      breaks <- waiver()
      labels <- waiver()
      # determine range of x-axis and expand it according to expansion factor
      limits <- scale$range_c$range + c(-1, 1) * expand[c(2, 4)]
    } else {
      # let ggplot handle breaks but use specific function for formatting labels
      breaks <- waiver()
      labels <- numberSPSS
      # extract range of x-axis and expand it according to expansion factor
      limits <- scale$get_limits()
      limits <- limits + c(-1, 1) * expand[c(1, 3)] * diff(limits)
    }
    # return information as list
    list(breaks = breaks, labels = labels, limits = limits, discrete = discrete)
  })
}
