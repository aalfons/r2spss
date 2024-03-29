# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------


#' Histogram
#'
#' Draw a histogram of a variable in a data frame.  The plot thereby mimics the
#' look of SPSS graphs.
#'
#' @param data  a data frame containing the variable to be plotted.
#' @param variable  a character string specifying the variable to be
#' plotted.
#' @param bins  an integer giving the number of bins for the histogram.
#' @param normal  a logical indicating whether to add a normal density with the
#' estimated mean and standard deviation (the default is \code{FALSE}).
#' @param normal.colour,normal.color,normal.linetype,normal.size,normal.alpha
#' aesthetics for the normal density.  In the unlikely event that both US and
#' UK spellings of color are supplied, the US spelling will take precedence.
#' @param digits  an integer giving the number of digits after the comma to be
#' printed in the summary statistics in the right plot margin.
#' @param limits  a list of arguments to be passed to
#' \code{\link[ggplot2]{expand_limits}}.  Typically, the list would contain
#' components \code{x} or \code{y} to specify values that should be included
#' in the range of the corresponding axis.
#' @param expand  a numeric value specifying the percentage of the range to be
#' used for padding the axes.  The default is 0.05 to expand the \eqn{x}-axis
#' by 5\% on both sides and the \eqn{y}-axis by 5\% on the upper end.  Note
#' that there is no padding on lower end of the \eqn{y}-axis to mimic SPSS
#' behavior.
#' @param version  a character string specifying whether the plot should mimic
#' the look of recent SPSS versions (\code{"modern"}) or older versions (<24;
#' \code{"legacy"}).
#' @param \dots  additional arguments to be passed down, in particular
#' aesthetics (see \code{\link[ggplot2]{geom_histogram}} and
#' \code{\link[ggplot2]{geom_line}}).
#'
#' @return  An object of class \code{"\link[ggplot2]{ggplot}"}, which produces
#' a histogram when printed.
#'
#' @note  Due to the inner workings of this function to mimic the look
#' of histograms in SPSS, it is not expected that the user adds
#' \code{\link[ggplot2]{scale_x_continuous}} or
#' \code{\link[ggplot2]{scale_y_continuous}} to the plot.  Instead, axis
#' limits and padding should be modified via the \code{limits} and
#' \code{expand} arguments.
#'
#' @author Andreas Alfons
#'
#' @examples
#' # load data
#' data("Eredivisie")
#' # log-transform market values
#' Eredivisie$logMarketValue <- log(Eredivisie$MarketValue)
#'
#' # plot histogram of log market values
#' histogram(Eredivisie, "logMarketValue", normal = TRUE,
#'           limits = list(x = c(9.5, 17.5)))
#'
#' @keywords hplot
#'
#' @importFrom stats dnorm sd
#' @import ggplot2
#' @export

histogram <- function(data, variable, bins = NULL, normal = FALSE,
                      normal.colour = NULL, normal.color = NULL,
                      normal.linetype = NULL, normal.size = NULL,
                      normal.alpha = NULL, digits = 3, limits = NULL,
                      expand = 0.05, version = r2spss_options$get("version"),
                      ...) {
  # initializations
  data <- as.data.frame(data)
  variable <- as.character(variable)
  if (length(variable) == 0) stop("a variable to display must be specified")
  variable <- variable[1]
  normal <- isTRUE(normal)
  # check which SPSS functionality to mimic
  version <- match.arg(version, choices = get_version_values())
  # extract variable of interest
  x <- data[, variable]
  # if necessary, determine the number of bins
  if (is.null(bins)) bins <- get_bins(x)
  else {
    if (!is.numeric(bins) || length(bins) == 0) {
      stop("the number of bins must be a single numeric value")
    }
    bins <- bins[1]
  }
  # compute summary statistics
  m <- mean(x)
  s <- sd(x)
  n <- length(x)
  # construct text for summary statistics to be included in top right corner
  format <- paste0("%.", digits, "f")
  statistics <- paste0("Mean = ", format, "\nStd. Dev. = ", format, "\nN = %d")
  statistics <- sprintf(statistics, m, s, n)
  # create an initial histogram
  initial <- ggplot() +
    stat_bin_SPSS(aes_string(x = variable), data = data, ..., bins = bins)
  if (!is.null(limits)) initial <- initial + do.call(expand_limits, limits)
  # extract necessary information on histogram
  histogram <- layer_data(initial)
  # determine range of x-axis and expand it according to expansion factor
  xlim <- c(min(histogram$xmin), max(histogram$xmax))
  xlim <- xlim + c(-1, 1) * expand * diff(xlim)
  # determine range of y-axis (but let ggplot handle expansion)
  # ylim <- c(0, max(histogram$count))
  ylim <- layer_scales(initial)$y$get_limits()
  if (ylim[1] > 0) ylim[1] <- 0
  # initialize plot as rectangles for histogram
  p <- ggplot() +
    geom_rect_SPSS(aes_string(xmin = "xmin", xmax = "xmax",
                              ymin = 0, ymax = "count"),
                   data = histogram, ..., version = version)
  # if requested, compute normal density and it add to plot
  if (normal) {
    # define grid for normal density
    grid <- seq(from = xlim[1], to = xlim[2], length.out = 1000)
    # compute rescaled normal density
    whichMax <- which.max(histogram$count)
    y <- dnorm(grid, mean = m, sd = s) *
      histogram[whichMax, "count"] /
      histogram[whichMax, "density"]
    # update upper limit of y-axis
    ylim[2] <- max(ylim[2], y)
    # combine information into data.frame
    density <- data.frame(x = grid, y = y)
    # add density to plot
    p <- p +
      geom_normal_SPSS(aes_string(x = "x", y = "y"),
                       data = density,
                       normal.colour = normal.colour,
                       normal.color = normal.color,
                       normal.linetype = normal.linetype,
                       normal.size = normal.size,
                       normal.alpha = normal.alpha, ...,
                       version = version)
  }
  # finalize plot
  p <- p +
    theme_SPSS(version = version) +
    theme(axis.ticks.length.y.right = unit(0, "pt"),
          axis.text.y.right = element_text(vjust = 1,
                                           margin = margin(l = 0.5,
                                                           unit = "line"))) +
    scale_x_continuous(labels = number_SPSS, limits = xlim,
                       expand = expansion(mult = 0)) +
    scale_y_continuous(labels = number_SPSS, limits = ylim,
                       expand = expansion(mult = c(0, expand)),
                       sec.axis = dup_axis(name = NULL,
                                           breaks = ylim[2],
                                           labels = statistics)) +
    labs(x = variable, y = "Frequency")
  # return plot
  p
}


# custom stat for histogram to get relevant information
stat_bin_SPSS <- function(..., position, orientation) stat_bin(...)

# custom geom for histogram with defaults to mimic appearance of SPSS
geom_rect_SPSS <- function(..., version = r2spss_options$get("version"),
                           # arguments to be ignored
                           stat, position, binwidth, bins, center,
                           boundary, breaks, closed, pad, orientation) {
  # obtain list of arguments with standardized names
  arguments <- standardize_args(list(...))
  # check colors
  if (is.null(arguments$color)) {
    arguments$color <- if (version == "legacy") "black" else "white"
  }
  if (is.null(arguments$fill)) {
    arguments$fill <- if (version == "legacy") "#D3CE97" else "#1192E8"
  }
  if (is.null(arguments$alpha)) arguments$alpha <- 1
  # check line type and size
  if (is.null(arguments$linetype)) arguments$linetype <- "solid"
  if (is.null(arguments$size)) arguments$size <- 0.5
  # call geom_rect()
  do.call(geom_rect, arguments)
}


# custom geom for plotting normal density to mimic appearance of SPSS
geom_normal_SPSS <- function(..., version = r2spss_options$get("version"),
                             # arguments to be ignored,
                             stat, position, binwidth, bins, center, boundary,
                             breaks, closed, pad, orientation, fill, bg) {
  # obtain list of arguments with standardized names
  arguments <- standardize_args(list(...))
  # check line color
  color <- arguments$normal.color
  if (is.null(color)) color <- arguments$normal.colour
  if (is.null(color)) color <- arguments$color
  if (is.null(color)) color <- "black"
  # check line type
  linetype <- arguments$normal.linetype
  if (is.null(linetype)) linetype <- arguments$linetype
  if (is.null(linetype)) linetype <- "solid"
  # check line size
  size <- arguments$normal.size
  if (is.null(size)) size <- arguments$size
  if (is.null(size)) size <- if (version == "legacy") 0.5 else 1
  # check transparency
  alpha <- arguments$normal.alpha
  if (is.null(alpha)) alpha <- arguments$alpha
  if (is.null(alpha)) alpha <- 1
  # overwrite arguments for geom_line()
  arguments$color <- color
  arguments$linetype <- linetype
  arguments$size <- size
  arguments$alpha <- alpha
  # remove normal.xxx arguments
  arguments$normal.colour <- NULL
  arguments$normal.color <- NULL
  arguments$normal.linetype <- NULL
  arguments$normal.size <- NULL
  arguments$normal.alpha <- NULL
  # call geom_line()
  do.call(geom_line, arguments)
}


# internal function to compute number of bins
get_bins <- function(x) {
  # maximum number of bins
  m <- 35
  # small range of integers: use a bin for each value and return break points
  if (is.integer(x)) {
    r <- range(x)
    if (diff(r) + 1 <= m) return(seq(r[1]-0.5, r[2]+0.5))
  }
  # otherwise: square root of number of observations or maximum number of bins
  n <- length(x)
  min(ceiling(sqrt(n)), m)
}
