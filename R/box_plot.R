# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------


#' Box Plots
#'
#' Draw box plots of variables in a data frame, including box plots for groups
#' of observations and box plots for separate variables.  The plots thereby
#' mimic the look of SPSS graphs.
#'
#' @param data  a data frame containing the variables to be plotted.
#' @param variables  a character vector specifying separate variables to be
#' plotted.  If \code{group} is not \code{NULL}, only the first variable is
#' used and box plots of groups of observations are drawn instead.
#' @param group  an character string specifying a grouping variable, or
#' \code{NULL} for no grouping.
#' @param cut.names  a logical indicating whether to cut long variable names or
#' group labels to 8 characters.  The default is \code{TRUE} for box plots of
#' separate variables, but \code{FALSE} for box plots of groups of observations
#' (which mimics SPSS behavior).
#' @param style  a character string specifying the box plot style.  Possible
#' values are \code{"T"} for T-bars (the default) or \code{"whiskers"} for
#' simple whiskers.
#' @param coef  a numeric vector of length 2 giving the multipliers of the
#' interquartile range for determining intermediate and extreme outliers,
#' respectively.
#' @param outlier.shape  an integer vector of length 2 giving the plot symbol
#' for intermediate and extreme outliers, respectively.
#' @param version  a character string specifying whether the plot should mimic
#' the look of recent SPSS versions (\code{"modern"}) or older versions (<24;
#' \code{"legacy"}).
#' @param \dots  additional arguments to be passed down, in particular
#' aesthetics (see \code{\link[ggplot2]{geom_boxplot}}).
#'
#' @return  An object of class \code{"\link[ggplot2]{ggplot}"}, which produces
#' a box plot when printed.
#'
#' @author Andreas Alfons
#'
#' @examples
#' ## paired sample
#' # load data
#' data("Exams")
#'
#' # plot grades on regular and resit exams
#' box_plot(Exams, c("Regular", "Resit"))
#'
#'
#' ## independent samples
#'
#' # load data
#' data("Eredivisie")
#' # log-transform market values
#' Eredivisie$logMarketValue <- log(Eredivisie$MarketValue)
#'
#' # plot log market values of Dutch and Foreign players
#' box_plot(Eredivisie, "logMarketValue", group = "Foreign")
#'
#' @keywords hplot
#'
#' @import ggplot2
#' @export

box_plot <- function(data, variables, group = NULL,
                     cut.names = NULL, style = c("T", "whiskers"),
                     coef = c(1.5, 3), outlier.shape = c(1, 42),
                     version = r2spss_options$get("version"), ...) {
  # initializations
  data <- as.data.frame(data)
  variables <- as.character(variables)
  group <- as.character(group)
  if (length(variables) == 0) stop("a variable to display must be specified")
  # check which SPSS functionality to mimic
  style <- match.arg(style)
  coef <- rep(as.numeric(coef), length.out = 2)
  version <- match.arg(version, choices = get_version_values())
  # check plot symbols for outliers
  outlier.shape <- rep(as.numeric(outlier.shape), length.out = 2)
  fatten <- ifelse(outlier.shape > 25, 2.75, 1)
  # create plot
  if (length(group) == 0) {
    # further initializations
    if (is.null(cut.names)) cut.names <- FALSE
    # suppress axis labels
    xlab <- NULL
    ylab <- NULL
    # restructure data into long format with additional grouping variable
    # (even if we have only one variable, this replicates SPSS behavior
    # since the variable names are used as labels on the x-axis)
    list <- lapply(variables, function(variable) {
      data.frame(x = variable, y = data[, variable], stringsAsFactors = TRUE)
    })
    data <- do.call(rbind, list)
    # define aesthetic mapping
    mapping <- aes_string(x = "x", y = "y")
  } else {
    # further initializations
    variables <- variables[1]
    group <- group[1]
    if (is.null(cut.names)) cut.names <- TRUE
    # make sure that grouping variable is a factor
    data[, group] <- as.factor(data[, group, drop = TRUE])
    # define default axis labels
    xlab <- group
    ylab <- variables
    # define aesthetic mapping
    mapping <- aes_string(x = group, y = variables)
  }
  # initialize plot
  p <- ggplot() +
    geom_boxplot_SPSS(mapping, data = data, ..., coef = coef[1],
                      version = version)
  # extract boxplot statistics
  stats <- layer_data(p)
  outlierList <- stats$outliers
  stats$outliers <- NULL
  # determine if we have outliers
  have_outliers <- length(unlist(outlierList, use.names = FALSE)) > 0
  # if applicable, determine which outliers are extreme outliers
  if (have_outliers) {
    outliers <- mapply(function(x, lower, upper, outliers) {
      if (length(outliers) > 0) {
        tmp <- coef[2] * (upper - lower)
        extreme <- (outliers < lower - tmp) | (outliers > upper + tmp)
        data.frame(x = x, y = outliers, extreme = extreme)
      }
    }, x = stats$x, lower = stats$lower, upper = stats$upper,
    outliers = outlierList, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    outliers <- do.call(rbind, outliers)
    # split outliers into intermediate and extreme outliers
    intermediate <- outliers[!outliers$extreme, c("x", "y"), drop = FALSE]
    extreme <- outliers[outliers$extreme, c("x", "y"), drop = FALSE]
  }
  # extract scales of axes
  scale_info <- layer_scales(p)
  scales <- lapply(scale_info, function(scale) {
    if (scale$is_discrete()) "discrete" else "continuous"
  })
  # if requested add T-shaped error bars rather than whiskers
  if (style == "T") {
    # construct data frame to complete error bars
    width <- 0.5 * (stats$xmax - stats$xmin)
    errorbars <- data.frame(xmin = stats$x - width / 2,
                            xmax = stats$x + width / 2,
                            stats[, c("ymin", "ymax")])
    # add line segments to boxplot to complete error bars
    p <- p +
      geom_error_SPSS(aes_string(x = "xmin", xend = "xmax",
                                 y = "ymin", yend = "ymin"),
                      data = errorbars, ...) +
      geom_error_SPSS(aes_string(x = "xmin", xend = "xmax",
                                 y = "ymax", yend = "ymax"),
                      data = errorbars, ...)
  }
  # if applicable, add intermediate and extreme outliers
  if (have_outliers) {
    p <- p +
      geom_outliers_SPSS(aes_string(x = "x", y = "y"), data = intermediate, ...,
                         outlier.shape = outlier.shape[1], fatten = fatten[1]) +
      geom_outliers_SPSS(aes_string(x = "x", y = "y"), data = extreme, ...,
                         outlier.shape = outlier.shape[2], fatten = fatten[2])
  }
  # finalize plot
  p <- p + theme_SPSS(version = version, scale.x = scales$x, scale.y = scales$y)
  if (scales$x == "continuous") p <- p + scale_x_continuous(labels = number_SPSS)
  else if (cut.names) p <- p + scale_x_discrete(labels = substr_SPSS)
  if (scales$y == "continuous") p <- p + scale_y_continuous(labels = number_SPSS)
  p <- p + labs(x = xlab, y = ylab)
  # return plot
  p
}


# custom geom for boxplot with defaults to mimic appearance of SPSS
geom_boxplot_SPSS <- function(..., version = r2spss_options$get("version"),
                              # arguments to be ignored
                              stat, outlier.colour, outlier.color, outlier.fill,
                              outlier.shape, outlier.size, outlier.stroke,
                              outlier.alpha, notch, notchwidth, orientation) {
  # obtain list of arguments with standardized names
  arguments <- standardize_args(list(...))
  # check colors
  if (is.null(arguments$color)) arguments$color <- "black"
  if (is.null(arguments$fill)) {
    arguments$fill <- if (version == "legacy") "#D3CE97" else "#1192E8"
  }
  if (is.null(arguments$alpha)) arguments$alpha <- 1
  # check lines
  if (is.null(arguments$linetype)) arguments$linetype <- "solid"
  if (is.null(arguments$size)) size <- 0.5
  # check width of boxes
  if (is.null(arguments$width)) arguments$width <- 0.25
  # check length of whiskers
  if (is.null(arguments$coef)) arguments$coef <- 1.5
  # suppress outliers, which will be plotted separately
  arguments$outlier.shape <- NA
  # suppress notches, as those are not supported by SPSS
  arguments$notch <- FALSE
  # call geom_boxplot()
  do.call(geom_boxplot, arguments)
}


# custom geom for boxplot with defaults to mimic appearance of SPSS
geom_error_SPSS <- function(..., stat, geom, position, outlier.colour,
                            outlier.color, outlier.fill, outlier.shape,
                            outlier.size, outlier.stroke, outlier.alpha,
                            notch, notchwidth, width, varwidth, coef,
                            orientation, shape, stroke, fill, pch, cex, bg) {
  # obtain list of arguments with standardized names
  arguments <- standardize_args(list(...))
  # check color
  if (is.null(arguments$color)) arguments$color <- "black"
  # check line types and size
  if (is.null(arguments$linetype)) arguments$linetype <- "solid"
  if (is.null(arguments$size)) size <- 0.5
  # call geom_segment()
  do.call(geom_segment, arguments)
}


# custom geom for plotting outliers to mimic appearance of SPSS
geom_outliers_SPSS <- function(..., fatten = 1,
                               # arguments to be ignored,
                               stat, geom, position, notch, notchwidth, width,
                               varwidth, coef, orientation, linetype, lty, lwd) {
  # obtain list of arguments with standardized names
  arguments <- standardize_args(list(...))
  # check color
  color <- arguments$outlier.color
  if (is.null(color)) color <- arguments$outlier.colour
  if (is.null(color)) color <- arguments$color
  if (is.null(color)) color <- "black"
  # check fill color (should be NULL for transparency)
  fill <- arguments$outlier.fill
  if (is.null(fill)) fill <- arguments$fill
  # check plot symbol
  shape <- arguments$outlier.shape
  if (is.null(shape)) shape <- 1
  # check size of plot symbol
  size <- arguments$outlier.size
  if (is.null(size)) size <- 2  # ggplot2 default is 1.5
  # check stroke of plot symbol
  stroke <- arguments$outlier.stroke
  if (is.null(stroke)) stroke <- 0.5
  # check transparency
  alpha <- arguments$outlier.alpha
  if (is.null(alpha)) alpha <- arguments$alpha
  if (is.null(alpha)) alpha <- 1
  # overwrite arguments for geom_point()
  arguments$color <- color
  arguments$fill <- fill
  arguments$shape <- shape
  arguments$size <- size * fatten
  arguments$stroke <- stroke
  arguments$alpha <- alpha
  # remove outlier.xxx arguments
  arguments$outlier.colour <- NULL
  arguments$outlier.color <- NULL
  arguments$outlier.fill <- NULL
  arguments$outlier.shape <- NULL
  arguments$outlier.size <- NULL
  arguments$outlier.stroke <- NULL
  arguments$outlier.alpha <- NULL
  # call geom_point()
  do.call(geom_point, arguments)
}
