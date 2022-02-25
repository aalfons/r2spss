# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------


#' Line Plots
#'
#' Draw connected lines for variables in a data frame.  The plot thereby mimics
#' the look of SPSS graphs.
#'
#' @param data  a data frame containing the variables to be plotted.
#' @param variables  a character vector specifying at least one variable to be
#' plotted on the \eqn{y}-axis.  In case of multiple variables, separate lines
#' are drawn for each variable and a legend is shown.
#' @param index  a character string specifying a variable to be plotted on the
#' \eqn{x}-axis, or \code{NULL} to plot the observations against their index.
#' @param version  a character string specifying whether the plot should mimic
#' the look of recent SPSS versions (\code{"modern"}) or older versions (<24;
#' \code{"legacy"}).
#' @param \dots  additional arguments to be passed down to
#' \code{\link[ggplot2]{geom_line}}.
#'
#' @return  An object of class \code{"\link[ggplot2]{ggplot}"}, which produces
#' a line plot when printed.
#'
#' @author Andreas Alfons
#'
#' @examples
#' # load data
#' data("Eredivisie")
#' # log-transform market values
#' Eredivisie$logMarketValue <- log(Eredivisie$MarketValue)
#'
#' # aggregate log market values by position
#' means <- aggregate(Eredivisie[, "logMarketValue", drop = FALSE],
#'                    Eredivisie[, "Position", drop = FALSE],
#'                    FUN = mean)
#'
#' # create profile plot
#' line_plot(means, "logMarketValue", "Position")
#'
#' # easier and fancier as the plot method of ANOVA results
#' oneway <- ANOVA(Eredivisie, "logMarketValue",
#'                 group = "Position")
#' plot(oneway)
#'
#' @keywords hplot
#'
#' @import ggplot2
#' @export

line_plot <- function(data, variables, index = NULL,
                      version = r2spssOptions$get("version"),
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
      geom_line_SPSS(mapping, data = data, ..., version = version,
                     grouped = FALSE)
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
      geom_line_SPSS(mapping, data = data, ..., version = version,
                     grouped = TRUE) +
      scale_color_SPSS(name = NULL, version = version)
    # define default y-axis label
    ylab <- "Value"
  }
  # extract scales of axes
  scales <- sapply(layer_scales(p), function(scale) {
    if (scale$is_discrete()) "discrete" else "continuous"
  })
  # finalize plot
  p <- p +
    theme_SPSS(version = version, scale.x = scales["x"], scale.y = scales["y"])
  if (scales["x"] == "continuous") {
    p <- p + scale_x_continuous(labels = numberSPSS)
  }
  if (scales["y"] == "continuous") {
    p <- p + scale_y_continuous(labels = numberSPSS)
  }
  p <- p + labs(x = xlab, y = ylab)
  # return plot
  p
}


# custom geom for lines with defaults to mimic appearance of SPSS
geom_line_SPSS <- function(..., version = r2spssOptions$get("version"),
                           grouped = FALSE) {
  # obtain list of arguments with standardized names
  arguments <- standardize_args(list(...))
  # default values according to SPSS version also depend on whether we have
  # multiple lines
  if (grouped) {
    # default line size
    if (is.null(arguments$size)) arguments$size <- 0.5
  } else {
    # default line color
    if (is.null(arguments$color)) arguments$color <- "black"
    # default line size is a bit thicker for modern SPSS look
    if (is.null(arguments$size)) {
      arguments$size <- if (version == "legacy") 0.5 else 1
    }
  }
  # check line type and transparency
  if (is.null(arguments$linetype)) arguments$linetype <- "solid"
  if (is.null(arguments$alpha)) arguments$alpha <- 1
  # call geom_line()
  do.call(geom_line, arguments)
}
