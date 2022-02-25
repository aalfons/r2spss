# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------


#' Scatter Plot and Scatter Plot Matrix
#'
#' Draw a scatter plot or a scatter plot matrix of variables in a data frame.
#' The plots thereby mimic the look of SPSS graphs.
#'
#' @param data  a data frame containing the variables to be plotted.
#' @param variables  a character vector specifying at least two variables to be
#' plotted.  In case of two variables, a simple scatter plot is produced with
#' the first variable on the \eqn{x}-axis and the second variable on the
#' \eqn{y}-axis.  In case of more than two variables, a scatter plot matrix is
#' produced.
#' @param version  a character string specifying whether the plot should mimic
#' the look of recent SPSS versions (\code{"modern"}) or older versions (<24;
#' \code{"legacy"}).
#' @param \dots  for a simple scatter plot, additional arguments are passed
#' down to \code{\link[ggplot2]{geom_point}}.  For a scatter plot matrix,
#' additional arguments to be passed down, in particular base graphics
#' parameters (see \code{\link[graphics]{par}}).
#'
#' @return
#' In case of a simple scatter plot, an object of class
#' \code{"\link[ggplot2]{ggplot}"}, which produces the plot when printed.
#'
#' In case of a scatter plot matrix, nothing is returned but a plot is produced.
#'
#' @note  Wile all other plots in \pkg{r2spss} are based on
#' \pkg{\link[ggplot2:ggplot2-package]{ggplot2}} (including the simple scatter
#' plot), the scatter plot matrix is built around base \R graphics.  This is
#' because \pkg{ggplot2} does not provide an implementation of a scatter plot
#' matrix, and an implementation based on separate scatter plots on a matrix
#' layout would be slow.
#'
#' @author Andreas Alfons
#'
#' @examples
#' # load data
#' data("Eredivisie")
#' # log-transform market values
#' Eredivisie$logMarketValue <- log(Eredivisie$MarketValue)
#'
#' # plot log market values against age
#' scatter_plot(Eredivisie, c("Age", "logMarketValue"))
#'
#' # scatterplot matrix of age, number of minutes played, and
#' # log market values
#' scatter_plot(Eredivisie, c("Age", "Minutes", "logMarketValue"))
#'
#' @keywords hplot
#'
#' @import ggplot2
#' @export

scatter_plot <- function(data, variables,
                         version = r2spssOptions$get("version"),
                         ...) {
  # initializations
  data <- as.data.frame(data)
  variables <- as.character(variables)
  if (length(variables) < 2) stop("at least two variables must be specified")
  # check which SPSS version to mimic
  version <- match.arg(version, choices = getVersionValues())
  # create plot
  if (length(variables) == 2) {
    # define default axis labels
    xlab <- variables[1]
    ylab <- variables[2]
    # define aesthetic mapping and initialize plot
    mapping <- aes_string(x = variables[1], y = variables[2])
    p <- ggplot() +
      geom_point_SPSS(mapping, data = data, ..., version = version,
                      grouped = FALSE)
    # extract scales of axes
    scales <- sapply(layer_scales(p), function(scale) {
      if (scale$is_discrete()) "discrete" else "continuous"
    })
    # finalize plot
    p <- p +
      theme_SPSS(version = version, scale.x = scales["x"],
                 scale.y = scales["y"])
    if (scales["x"] == "continuous") {
      p <- p + scale_x_continuous(labels = numberSPSS)
    }
    if (scales["y"] == "continuous") {
      p <- p + scale_y_continuous(labels = numberSPSS)
    }
    p <- p + labs(x = xlab, y = ylab)
    # return plot
    p
  } else {
    ## scatterplot matrix using base R graphics
    # obtain list of arguments with standardized names to allow for
    # ggplot2-style aesthetics
    arguments <- standardize_args(list(...), replace = get_par_mapping("point"))
    # call workhorse function
    arguments <- c(list(data[, variables], version = version), arguments)
    do.call(pairs_SPSS, arguments)
  }
}


# custom geom for lines with defaults to mimic appearance of SPSS
# Note: Setting defaults like that works if the geom remains internal, but it
# would probably create issues if we want to export this to users (and they for
# instance set the color based on a variable).  We could keep this internal and
# in addition provide a function that sets defaults for the entire session.
# This would provide a nice set of functions together with theme_SPSS() and
# scale_xxx_SPSS() to make any plot look like SPSS.
geom_point_SPSS <- function(..., version = r2spssOptions$get("version"),
                            grouped = FALSE) {
  # obtain list of arguments with standardized names
  arguments <- standardize_args(list(...))
  # default values according to SPSS version also depend on whether we have
  # multiple groups (for instance, in a profile plot from two-way ANOVA)
  if (grouped) {
    # default plot symbol
    if (is.null(arguments$shape)) arguments$shape <- 1
  } else {
    # default colors
    if (is.null(arguments$color)) arguments$color <- "black"
    if (version != "legacy" && is.null(arguments$fill)) {
      arguments$fill <- "#1192E8"
    }
    # default plot symbol
    if (is.null(arguments$shape)) {
      arguments$shape <- if (version == "legacy") 1 else 21
    }
  }
  # check size of plot symbol and transparency
  if (is.null(arguments$size)) arguments$size <- 2  # ggplot2 default is 1.5
  if (is.null(arguments$stroke)) arguments$stroke <- 0.5
  if (is.null(arguments$alpha)) arguments$alpha <- 1
  # call geom_point()
  do.call(geom_point, arguments)
}


# internal function for scatterplot matrix with different defaults
# Note: This is uses base graphics, not ggplot2!
#' @importFrom graphics box mtext par plot points rect
pairs_SPSS <- function(x, version = "modern", ..., frame.plot = TRUE,
                       oma = NULL, pch = NULL, col = "black", bg = NULL,
                       main = NULL, font.main = 2, cex.main = 1.2,
                       font.lab = NULL, cex.lab = 1,
                       # the following arguments are currently ignored
                       type = "p", log = "", sub = NULL, xlab = NULL,
                       ylab = NULL, ann = TRUE, axes = FALSE) {
  # initializations
  p <- ncol(x)
  labels <- names(x)
  if (is.null(oma)) {
    top <- if (is.null(main) || nchar(main) == 0) 0 else 3
    oma <- c(2, 2, top, 0) + 0.1
  }
  legacy <- version == "legacy"
  # default values for graphical parameters depending on SPSS version to mimic
  if (legacy) {
    if (is.null(pch)) pch <- 1            # plot symbol
    if (is.null(bg)) bg <- "#F0F0F0"      # here: background of plot area
    if (is.null(font.lab)) font.lab <- 2  # variable names in bold
  } else {
    if (is.null(pch)) pch <- 21           # plot symbol
    if (is.null(bg)) bg <- "#1192E8"      # here: fill color of points
    if (is.null(font.lab)) font.lab <- 1  # variable names in normal font
  }
  # set plot margins
  op <- par(mfrow = c(p, p), mar = c(0, 0, 0, 0), oma = oma)
  on.exit(par(op))
  # create plots
  for (i in seq_len(p)) {
    for (j in seq_len(p)) {
      # initialize current plot
      plot(x[, j], x[, i], type = "n", ann = FALSE, axes = FALSE, ...)
      # plot background
      if (legacy) {
        usr <- par("usr")
        rect(usr[1], usr[3], usr[2], usr[4], col = bg, border = NA)
      }
      # add frame around plot
      if (frame.plot) box()
      # add points
      if (i != j) {
        points(x[, j], x[, i], pch = pch, col = col, bg = bg, ...)
      }
      # add variable labels
      if (j == 1) {
        mtext(labels[i], side = 2, line = 0.25, font = font.lab, cex = cex.lab)
      }
      if (i == p) {
        mtext(labels[j], side = 1, line = 0.5, font = font.lab, cex = cex.lab)
      }
    }
  }
  # add title
  mtext(main, side = 3, line = 1, outer = TRUE,
        font = font.main, cex = cex.main)
}
