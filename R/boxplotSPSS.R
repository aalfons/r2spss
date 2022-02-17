# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' @import ggplot2
#' @export

bwplotSPSS <- function(data, variables, group = NULL,
                       version = r2spssOptions$get("version"),
                       cut.names = NULL, width = 0.25, coef = 1.5,
                       outlier.shape = c(1, 42),
                       outlier.size = 2,  # ggplot2 default is 1.5
                       expand = 0.05, ...) {
  # initializations
  data <- as.data.frame(data)
  variables <- as.character(variables)
  group <- as.character(group)
  if (length(variables) == 0) stop("a variable to display must be specified")
  if (width > 1) width <- 1
  # create plot
  if (length(group) == 0) {
    if (is.null(cut.names)) cut.names <- FALSE
    # suppress axis labels
    xlab <- NULL
    ylab <- NULL
    if (length(variables) == 1) {
      # define aesthetic mapping
      mapping <- aes_string(y = variables)
    } else {
      # restructure data into long format with additional grouping variable
      list <- lapply(variables, function(variable) {
        data.frame(x = variable, y = data[, variable], stringsAsFactors = TRUE)
      })
      data <- do.call(rbind, list)
      # define aesthetic mapping
      mapping <- aes_string(x = "x", y = "y")
    }

  } else {
    # further initializations
    variables <- variables[1]
    group <- group[1]
    if (is.null(cut.names)) cut.names <- TRUE
    # define default axis labels
    xlab <- group
    ylab <- variables
    # define aesthetic mapping
    mapping <- aes_string(x = group, y = variables)
  }
  # create an initial boxplot
  initial <- ggplot(data, mapping) + geom_boxplot(coef = coef, width = 1)
  # extract necessary information on histrogram
  keep <- c("x", "xmin", "xmax", "ymin", "lower", "middle", "upper", "ymax",
            "outliers")
  stats <- layer_data(initial)[, keep]
  outlierList <- stats$outliers
  # determine which outliers are extreme outliers
  outliers <- mapply(function(x, lower, middle, upper, outliers) {
    if (length(outliers) > 0) {
      extreme <- abs(outliers - middle) > 2 * coef * (upper - lower)
      data.frame(x = x, y = outliers, extreme = extreme)
    }
  }, x = stats$x, lower = stats$lower, middle = stats$middle,
  upper = stats$upper, outliers = outlierList, SIMPLIFY = FALSE,
  USE.NAMES = FALSE)
  outliers <- do.call(rbind, outliers)
  # split outliers into intermediate and extreme outliers
  intermediate <- outliers[!outliers$extreme, c("x", "y")]
  extreme <- outliers[outliers$extreme, c("x", "y")]
  # construct data frame to complete error bars
  fraction <- 0.5
  errorbars <- stats[, c("x", "ymin", "ymax")]
  errorbars$xmin <- errorbars$x - fraction * width / 2
  errorbars$xmax <- errorbars$x + fraction * width / 2
  # plot symbols > 25 are strings and need to be adjusted for size
  if (!is.numeric(outlier.shape)) {
    stop("plot symbols for outliers should be numeric")
  }
  if (outlier.shape[1] > 25) outlier.shape[1] <- formals()$outlier.shape[1]
  fatten <- if (outlier.shape[2] > 25) 2.75 else 1
  # determine range of x-axis and expand it according to expansion factor
  xlim <- c(min(stats$xmin), max(stats$xmax))
  xlim <- xlim + c(-1, 1) * expand * diff(xlim)
  # extract labels for x-axis
  if (length(group) == 0) labels <- variables
  else labels <- layer_scales(initial)$x$get_labels()
  if (cut.names) labels <- substr(labels, start = 1, stop = 8)
  # determine range of y-axis and expand it according to expansion factor
  ylim <- layer_scales(initial)$y$get_limits()
  ylim <- ylim + c(-1, 1) * expand * diff(ylim)
  # construct plot
  ggplot() +
    geom_segment(aes(x = xmin, xend = xmax, y = ymin, yend = ymin),
                 data = errorbars) +
    geom_segment(aes(x = xmin, xend = xmax, y = ymax, yend = ymax),
                 data = errorbars) +
    geom_boxplot(aes(x = x, ymin = ymin, lower = lower, middle = middle,
                     upper = upper, ymax = ymax, group = x), data = stats,
                 stat = "identity", fill = "#1192E8",
                 outlier.shape = NA, width = width) +
    geom_point(aes(x = x, y = y), data = intermediate,
               shape = outlier.shape[1], size = outlier.size) +
    geom_point(aes(x = x, y = y), data = extreme,
               shape = outlier.shape[2], size = fatten * outlier.size) +
    geom_hline(yintercept = ylim[1]) +
    geom_vline(xintercept = xlim[1]) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold"),
          axis.text.x = element_text(size = 11),
          axis.ticks.length = unit(0, "points"),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "#AEAEAE"),
          panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks = stats$x, labels = labels,
                       limits = xlim, expand = expansion(mult = 0)) +
    scale_y_continuous(labels = numberSPSS, limits = ylim,
                       expand = expansion(mult = 0)) +
    labs(x = xlab, y = ylab)
}


#' @importFrom scales number
#' @export

numberSPSS <- function(x, big.mark = "", ...) {
  scales::number(x, big.mark = big.mark, ...)
}
