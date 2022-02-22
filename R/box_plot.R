# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' @import ggplot2
#' @export

box_plot <- function(data, variables, group = NULL,
                     version = r2spssOptions$get("version"),
                     error.bar = c("T", "whiskers"), cut.names = NULL,
                     outlier.shape = c(1, 42), ...) {
  # initializations
  data <- as.data.frame(data)
  variables <- as.character(variables)
  group <- as.character(group)
  if (length(variables) == 0) stop("a variable to display must be specified")
  # check which SPSS functionality to mimic
  version <- match.arg(version, choices = getVersionValues())
  error.bar <- match.arg(error.bar)
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
  # initialize plot
  p <- ggplot() +
    geom_boxplot_SPSS(mapping, data = data, ..., version = version)
  # extract boxplot statistics
  stats <- layer_data(p)
  outlierList <- stats$outliers
  stats$outliers <- NULL
  # extract length of whiskers
  coef <- list(...)$coef
  if (is.null(coef)) coef <- 1.5
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
  # extract scales of axes
  scale_info <- layer_scales(p)
  scales <- lapply(scale_info, function(scale) {
    if (scale$is_discrete()) "discrete" else "continuous"
  })
  # if requested add T-shaped error bars rather than whiskers
  if (error.bar == "T") {
    # construct data frame to complete error bars
    width <- 0.5 * (stats$xmax - stats$xmin)
    errorbars <- data.frame(xmin = stats$x - width / 2,
                            xmax = stats$x + width / 2,
                            stats[, c("ymin", "ymax")])
    # add line segments to boxplot to complete error bars
    p <- p +
      geom_error_SPSS(aes(x = xmin, xend = xmax, y = ymin, yend = ymin),
                      data = errorbars, ...) +
      geom_error_SPSS(aes(x = xmin, xend = xmax, y = ymax, yend = ymax),
                      data = errorbars, ...)
  }
  # add intermediate and extreme outliers
  p <- p +
    geom_outliers_SPSS(aes(x = x, y = y), data = intermediate, ...,
                       outlier.shape = outlier.shape[1], fatten = fatten[1]) +
    geom_outliers_SPSS(aes(x = x, y = y), data = extreme, ...,
                       outlier.shape = outlier.shape[2], fatten = fatten[2])
  # finalize plot
  p <- p + theme_SPSS(version = version, scale.x = scales$x, scale.y = scales$y)
  if (scales$x == "continuous") p <- p + scale_x_continuous(labels = numberSPSS)
  else if (cut.names) p <- p + scale_x_discrete(labels = substrSPSS)
  if (scales$y == "continuous") p <- p + scale_y_continuous(labels = numberSPSS)
  p <- p + labs(x = xlab, y = ylab)
  # return plot
  p
}


# custom geom for boxplot with defaults to mimic appearance of SPSS
geom_boxplot_SPSS <- function(..., version = r2spssOptions$get("version"),
                              # arguments to be ignored
                              stat, outlier.colour, outlier.color, outlier.fill,
                              outlier.shape, outlier.size, outlier.stroke,
                              outlier.alpha, notch, notchwidth, orientation) {
  # obtain list of arguments with standardized names
  arguments <- standardize_arguments(...)
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
  arguments <- standardize_arguments(...)
  # check color
  if (is.null(arguments$color)) arguments$color <- "black"
  # check line types and size
  if (is.null(arguments$linetype)) arguments$linetype <- "solid"
  if (is.null(arguments$size)) size <- 0.5
  # call geom_segment()
  do.call(geom_segment, arguments)
}

# custom geom for plotting outliers to mimic appearance of SPSS
geom_outliers_SPSS <- function(..., outlier.colour = "black",
                               outlier.color = "black",
                               outlier.fill = NULL,
                               outlier.shape = 1, outlier.size = 2,
                               outlier.stroke = 0.5, outlier.alpha = 1,
                               fatten = 1,
                               # arguments to be ignored,
                               stat, geom, position, notch, notchwidth, width,
                               varwidth, coef, orientation, linetype, lty, lwd) {
  # obtain list of arguments with standardized names
  arguments <- standardize_arguments(...)
  # check border color
  if (missing(outlier.color)) {
    if (!missing(outlier.colour)) outlier.color <- outlier.colour
    else if (!is.null(arguments$color)) outlier.color <- arguments$color
  }
  # check fill color
  if (missing(outlier.fill) && !is.null(arguments$fill)) {
    outlier.fill <- arguments$fill
  }
  # check transparency
  if (missing(outlier.alpha) && !is.null(arguments$alpha)) {
    outlier.alpha <- arguments$alpha
  }
  # overwrite arguments for geom_point()
  arguments$color <- outlier.color
  arguments$fill <- outlier.fill
  arguments$shape <- outlier.shape
  arguments$size <- outlier.size * fatten
  arguments$stroke <- outlier.stroke
  arguments$alpha <- outlier.alpha
  # call geom_point()
  do.call(geom_point, arguments)
}
