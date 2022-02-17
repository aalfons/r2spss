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
    size <- if (version == "legacy") 0.5 else 1
    p <- ggplot() +
      geom_line(mapping, data = data, color = "black", size = size)
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
      scale_color_SPSS(name = NULL, version = version)
    # define default y-axis label
    ylab <- "Value"
  }
  # extract scales of axes
  # scales <- extract_scales(p, expand = expand)
  scales <- sapply(layer_scales(p), function(scale) {
    if (scale$is_discrete()) "discrete" else "continuous"
  })
  # finalize plot
  p <- p +
    # theme_SPSS(version = version,
    #            scale.x = if (scales$x$discrete) "discrete" else "continuous",
    #            scale.y = if (scales$y$discrete) "discrete" else "continuous")
    theme_SPSS(version = version, scale.x = scales["x"], scale.y = scales["y"])
  # if (scales$x$discrete) {
  #   p <- p +
  #     expand_limits(x = scales$x$limits) +
  #     scale_x_discrete(expand = expansion(add = 0))
  # } else {
  #   p <- p +
  #     scale_x_continuous(labels = numberSPSS, limits = scales$x$limits,
  #                        expand = expansion(mult = 0))
  # }
  # if (scales$y$discrete) {
  #   p <- p +
  #     expand_limits(y = scales$y$limits) +
  #     scale_y_discrete(expand = expansion(add = 0))
  # } else {
  #   p <- p +
  #     scale_y_continuous(labels = numberSPSS, limits = scales$y$limits,
  #                        expand = expansion(mult = 0))
  # }
  if (scales["x"] == "continuous") p <- p + scale_x_continuous(labels = numberSPSS)
  if (scales["y"] == "continuous") p <- p + scale_y_continuous(labels = numberSPSS)
  p <- p + labs(x = xlab, y = ylab)
  # return plot
  p
}
