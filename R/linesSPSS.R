# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' @import ggplot2
#' @export

lineplotSPSS <- function(data, variables, index = NULL,
                         version = r2spssOptions$get("version"),
                         fatten = NULL, ...) {
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
                     fatten = fatten)
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
                     fatten = fatten) +
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
                           fatten = NULL) {
  # initializations
  version <- match.arg(version, choices = getVersionValues())
  if (is.null(fatten)) fatten <- if (version == "legacy") 1 else 2
  # extract argument names
  arguments <- list(...)
  argument_names <- names(arguments)
  # replace argument names with standardized ones
  standardized_names <- standardise_aes_names(argument_names)
  names(arguments) <- standardized_names
  # check size of lines
  size <- arguments$size
  if (is.null(size)) size <- 0.5
  # if we have a single line, set default values according to SPSS version
  group <- arguments$mapping$group
  if (is.null(group) || group == 1) {
    # # default line color
    # if (is.null(arguments$colour)) arguments$colour <- "black"
    # make line thicker
    arguments$size <- fatten * size
  } else arguments$size <- size
  # call geom_line()
  do.call(geom_line, arguments)
}
