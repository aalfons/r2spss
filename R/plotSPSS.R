# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------


#' @import ggplot2
#' @export

scatterSPSS <- function(data, variables,
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
      geom_point_SPSS(mapping, data = data, ..., version = version)
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
    # scatterplot matrix using base R graphics
    .pairs(data[, variables], version = version, ...)
  }
}


# custom geom for lines with defaults to mimic appearance of SPSS
# TODO: setting defaults like that works if the geom remains internal, but it
#       will probably create issues if we want to export this to users (and
#       they for instance set the color based on a variable)
geom_point_SPSS <- function(..., version = r2spssOptions$get("version")) {
  # initializations
  version <- match.arg(version, choices = getVersionValues())
  # extract argument names
  arguments <- list(...)
  argument_names <- names(arguments)
  # replace argument names with standardized ones
  standardized_names <- standardise_aes_names(argument_names)
  names(arguments) <- standardized_names
  # check plot symbol
  if (is.null(arguments$shape)) {
    arguments$shape <- if (version == "legacy") 1 else 21
  }
  # check colors
  if (is.null(arguments$colour)) arguments$colour <- "black"
  if (version != "legacy" && is.null(arguments$fill)) {
    arguments$fill <- "#1192E8"
  }
  if (is.null(arguments$alpha)) arguments$alpha <- 1
  # check size of plot symbol
  if (is.null(arguments$size)) arguments$size <- 2
  if (is.null(arguments$stroke)) arguments$stroke <- 0.5
  # call geom_point()
  do.call(geom_point, arguments)
}
