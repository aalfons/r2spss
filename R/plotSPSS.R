# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' @import ggplot2
#' @export

scatterSPSS <- function(data, variables,
                        version = r2spssOptions$get("version"),
                        expand = 0.05, ...) {
  # initializations
  data <- as.data.frame(data)
  variables <- as.character(variables)
  if (length(variables) < 2) stop("at least two variables must be specified")
  version <- match.arg(version, choices = getVersionOptions())
  # create plot
  if (length(variables) == 2) {
    # define default axis labels
    xlab <- variables[1]
    ylab <- variables[2]
    # determine range of x-axis and expand it according to expansion factor
    xlim <- range(data[, variables[1]], na.rm = TRUE)
    xlim <- xlim + c(-1, 1) * expand * diff(xlim)
    # determine range of y-axis and expand it according to expansion factor
    ylim <- range(data[, variables[2]], na.rm = TRUE)
    ylim <- ylim + c(-1, 1) * expand * diff(ylim)
    # construct plot
    ggplot() +
      geom_point(aes_string(x = variables[1], y = variables[2]), data = data,
                 shape = 21, fill = "#009ECC", size = 2) +
      geom_hline(yintercept = ylim[1]) +
      geom_vline(xintercept = xlim[1]) +
      theme_bw() +
      theme(axis.title = element_text(face = "bold"),
            axis.ticks.length = unit(0, "points"),
            panel.border = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "#B7B7B7"),
            panel.grid.minor = element_blank()) +
      scale_x_continuous(labels = numberSPSS, limits = xlim,
                         expand = expansion(mult = 0)) +
      scale_y_continuous(labels = numberSPSS, limits = ylim,
                         expand = expansion(mult = 0)) +
      labs(x = xlab, y = ylab)
  } else {
    # scatterplot matrix using base R graphics
    .pairs(data[, variables], version = version, ...)
  }
}
