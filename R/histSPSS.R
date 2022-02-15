# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' @importFrom stats dnorm sd
#' @importFrom ggplot2 ggplot geom_histogram geom_rect geom_density layer_data
#' @export

histogramSPSS <- function(data, variable, normal = FALSE,
                          bins = NULL, expand = 0.05, ...) {
  # initializations
  data <- as.data.frame(data)
  variable <- as.character(variable)
  if (length(variable) == 0) stop("a variable to display must be specified")
  variable <- variable[1]
  # extract variable of interest
  x <- data[, variable]
  # if necessary, determine the number of bins
  if (is.null(bins)) bins <- getBins(x)
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
  # create an initial histogram
  initial <- ggplot(data, aes_string(x = variable)) +
    geom_histogram(bins = bins)
  # extract necessary information on histrogram
  keep <- c("x", "xmin", "xmax", "count", "density")
  histogram <- layer_data(initial)[, keep]
  # determine range of x-axis and expand it according to expansion factor
  xlim <- c(min(histogram$xmin), max(histogram$xmax))
  xlim <- xlim + c(-1, 1) * expand * diff(xlim)
  # # make bars of histogram a tiny bit smaller
  # FIXME: can we determine width of one pixel?
  # binwidth <- histogram[1, "xmax"] - histogram[1, "xmin"]
  # adjusted <- 0.95 * binwidth / 2
  # histogram$xmin <- histogram$x - adjusted
  # histogram$xmax <- histogram$x + adjusted
  # initialize plot as rectangles for histogram
  p <- ggplot() +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = count),
              data = histogram, color = "white", fill = "#009CEE")
  # if requested, compute normal density and it add to plot
  if (normal) {
    # define grid for normal density
    grid <- seq(from = xlim[1], to = xlim[2], length.out = 100)
    # compute rescaled normal density
    whichMax <- which.max(histogram$count)
    density <- dnorm(grid, mean = m, sd = s) *
      histogram[whichMax, "count"] / histogram[whichMax, "density"]
    # combine information into data.frame
    density <- data.frame(x = grid, y = density)
    # add density to plot
    p <- p +
      geom_line(aes(x = x, y = y), data = density, color = "black", size = 1)
  }
  # finalize appearance of plot
  p <- p +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = xlim[1]) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold"),
          axis.ticks.length = unit(0, "points"),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_x_continuous(limits = xlim, expand = expansion(mult = c(0, 0))) +
    scale_y_continuous(expand = expansion(mult = c(0, expand))) +
    labs(x = variable, y = "Frequency")
  # return plot
  p
}

# compute number of bins or breakpoints
getBins <- function(x) {
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
