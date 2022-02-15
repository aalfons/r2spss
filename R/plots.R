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
#' @param xlab,ylab  the axis labels.
#' @param \dots  additional arguments to be passed down, in particular
#' graphical parameters (see \code{\link[graphics]{par}}).
#'
#' @return  Nothing is returned, but a plot is produced.
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
#' linesSPSS(means, "logMarketValue", "Position")
#'
#' # easier and fancier as the plot method of ANOVA results
#' oneway <- ANOVA(Eredivisie, "logMarketValue",
#'                 group = "Position")
#' plot(oneway)
#'
#' @keywords hplot
#'
#' @importFrom graphics axis legend lines matlines matplot par plot rect
#' @export

linesSPSS <- function(data, variables, index = NULL,
                      xlab = NULL, ylab = NULL, ...) {
  # initializations
  data <- as.data.frame(data)
  n <- nrow(data)
  variables <- as.character(variables)
  if (length(variables) == 0) stop("a variable to display must be specified")
  # check index to plot on x-axis
  if (is.null(index) || length(index) == 0) {
    xval <- seq_len(n)
    if (is.null(xlab)) xlab <- "Case Number"
  } else {
    index <- as.character(index)
    xval <- data[, index[1]]
    if (is.null(xlab)) xlab <- index[1]
  }
  # create plot
  if (length(variables) == 1) {
    if (is.null(ylab)) ylab <- variables
    .lines(xval, data[, variables], xlab=xlab, ylab=ylab, ...)
  } else .matlines(xval, data[, variables], xlab=xlab, ylab=ylab, ...)
}

# internal function for line plot with different defaults
.lines <- function(x, y, ..., type = c("l", "o"), xlim = NULL, ylim = NULL,
                   mar = NULL, bg = "#F0F0F0", main = NULL, xlab = NULL,
                   ylab = NULL, font.lab = 2, cex.lab = 1.2, las = 1,
                   # the following arguments are currently ignored
                   axes = TRUE, xaxt = "s", yaxt = "s", log = "", sub = NULL) {
  # initializations
  type <- match.arg(type)
  if (is.null(mar)) {
    top <- if (is.null(main) || nchar(main) == 0) 0 else 2
    bottom <- if (is.null(xlab) || nchar(xlab) == 0) 2 else 4
    left <- if (is.null(ylab) || nchar(ylab) == 0) 2 else 4
    mar <- c(bottom, left, top, 0) + 0.1
  }
  # set plot margins
  op <- par(mar=mar, las=las, font.lab=font.lab)
  oo <- options(scipen = .Machine$integer.max/2) # doesn't work with integer.max
  on.exit({
    par(op)
    options(oo)
  })
  # initialize plot
  if (is.character(x)) {
    labs <- x
    x <- seq_along(x)
    if (is.null(xlim)) xlim <- range(x) + c(-0.5, 0.5)
    plot(x, y, ..., type="n", xlim=xlim, ylim=ylim, main=main, xlab=xlab,
         ylab=ylab, font.lab=font.lab, cex.lab=cex.lab, xaxt="n")
    axis(side=1, at=x, labels=labs)
  } else {
    plot(x, y, ..., type="n", xlim=xlim, ylim=ylim, main=main, xlab=xlab,
         ylab=ylab, font.lab=font.lab, cex.lab=cex.lab)
  }
  # plot background
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col=bg, border=NA)
  # add points
  lines(x, y, type=type, ...)
}

# internal function for matrix line plot with different defaults
.matlines <- function(x, y, ..., type = c("l", "o"), xlim = NULL, ylim = NULL,
                      mar = NULL, bg = "#F0F0F0", lty = 1, lwd = 1.5, pch = 1,
                      col = paletteSPSS(), main = NULL, xlab = NULL,
                      ylab = NULL, title = NULL, font.lab = 2, cex.lab = 1.2,
                      las = 1,
                      # the following arguments are currently ignored
                      axes = TRUE, xaxt = "s", yaxt = "s", log = "",
                      sub = NULL, add = FALSE, verbose = FALSE) {
  # initializations
  y <- as.matrix(y)
  labels <- colnames(y)
  type <- match.arg(type)
  if (is.null(mar)) {
    top <- if (is.null(main) || nchar(main) == 0) 0 else 2
    bottom <- if (is.null(xlab) || nchar(xlab) == 0) 2 else 4
    left <- if (is.null(ylab) || nchar(ylab) == 0) 2 else 4
    mar <- c(bottom, left, top, 10) + 0.1
  }
  # set plot margins
  op <- par(mar=mar, las=las, font.lab=font.lab)
  oo <- options(scipen = .Machine$integer.max/2) # doesn't work with integer.max
  on.exit({
    par(op)
    options(oo)
  })
  # initialize plot
  if (is.character(x)) {
    labs <- x
    x <- seq_along(x)
    if (is.null(xlim)) xlim <- range(x) + c(-0.5, 0.5)
    matplot(x, y, ..., type="n", xlim=xlim, ylim=ylim, main=main, xlab=xlab,
            ylab=ylab, font.lab=font.lab, cex.lab=cex.lab, xaxt="n",
            add=FALSE, verbose=FALSE)
    axis(side=1, at=x, labels=labs)
  } else {
    matplot(x, y, ..., type="n", xlim=xlim, ylim=ylim, main=main, xlab=xlab,
            ylab=ylab, font.lab=font.lab, cex.lab=cex.lab, add=FALSE,
            verbose=FALSE)
  }
  # plot background
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col=bg, border=NA)
  # add points
  matlines(x, y, type=type, lty=lty, lwd=lwd, pch=pch, col=col,
           ..., verbose=FALSE)
  # add legend
  legend(usr[2], usr[4], title=title, legend=labels,
         lty=lty, lwd=lwd, col=col, bty="n", xpd=NA)
}


#' Boxplots
#'
#' Draw boxplots of variables in a data frame, including boxplots for groups of
#' observations and boxplots for separate variables.  The plots thereby mimic
#' the look of SPSS graphs.
#'
#' @param data  a data frame containing the variables to be plotted.
#' @param variables  a character vector specifying separate variables to be
#' plotted.  If \code{group} is not \code{NULL}, only the first variable is
#' used and boxplots of groups of observations are drawn instead.
#' @param group  an character string specifying a grouping variable, or
#' \code{NULL} for no grouping.
#' @param xlab,ylab  the axis labels.
#' @param cut.names  a logical indicating whether to cut long variable names or
#' group labels to 8 characters.  The default is \code{TRUE} for boxplots of
#' separate variables, but \code{FALSE} for boxplots of groups of observations
#' (which mimics SPSS behavior).
#' @param \dots  additional arguments to be passed down, in particular
#' graphical parameters (see \code{\link[graphics]{boxplot}} and
#' \code{\link[graphics]{par}}).
#'
#' @return  A list containing summary statistics is returned invisibly (see
#' \code{\link[graphics]{boxplot}}).
#'
#' @author Andreas Alfons
#'
#' @examples
#' ## paired sample
#' # load data
#' data("Exams")
#'
#' # plot grades on regular and resit exams
#' boxplotSPSS(Exams, c("Regular", "Resit"))
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
#' boxplotSPSS(Eredivisie, "logMarketValue", group = "Foreign")
#'
#' @keywords hplot
#'
#' @importFrom graphics boxplot par points rect
#' @importFrom stats as.formula
#' @export

boxplotSPSS <- function(data, variables, group = NULL, xlab = NULL,
                        ylab = NULL, cut.names = NULL, ...) {
  # initializations
  data <- as.data.frame(data)
  variables <- as.character(variables)
  group <- as.character(group)
  if (length(variables) == 0) stop("a variable to display must be specified")
  # create plot
  if (length(group) == 0) {
    if (is.null(cut.names)) cut.names <- FALSE
    .boxplot(data[, variables, drop=FALSE], xlab=xlab, ylab=ylab,
             cut.names=cut.names, ...)
  } else {
    if (is.null(cut.names)) cut.names <- TRUE
    if (is.null(xlab)) xlab <- group[1]
    if (is.null(ylab)) ylab <- variables[1]
    f <- as.formula(paste0(variables[1], "~", group[1]))
    .boxplot(f, data=data, xlab=xlab, ylab=ylab, cut.names=cut.names, ...)
  }
}

# internal function with different defaults
.boxplot <- function(..., mar = NULL, bg = "#F0F0F0", boxwex = 0.5,
                     border = par("fg"), lty = 1, col = "#D3CE97",
                     outline = TRUE, pch = c(1, 42), cex = c(1, 1.5),
                     main = NULL, xlab = NULL, ylab = NULL, font.lab = 2,
                     cex.lab = 1.2, axes = TRUE, las = 1, names = NULL,
                     show.names = TRUE, cut.names = FALSE,
                     # the following arguments are currently ignored
                     plot = TRUE, range = 1.5, horizontal = FALSE,
                     add = FALSE, at = NULL, log = "", sub = NULL) {
  # initializations
  if (is.null(mar)) {
    top <- if (is.null(main) || nchar(main) == 0) 0 else 2
    bottom <- if (is.null(xlab) || nchar(xlab) == 0) 2 else 4
    left <- if (is.null(ylab) || nchar(ylab) == 0) 2 else 4
    mar <- c(bottom, left, top, 0) + 0.1
  }
  # set plot margins
  op <- par(font.lab=font.lab, mar=mar, las=las)
  oo <- options(scipen = .Machine$integer.max/2) # doesn't work with integer.max
  on.exit({
    par(op)
    options(oo)
  })
  # get boxplot statistics and initialize plot
  b <- boxplot(..., boxwex=boxwex, outline=outline, axes = FALSE)
  # plot background
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col=bg, border=NA)
  # cut group names to 8 characters if requested
  if (is.null(names)) names <- b$names
  if (cut.names) names <- substr(names, 1, 8)
  # add boxplot
  b <- boxplot(..., boxwex=boxwex, border=border, lty=lty, col=col,
               outline=FALSE, main=main, xlab=xlab, ylab=ylab,
               font.lab=font.lab, cex.lab=cex.lab, axes=axes,
               names=names, show.names=show.names, add=TRUE)
  # add outliers
  if (outline && length(b$out) > 0) {
    # recycle graphical parameters
    pch <- rep_len(pch, 2)
    cex <- rep_len(cex, 2)
    ngroups <- ncol(b$stats)
    intpch <- rep_len(pch[1], ngroups)  # plot symbol for intermediate outliers
    intcex <- rep_len(cex[1], ngroups)  # symbol size for intermediate outliers
    extpch <- rep_len(pch[2], ngroups)  # plot symbol for extreme outliers
    extcex <- rep_len(cex[2], ngroups)  # symbol size for extreme outliers
    border <- rep_len(border, ngroups)
    col <- rep_len(col, ngroups)
    # find extreme outliers
    boxheights <- b$stats[4, ] - b$stats[2, ]
    extreme <- abs(b$out - b$stats[3, b$group]) > 2*range * boxheights[b$group]
    # plot intermediate outliers
    intout <- b$out[!extreme]
    intgroup <- b$group[!extreme]
    points(intgroup, intout, pch=intpch[intgroup], cex=intcex[intgroup],
           col=border[intgroup], bg=col[intgroup])
    # plot extreme outliers
    extout <- b$out[extreme]
    extgroup <- b$group[extreme]
    points(extgroup, extout, pch=extpch[extgroup], cex=extcex[extgroup],
           col=border[extgroup], bg=col[extgroup])
  }
  # return boxplot statistics
  invisible(b)
}


#' Scatterplot and Scatterplot Matrix
#'
#' Draw a scatterplot or a scatterplot matrix of variables in a data frame.
#' The plots thereby mimic the look of SPSS graphs.
#'
#' @param data  a data frame containing the variables to be plotted.
#' @param variables  a character vector specifying at least two variables to be
#' plotted.  In case of two variables, a simple scatterplot is produced with
#' the first variable on the \eqn{x}-axis and the second variable on the
#' \eqn{y}-axis.  In case of more than two variables, a scatterplot matrix is
#' produced.
#' @param xlab,ylab  the axis labels for a simple scatterplot (the default is
#' to use the variable names).  This is ignored for a scatterplot matrix.
#' @param version  a character string specifying whether the plot should mimic
#' the look of recent SPSS versions (\code{"modern"}) or older versions (<24;
#' \code{"legacy"}).
#' @param \dots  additional arguments to be passed down, in particular
#' graphical parameters (see \code{\link[graphics]{par}}).
#'
#' @return  Nothing is returned, but a plot is produced.
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
#' plotSPSS(Eredivisie, c("Age", "logMarketValue"))
#'
#' # scatterplot matrix of age, number of minutes played, and
#' # log market values
#' plotSPSS(Eredivisie, c("Age", "Minutes", "logMarketValue"))
#'
#' @keywords hplot
#'
#' @importFrom graphics box mtext par plot points rect
#' @export

plotSPSS <- function(data, variables, xlab = NULL, ylab = NULL,
                     version = r2spssOptions$get("version"), ...) {
  # initializations
  data <- as.data.frame(data)
  variables <- as.character(variables)
  if (length(variables) < 2) stop("at least two variables must be specified")
  version <- match.arg(version, choices = getVersionOptions())
  # create plot
  if (length(variables) == 2) {
    if (is.null(xlab)) xlab <- variables[1]
    if (is.null(ylab)) ylab <- variables[2]
    .plot(data[, variables[1]], data[, variables[2]], xlab=xlab, ylab=ylab, ...)
  } else .pairs(data[, variables], version = version, ...)
}

# internal function for scatter plot with different defaults
.plot <- function(x, y, ..., mar = NULL, bg = "#F0F0F0", main = NULL,
                  xlab = NULL, ylab = NULL, font.lab = 2, cex.lab = 1.2,
                  las = 1,
                  # the following arguments are currently ignored
                  type = "p", log = "", sub = NULL) {
  # initializations
  if (is.null(mar)) {
    top <- if (is.null(main) || nchar(main) == 0) 0 else 2
    bottom <- if (is.null(xlab) || nchar(xlab) == 0) 2 else 4
    left <- if (is.null(ylab) || nchar(ylab) == 0) 2 else 4
    mar <- c(bottom, left, top, 0) + 0.1
  }
  # set plot margins
  op <- par(mar=mar, las=las)
  oo <- options(scipen = .Machine$integer.max/2) # doesn't work with integer.max
  on.exit({
    par(op)
    options(oo)
  })
  # initialize plot
  plot(x, y, ..., type="n", main=main, xlab=xlab, ylab=ylab,
       font.lab=font.lab, cex.lab=cex.lab)
  # plot background
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col=bg, border=NA)
  # add points
  points(x, y, ...)
}

# internal function for scatterplot matrix with different defaults
.pairs <- function(x, version = "modern", ..., frame.plot = TRUE, oma = NULL,
                   pch = NULL, col = "black", bg = NULL, main = NULL,
                   font.main = 2, cex.main = 1.2, font.lab = NULL, cex.lab = 1,
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
    if (is.null(bg)) bg <- "#009CEE"      # here: fill color of points
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


#' Histogram
#'
#' Draw a histogram of a variable in a data frame.  The plot thereby mimics the
#' look of SPSS graphs.
#'
#' @param data  a data frame containing the variable to be plotted.
#' @param variable  a character string specifying the variable to be
#' plotted.
#' @param normal  a logical indicating whether to add a normal density with the
#' estimated mean and standard deviation (the default is \code{FALSE}).
#' @param xlab,ylab  the axis labels (the default is to use the variable name
#' for the \eqn{x}-axis label and \code{"Frequency"} as the \eqn{y}-axis label).
#' @param \dots  additional arguments to be passed down, in particular
#' graphical parameters (see \code{\link[graphics]{hist}} and
#' \code{\link[graphics]{par}}).
#'
#' @return  An object of class \code{"histogram"} is returned invisibly (see
#' \code{\link[graphics]{hist}}).
#'
#' @author Andreas Alfons
#'
#' @examples
#' # load data
#' data("Eredivisie")
#' # log-transform market values
#' Eredivisie$logMarketValue <- log(Eredivisie$MarketValue)
#'
#' # plot histogram of log market values
#' histSPSS(Eredivisie, "logMarketValue")
#'
#' @keywords hplot
#'
#' @importFrom graphics box hist mtext par plot rect
#' @importFrom stats dnorm sd
#' @export

histSPSS <- function(data, variable, normal = FALSE,
                     xlab = NULL, ylab = NULL, ...) {
  # initializations
  data <- as.data.frame(data)
  variable <- as.character(variable)
  if (length(variable) == 0) stop("a variable to display must be specified")
  if (is.null(xlab)) xlab <- variable[1]
  if (is.null(ylab)) ylab <- "Frequency"
  # create plot
  h <- .hist(data[, variable[1]], normal=normal, xlab=xlab, ylab=ylab, ...)
  h$xname <- variable[1]
  invisible(h)
}

# internal function with different defaults
.hist <- function(x, ..., breaks = getBins, normal = TRUE, ylim = NULL,
                  frame.plot = TRUE, mar = NULL, bg = "#F0F0F0",
                  border = par("fg"), col = "#D3CE97", main = NULL,
                  xlab = NULL, ylab = NULL, font.lab = 2, cex.lab = 1.2,
                  las = 1,
                  # the following arguments are currently ignored
                  freq = TRUE, probability = !freq, plot = TRUE,
                  warn.unused = FALSE, add = FALSE, log = "", sub = NULL) {
  # initializations
  x <- x[is.finite(x)]
  if (is.null(mar)) {
    top <- if (is.null(main) || nchar(main) == 0) 0 else 2
    bottom <- if (is.null(xlab) || nchar(xlab) == 0) 2 else 4
    left <- if (is.null(ylab) || nchar(ylab) == 0) 2 else 4
    mar <- c(bottom, left, top, 8) + 0.1
  }
  # set graphical parameters
  op <- par(mar=mar, yaxs="i", las=las)
  oo <- options(scipen = .Machine$integer.max/2) # doesn't work with integer.max
  on.exit({
    par(op)
    options(oo)
  })
  # get histogram statistics
  h <- hist(x, ..., breaks=breaks, plot=FALSE, warn.unused=FALSE)
  # compute summary statistics
  m <- mean(x)
  s <- sd(x)
  n <- length(x)
  # compute normal density if requested
  if (normal) {
    nbreaks <- length(h$breaks)
    rbreaks <- h$breaks[c(1, nbreaks)]
    step <- diff(rbreaks) / (nbreaks-1)
    grid <- seq(rbreaks[1]-3*step, rbreaks[2]+3*step, length.out=100)
    whichMax <- which.max(h$counts)
    density <- dnorm(grid, mean=m, sd=s)*h$counts[whichMax]/h$density[whichMax]
  } else density <- NULL
  # find y-axis limit and initialize plot
  if (is.null(ylim)) ylim <- c(0, max(h$counts, density))
  if (op$yaxs == "r") {
    if (ylim[1] != 0) ylim[1] <- ylim[1] - 0.04 * diff(ylim)
    ylim[2] <- ylim[2] + 0.04 * diff(ylim)
  }
  plot(h, ..., ylim=ylim, main=main, xlab=xlab, ylab=ylab,
       font.lab=font.lab, cex.lab=cex.lab)
  # plot background
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col=bg, border=NA)
  # add frame around plot
  if (frame.plot) box()
  # add histogram
  plot(h, ..., border=border, col=col, add=TRUE)
  mtext(sprintf("Mean = %.3f\nStd. Dev. = %.3f\nN = %d", m, s, n),
        side=4, line=0.5, las=1)
  # add normal density if requested
  if (normal) lines(grid, density, col=border)
  # return histogram statistics
  invisible(h)
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


#' SPSS Color Palette
#'
#' Color palette used by SPSS (e.g., for multiple lines in a plot).
#'
#' @return A character vector specifying 30 colors as used by SPSS.
#'
#' @author Andreas Alfons
#'
#' @examples
#' df <- data.frame(x = 1:30, y = 0)
#' colors <- paletteSPSS()
#' plotSPSS(df, c("x", "y"), col = colors, pch = 16)
#'
#' @keywords color
#'
#' @importFrom grDevices rgb
#' @export

paletteSPSS <- function() {
  # define red, green and blue vectors
  red <-   c( 62,  46, 211, 121, 251, 239,  72, 204, 122, 10, 248, 221,  26, 204, 187, 153, 0, 182, 255, 121, 112, 51, 172, 162,  93, 228,  39, 184, 102,  13)
  green <- c( 88, 184, 206,  40, 248,  51, 194, 204, 170, 86, 152, 186,  95, 255,  63, 153, 0, 231, 255, 122, 220, 51, 208,  22,  97, 228, 139, 155, 102, 141)
  blue <-  c(172,  72, 151, 125, 115,  56, 197, 204, 213, 44,  29, 241, 118, 204, 127, 153, 0, 232, 255, 167, 132, 51, 238,  25, 255, 228, 172, 201, 102,  70)
  # return colors
  rgb(red, green, blue, maxColorValue=255)
}
