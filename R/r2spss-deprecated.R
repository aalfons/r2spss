# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------


#' Deprecated functions in r2spss
#'
#' These functions are deprecated and may be removed as soon as the next
#' release of \pkg{r2spss}.  The plot functions \code{plotSPSS},
#' \code{linesSPSS}, \code{boxSPSS}, and \code{histSPSS} are built around
#' base \R graphics and have been superseded by functions built on
#' \code{\link[ggplot2:ggplot2-package]{ggplot2}}.
#'
#' \code{plotSPSS} draws a scatter plot or a scatter plot matrix of variables
#' in a data frame.
#'
#' \code{linesSPSS} draws connected lines for variables in a data frame.
#'
#' \code{boxplotSPSS} draw box plots of variables in a data frame, including
#' box plots for groups of observations and box plots for separate variables.
#'
#' \code{histSPSS} draws a histogram of a variable in a data frame.
#'
#' The plots thereby mimic the look of graphs in older versions of SPSS (<24).
#'
#' @name r2spss-deprecated
#'
#' @param data  a data frame containing the variables to be plotted.
#' @param variables
#' For \code{plotSPSS}, a character vector specifying at
#' least two variables to be plotted.  In case of two variables, a simple
#' scatter plot is produced with the first variable on the \eqn{x}-axis and the
#' second variable on the \eqn{y}-axis.  In case of more than two variables, a
#' scatter plot matrix is produced.
#'
#' For \code{linesSPSS}, a character vector specifying at least one variable
#' to be plotted on the \eqn{y}-axis.  In case of multiple variables, separate
#' lines are drawn for each variable and a legend is shown.
#'
#' For \code{boxplotSPSS}, a character vector specifying separate variables to
#' be plotted.  If \code{group} is not \code{NULL}, only the first variable is
#' used and box plots of groups of observations are drawn instead.
#'
#' @param variable  a character string specifying the variable to be plotted.
#' @param index  a character string specifying a variable to be plotted on the
#' \eqn{x}-axis, or \code{NULL} to plot the observations against their index.
#' @param group  an character string specifying a grouping variable, or
#' \code{NULL} for no grouping.
#' @param normal  a logical indicating whether to add a normal density with the
#' estimated mean and standard deviation (the default is \code{FALSE}).
#' @param xlab,ylab  the axis labels.
#' @param cut.names  a logical indicating whether to cut long variable names or
#' group labels to 8 characters.  The default is \code{TRUE} for box plots of
#' separate variables, but \code{FALSE} for box plots of groups of observations
#' (which mimics SPSS behavior).
#' @param \dots  additional arguments to be passed down, in particular
#' graphical parameters (see \code{\link[graphics]{boxplot}},
#' \code{\link[graphics]{hist}}, and \code{\link[graphics]{par}}).
#'
#' @return
#'
#' \code{plotSPSS} and \code{linesSPSS} do not return anything but produce a
#' plot.
#'
#' \code{boxplotSPSS} returns a list containing summary statistics invisibly
#' (see \code{\link[graphics]{boxplot}}) and produces a plot.
#'
#' \code{histSPSS} returns an object of class \code{"histogram"} invisibly (see
#' \code{\link[graphics]{hist}}) and produces a plot.
#'
#' @author Andreas Alfons
#'
#' @keywords hplot

NULL


#' @rdname r2spss-deprecated
#' @importFrom graphics box mtext par plot points rect
#' @export

plotSPSS <- function(data, variables, xlab = NULL, ylab = NULL, ...) {
  .Deprecated("scatter_plot")
  # initializations
  data <- as.data.frame(data)
  variables <- as.character(variables)
  if (length(variables) < 2) stop("at least two variables must be specified")
  # create plot
  if (length(variables) == 2) {
    if (is.null(xlab)) xlab <- variables[1]
    if (is.null(ylab)) ylab <- variables[2]
    .plot(data[, variables[1]], data[, variables[2]], xlab=xlab, ylab=ylab, ...)
  } else {
    .pairs <- function(..., version) pairs_SPSS(..., version = "legacy")
    .pairs(data[, variables], ...)
  }
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


#' @rdname r2spss-deprecated
#' @importFrom graphics axis legend lines matlines matplot par plot rect
#' @export

linesSPSS <- function(data, variables, index = NULL,
                      xlab = NULL, ylab = NULL, ...) {
  .Deprecated("line_plot")
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


#' @rdname r2spss-deprecated
#' @importFrom graphics boxplot par points rect
#' @importFrom stats as.formula
#' @export

boxplotSPSS <- function(data, variables, group = NULL, xlab = NULL,
                        ylab = NULL, cut.names = NULL, ...) {
  .Deprecated("box_plot")
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


#' @rdname r2spss-deprecated
#' @importFrom graphics box hist mtext par plot rect
#' @importFrom stats dnorm sd
#' @export

histSPSS <- function(data, variable, normal = FALSE,
                     xlab = NULL, ylab = NULL, ...) {
  .Deprecated("histogram")
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
.hist <- function(x, ..., breaks = get_bins, normal = TRUE, ylim = NULL,
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
