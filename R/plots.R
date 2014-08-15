#' @export
boxplotSPSS <- function(data, variables, category = NULL,
                        xlab = NULL, ylab = NULL,
                        cut.names = NULL, ...) {
  # initializations
  data <- as.data.frame(data)
  variables <- as.character(variables)
  category <- as.character(category)
  if (length(variables) == 0) {
    stop("a variable to be summarized must be specified")
  }
  # create plot
  if (length(category) == 0) {
    if (is.null(cut.names)) cut.names <- FALSE
    .boxplot(data[, variables, drop=FALSE], xlab=xlab, ylab=ylab,
             cut.names=cut.names, ...)
  } else {
    if (is.null(cut.names)) cut.names <- TRUE
    if (is.null(xlab)) xlab <- category[1]
    if (is.null(ylab)) ylab <- variables[1]
    f <- as.formula(paste0(variables[1], "~", category[1]))
    .boxplot(f, data=data, xlab=xlab, ylab=ylab, cut.names=cut.names, ...)
  }
}

# internal function with different defaults
.boxplot <- function(..., mar = NULL, bg = "#F0F0F0", boxwex = 0.5,
                     border = par("fg"), lty = 1, col = "#D3CE97",
                     outline = TRUE, pch = c(1, 42), cex = c(1, 1.5),
                     main = NULL, xlab = NULL, ylab = NULL, cex.lab = 1.2,
                     axes = TRUE, names = NULL, show.names = TRUE,
                     cut.names = FALSE,
                     # the following arguments are currently ignored
                     plot = TRUE, range = 1.5, horizontal = FALSE,
                     add = FALSE, at = NULL, log = "") {
  # initializations
  if (is.null(mar)) {
    top <- if (is.null(main) || nchar(main) == 0) 0 else 2
    bottom <- if (is.null(xlab) || nchar(xlab) == 0) 2 else 4
    left <- if (is.null(ylab) || nchar(ylab) == 0) 2 else 4
    mar <- c(bottom, left, top, 0) + 0.1
  }
  # set plot margins
  op <- par(mar=mar)
  on.exit(par(op))
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
               outline=FALSE, main=main, xlab=xlab, ylab=ylab, cex.lab=cex.lab,
               axes=axes, names=names, show.names=show.names, add=TRUE)
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

#' @export
histSPSS <- function(data, variable, normal = FALSE, xlab = variable,
                     ylab = "Frequency",...) {
  # initializations
  data <- as.data.frame(data)
  variable <- as.character(variable)
  if (length(variable) == 0) {
    stop("a variable to be summarized must be specified")
  }
  # create plot
  h <- .hist(data[, variable], normal=normal, xlab=xlab, ylab=ylab, ...)
  h$xname <- variable
  invisible(h)
}

# internal function with different defaults
.hist <- function(x, ..., breaks = getBins, normal = FALSE, ylim = NULL,
                  frame.plot = TRUE, mar = NULL, bg = "#F0F0F0",
                  border = par("fg"), col = "#D3CE97", main = NULL,
                  xlab = NULL, ylab = NULL, cex.lab = 1.2,
                  # the following arguments are currently ignored
                  freq = TRUE, probability = !freq, plot = TRUE,
                  warn.unused = FALSE, add = FALSE) {
  # initializations
  x <- x[is.finite(x)]
  if (is.null(mar)) {
    top <- if (is.null(main) || nchar(main) == 0) 0 else 2
    bottom <- if (is.null(xlab) || nchar(xlab) == 0) 2 else 4
    left <- if (is.null(ylab) || nchar(ylab) == 0) 2 else 4
    mar <- c(bottom, left, top, 8) + 0.1
  }
  # set graphical parameters
  op <- par(mar=mar, yaxs="i")
  on.exit(par(op))
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
  plot(h, ..., ylim=ylim, main=main, xlab=xlab, ylab=ylab, cex.lab=cex.lab)
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
