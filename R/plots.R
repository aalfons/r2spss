#' @export
boxplotSPSS <- function(data, variables, categories = NULL,
                        xlab = NULL, ylab = NULL,
                        cut.names = NULL, ...) {
  # initializations
  data <- as.data.frame(data)
  variables <- as.character(variables)
  categories <- as.character(categories)
  if (length(variables) == 0) {
    stop("a variable to be summarized must be specified")
  }
  # create plot
  if (length(categories) == 0) {
    if (is.null(cut.names)) cut.names <- FALSE
    .boxplot(data[, variables, drop=FALSE], xlab=xlab, ylab=ylab,
             cut.names=cut.names, ...)
  } else {
    if (is.null(cut.names)) cut.names <- TRUE
    if (is.null(xlab)) xlab <- categories[1]
    if (is.null(ylab)) ylab <- variables[1]
    f <- as.formula(paste0(variables[1], "~", categories[1]))
    .boxplot(f, data=data, xlab=xlab, ylab=ylab, cut.names=cut.names, ...)
  }
}

# internal function with different defaults
.boxplot <- function(..., mar = NULL, bg = "#F0F0F0", boxwex = 0.5,
                     border = par("fg"), lty = 1, col = "#D3CE97",
                     outline = TRUE, pch = c(1, 42), cex = c(1, 1.5),
                     xlab = NULL, ylab = NULL, cex.lab = 1.2, axes = TRUE,
                     names = NULL, show.names = TRUE, cut.names = FALSE,
                     # the following arguments are currently ignored
                     plot = TRUE, range = 1.5, horizontal = FALSE,
                     add = FALSE, at = NULL, log = "") {
  # initializations
  if (is.null(mar)) {
    bottom <- if (is.null(xlab) || nchar(xlab) == 0) 2 else 4
    left <- if (is.null(ylab) || nchar(ylab) == 0) 2 else 4
    mar <- c(bottom, left, 0, 0) + 0.1
  }
  # set plot margins
  op <- par(mar=mar)
  on.exit(par(op))
  # get boxplot information and initialize plot
  b <- boxplot(..., boxwex=boxwex, outline=outline, axes = FALSE)
  # plot background
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col=bg, border=NA)
  # cut group names to 8 characters if requested
  if (is.null(names)) names <- b$names
  if (cut.names) names <- substr(names, 1, 8)
  # add boxplot
  b <- boxplot(..., boxwex=boxwex, border=border, lty=lty, col=col,
               outline=FALSE, xlab=xlab, ylab=ylab, cex.lab=cex.lab,
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
