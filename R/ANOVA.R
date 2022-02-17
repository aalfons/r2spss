# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' One-way and Two-way ANOVA
#'
#' Perform one-way or two-way ANOVA on variables of a data set.  The output is
#' printed as a LaTeX table that mimics the look of SPSS output, and a plot of
#' the results mimics the look of SPSS graphs.
#'
#' The \code{print} method first calls the \code{toSPSS} method followed by
#' \code{\link[=toLatex.SPSSTable]{toLatex}}.  Further customization can be
#' done by calling those two functions separately, and modifying the object
#' returned by \code{toSPSS}.
#'
#' @param data  a data frame containing the variables.
#' @param variable  a character string specifying the numeric variable of
#' interest.
#' @param group  a character vector specifying one or two grouping variables.
#' @param conf.level  a number between 0 and 1 giving the confidence level of
#' the confidence interval.
#' @param object,x  an object of class \code{"ANOVASPSS"} as returned by
#' function \code{ANOVA}.
#' @param statistics  a character string or vector specifying which SPSS tables
#' to produce.  Available options are \code{"descriptives"} for descriptive
#' statistics, \code{"variance"} for Levene's test on homogeneity of the
#' variances, and \code{"test"} for ANOVA results.  For the \code{toSPSS}
#' method, only one option is allowed (the default is the table of ANOVA
#' results), but the \code{print} method allows several options (the default
#' is to print all tables).
#' @param version  a character string specifying whether the table should
#' mimic the content and look of recent SPSS versions (\code{"modern"}) or
#' older versions (<24; \code{"legacy"}).  The main differences in terms of
#' content are that recent versions include different variations of Levene's
#' test, and that small p-values are displayed differently.
#' @param digits  an integer giving the number of digits after the comma to be
#' printed in the SPSS tables.
#' @param \dots  for  the \code{toSPSS} and \code{print} methods, additional
#' arguments to be passed down to \code{\link{formatSPSS}}.  For the
#' \code{plot} method, additional arguments to be passed down to
#' \code{\link{linesSPSS}}, in particular graphical parameters.
#'
#' @return
#' \code{ANOVA} returns an object of class \code{"ANOVASPSS"} with the
#' following components:
#' \describe{
#'   \item{\code{descriptives}}{a data frame containing per-group descriptive
#'   statistics.}
#'   \item{\code{levene}}{an object as returned by
#'   \code{\link[car]{leveneTest}} (if \code{version = "legacy"}); or a
#'   list of such objects containing different variations of Levene's test
#'   (if \code{version = "modern"}).}
#'   \item{\code{test}}{a data frame containing the ANOVA table.}
#'   \item{\code{variable}}{a character string containing the name of the
#'   numeric variable of interest.}
#'   \item{\code{group}}{a character vector containing the name(s) of the
#'   grouping variable(s).}
#'   \item{\code{i}}{an integer giving the number of groups in the (first)
#'   grouping variable.}
#'   \item{\code{j}}{an integer giving the number of groups in the second
#'   grouping variable (only two-way ANOVA).}
#'   \item{\code{conf.level}}{numeric; the confidence level used.}
#'   \item{\code{type}}{a character string giving the type of ANOVA performed
#'   (\code{"one-way"} or \code{"two-way"}).}
#' }
#'
#' The \code{toSPSS} method returns an object of class \code{"SPSSTable"}
#' which contains all relevant information in the required format to produce
#' the LaTeX table.  See \code{\link[=toLatex.SPSSTable]{toLatex}} for possible
#' components and how to further customize the LaTeX table based on the
#' returned object.
#'
#' The \code{print} method produces a LaTeX table that mimics the look of SPSS
#' output.
#'
#' The \code{plot} method does not return anything, but produces a profile plot
#' of the ANOVA results.
#'
#' @note
#' The test statistic and p-value for Levene's test based on the trimmed mean
#' (only returned for \code{version = "modern"}) differ slightly from those
#' returned by SPSS.  Function \code{\link{trimmedMean}} rounds the number of
#' observations to be trimmed in a different manner than the base \R function
#' \code{\link{mean}}, which brings the results closer to those of SPSS, but
#' they are still not identical.
#'
#' LaTeX tables that mimic recent versions of SPSS (\code{version = "modern"})
#' may require several LaTeX compilations to be displayed correctly.
#'
#' @author Andreas Alfons
#'
#' @examples
#' # load data
#' data("Eredivisie")
#' # log-transform market values
#' Eredivisie$logMarketValue <- log(Eredivisie$MarketValue)
#'
#' # one-way ANOVA
#' oneway <- ANOVA(Eredivisie, "logMarketValue",
#'                 group = "Position")
#' oneway        # print LaTeX table
#' plot(oneway)  # create profile plot
#'
#' # two-way ANOVA
#' twoway <- ANOVA(Eredivisie, "logMarketValue",
#'                 group = c("Position", "Foreign"))
#' twoway        # print LaTeX table
#' plot(twoway)  # create profile plot
#'
#' @keywords htest
#'
#' @importFrom stats anova aov as.formula lm median qt
#' @importFrom car Anova leveneTest
#' @export

## FIXME: test statistic and p-value for Levene's test based on the trimmed
##        mean differ slightly from SPSS, even though SPSS documentation
##        states that 5% of values from each end of the distribution, see
##        https://www.ibm.com/docs/en/spss-statistics/version-missing?topic=variance-testing-homogeneity-variances
##
## Maybe the way 5% of observations is rounded differs in R and SPSS?  Write
## a function for the trimmed mean and check with different ways of rounding.

ANOVA <- function(data, variable, group, conf.level = 0.95) {
  ## initializations
  data <- as.data.frame(data)
  variable <- as.character(variable)
  group <- as.character(group)
  if (length(variable) == 0) stop("a variable to test must be specified")
  if (length(group) == 0) stop("a grouping variable must be specified")
  ## select type of ANOVA
  x <- data[, variable[1]]
  if (length(group) == 1) {
    ## one-way ANOVA
    by <- as.factor(data[, group[1]])
    i <- nlevels(by)
    if (i < 2) {
      stop("one-way ANOVA requires at least two groups")
    }
    ok <- is.finite(x) & !is.na(by)
    x <- x[ok]
    by <- by[ok]
    # compute descriptives
    n <- .tapply(x, by, length)
    if (any(is.na(n))) stop("unused factor levels")
    mean <- .tapply(x, by, mean)
    sd <- .tapply(x, by, sd)
    se <- sd / sqrt(n)
    alpha <- 1 - conf.level
    q <- sapply(n-1, function(df) qt(1 - alpha/2, df=df))
    lower <- mean - q * se
    upper <- mean + q * se
    min <- .tapply(x, by, min)
    max <- .tapply(x, by, max)
    desc <- data.frame(N=n, Mean=mean, "Std. Deviation"=sd, "Std. Error"=se,
                       "Lower Bound"=lower, "Upper Bound"=upper, Minumum=min,
                       Maximum=max, check.names=FALSE)
    # test variances with various versions of Levene's test
    leveneMedian <- leveneTest(x, by, center="median")
    # compute adjusted degrees of freedom with Satterthwaite approximation
    medians <- tapply(x, by, median)  # group medians
    z <- abs(x - medians[by])         # absolute residuals
    u <- tapply(z, by, function(z_i) sum((z_i - mean(z_i))^2))  # sum of squares
    nu <- n[seq_len(i)] - 1           # per-group degrees of freedom
    df <- sum(u)^2 / sum(u^2 / nu)    # adjusted degrees of freedom
    # compute Levene's test with adjusted degrees of freedom
    leveneAdj <- leveneMedian
    leveneAdj[2, "Df"] <- df
    leveneAdj[1, "Pr(>F)"] <- pf(leveneAdj[1, "F value"],
                                 df1 = leveneAdj[1, "Df"],
                                 df2 = df, lower.tail = FALSE)
    levene <- list(mean = leveneTest(x, by, center="mean"),
                   median = leveneMedian, adjusted = leveneAdj,
                   # trimmed = leveneTest(x, by, center="mean", trim=0.05),
                   trimmed = leveneTest(x, by, center="trimmedMean", trim=0.05))
    # perform ANOVA
    formula <- as.formula(paste(variable[1], "~", group[1]))
    data <- data.frame(x, by)
    names(data) <- c(variable[1], group[1])
    fit <- aov(formula, data=data)
    test <- summary(fit)[[1]][, c(2, 1, 3:5)]
    row.names(test) <- c("Between Groups", "Within Groups")
    test$Df <- as.integer(test$Df)
    # construct object
    out <- list(descriptives=desc, levene=levene, test=as.data.frame(test),
                variable=variable[1], group=group[1], i=i,
                conf.level=conf.level, type="one-way")
  } else {
    ## two-way ANVOA
    first <- as.factor(data[, group[1]])
    second <- as.factor(data[, group[2]])
    i <- nlevels(first)
    j <- nlevels(second)
    if (i < 2 || j < 2) {
      stop("two-way ANOVA requires at least two groups in each factor")
    }
    ok <- is.finite(x) & !is.na(first) & !is.na(second)
    x <- x[ok]
    first <- first[ok]
    second <- second[ok]
    # compute descriptives
    f <- list(first, second)
    n <- .by(x, f, length)
    if (any(is.na(n))) stop("unused factor levels")
    mean <- .by(x, f, mean)
    sd <- .by(x, f, sd)
    lFirst <- c(levels(first), "Total")
    lSecond <- c(levels(second), "Total")
    info <- data.frame(factor(rep(lFirst, each=j+1), levels=lFirst),
                       factor(rep.int(lSecond, i+1), levels=lSecond))
    names(info) <- group[1:2]
    desc <- data.frame(info, Mean=mean, "Std. Deviation"=sd, N=n,
                       check.names=FALSE, row.names=NULL)
    # # test variances
    # levene <- leveneTest(x, interaction(first, second), center="mean")
    # test variances with various versions of Levene's test
    by <- interaction(first, second)
    leveneMedian <- leveneTest(x, by, center="median")
    # compute adjusted degrees of freedom with Satterthwaite approximation
    medians <- tapply(x, by, median)  # group medians
    z <- abs(x - medians[by])         # absolute residuals
    u <- tapply(z, by, function(z_i) sum((z_i - mean(z_i))^2))  # sum of squares
    nu <- tapply(x, by, length) - 1   # per-group degrees of freedom
    df <- sum(u)^2 / sum(u^2 / nu)    # adjusted degrees of freedom
    # compute Levene's test with adjusted degrees of freedom
    leveneAdj <- leveneMedian
    leveneAdj[2, "Df"] <- df
    leveneAdj[1, "Pr(>F)"] <- pf(leveneAdj[1, "F value"],
                                 df1 = leveneAdj[1, "Df"],
                                 df2 = df, lower.tail = FALSE)
    levene <- list(mean = leveneTest(x, by, center="mean"),
                   median = leveneMedian, adjusted = leveneAdj,
                   # trimmed = leveneTest(x, by, center="mean", trim=0.05),
                   trimmed = leveneTest(x, by, center="trimmedMean", trim=0.05))
    # perform ANOVA
    opts <- options(contrasts=c("contr.sum", "contr.poly"))
    on.exit(options(opts))
    formula <- as.formula(paste(variable[1], "~", group[1], "*", group[2]))
    data <- data.frame(x, first, second)
    names(data) <- c(variable[1], group[1:2])
    fit <- aov(formula, data=data)
    test <- Anova(fit, type="III")
    row.names(test) <- c("Intercept", group[1:2],
                         paste0(group[1:2], collapse=" * "),
                         "Error")
    formula1 <- as.formula(paste(variable[1], "~ 1"))
    fit1 <- lm(formula1, data=data)
    model <- anova(fit1, fit)[2, c(4, 3, 5:6)]
    dimnames(model) <- list("Corrected Model", names(test))
    test <- rbind(model, test)
    test <- cbind(test[, c("Sum Sq", "Df")], "Mean Sq"=test[, "Sum Sq"]/test$Df,
                  test[, c("F value", "Pr(>F)")])
    corrected <- colSums(test[c("Corrected Model", "Error"),])
    corrected["Mean Sq"] <- NA
    total <- colSums(rbind(test["Intercept",], corrected))
    test <- rbind(test, "Total"=total, "Corrected Total"=corrected)
    test$Df <- as.integer(test$Df)
    # construct object
    out <- list(descriptives=desc, levene=levene, test=test,
                variable=variable[1], group=group[1:2], i=i,
                j=j, conf.level=conf.level, type="two-way")
  }
  ## return results
  class(out) <- "ANOVASPSS"
  out
}


#' @rdname ANOVA
#' @export

toSPSS.ANOVASPSS <- function(object,
                             statistics = c("test", "variance", "descriptives"),
                             version = r2spssOptions$get("version"),
                             digits = 3, ...) {

  ## initializations
  statistics <- match.arg(statistics)
  if (object$type == "one-way") sub <- object$variable
  else sub <- paste("Dependent Variable:", object$variable)

  ## put requested results into SPSS format
  if (statistics == "descriptives") {

    if (object$type == "one-way") {
      ## format table nicely
      formatted <- formatSPSS(object$descriptives, digits = digits, ...)
      ## construct header layout
      cn <- c("", names(object$descriptives))
      nc <- length(cn)
      # merged header for confidence interval
      lower <- grep("Lower", cn)
      upper <- grep("Upper", cn)
      ciText <- paste0(format(100*object$conf.level, digits = digits),
                       "\\% Confidence\nInterval for Mean")
      # top-level header
      top <- data.frame(first = c(seq_len(lower-1), lower,
                                  seq(from = upper+1, to = nc)),
                        last = c(seq_len(lower-1), upper,
                                 seq(from = upper+1, to = nc)),
                        text = c(rep.int("", lower-1), ciText,
                                 rep.int("", nc-upper)))
      # bottom-level header
      bottom <- wrapText(cn, limit = 8)
      # construct header
      header <- list(top, bottom)
      ## construct list containing all necessary information
      spss <- list(table = formatted, main = "Descriptives", sub = sub,
                   header = header, rowNames = TRUE, info = 0)
    } else if (object$type == "two-way") {
      # extract relevant information
      descriptives <- object$descriptives
      first <- object$group[1]
      nLevels <- c(nlevels(descriptives[, first]),
                   nlevels(descriptives[, object$group[2]]))
      # print values of first grouping variable only on first occurrence
      descriptives[, first] <- ifelse(duplicated(descriptives[, first]),
                                      "", as.character(descriptives[, first]))
      # format table nicely
      formatted <- formatSPSS(descriptives, digits = digits, ...)
      # define header with line breaks
      header <- wrapText(names(descriptives), limit = 12)
      # define positions for major grid lines
      major <- seq(from = nLevels[2], by = nLevels[2],
                   length.out = nLevels[1] - 1)
      # construct list containing all necessary information
      spss <- list(table = formatted, main = "Descriptive Statistics",
                   sub = sub, header = header, rowNames = FALSE, info = 2,
                   major = major)
    } else stop("type of ANOVA not supported")

  } else {

    ## initializations
    version <- match.arg(version, choices = getVersionValues())
    legacy <- version == "legacy"

    if (statistics == "variance") {

      ## create table with Levene test results
      if (legacy) {
        # put Levene test results into SPSS format
        levene <- object$levene$mean
        levene <- data.frame("Levene Statistic" = levene[1, "F value"],
                             "df1" = as.integer(levene$Df[1]),
                             "df2" = levene$Df[2],
                             "Sig." = levene[1, "Pr(>F)"],
                             row.names = NULL, check.names = FALSE)
        # format table nicely
        formatted <- formatSPSS(levene, digits = digits, ...)
      } else {
        # put Levene test results into SPSS format
        levene <- lapply(object$levene, function(levene) {
          data.frame("Levene Statistic" = levene[1, "F value"],
                     "df1" = as.integer(levene$Df[1]),
                     "df2" = levene$Df[2],
                     "Sig." = levene[1, "Pr(>F)"],
                     row.names = NULL, check.names = FALSE)
        })
        levene <- do.call(rbind, levene)
        # format table nicely
        args <- list(levene, digits = digits, ...)
        if (is.null(args$pValue)) args$pValue <- names(levene) == "Sig."
        if (is.null(args$checkInt)) args$checkInt <- names(levene) == "df2"
        formatted <- do.call(formatSPSS, args)
        # define header with line breaks
        header <- c("", "", wrapText(names(levene), limit = 8))
        # define nice labels for the rows
        rowLabels <- c(mean = "Mean", median = "Median",
                       adjusted = "Median and\nwith adjusted df",
                       trimmed = "trimmed\nmean")
        rowLabels <- paste("Based on", rowLabels[row.names(levene)])
        # # define column widths
        # width <- c("", "0.3\\linewidth", rep.int("", ncol(levene)))
      }
      ## construct list containing all necessary information
      if (object$type == "one-way") {
        if (legacy) {
          spss <- list(table = formatted,
                       main = "Test of Homogeneity of Variances",
                       header = TRUE, rowNames = FALSE, info = 0,
                       version = "legacy")
        } else {
          spss <- list(table = formatted,
                       main = "Tests of Homogeneity of Variances",
                       header = header, label = object$variable,
                       rowNames = rowLabels, info = 0, #width = width,
                       version = "modern")
        }
      } else if (object$type == "two-way") {
        # define text in footnotes
        footnotes <- c(paste("Tests the null hypothesis that the error",
                             "variance of the dependent variable is equal",
                             "across groups."),
                       if (!legacy) paste("Dependent variable:", object$variable),
                       paste("Design: Intercept +", object$group[1], "+",
                              object$group[2], "+", object$group[1], "*",
                              object$group[2]))
        # construct list containing information
        if (legacy) {
          # define main title
          main <- "Levene's Test of Equality of\nError Variances"
          # define footnotes
          footnotes <- data.frame(marker = c("", "a"), row = c(NA, "main"),
                                  column = rep(NA_integer_, 2),
                                  text = wrapText(footnotes, limit = c(35, 32)))
          # construct list
          spss <- list(table = formatted, main = main, sub = sub,
                       header = TRUE, rowNames = FALSE, info = 0,
                       footnotes = footnotes, version = "legacy")
        } else {
          # define main title
          main <- "Levene's Test of Equality of Error Variances"
          # define footnotes
          footnotes <- data.frame(marker = c("", "a", "b"),
                                  row = c(NA, "main", "main"),
                                  column = rep(NA_integer_, 3),
                                  text = wrapText(footnotes,
                                                  limit = c(75, 72, 72)))
          # construct list
          spss <- list(table = formatted, main = main, header = header,
                       label = object$variable, rowNames = rowLabels, info = 0,
                       # width = width,
                       footnotes = footnotes, version = "modern")
        }
      } else stop("type of ANOVA not supported")

    } else if (statistics == "test") {

      ## put ANOVA table into SPSS format
      if (object$type == "one-way") {
        test <- data.frame("Sum of Squares" = c(object$test[, "Sum Sq"],
                                                sum(object$test[, "Sum Sq"])),
                           "df" = c(object$test$Df, sum(object$test$Df)),
                           "Mean Square" = c(object$test[, "Mean Sq"], NA_real_),
                           "F" = c(object$test[, "F value"], NA_real_),
                           "Sig." = c(object$test[, "Pr(>F)"], NA_real_),
                           row.names = NULL, check.names = FALSE)
        row.names(test) <- c(row.names(object$test), "Total")
      } else if(object$type == "two-way") {
        test <- object$test
        names(test) <- c("Type III Sum of Squares", "df",
                         "Mean Square", "F", "Sig.")
      } else stop("type of ANOVA not supported")
      ## format table nicely
      if (legacy) formatted <- formatSPSS(test, digits = digits, ...)
      else {
        args <- list(test, digits = digits, ...)
        if (is.null(args$pValue)) args$pValue <- names(test) == "Sig."
        formatted <- do.call(formatSPSS, args)
      }
      ## construct list containing all necessary information
      if (object$type == "one-way") {
        spss <- list(table = formatted, main = "ANOVA", sub = sub,
                     header = TRUE, rowNames = TRUE, info = 0,
                     version = version)
      } else if (object$type == "two-way") {
        # define header with line breaks
        header <- c("Source", wrapText(names(test), limit = 12))
        # define footnotes
        RSq <- 1 - object$test["Error", "Sum Sq"] / object$test["Corrected Total", "Sum Sq"]
        AdjRSq <- 1 - object$test["Error", "Mean Sq"] /
          (object$test["Corrected Total", "Sum Sq"] / object$test["Corrected Total", "Df"])
        footnote <- paste0("R Squared = ", formatSPSS(RSq, digits=digits),
                           " (Adjusted R Squared = ",
                           formatSPSS(AdjRSq, digits=digits), ")")
        footnotes <- data.frame(marker = "a", row = 1, column = 1,
                                text = footnote)
        # construct list
        spss <- list(table = formatted,
                     main = "Tests of Between-Subject Effects", sub = sub,
                     header = header, rowNames = TRUE, info = 0,
                     footnotes = footnotes, version = version)
      }

    } else stop ("type of 'statistics' not supported")  # shouldn't happen

  }

  # add class and return object
  class(spss) <- "SPSSTable"
  spss

}


#' @rdname ANOVA
#' @export

print.ANOVASPSS <- function(x,
                            statistics = c("descriptives", "variance", "test"),
                            version = r2spssOptions$get("version"), ...) {

  ## initializations
  count <- 0
  statistics <- match.arg(statistics, several.ok = TRUE)
  version <- match.arg(version, choices = getVersionValues())

  ## print LaTeX table for descriptives
  if ("descriptives" %in% statistics) {
    cat("\n")
    # put frequencies into SPSS format
    spss <- toSPSS(x, statistics = "descriptives", version = version, ...)
    # print LaTeX table
    toLatex(spss, version = version)
    cat("\n")
    count <- count + 1
  }

  ## print LaTeX table for Levene test
  if ("variance" %in% statistics) {
    if (count == 0) cat("\n")
    else cat("\\medskip\n")
    # put frequencies into SPSS format
    spss <- toSPSS(x, statistics = "variance", version = version, ...)
    # print LaTeX table
    toLatex(spss, version = version)
    cat("\n")
  }

  ## print LaTeX table for ANOVA
  if ("test" %in% statistics) {
    if (count == 0) cat("\n")
    else cat("\\medskip\n")
    # put frequencies into SPSS format
    spss <- toSPSS(x, statistics = "test", version = version, ...)
    # print LaTeX table
    toLatex(spss, version = version)
    cat("\n")
  }

}


#' @rdname ANOVA
#'
#' @param y  ignored (only included because it is defined for the generic
#' function \code{\link[graphics]{plot}}).
#' @param which  for two-way ANOVA, an integer with possible values \code{1} or
#' \code{2} indicating whether the first or the second factor should be used on
#' the \eqn{x}-axis.  The other factor will then be used for drawing separate
#' lines.  For one-way ANOVA, this is not meaningful and ignored.
#' @param type  a character string specifying the type of lines.  Possible
#' values are \code{"o"} (the default) for overplotted points and lines, and
#' \code{"l"} for lines only.
#' @param main,xlab,ylab  the plot title and axis labels.
#'
#' @export

plot.ANOVASPSS <- function(x, y, which = 1, type = "o", main = NULL,
                           xlab = NULL, ylab = NULL, ...) {
  if (x$type == "one-way") {
    if (is.null(xlab)) xlab <- x$group
    if (is.null(ylab)) ylab <- paste("Mean of", x$variable)
    desc <- x$descriptives
    n <- nrow(x$descriptives)
    labs <- row.names(x$descriptives)[-n]
    means <- x$descriptives$Mean[-n]
    .lines(labs, means, type=type, main=main, xlab=xlab, ylab=ylab, ...)
  } else if (x$type == "two-way") {
    if (length(which) != 1 || !which %in% 1:2) which <- formals()$which
    axis <- x$group[which]
    lines <- x$group[-which]
    if (is.null(main)) main <- paste("Estimated Marginal Means of", x$variable)
    if (is.null(xlab)) xlab <- axis
    if (is.null(ylab)) ylab <- "Estimated Marginal Means"
    desc <- x$descriptives
    keep <- desc[, x$group[1]] != "Total" & desc[, x$group[2]] != "Total"
    labs <- setdiff(levels(desc[, axis]), "Total")
    means <- do.call(cbind, split(desc[keep, "Mean"], desc[keep, lines]))
    .matlines(labs, means, type=type, main=main, xlab=xlab, ylab=ylab,
              title=lines, ...)
  } else stop("type of ANOVA not supported")
}


# apply a function on each group as well as all observations (one factor)
.tapply <- function(X, INDEX, FUN) c(tapply(X, INDEX, FUN), Total=FUN(X))

# divide data into groups as well as all observations (one factor)
.split <- function(x, f) c(split(x, f, drop=FALSE), list(Total=x))

# apply a function on each group as well as all observations (two factors)
.by <- function(X, INDICES, FUN) {
  s <- .split(seq_along(X), INDICES[[1]])
  unlist(lapply(s, function(i) .tapply(X[i], INDICES[[2]][i], FUN)))
}

# recode a factor such that the last level becomes the first
recodeSPSS <- function(x) {
  x <- as.factor(x)
  l <- levels(x)
  n <- length(l)
  factor(as.character(x), levels=c(l[n], l[-n]))
}
