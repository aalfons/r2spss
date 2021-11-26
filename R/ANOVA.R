# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' One-way and Two-way ANOVA
#'
#' Perform one-way or two-way ANOVA on variables of a data set.  The output is
#' printed as a LaTeX table that mimics the look of SPSS output (version <24),
#' and a plot of the results mimics the look of SPSS graphs.
#'
#' @param data  a data frame containing the variables.
#' @param variable  a character string specifying the numeric variable of
#' interest.
#' @param group  a character vector specifying one or two grouping variables.
#' @param conf.level  a number between 0 and 1 giving the confidence level of
#' the confidence interval.
#' @param \dots  For the \code{plot} method, additional arguments to be passed
#' down, in particular graphical parameters (see also \code{\link{linesSPSS}}).
#' For the \code{print} method, additional arguments are currently ignored.
#'
#' @return
#' An object of class \code{"ANOVASPSS"} with the following components:
#' \describe{
#'   \item{\code{descriptives}}{a data frame containing per-group descriptive
#'   statistics.}
#'   \item{\code{levene}}{an object as returned by
#'   \code{\link[car]{leveneTest}}.}
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
#' The \code{print} method produces a LaTeX table that mimics the look of SPSS
#' output (version <24).
#'
#' The \code{plot} method does not return anything, but produces a profile plot
#' of the ANOVA results.
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
#' @importFrom stats anova aov as.formula lm qt
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
#'
#' @param x  an object of class \code{"ANOVASPSS"} as returned by function
#' \code{ANOVA}.
#' @param digits  an integer giving the number of digits after the comma to be
#' printed in the LaTeX tables.
#' @param statistics  a character vector specifying which LaTeX tables should
#' be printed.  Available options are \code{"descriptives"} for descriptive
#' statistics, \code{"variance"} for Levene's test on homogeneity of the
#' variances, and \code{"test"} for ANOVA results.  The default is to print all
#' tables.
#'
#' @export

print.ANOVASPSS <- function(x, digits = 3,
                            statistics = c("descriptives", "variance", "test"),
                            theme = c("modern", "legacy"), ...) {

  ## initializations
  count <- 0
  statistics <- match.arg(statistics, several.ok=TRUE)
  theme <- match.arg(theme)
  legacy <- theme == "legacy"

  ## print LaTeX table for descriptives
  if ("descriptives" %in% statistics) {
    cat("\n")
    formatted <- formatSPSS(x$descriptives, digits=digits)
    # print LaTeX table
    if (x$type == "one-way") {
      # initialize LaTeX table
      if (legacy) cat("\\begin{tabular}{|l|r|r|r|r|r|r|r|r|}\n")
      else {
        cat(latexTabular(info = 1, results = 8))
        cat("\n")
      }
      # print table header
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{9}{c}{\\textbf{Descriptives}} \\\\\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{9}{l}{", x$variable, "} \\\\\n", sep="")
      if (legacy) {
        cat("\\hline\n")
        cat(" & & & & & \\multicolumn{2}{c|}{", format(100*x$conf.level, digits=digits), "\\% Confidence} & & \\\\\n", sep="")
        cat(" & & & & & \\multicolumn{2}{c|}{Interval for Mean} & & \\\\\n")
        cat("\\cline{6-7}\n")
        cat(" & & & \\multicolumn{1}{c|}{Std.} & \\multicolumn{1}{c|}{Std.} & \\multicolumn{1}{c|}{Lower} & \\multicolumn{1}{c|}{Upper} & & \\\\\n")
        cat(" & \\multicolumn{1}{c|}{N} & \\multicolumn{1}{c|}{Mean} & \\multicolumn{1}{c|}{Deviation} & \\multicolumn{1}{c|}{Error} & \\multicolumn{1}{c|}{Bound} & \\multicolumn{1}{c|}{Bound} & \\multicolumn{1}{c|}{Minimum} & \\multicolumn{1}{c|}{Maximum} \\\\\n")
      } else {
        cat(latexMulticolumn("", 1, "l"), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn(paste0(format(100*x$conf.level, digits=digits), "\\% Confidence"), 2, right = TRUE), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("", 1), "\\\\\n")
        cat(latexMulticolumn("", 1, "l"), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("Interval for Mean", 2, right = TRUE), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("", 1), "\\\\\n")
        cat(latexMulticolumn("", 1, "l"), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("Std.", 1, right = TRUE), "&",
            latexMulticolumn("Std.", 1, right = TRUE), "&",
            latexMulticolumn("Lower", 1, right = TRUE), "&",
            latexMulticolumn("Upper", 1, right = TRUE), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("", 1), "\\\\\n")
        cat(latexMulticolumn("", 1, "l"), "&",
            latexMulticolumn("N", 1, right = TRUE), "&",
            latexMulticolumn("Mean", 1, right = TRUE), "&",
            latexMulticolumn("Deviation", 1, right = TRUE), "&",
            latexMulticolumn("Error", 1, right = TRUE), "&",
            latexMulticolumn("Bound", 1, right = TRUE), "&",
            latexMulticolumn("Bound", 1, right = TRUE), "&",
            latexMulticolumn("Minimum", 1, right = TRUE), "&",
            latexMulticolumn("Maximum", 1), "\\\\\n")
      }
      cat("\\hline\n")
      # print table
      for (rn in rownames(formatted)) {
        cat(rn, "&", paste0(formatted[rn, ], collapse=" & "), "\\\\\n")
      }
      cat("\\hline\n")
    } else if (x$type == "two-way") {
      formatted[duplicated(formatted[, 1]), 1] <- ""
      # initialize LaTeX table
      if (legacy) cat("\\begin{tabular}{|ll|r|r|r|}\n")
      else {
        cat(latexTabular(info = 2, results = 3))
        cat("\n")
      }
      # print table header
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{5}{c}{\\textbf{Descriptive Statistics}} \\\\\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{5}{l}{Dependent variable: ", x$variable, "} \\\\\n", sep="")
      if (legacy) {
        cat("\\hline\n")
        cat(" & & & \\multicolumn{1}{c|}{Std.} & \\\\\n")
        cat(x$group[1], "&", x$group[2], "& \\multicolumn{1}{c|}{Mean} & \\multicolumn{1}{c|}{Deviation} & \\multicolumn{1}{c|}{N} \\\\\n")
      } else {
        cat(latexMulticolumn("", 1, "l"), "&",
            latexMulticolumn("", 1, "l"), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("Std.", 1, right = TRUE), "&",
            latexMulticolumn("", 1), "\\\\\n")
        cat(latexMulticolumn(x$group[1], 1, "l"), "&",
            latexMulticolumn(x$group[2], 1, "l"), "&",
            latexMulticolumn("Mean", 1, right = TRUE), "&",
            latexMulticolumn("Deviation", 1, right = TRUE), "&",
            latexMulticolumn("N", 1), "\\\\\n")
      }
      cat("\\hline\n")
      # print table
      for (i in seq_len(nrow(formatted))) {
        cat(paste0(formatted[i, ], collapse=" & "), "\\\\\n")
        if (i %% (x$j+1) == 0) cat("\\hline\n")
      }
    } else stop("type of ANOVA not supported")
    # finalize LaTeX table
    cat("\\noalign{\\smallskip}\n")
    cat("\\end{tabular}\n")
    cat("\n")
    count <- count + 1
  }

  ## print LaTeX table for Levene test
  if ("variance" %in% statistics) {
    if (count == 0) cat("\n")
    else cat("\\medskip\n")
    if (legacy) {
      levene <- x$levene$mean
      levene <- data.frame("Levene Statistic"=levene[1, "F value"],
                           df1=levene$Df[1], df2=levene$Df[2],
                           "Sig."=levene[1, "Pr(>F)"],
                           check.names=FALSE, row.names=NULL)
      formatted <- formatSPSS(levene, digits=digits)
    } else {
      formatted <- lapply(x$levene, function(levene) {
        levene <- data.frame("Levene Statistic"=levene[1, "F value"],
                   df1=as.integer(levene$Df[1]), df2=levene$Df[2],
                   "Sig."=levene[1, "Pr(>F)"],
                   check.names=FALSE, row.names=NULL)
        formatSPSS(levene, digits=digits)
      })
      labels <- c(mean = "Mean", median = "Median",
                  adjusted = "Median and with adjusted df",
                  trimmed = "trimmed mean")
    }
    # print LaTeX table
    if (x$type == "one-way") {
      # initialize LaTeX table
      if (legacy) cat("\\begin{tabular}{|r|r|r|r|}\n")
      else {
        align <- c("l", "L{0.3\\linewidth}", "r", "r", "r", "r")
        cat(latexTabular(info = 2, results = 4, alignment = align))
        cat("\n")
      }
      # print table header
      cat("\\noalign{\\smallskip}\n")
      if (legacy) cat("\\multicolumn{4}{c}{\\textbf{Test of Homogeneity of Variances}} \\\\\n")
      else cat("\\multicolumn{6}{c}{\\textbf{Tests of Homogeneity of Variances}} \\\\\n")
      cat("\\noalign{\\smallskip}\n")
      if (legacy) {
        cat("\\multicolumn{4}{l}{", x$variable, "} \\\\\n", sep="")
        cat("\\hline\n")
        cat("\\multicolumn{1}{|c|}{Levene Statistic} & \\multicolumn{1}{c|}{df1} & \\multicolumn{1}{c|}{df2} & \\multicolumn{1}{c|}{Sig.} \\\\\n")
      } else {
        cat(latexMulticolumn("", 1, "l"), "&",
            latexMulticolumn("", 1, "l"), "&",
            latexMulticolumn("Levene", 1, right = TRUE), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("", 1), "\\\\\n")
        cat(latexMulticolumn("", 1, "l"), "&",
            latexMulticolumn("", 1, "l"), "&",
            latexMulticolumn("Statistic", 1, right = TRUE), "&",
            latexMulticolumn("df1", 1, right = TRUE), "&",
            latexMulticolumn("df2", 1, right = TRUE), "&",
            latexMulticolumn("Sig.", 1), "\\\\\n")
      }
      cat("\\hline\n")
      # print table
      if (legacy) cat(paste(formatted, collapse=" & "), "\\\\\n")
      else {
        for (method in names(formatted)) {
          # print current version of Levene's test
          cat(if (method == "mean") x$variable else "",
              "& Based on", labels[method], "&",
              paste(formatted[[method]], collapse=" & "), "\\\\\n")
        }
      }
      cat("\\hline\n")
    } else if (x$type == "two-way") {
      # initialize LaTeX table
      if (legacy) cat("\\begin{tabular}{|r|r|r|r|}\n")
      else {
        align <- c("l", "L{0.3\\linewidth}", "r", "r", "r", "r")
        cat(latexTabular(info = 2, results = 4, alignment = align))
        cat("\n")
      }
      # print table header
      cat("\\noalign{\\smallskip}\n")
      if (legacy) {
        cat("\\multicolumn{4}{c}{\\textbf{Levene's Test of Equality of}} \\\\\n")
        cat("\\multicolumn{4}{c}{\\textbf{Error Variances}$^{\\text{a}}$} \\\\\n")
      } else {
        cat("\\multicolumn{6}{c}{\\textbf{Levene's Test of Equality of Error Variances}$^{\\text{a,b}}$} \\\\\n")
      }
      cat("\\noalign{\\smallskip}\n")
      if (legacy) {
        cat("\\multicolumn{4}{l}{Dependent variable: ", x$variable, "} \\\\\n", sep="")
        cat("\\hline\n")
        cat("\\multicolumn{1}{|c|}{\\,\\,\\,\\, F \\,\\,\\,\\,} & \\multicolumn{1}{c|}{\\,\\, df1 \\,\\,} & \\multicolumn{1}{c|}{\\,\\, df2 \\,\\,} & \\multicolumn{1}{c|}{\\,\\, Sig. \\,\\,} \\\\\n")
      } else {
        cat(latexMulticolumn("", 1, "l"), "&",
            latexMulticolumn("", 1, "l"), "&",
            latexMulticolumn("Levene", 1, right = TRUE), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("", 1), "\\\\\n")
        cat(latexMulticolumn("", 1, "l"), "&",
            latexMulticolumn("", 1, "l"), "&",
            latexMulticolumn("Statistic", 1, right = TRUE), "&",
            latexMulticolumn("df1", 1, right = TRUE), "&",
            latexMulticolumn("df2", 1, right = TRUE), "&",
            latexMulticolumn("Sig.", 1), "\\\\\n")
      }
      cat("\\hline\n")
      # print table
      if (legacy) cat(paste0(formatted, collapse=" & "), "\\\\\n")
      else {
        for (method in names(formatted)) {
          # print current version of Levene's test
          cat(if (method == "mean") x$variable else "",
              "& Based on", labels[method], "&",
              paste(formatted[[method]], collapse=" & "), "\\\\\n")
        }
      }
      cat("\\hline\n")
      if (legacy) {
        cat("\\multicolumn{4}{l}{Tests the null hypothesis that the} \\\\\n")
        cat("\\multicolumn{4}{l}{error variance of the dependent} \\\\\n")
        cat("\\multicolumn{4}{l}{variable is equal across groups.} \\\\\n")
      } else {
        cat("\\multicolumn{6}{l}{Tests the null hypothesis that the error variance of the dependent} \\\\\n")
        cat("\\multicolumn{6}{l}{variable is equal across groups.} \\\\\n")
      }
      cat("\\noalign{\\smallskip}\n")
      if (legacy) {
        cat("\\multicolumn{4}{l}{a. Design: Intercept + ", x$group[1],
            " +} \\\\\n", sep="")
        cat("\\multicolumn{4}{l}{\\phantom{a. }", x$group[2], " + ",
            x$group[1], " * ", x$group[2], "} \\\\\n", sep="")
      } else {
        cat("\\multicolumn{6}{l}{a. Dependent variable: ", x$variable,
            "} \\\\\n", sep="")
        cat("\\multicolumn{6}{l}{b. Design: Intercept + ", x$group[1],
            " + ", x$group[2], " + ", x$group[1], " * ", x$group[2],
            "} \\\\\n", sep="")
      }
    } else stop("type of ANOVA not supported")
    # finalize LaTeX table
    cat("\\noalign{\\smallskip}\n")
    cat("\\end{tabular}\n")
    cat("\n")
  }

  ## print LaTeX table for ANOVA table
  if ("test" %in% statistics) {
    if (count == 0) cat("\n")
    else cat("\\medskip\n")
    if (legacy) formatted <- formatSPSS(x$test, digits=digits, pValue=FALSE)
    else formatted <- formatSPSS(x$test, digits=digits)
    # print LaTeX table
    if (x$type == "one-way") {
      if (legacy) cat("\\begin{tabular}{|l|r|r|r|r|r|}\n")
      else {
        cat(latexTabular(info = 1, results = 5))
        cat("\n")
      }
      # print table header
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{6}{c}{\\textbf{ANOVA}} \\\\\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{6}{l}{", x$variable, "} \\\\\n", sep="")
      if (legacy) {
        cat("\\hline\n")
        cat(" & \\multicolumn{1}{|c|}{Sum of Squares} & \\multicolumn{1}{c|}{df} & \\multicolumn{1}{c|}{Mean Square} & \\multicolumn{1}{c|}{F} & \\multicolumn{1}{c|}{Sig.} \\\\\n")
      } else {
        cat(latexMulticolumn("", 1, "l"), "&",
            latexMulticolumn("Sum of Squares", 1, right = TRUE), "&",
            latexMulticolumn("df", 1, right = TRUE), "&",
            latexMulticolumn("Mean Square", 1, right = TRUE), "&",
            latexMulticolumn("F", 1, right = TRUE), "&",
            latexMulticolumn("Sig.", 1), "\\\\\n")
      }
      cat("\\hline\n")
      # print table
      for (rn in rownames(formatted)) {
        cat(rn, "&", paste0(formatted[rn, ], collapse=" & "), "\\\\\n")
      }
      cat("Total &", formatSPSS(sum(x$test[, "Sum Sq"]), digits=digits), "&", sum(x$test$Df), "& & & \\\\\n")
      cat("\\hline\n")
    } else if (x$type == "two-way") {
      RSq <- 1 - x$test["Error", "Sum Sq"] / x$test["Corrected Total", "Sum Sq"]
      AdjRSq <- 1 - x$test["Error", "Mean Sq"] /
        (x$test["Corrected Total", "Sum Sq"] / x$test["Corrected Total", "Df"])
      formatted[1, "Sum Sq"] <- paste0(formatted[1, "Sum Sq"], "$^{\\text{a}}$")
      if (legacy) cat("\\begin{tabular}{|l|r|r|r|r|r|}\n")
      else {
        cat(latexTabular(info = 1, results = 5))
        cat("\n")
      }
      # print table header
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{6}{c}{\\textbf{Tests of Between-Subject Effects}} \\\\\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{6}{l}{Dependent Variable: ", x$variable, "} \\\\\n", sep="")
      if (legacy) {
        cat("\\hline\n")
        cat(" & \\multicolumn{1}{c|}{Type III Sum} & & & & \\\\\n")
        cat("Source & \\multicolumn{1}{c|}{of Squares} & \\multicolumn{1}{c|}{df} & \\multicolumn{1}{c|}{Mean Square} & \\multicolumn{1}{c|}{F} & \\multicolumn{1}{c|}{Sig.} \\\\\n")
      } else {
        cat(latexMulticolumn("", 1, "l"), "&",
            latexMulticolumn("Type III Sum", 1, right = TRUE), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("", 1), "\\\\\n")
        cat(latexMulticolumn("Source", 1, "l"), "&",
            latexMulticolumn("of Squares", 1, right = TRUE), "&",
            latexMulticolumn("df", 1, right = TRUE), "&",
            latexMulticolumn("Mean Square", 1, right = TRUE), "&",
            latexMulticolumn("F", 1, right = TRUE), "&",
            latexMulticolumn("Sig.", 1), "\\\\\n")
      }
      cat("\\hline\n")
      # print table
      for (rn in rownames(formatted)) {
        cat(rn, "&", paste0(formatted[rn, ], collapse=" & "), "\\\\\n")
      }
      cat("\\hline\n")
      cat("\\multicolumn{6}{l}{a. R Squared = ", formatSPSS(RSq, digits=digits), " (Adjusted R Squared = ", formatSPSS(AdjRSq, digits=digits), ")} \\\\\n", sep="")
    } else stop("type of ANOVA not supported")
    # finalize LaTeX table
    cat("\\noalign{\\smallskip}\n")
    cat("\\end{tabular}\n")
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


## Compute the trimmed mean.  This function assumes that there are no
## missing values.  Base R function mean() always rounds down the number
## of observations to trim.  SPSS seems to have a different behavior.  With
## rounding to the nearest integer, results for Levene's test based no the
## trimmed means are closer to SPSS results, but still not identical.
#' @export

trimmedMean <- function(x, trim = 0.05) {
  # number of observations
  n <- length(x)
  # trim observations from both ends
  if (trim > 0 && n > 0) {
    # low <- floor(n * trim) + 1  # default R behavior
    low <- round(n * trim) + 1    #
    high <- n + 1 - low
    if (low <= high) {
      x <- sort.int(x, partial = unique(c(low, high)))[low:high]
    } else median(x, na.rm = FALSE)
  }
  # comupte mean
  mean(x, na.rm = FALSE)
}

