# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' One-way and Two-way ANOVA
#'
#' Perform one-way or two-way ANOVA on variables of a data set.  The output is
#' printed as a LaTeX table that mimics the look of SPSS output (version <24).
#'
#' @param data  a data frame containing the variables.
#' @param variable  a character string specifying the numeric variable of
#' interest.
#' @param group  a character vector specifying one or two grouping variables.
#' @param conf.level  a number between 0 and 1 giving the confidence level of
#' the confidence interval.
#'
#' @return  An object of class \code{ANOVA}.  The \code{print} method produces
#' a LaTeX table that mimics the look of SPSS output (version <24).
#'
#' @author Andreas Alfons
#'
#' @keywords htest
#'
#' @importFrom stats anova aov as.formula lm qt
#' @importFrom car Anova leveneTest
#' @export

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
    # test variances
    levene <- leveneTest(x, by, center="mean")
    # perform ANOVA
    formula <- as.formula(paste(variable[1], "~", group[1]))
    data <- data.frame(x, by)
    names(data) <- c(variable[1], group[1])
    fit <- aov(formula, data=data)
    test <- summary(fit)[[1]][, c(2, 1, 3:5)]
    row.names(test) <- c("Between Groups", "Within Groups")
    test$Df <- as.integer(test$Df)
    # construct object
    out <- list(descriptives=desc, levene=levene, test=test,
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
    # test variances
    levene <- leveneTest(x, interaction(first, second), center="mean")
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
  class(out) <- "ANOVA"
  out
}

#' @rdname ANOVA
#'
#' @param x  an object of class \code{"ANOVA"} as returned by function
#' \code{ANOVA}.
#' @param digits  an integer giving the number of digits after the comma to be
#' printed in the LaTeX tables.
#' @param statistics  a character vector specifying which LaTeX tables should
#' be printed.  Available options are \code{"descriptives"} for descriptive
#' statistics, \code{"variance"} for Levene's test on homogeneity of the
#' variances, and \code{"test"} for ANOVA results.  The default is to print all
#' tables.
#' @param \dots currently ignored.
#'
#' @export

print.ANOVA <- function(x, digits = 3,
                        statistics = c("descriptives", "variance", "test"),
                        ...) {

  ## initializations
  count <- 0
  statistics <- match.arg(statistics, several.ok=TRUE)

  ## print LaTeX table for descriptives
  if ("descriptives" %in% statistics) {
    cat("\n")
    formatted <- formatSPSS(x$descriptives, digits=digits)
    # print LaTeX table
    if (x$type == "one-way") {
      # initialize LaTeX table
      cat("\\begin{tabular}{|l|r|r|r|r|r|r|r|r|}\n")
      # print table header
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{9}{c}{\\textbf{Descriptives}} \\\\\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{9}{l}{", x$variable, "} \\\\\n", sep="")
      cat("\\hline\n")
      cat(" & & & & & \\multicolumn{2}{|c|}{", format(100*x$conf.level, digits=digits), "\\% Confidence} & & \\\\\n", sep="")
      cat(" & & & & & \\multicolumn{2}{|c|}{Interval for Mean} & & \\\\\n")
      cat("\\cline{6-7}\n")
      cat(" & & & \\multicolumn{1}{|c|}{Std.} & \\multicolumn{1}{|c|}{Std.} & \\multicolumn{1}{|c|}{Lower} & \\multicolumn{1}{|c|}{Upper} & & \\\\\n")
      cat(" & \\multicolumn{1}{|c|}{N} & \\multicolumn{1}{|c|}{Mean} & \\multicolumn{1}{|c|}{Deviation} & \\multicolumn{1}{|c|}{Error} & \\multicolumn{1}{|c|}{Bound} & \\multicolumn{1}{|c|}{Bound} & \\multicolumn{1}{|c|}{Minimum} & \\multicolumn{1}{|c|}{Maximum} \\\\\n")
      cat("\\hline\n")
      # print table
      for (rn in rownames(formatted)) {
        cat(rn, "&", paste0(formatted[rn, ], collapse=" & "), "\\\\\n")
      }
      cat("\\hline\n")
    } else if (x$type == "two-way") {
      formatted[duplicated(formatted[, 1]), 1] <- ""
      # initialize LaTeX table
      cat("\\begin{tabular}{|ll|r|r|r|}\n")
      # print table header
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{5}{c}{\\textbf{Descriptives}} \\\\\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{5}{l}{Dependent variable: ", x$variable, "} \\\\\n", sep="")
      cat("\\hline\n")
      cat(" & & & \\multicolumn{1}{|c|}{Std.} & \\\\\n")
      cat(x$group[1], "&", x$group[2], "& \\multicolumn{1}{|c|}{Mean} & \\multicolumn{1}{|c|}{Deviation} & \\multicolumn{1}{|c|}{N} \\\\\n")
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
    levene <- data.frame("Levene Statistic"=x$levene[1, "F value"],
                         df1=x$levene$Df[1], df2=x$levene$Df[2],
                         "Sig."=x$levene[1, "Pr(>F)"],
                         check.names=FALSE, row.names=NULL)
    formatted <- formatSPSS(levene, digits=digits)
    # print LaTeX table
    if (x$type == "one-way") {
      # initialize LaTeX table
      cat("\\begin{tabular}{|r|r|r|r|}\n")
      # print table header
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{4}{c}{\\textbf{Test of Homogeneity of Variances}} \\\\\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{4}{l}{", x$variable, "} \\\\\n", sep="")
      cat("\\hline\n")
      cat("\\multicolumn{1}{|c|}{Levene Statistic} & \\multicolumn{1}{|c|}{df1} & \\multicolumn{1}{|c|}{df2} & \\multicolumn{1}{|c|}{Sig.} \\\\\n")
      cat("\\hline\n")
      # print table
      cat(paste0(formatted, collapse=" & "), "\\\\\n")
      cat("\\hline\n")
    } else if (x$type == "two-way") {
      # initialize LaTeX table
      cat("\\begin{tabular}{|r|r|r|r|}\n")
      # print table header
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{4}{c}{\\textbf{Levene's Test of Equality of}} \\\\\n")
      cat("\\multicolumn{4}{c}{\\textbf{Error Variances}$^{\\text{a}}$} \\\\\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{4}{l}{Dependent variable: ", x$variable, "} \\\\\n", sep="")
      cat("\\hline\n")
      cat("\\multicolumn{1}{|c|}{\\,\\,\\,\\, F \\,\\,\\,\\,} & \\multicolumn{1}{|c|}{\\,\\, df1 \\,\\,} & \\multicolumn{1}{|c|}{\\,\\, df2 \\,\\,} & \\multicolumn{1}{|c|}{\\,\\, Sig. \\,\\,} \\\\\n")
      cat("\\hline\n")
      # print table
      cat(paste0(formatted, collapse=" & "), "\\\\\n")
      cat("\\hline\n")
      cat("\\multicolumn{4}{l}{Tests the null hypothesis that the} \\\\\n")
      cat("\\multicolumn{4}{l}{error variance of the dependent} \\\\\n")
      cat("\\multicolumn{4}{l}{variable is equal across groups.} \\\\\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{4}{l}{a. Design: Intercept + ", x$group[1], " +} \\\\\n", sep="")
      cat("\\multicolumn{4}{l}{\\phantom{a. }", x$group[2], " + ", x$group[1], " * ", x$group[2], "} \\\\\n", sep="")
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
    formatted <- formatSPSS(x$test, digits=digits)
    # print LaTeX table
    if (x$type == "one-way") {
      cat("\\begin{tabular}{|l|r|r|r|r|r|}\n")
      # print table header
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{6}{c}{\\textbf{ANOVA}} \\\\\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{6}{l}{", x$variable, "} \\\\\n", sep="")
      cat("\\hline\n")
      cat(" & \\multicolumn{1}{|c|}{Sum of Squares} & \\multicolumn{1}{|c|}{df} & \\multicolumn{1}{|c|}{Mean Square} & \\multicolumn{1}{|c|}{F} & \\multicolumn{1}{|c|}{Sig.} \\\\\n")
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
      cat("\\begin{tabular}{|l|r|r|r|r|r|}\n")
      # print table header
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{6}{c}{\\textbf{Tests of Between-Subject Effects}} \\\\\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{6}{l}{Dependent Variable: ", x$variable, "} \\\\\n", sep="")
      cat("\\hline\n")
      cat(" & \\multicolumn{1}{|c|}{Type III Sum} & & & & \\\\\n")
      cat("Source & \\multicolumn{1}{|c|}{of Squares} & \\multicolumn{1}{|c|}{df} & \\multicolumn{1}{|c|}{Mean Square} & \\multicolumn{1}{|c|}{F} & \\multicolumn{1}{|c|}{Sig.} \\\\\n")
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

#' @export
plot.ANOVA <- function(x, y, which = 1, type = "o", main = NULL,
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
