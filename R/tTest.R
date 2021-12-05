# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' t Tests
#'
#' Perform a one-sample t test, a paired-sample t test or an
#' independent-samples t test on variables of a data set.  The output
#' is printed as a LaTeX table that mimics the look of SPSS output
#' (version <24).
#'
#' @param data  a data frame containing the variables.
#' @param variables  a character vector specifying numeric variable(s) to be
#' used for testing the mean(s).  If \code{group} is \code{NULL}, a one-sample
#' t test is performed if only one variable is specified, and a paired-sample
#' t test is performed if two variables are specified.  If a grouping variable
#' is specified in \code{group}, an independent-samples t-test is performed and
#' this should be a character string specifying the numeric variable of
#' interest.
#' @param group a character string specifying a grouping variable for an
#' independent-samples t-test, or \code{NULL}.
#' @param mu  a number indicating the true value of the mean for a one-sample
#' t test.
#' @param conf.level  a number between 0 and 1 giving the confidence level of
#' the confidence interval.
#'
#' @return  An object of class \code{"tTestSPSS"} with the following
#' components:
#' \describe{
#'   \item{\code{statistics}}{a data frame containing the relevant descriptive
#'   statistics.}
#'   \item{\code{test}}{an object of class \code{"htest"} as returned by
#'   \code{\link[stats]{t.test}} (only one-sample and paired-sample tests).}
#'   \item{\code{variables}}{a character vector containing the name(s) of the
#'   relevant numeric variable(s).}
#'   \item{\code{n}}{an integer giving the number of observations (only
#'   paired-sample test).}
#'   \item{\code{levene}}{an object as returned by
#'   \code{\link[car]{leveneTest}} (only independent-samples test).}
#'   \item{\code{pooled}}{an object of class \code{"htest"} as returned
#'   by \code{\link[stats]{t.test}} assuming equal variances (only
#'   independent-samples test).}
#'   \item{\code{satterthwaite}}{an object of class \code{"htest"} as returned
#'   by \code{\link[stats]{t.test}} not assuming equal variance (only
#'   independent-samples test).}
#'   \item{\code{group}}{a character string containing the name of the
#'   grouping variable (only independent-samples test).}
#'   \item{\code{type}}{a character string giving the type of t test performed
#'   (\code{"one-sample"}, \code{"paired"}, or \code{"independent"}).}
#' }
#'
#' The \code{print} method produces a LaTeX table that mimics the look of SPSS
#' output (version <24).
#'
#' @author Andreas Alfons
#'
#' @examples
#' ## one-sample and paired-sample t test
#'
#' # load data
#' data("Exams")
#'
#' # test whether the average grade on the resit
#' # differs from 5.5 (minimum passing grade)
#' tTest(Exams, "Resit", mu = 5.5)
#'
#' # test whether average grades differ between the
#' # regular exam and the resit
#' tTest(Exams, c("Resit", "Regular"))
#'
#'
#' ## independent-samples t test
#'
#' # load data
#' data("Eredivisie")
#' # log-transform market values
#' Eredivisie$logMarketValue <- log(Eredivisie$MarketValue)
#'
#' # test whether average log market values differ between
#' # Dutch and foreign players
#' tTest(Eredivisie, "logMarketValue", group = "Foreign")
#'
#' @keywords htest
#'
#' @importFrom stats sd t.test
#' @importFrom car leveneTest
#' @export

tTest <- function(data, variables, group = NULL, mu = 0, conf.level = 0.95) {
  ## initializations
  data <- as.data.frame(data)
  variables <- as.character(variables)
  group <- as.character(group)
  if (length(variables) == 0) stop("a variable to test must be specified")
  ## select t test
  if (length(group) == 0) {
    if (length(variables) == 1) {
      ## one-sample t test
      x <- data[, variables[1]]
      # compute statistics
      stat <- .stat(x)
      row.names(stat) <- variables[1]
      # perform test
      ok <- is.finite(x)
      x <- x[ok]
      test <- t.test(x, mu=mu, conf.level=conf.level)
      # construct object
      out <- list(statistics=stat, test=test, variables=variables[1],
                  type="one-sample")
    } else {
      ## paired-sample t test
      x <- data[, variables[1]]
      y <- data[, variables[2]]
      # compute statistics
      stat <- rbind(.stat(x), .stat(y))
      row.names(stat) <- variables[1:2]
      # perform test
      ok <- is.finite(x) & is.finite(y)
      x <- x[ok]
      y <- y[ok]
      test <- t.test(x, y, mu=0, paired=TRUE, conf.level=conf.level)
      # construct object
      out <- list(statistics=stat, test=test, variables=variables[1:2],
                  n=length(x), type="paired")
    }
  } else {
    ## independent-samples t test
    variable <- data[, variables[1]]
    by <- as.factor(data[, group[1]])
    if (nlevels(by) != 2) {
      stop("independent-samples t test requires exactly two groups")
    }
    ok <- is.finite(variable) & !is.na(by)
    variable <- variable[ok]
    by <- by[ok]
    # perform tests
    levene <- leveneTest(variable, by, center="mean")
    x <- variable[by == levels(by)[1]]
    y <- variable[by == levels(by)[2]]
    pooled <- t.test(x, y, mu=0, paired=FALSE, var.equal=TRUE,
                     conf.level=conf.level)
    satterthwaite <- t.test(x, y, mu=0, paired=FALSE, var.equal=FALSE,
                            conf.level=conf.level)
    # compute statistics
    stat <- rbind(.stat(x), .stat(y))
    row.names(stat) <- levels(by)
    # construct object
    out <- list(statistics=stat, levene=levene, pooled=pooled,
                satterthwaite=satterthwaite, variables=variables[1],
                group=group[1], type="independent")
  }
  ## return results
  class(out) <- "tTestSPSS"
  out
}

# compute mean, standard deviation and standard error
.stat <- function(x) {
  # initializations
  ok <- is.finite(x)
  x <- x[ok]
  # compute statistics
  n <- length(x)
  mean <- mean(x)
  sd <- sd(x)
  se <- sd / sqrt(n)
  # return data frame
  data.frame(N=n, Mean=mean, "Std. Deviation"=sd, "Std. Error Mean"=se,
             check.names=FALSE)
}


## convert R results to all necessary information for SPSS-like table
#' @export

toSPSS.tTestSPSS <- function(object, statistics = c("test", "statistics"),
                             version = c("modern", "legacy"), digits = 3, ...) {

  ## initializations
  statistics <- match.arg(statistics)

  if (statistics == "statistics") {

    # format table nicely
    formatted <- formatSPSS(object$statistics, digits = digits, ...)
    # define main title
    prefix <- switch(object$type, "one-sample" = "One-Sample",
                     paired = "Paired Samples", independent = "Group")
    main <- paste(prefix, "Statistics")
    # add line breaks to column names for header
    header <- wrapText(names(object$statistics), limit = 10)
    # construct list containing all necessary information
    if (object$type == "one-sample") {
      # define header
      header <- c("", header)
      # construct list
      spss <- list(table = formatted, main = main, header = header,
                   rowNames = TRUE, info = 0)
    } else if (object$type == "paired") {
      # initializations
      version <- match.arg(version)
      legacy <- version == "legacy"
      # define header and label
      if (legacy) {
        header <- c("", header)
        label <- NULL
      } else {
        header <- c("", "", header)
        label <- "Pair 1"
      }
      # construct list
      spss <- list(table = formatted, main = main, header = header,
                   label = label, rowNames = TRUE, info = 0,
                   version = version)
    } else if (object$type == "independent") {
      # define header and label
      header <- c("", object$group, header)
      label <- object$variables
      # construct list
      spss <- list(table = formatted, main = main, header = header,
                   label = label, rowNames = TRUE, info = 0)
    } else stop("type of test not supported")

  } else if (statistics == "test") {

    # initializations
    version <- match.arg(version)
    legacy <- version == "legacy"
    # define main title
    prefix <- switch(object$type, "one-sample" = "One-Sample",
                     paired = "Paired Samples",
                     independent = "Independent Samples")
    main <- paste(prefix, "Test")

    if (object$type == "independent") {

      ## initializations
      wrap <- 10
      ## extract results for test on variance
      levene <- data.frame("F" = object$levene[, "F value"],
                           "Sig." = object$levene[, "Pr(>F)"],
                           row.names = NULL, check.names = FALSE)
      ## extract p-values
      p <- c(object$pooled$p.value, object$satterthwaite$p.value)
      if (legacy) {
        # part of data frame
        p <- data.frame("Sig. (2-tailed)" = p, row.names = NULL,
                        check.names = FALSE)
        # part of header
        pHeader <- as.list("Sig. (2-\ntailed)")

      } else {
        # part of data frame
        p <- data.frame("One-Sided p" = p/2, "Two-Sided p" = p,
                        row.names = NULL,  check.names = FALSE)
      }
      ## extract results for test with equal variances assumed
      est <- object$pooled$estimate[1] - object$pooled$estimate[2]
      df <- as.integer(object$pooled$parameter)
      ci <- object$pooled$conf.int
      gamma <- attr(ci, "conf.level")
      alpha <- 1 - gamma
      se <- diff(ci) / (2 * qt(1-alpha/2, df = df))
      ## construct data frame for test with equal variances assumed
      pooled <- data.frame("t" = object$pooled$statistic,
                           "df" = df, p[1, , drop = FALSE],
                           "Mean Difference" = est,
                           "Std. Error Difference" = se,
                           "Lower" = ci[1], "Upper" = ci[2],
                           row.names = NULL, check.names = FALSE)
      ## extract results for test with equal variances not assumed
      est <- object$satterthwaite$estimate[1] - object$satterthwaite$estimate[2]
      df <- object$satterthwaite$parameter
      ci <- object$satterthwaite$conf.int
      gamma <- attr(ci, "conf.level")
      alpha <- 1 - gamma
      se <- diff(ci) / (2 * qt(1-alpha/2, df = df))
      ## construct data frame for test with equal variances not assumed
      satterthwaite <- data.frame("t" = object$satterthwaite$statistic,
                                  "df" = df, p[2, , drop = FALSE],
                                  "Mean Difference" = est,
                                  "Std. Error Difference" = se,
                                  "Lower" = ci[1], "Upper" = ci[2],
                                  row.names = NULL, check.names = FALSE)
      ## put test results in SPSS format
      test <- cbind(levene,
                    rbind(pooled = pooled, satterthwaite = satterthwaite))
      ## format table nicely
      args <- list(test, digits = digits, ...)
      if (!legacy && is.null(args$pValue)) {
        args$pValue <- grepl("Sig", names(test), fixed = TRUE) |
          grepl("-Sided", names(test), fixed = TRUE)
      }
      if (is.null(args$checkInt)) args$checkInt <- names(test) == "df"
      formatted <- do.call(formatSPSS, args)
      ## construct header
      cn <- c("", "", names(test))
      # construct top-level header
      f <- which(cn == "F")
      sig <- which(cn == "Sig.")
      top <- data.frame(first = c(seq_len(f-1), f, sig+1),
                        last = c(seq_len(f-1), sig, length(cn)),
                        text = c(rep.int("", f-1),
                                 "Levene's Test\nfor Equality\nof Variances",
                                 "t-test for Equality of Means"))
      # merged header for p-values
      if (legacy) {
        onesided <- twosided <- grep("2-tailed", cn, fixed = TRUE)
        pText <- ""
      } else {
        onesided <- grep("One-Sided", cn, fixed = TRUE)
        twosided <- grep("Two-Sided", cn, fixed = TRUE)
        pText <- c("Significance")
      }
      # merged header for confidence interval
      lower <- which(cn == "Lower")
      upper <- which(cn == "Upper")
      ciText <- paste0(format(100*gamma, digits = digits),
                       "\\% Confidence\nInterval of the\nDifference")
      # construct mid-level header
      middle <- data.frame(first = c(seq_len(onesided-1), onesided,
                                     seq(from = twosided+1, to = lower-1),
                                     lower),
                           last = c(seq_len(onesided-1), twosided,
                                    seq(from = twosided+1, to = lower-1),
                                    upper),
                           text = c(rep.int("", onesided-1), pText,
                                    rep.int("", lower-twosided-1),
                                    ciText))
      # construct bottom-level header
      bottom <- c(wrapText(cn, limit = wrap))
      bottom <- gsub("-", "-\n", bottom, fixed = TRUE)
      header <- list(top, middle, bottom)
      ## define nice labels for the rows
      rowLabels <- c(pooled = "assumed", satterthwaite = "not assumed")
      rowLabels <- paste("Equal variances", rowLabels[row.names(test)])
      rowLabels <- gsub(" ", "\n", rowLabels, fixed = TRUE)
      ## construct list containing all necessary information
      spss <- list(table = formatted, main = main, label = object$variables,
                   header = header, rowNames = rowLabels, info = 0,
                   # width = width,
                   version = version)

    } else {

      # initializations
      wrap <- if (object$type == "one-sample") 10 else 8
      # extract results
      est <- object$test$estimate
      t <- object$test$statistic
      df <- as.integer(object$test$parameter)
      # extract significance
      if (legacy) {
        # part of data frame
        p <- data.frame("Sig. (2-tailed)" = object$test$p.value,
                        row.names = NULL, check.names = FALSE)
      } else {
        # part of data frame
        p <- data.frame("One-Sided p" = object$test$p.value/2,
                        "Two-Sided p" = object$test$p.value,
                        row.names = NULL,  check.names = FALSE)
      }
      # put test results in SPSS format
      if (object$type == "one-sample") {
        # extract more results
        mu <- object$test$null.value
        ci <- object$test$conf.int - mu
        gamma <- attr(ci, "conf.level")
        # construct data frame
        test <- data.frame("t" = t, "df" = df, p,
                           "Mean Difference" = est-mu,
                           "Lower" = ci[1], "Upper" = ci[2],
                           row.names = object$variables,
                           check.names = FALSE)
      } else if (object$type == "paired") {
        # extract more results
        ci <- object$test$conf.int
        gamma <- attr(ci, "conf.level")
        alpha <- 1 - gamma
        se <- diff(ci) / (2 * qt(1-alpha/2, df = df))
        sd <- se * sqrt(object$n)
        rn <- paste0(object$variables, collapse = " - ")
        # construct data frame
        test <- data.frame("Mean" = est, "Std. Deviation" = sd,
                           "Std. Error Mean" = se, "Lower" = ci[1],
                           "Upper" = ci[2], "t" = t, "df" = df, p,
                           row.names = rn, check.names = FALSE)
      }
      # format results nicely
      if (legacy) formatted <- formatSPSS(test, digits = digits, ...)
      else {
        args <- list(test, digits = digits, ...)
        if (is.null(args$pValue)) {
          args$pValue <- grepl("-Sided", names(test), fixed = TRUE)
        }
        formatted <- do.call(formatSPSS, args)
      }
      ## construct header
      cn <- c(if (object$type == "paired" && !legacy) "", "", names(test))
      # merged header for p-values
      if (legacy) {
        onesided <- twosided <- grep("2-tailed", cn, fixed = TRUE)
        pText <- ""
      } else {
        onesided <- grep("One-Sided", cn, fixed = TRUE)
        twosided <- grep("Two-Sided", cn, fixed = TRUE)
        pText <- c("Significance")
      }
      # merged header for confidence interval
      lower <- which(cn == "Lower")
      upper <- which(cn == "Upper")
      ciText <- paste0(format(100*gamma, digits = digits),
                       "\\% Confidence\nInterval of the\nDifference")
      # construct top-level and mid-level header
      if (object$type == "one-sample") {
        # top-level
        topText <- paste("Test Value =",
                         format(object$test$null.value, digits = digits))
        top <- data.frame(first = c(1:2), last = c(1, length(cn)),
                          text = c("", topText))
        # mid-level
        middle <- data.frame(first = c(seq_len(onesided-1), onesided,
                                       seq(from = twosided+1, to = lower-1),
                                       lower),
                             last = c(seq_len(onesided-1), twosided,
                                      seq(from = twosided+1, to = lower-1),
                                      upper),
                             text = c(rep.int("", onesided-1), pText,
                                      rep.int("", lower-twosided-1),
                                      ciText))
      } else {
        blank <- if (object$type == "paired" && !legacy) 2 else 1
        # top-level
        top <- data.frame(first = c(seq_len(blank), blank+1,
                                    seq(from = upper+1, to = onesided-1),
                                    onesided),
                          last = c(seq_len(blank), upper,
                                   seq(from = upper+1, to = onesided-1),
                                   twosided),
                          text = c(rep.int("", blank),
                                   "Paired Differences",
                                   rep.int("", onesided-upper-1), pText))
        # mid-level
        middle <- data.frame(first = c(seq_len(lower-1), lower,
                                       seq(from = upper+1, to = length(cn))),
                             last = c(seq_len(lower-1), upper,
                                      seq(from = upper+1, to = length(cn))),
                             text = c(rep.int("", lower-1), ciText,
                                      rep.int("", length(cn)-upper)))
      }
      # construct bottom-level header
      bottom <- c(wrapText(cn, limit = wrap))
      if (legacy) bottom <- gsub("\n(2-", "(2-\n", bottom, fixed = TRUE)
      else if (wrap < 9) bottom <- gsub("-", "-\n", bottom, fixed = TRUE)
      header <- list(top, middle, bottom)
      # construct list containing all necessary information
      if (object$type == "one-sample") {
        spss <- list(table = formatted, main = main, header = header,
                     rowNames = TRUE, info = 0, version = version)
      } else if (object$type == "paired") {
        label <- if (!legacy) "Pair 1"
        spss <- list(table = formatted, main = main, label = label,
                     header = header, rowNames = TRUE, info = 0,
                     version = version)
      }

    }

  } else stop ("type of 'statistics' not supported")  # shouldn't happen

  # add class and return object
  class(spss) <- "SPSSTable"
  spss

}


#' @rdname tTest
#'
#' @param x  an object of class \code{"tTestSPSS"} as returned by function
#' \code{tTest}.
#' @param digits  an integer giving the number of digits after the comma to be
#' printed in the LaTeX tables.
#' @param statistics  a character vector specifying which LaTeX tables should
#' be printed.  Available options are \code{"statistics"} for descriptive
#' statistics and \code{"test"} for test results.  The default is to print both
#' tables.
#' @param \dots currently ignored.
#'
#' @importFrom stats qt
#' @export

print.tTestSPSS <- function(x, statistics = c("statistics", "test"),
                            theme = c("modern", "legacy"), digits = 3, ...) {

  ## initializations
  count <- 0
  statistics <- match.arg(statistics, several.ok = TRUE)
  theme <- match.arg(theme)

  ## print LaTeX table for ranks
  if ("statistics" %in% statistics) {
    cat("\n")
    # put table into SPSS format
    spss <- toSPSS(x, digits = digits, statistics = "statistics",
                   version = theme, ...)
    # print LaTeX table
    toLatex(spss, theme = theme)
    cat("\n")
    count <- count + 1
  }

  ## print LaTeX table for test
  if ("test" %in% statistics) {
    if (count == 0) cat("\n")
    else cat("\\medskip\n")
    # put test results into SPSS format
    spss <- toSPSS(x, digits = digits, statistics = "test",
                   version = theme, ...)
    # print LaTeX table
    toLatex(spss, theme = theme)
    cat("\n")
  }

}

# print.tTestSPSS <- function(x, digits = 3,
#                             statistics = c("statistics", "test"),
#                             ...) {
#
#   ## initializations
#   count <- 0
#   statistics <- match.arg(statistics, several.ok=TRUE)
#
#   ## print LaTeX table for test
#   if ("test" %in% statistics) {
#
#     ## collect output for test
#     if (x$type == "one-sample") {
#       est <- x$test$estimate
#       mu <- x$test$null.value
#       ci <- x$test$conf.int - mu
#       gamma <- attr(ci, "conf.level")
#       test <- data.frame(t=x$test$statistic, df=as.integer(x$test$parameter),
#                          "Sig."=x$test$p.value, "Mean Difference"=est-mu,
#                          Lower=ci[1], Upper=ci[2], check.names=FALSE,
#                          row.names=x$variables)
#       formatted <- formatSPSS(test, digits=digits)
#     } else if (x$type == "paired") {
#       df <- as.integer(x$test$parameter)
#       ci <- x$test$conf.int
#       gamma <- attr(ci, "conf.level")
#       alpha <- 1 - gamma
#       se <- diff(ci) / (2 * qt(1-alpha/2, df=df))
#       sd <- se * sqrt(x$n)
#       rn <- paste0(x$variables, collapse=" - ")
#       test <- data.frame(Mean=x$test$estimate, "Std. Deviation"=sd,
#                          "Std. Error Mean"=se, Lower=ci[1], Upper=ci[2],
#                          t=x$test$statistic, df=df, "Sig."=x$test$p.value,
#                          check.names=FALSE, row.names=rn)
#       formatted <- formatSPSS(test, digits=digits)
#     } else if (x$type == "independent") {
#       levene <- data.frame("F"=x$levene[, "F value"],
#                            "Sig."=x$levene[, "Pr(>F)"],
#                            check.names=FALSE, row.names=NULL)
#       # equal variances assumed
#       est <- x$pooled$estimate[1] - x$pooled$estimate[2]
#       df <- as.integer(x$pooled$parameter)
#       ci <- x$pooled$conf.int
#       gamma <- attr(ci, "conf.level")
#       alpha <- 1 - gamma
#       se <- diff(ci) / (2 * qt(1-alpha/2, df=df))
#       pooled <- data.frame(t=x$pooled$statistic, df=df, "Sig."=x$pooled$p.value,
#                            "Mean Difference"=est, "Std. Error Difference"=se,
#                            Lower=ci[1], Upper=ci[2], check.names=FALSE,
#                            row.names=NULL)
#       # equal variances not assumed
#       est <- x$satterthwaite$estimate[1] - x$satterthwaite$estimate[2]
#       df <- x$satterthwaite$parameter
#       ci <- x$satterthwaite$conf.int
#       gamma <- attr(ci, "conf.level")
#       alpha <- 1 - gamma
#       se <- diff(ci) / (2 * qt(1-alpha/2, df=df))
#       satterthwaite <- data.frame(t=x$satterthwaite$statistic, df=df,
#                                   "Sig."=x$satterthwaite$p.value,
#                                   "Mean Difference"=est,
#                                   "Std. Error Difference"=se,
#                                   Lower=ci[1], Upper=ci[2],
#                                   check.names=FALSE, row.names=NULL)
#       # combine tests
#       formatted <- rbind(formatSPSS(pooled, digits=digits),
#                          formatSPSS(satterthwaite, digits=digits))
#       formatted <- cbind(formatSPSS(levene, digits=digits), formatted)
#     } else stop("type of test not supported")
#
#     ## print LaTeX table
#     if (count == 0) cat("\n")
#     if (x$type == "one-sample") {
#       cat("\\begin{tabular}{|l|r|r|r|r|r|r|}\n")
#       cat("\\noalign{\\smallskip}\n")
#       cat("\\multicolumn{7}{c}{\\textbf{One-Sample Test}} \\\\\n")
#       cat("\\noalign{\\smallskip}\\hline\n")
#       cat(" & \\multicolumn{6}{|c|}{Test Value = ", format(x$test$null.value, digits=digits), "} \\\\\n", sep="")
#       cat("\\cline{2-7}\n")
#       cat(" & & & & & \\multicolumn{2}{|c|}{", format(100*gamma, digits=digits), "\\% Confidence} \\\\\n", sep="")
#       cat(" & & & & & \\multicolumn{2}{|c|}{Interval of the} \\\\\n")
#       cat(" & & & \\multicolumn{1}{|c|}{Sig. (2-} & \\multicolumn{1}{|c|}{Mean} & \\multicolumn{2}{|c|}{Difference} \\\\\n")
#       cat("\\cline{6-7}\n")
#       cat(" & \\multicolumn{1}{|c|}{t} & \\multicolumn{1}{|c|}{df} & \\multicolumn{1}{|c|}{tailed)} & \\multicolumn{1}{|c|}{Difference} & \\multicolumn{1}{|c|}{Lower} & \\multicolumn{1}{|c|}{Upper} \\\\\n")
#       cat("\\hline\n")
#       cat(rownames(formatted), "&", paste0(formatted, collapse=" & "), "\\\\\n")
#     } else if (x$type == "paired") {
#       cat("\\begin{tabular}{|l|r|r|r|r|r|r|r|r|}\n")
#       cat("\\noalign{\\smallskip}\n")
#       cat("\\multicolumn{9}{c}{\\textbf{Paired Samples Test}} \\\\\n")
#       cat("\\noalign{\\smallskip}\\hline\n")
#       cat(" & \\multicolumn{5}{|c|}{Paired Differences} & & & \\\\\n", sep="")
#       cat("\\cline{2-6}\n")
#       cat(" & & & & \\multicolumn{2}{|c|}{", format(100*gamma, digits=digits), "\\% Confidence} & & & \\\\\n", sep="")
#       cat(" & & & \\multicolumn{1}{|c|}{Std.} & \\multicolumn{2}{|c|}{Interval of the} & & & \\\\\n")
#       cat(" & & \\multicolumn{1}{|c|}{Std.} & \\multicolumn{1}{|c|}{Error} & \\multicolumn{2}{|c|}{Difference} & & & \\multicolumn{1}{|c|}{Sig. (2-} \\\\\n")
#       cat("\\cline{5-6}\n")
#       cat(" & \\multicolumn{1}{|c|}{Mean} & \\multicolumn{1}{|c|}{Deviation} & \\multicolumn{1}{|c|}{Mean} & \\multicolumn{1}{|c|}{Lower} & \\multicolumn{1}{|c|}{Upper} & \\multicolumn{1}{|c|}{t} & \\multicolumn{1}{|c|}{df} & \\multicolumn{1}{|c|}{tailed)} \\\\\n")
#       cat("\\hline\n")
#       cat(rownames(formatted), "&", paste0(formatted, collapse=" & "), "\\\\\n")
#     } else if (x$type == "independent") {
#       cat("\\begin{tabular}{|ll|r|r|r|r|r|r|r|r|r|}\n")
#       cat("\\noalign{\\smallskip}\n")
#       cat("\\multicolumn{11}{c}{\\textbf{Independent Samples Test}} \\\\\n")
#       cat("\\noalign{\\smallskip}\\hline\n")
#       cat(" & & \\multicolumn{2}{|c|}{Levene's Test} & \\multicolumn{7}{|c|}{} \\\\\n", sep="")
#       cat(" & & \\multicolumn{2}{|c|}{for Equality} & \\multicolumn{7}{|c|}{} \\\\\n", sep="")
#       cat(" & & \\multicolumn{2}{|c|}{of Variances} & \\multicolumn{7}{|c|}{t-test for Equality of Means} \\\\\n", sep="")
#       cat("\\cline{3-11}\n")
#       cat(" & & & & & & & & & \\multicolumn{2}{|c|}{", format(100*gamma, digits=digits), "\\% Confidence} \\\\\n", sep="")
#       cat(" & & & & & & & & & \\multicolumn{2}{|c|}{Interval of the} \\\\\n")
#       cat(" & & & & & & \\multicolumn{1}{|c|}{Sig. (2-} & \\multicolumn{1}{|c|}{Mean} & \\multicolumn{1}{|c|}{Std. Error} & \\multicolumn{2}{|c|}{Difference} \\\\\n")
#       cat("\\cline{10-11}\n")
#       cat(" & & \\multicolumn{1}{|c|}{F} & \\multicolumn{1}{|c|}{Sig.} & \\multicolumn{1}{|c|}{t} & \\multicolumn{1}{|c|}{df} & \\multicolumn{1}{|c|}{tailed)} & \\multicolumn{1}{|c|}{Difference} & \\multicolumn{1}{|c|}{Difference} & \\multicolumn{1}{|c|}{Lower} & \\multicolumn{1}{|c|}{Upper} \\\\\n")
#       cat("\\hline\n")
#       cat(x$variables, "& Equal &", paste0(formatted[1,], collapse=" & "), "\\\\\n")
#       cat(" & variances & & & & & & & & & \\\\\n")
#       cat(" & assumed & & & & & & & & & \\\\\n")
#       cat("\\hline\n")
#       cat(" & Equal &", paste0(formatted[2,], collapse=" & "), "\\\\\n")
#       cat(" & variances & & & & & & & & & \\\\\n")
#       cat(" & not & & & & & & & & & \\\\\n")
#       cat(" & assumed & & & & & & & & & \\\\\n")
#     } else stop("type of test not supported")
#     # finalize LaTeX table
#     cat("\\hline\\noalign{\\smallskip}\n")
#     cat("\\end{tabular}\n")
#     cat("\n")
#   }
# }
