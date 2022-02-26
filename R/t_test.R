# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' t Tests
#'
#' Perform a one-sample t test, a paired-sample t test or an
#' independent-samples t test on variables of a data set.  The output
#' is printed as a LaTeX table that mimics the look of SPSS output.
#'
#' The \code{print} method first calls the \code{toSPSS} method followed by
#' \code{\link[=toLatex.SPSSTable]{toLatex}}.  Further customization can be
#' done by calling those two functions separately, and modifying the object
#' returned by \code{toSPSS}.
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
#' @param object,x  an object of class \code{"t_test_SPSS"} as returned by
#' function \code{t_test}.
#' @param statistics  a character string or vector specifying which SPSS tables
#' to produce.  Available options are \code{"statistics"} for descriptive
#' statistics and \code{"test"} for test results.  For the \code{toSPSS}
#' method, only one option is allowed (the default is the table of test
#' results), but the \code{print} method allows several options (the default
#' is to print all tables).
#' @param version  a character string specifying whether the table should
#' mimic the content and look of recent SPSS versions (\code{"modern"}) or
#' older versions (<24; \code{"legacy"}).  The main differences in terms of
#' content are that recent SPSS versions show a one-sided p-value in addition
#' to the two-sided p-value, and that small p-values are displayed differently.
#' For the paired-sample test, recent versions of SPSS also display a label
#' \emph{Pair 1} for the selected pair of variables.
#' @param digits  an integer giving the number of digits after the comma to be
#' printed in the SPSS tables.
#' @param \dots additional arguments to be passed down to
#' \code{\link{format_SPSS}}.
#'
#' @return  An object of class \code{"t_test_SPSS"} with the following
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
#' The \code{toSPSS} method returns an object of class \code{"SPSSTable"}
#' which contains all relevant information in the required format to produce
#' the LaTeX table.  See \code{\link[=toLatex.SPSSTable]{toLatex}} for possible
#' components and how to further customize the LaTeX table based on the
#' returned object.
#'
#' The \code{print} method produces a LaTeX table that mimics the look of SPSS
#' output.
#'
#' @note
#' LaTeX tables that mimic recent versions of SPSS (\code{version = "modern"})
#' may require several LaTeX compilations to be displayed correctly.
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
#' t_test(Exams, "Resit", mu = 5.5)
#'
#' # test whether average grades differ between the
#' # regular exam and the resit
#' t_test(Exams, c("Resit", "Regular"))
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
#' t_test(Eredivisie, "logMarketValue", group = "Foreign")
#'
#' @keywords htest
#'
#' @importFrom stats sd t.test
#' @importFrom car leveneTest
#' @export

t_test <- function(data, variables, group = NULL, mu = 0, conf.level = 0.95) {
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
      test <- t.test(x, mu = mu, conf.level = conf.level)
      # construct object
      out <- list(statistics = stat, test = test, variables = variables[1],
                  type = "one-sample")
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
      test <- t.test(x, y, mu = 0, paired = TRUE, conf.level = conf.level)
      # construct object
      out <- list(statistics = stat, test = test, variables = variables[1:2],
                  n = length(x), type = "paired")
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
    levene <- leveneTest(variable, by, center = "mean")
    x <- variable[by == levels(by)[1]]
    y <- variable[by == levels(by)[2]]
    pooled <- t.test(x, y, mu = 0, paired = FALSE, var.equal = TRUE,
                     conf.level = conf.level)
    satterthwaite <- t.test(x, y, mu = 0, paired = FALSE, var.equal = FALSE,
                            conf.level = conf.level)
    # compute statistics
    stat <- rbind(.stat(x), .stat(y))
    row.names(stat) <- levels(by)
    # construct object
    out <- list(statistics = stat, levene = levene, pooled = pooled,
                satterthwaite = satterthwaite, variables = variables[1],
                group = group[1], type = "independent")
  }
  ## return results
  class(out) <- "t_test_SPSS"
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
  data.frame(N = n, Mean = mean, "Std. Deviation" = sd,
             "Std. Error Mean" = se, check.names = FALSE)
}


#' @rdname t_test
#' @importFrom stats qt
#' @export

toSPSS.t_test_SPSS <- function(object, statistics = c("test", "statistics"),
                               version = r2spss_options$get("version"),
                               digits = 3, ...) {

  ## initializations
  statistics <- match.arg(statistics)

  if (statistics == "statistics") {

    # format table nicely
    formatted <- format_SPSS(object$statistics, digits = digits, ...)
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
      version <- match.arg(version, choices = get_version_values())
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
    version <- match.arg(version, choices = get_version_values())
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
      if (!legacy && is.null(args$p_value)) {
        args$p_value <- grepl("Sig", names(test), fixed = TRUE) |
          grepl("-Sided", names(test), fixed = TRUE)
      }
      if (is.null(args$check_int)) args$check_int <- names(test) == "df"
      formatted <- do.call(format_SPSS, args)
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
      # define header
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

      ## initializations
      wrap <- if (object$type == "one-sample") 10 else 8
      ## extract results
      est <- object$test$estimate
      t <- object$test$statistic
      df <- as.integer(object$test$parameter)
      ## extract significance
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
      ## put test results in SPSS format
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
      ## format results nicely
      if (legacy) formatted <- format_SPSS(test, digits = digits, ...)
      else {
        args <- list(test, digits = digits, ...)
        if (is.null(args$p_value)) {
          args$p_value <- grepl("-Sided", names(test), fixed = TRUE)
        }
        formatted <- do.call(format_SPSS, args)
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
      # define header
      header <- list(top, middle, bottom)
      ## construct list containing all necessary information
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


#' @rdname t_test
#' @export

print.t_test_SPSS <- function(x, statistics = c("statistics", "test"),
                              version = r2spss_options$get("version"),
                              digits = 3, ...) {

  ## initializations
  count <- 0
  statistics <- match.arg(statistics, several.ok = TRUE)
  version <- match.arg(version, choices = get_version_values())

  ## print LaTeX table for ranks
  if ("statistics" %in% statistics) {
    cat("\n")
    # put table into SPSS format
    spss <- toSPSS(x, digits = digits, statistics = "statistics",
                   version = version, ...)
    # print LaTeX table
    toLatex(spss, version = version)
    cat("\n")
    count <- count + 1
  }

  ## print LaTeX table for test
  if ("test" %in% statistics) {
    if (count == 0) cat("\n")
    else cat("\\medskip\n")
    # put test results into SPSS format
    spss <- toSPSS(x, digits = digits, statistics = "test",
                   version = version, ...)
    # print LaTeX table
    toLatex(spss, version = version)
    cat("\n")
  }

}


#' @rdname t_test
#' @export

tTest <- function(data, variables, group = NULL, mu = 0, conf.level = 0.95) {
  .Deprecated("t_test")
  t_test(data, variables = variables, group = group,
         mu = mu, conf.level = conf.level)
}
