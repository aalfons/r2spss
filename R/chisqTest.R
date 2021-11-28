# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' \eqn{\chi^{2}}{Chi-squared} Tests
#'
#' Perform a \eqn{\chi^{2}}{chi-squared} goodness-of-fit test or a
#' \eqn{\chi^{2}}{chi-squared} test on independence on variables of
#' a data set.  The output is printed as a LaTeX table that mimics
#' the look of SPSS output (version <24).
#'
#' @param data  a data frame containing the variables.
#' @param variables  a character vector specifying the categorical variable(s)
#' of interest.  If only one variable is specified, a goodness-of-fit test is
#' performed.  If two variables are specified, a test on independence is
#' performed (with the first variable used for the rows and the second variable
#' for the columns of the crosstabulation).
#' @param p  a vector of probabilities for the categories in the
#' goodness-of-fit test.
#'
#' @return
#' An object of class \code{"chisqTestSPSS"} with the following components:
#' \describe{
#'   \item{\code{chisq}}{a list containing the results of the
#'   \eqn{\chi^{2}}{chi-squared} test.}
#'   \item{\code{lr}}{a list containing the results of a likelihood ratio
#'   test (only test on independence).}
#'   \item{\code{observed}}{a table containing the observed frequencies.}
#'   \item{\code{expected}}{a vector or matrix containing the expected
#'   frequencies.}
#'   \item{\code{n}}{an integer giving the number of observations.}
#'   \item{\code{k}}{an integer giving the number of groups (only
#'   goodness-of-fit test).}
#'   \item{\code{r}}{an integer giving the number of groups in the first
#'   variable corresponding to the rows (only test on independence).}
#'   \item{\code{c}}{an integer giving the number of groups in the second
#'   variable corresponding to the columns (only test on independence).}
#'   \item{\code{variables}}{a character vector containing the name(s) of the
#'   categorical variable(s) of interest.}
#'   \item{\code{type}}{a character string giving the type of
#'   \eqn{\chi^{2}}{chi-squared} test performed (\code{"goodness-of-fit"}
#'   or \code{"independence"}).}
#' }
#'
#' The \code{print} method produces a LaTeX table that mimics the look of SPSS
#' output (version <24).
#'
#' @note The test on independence also reports the results of a likelihood
#' ratio test.
#'
#' @author Andreas Alfons
#'
#' @examples
#' # load data
#' data("Eredivisie")
#'
#' # test whether playing position and dummy variable for
#' # foreign players are independent
#' chisqTest(Eredivisie, c("Position", "Foreign"))
#'
#' # test whether the traditional Dutch 4-3-3 (total football)
#' # is still reflected in player composition
#' chisqTest(Eredivisie, "Position", p = c(1, 4, 3, 3)/11)
#'
#' @keywords htest
#'
#' @importFrom stats pchisq
#' @export

chisqTest <- function(data, variables, p = NULL) {
  ## initializations
  data <- as.data.frame(data)
  variables <- as.character(variables)
  if (length(variables) == 0) stop("a variable to test must be specified")
  ## select type of test
  if (length(variables) == 1) {
    ## chi-square goodness-of-fit test
    # check factor
    x <- as.factor(data[, variables[1]])
    k <- nlevels(x)
    if (k < 2) {
      stop("chi-square goodness-of-fit test requires at least two groups")
    }
    ok <- !is.na(x)
    x <- x[ok]
    # compute observed frequencies
    observed <- table(x, dnn=variables[1])
    n <- sum(observed)
    if (n == 0) stop("at least one cell must be nonzero")
    # compute expected frequencies
    if (is.null(p)) p <- rep.int(1/k, k)
    else {
      p <- rep(p, length.out=k)
      p <- p / sum(p)
    }
    expected <- n * p
    names(expected) <- names(observed)
    # perform chi-square test
    stat <- sum((observed - expected)^2 / expected)
    p <- pchisq(stat, df=k-1, lower.tail=FALSE)
    chisq <- list(statistic=stat, parameter=k-1, p.value=p)
    # construct object
    out <- list(chisq=chisq, observed=observed, expected=expected, n=n,
                k=k, variables=variables[1], type="goodness-of-fit")
  } else {
    ## chi-square test of independence
    # check factors
    row <- as.factor(data[, variables[1]])
    col <- as.factor(data[, variables[2]])
    r <- nlevels(row)
    c <- nlevels(col)
    if (r < 2 || c < 2) {
      stop("chi-square test of independence requires",
           "at least two groups in each factor")
    }
    ok <- !is.na(row) & !is.na(col)
    row <- row[ok]
    col <- col[ok]
    # compute observed and expected frequencies
    observed <- table(row, col, dnn=variables[1:2])
    n <- sum(observed)
    if (n == 0) stop("at least one cell must be nonzero")
    expected <- outer(rowSums(observed), colSums(observed), "*") / n
    df <- (r-1) * (c-1)
    # perform chi-square test
    stat <- sum((observed - expected)^2 / expected)
    p <- pchisq(stat, df=df, lower.tail=FALSE)
    chisq <- list(statistic=stat, parameter=df, p.value=p)
    # perform likelihood ratio test
    keep <- observed != 0
    stat <- 2 * sum(observed[keep] * log(observed[keep]/expected[keep]))
    p <- pchisq(stat, df=df, lower.tail=FALSE)
    lr <- list(statistic=stat, parameter=df, p.value=p)
    # construct object
    out <- list(chisq=chisq, lr=lr, observed=observed, expected=expected, n=n,
                r=r, c=c, variables=variables[1:2], type="independence")
  }
  ## return results
  class(out) <- "chisqTestSPSS"
  out
}


#' @rdname chisqTest
#'
#' @param x  an object of class \code{"chisqTestSPSS"} as returned by function
#' \code{chisqTest}.
#' @param digits  an integer vector giving the number of digits after the comma
#' to be printed in the LaTeX tables.  The first element corresponds to the
#' number of digits in the table of frequencies, and the second element
#' corresponds to the number of digits in the table for the test.
#' @param statistics  a character vector specifying which LaTeX tables should
#' be printed.  Available options are \code{"frequencies"} for a table of the
#' observed and expected frequencies, and \code{"test"} for test results.  The
#' default is to print both tables.
#' @param \dots currently ignored.
#'
#' @export

print.chisqTestSPSS <- function(x, digits = c(1, 3),
                                statistics = c("frequencies", "test"),
                                theme = c("modern", "legacy"), ...) {

  ## initializations
  count <- 0
  statistics <- match.arg(statistics, several.ok=TRUE)
  theme <- match.arg(theme)
  legacy <- theme == "legacy"
  fmt <- paste0("%.", digits[1], "f")  # number format for expected counts

  ## print LaTeX table for frequencies
  if ("frequencies" %in% statistics) {
    # extract frequencies
    observed <- x$observed
    expected <- x$expected
    cat("\n")
    if (x$type == "goodness-of-fit") {
      # create table of frequencies in SPSS format
      observed <- c(observed, Total = x$n)
      expected <- c(expected, Total = NA_real_)
      frequencies <- data.frame("Observed N" = observed,
                                "Expected N" = expected,
                                Residual = observed - expected,
                                check.names = FALSE)
      # print LaTeX table
      latexTableSPSS(frequencies, main = x$variables, rowNames = TRUE,
                     info = 0, theme = theme, digits = digits[1])
    } else if (x$type == "independence") {
      # add totals
      observed <- cbind(observed, Total = rowSums(observed))
      observed <- rbind(observed, Total = colSums(observed))
      expected <- cbind(expected, Total = rowSums(expected))
      expected <- rbind(expected, Total = colSums(expected))
      # create cross table in SPSS format
      rowNames <- rownames(observed)
      countLabels <- c("Count", "Expected Count")
      crosstab <- lapply(rowNames, function(i) {
        if (i == rowNames[1]) label <- c(x$variables[1], "")
        else if (i == "Total") label <- c(i, "")
        else label <- c("", "")
        row <- if (i == "Total") c("", "") else c(i, "")
        counts <- rbind(as.character(observed[i, ]),
                        sprintf(fmt, expected[i, ]))
        data.frame(Label = label, Row = row, Type = countLabels, counts,
                   check.names = FALSE, stringsAsFactors = FALSE)
      })
      crosstab <- do.call(rbind, crosstab)
      # construct main title
      main <- paste(x$variable[1], "*", x$variable[2], "Crosstabulation")
      # construct list defining header layout
      header <- list("", "", "", Foreign = colnames(x$observed), "Total")
      # define positions for minor grid lines
      minor <- data.frame(row = 2 * seq_len(x$r-1), first = 2,
                          last = c(ncol(crosstab)))
      # print LaTeX table
      latexTableSPSS(crosstab, main = main, header = header, rowNames = FALSE,
                     info = 3, major = 2*x$r, minor = minor, theme = theme)
    } else stop("type of test not supported")
    cat("\n")
    count <- count + 1
  }

  ## print LaTeX table for chi-square test
  if ("test" %in% statistics) {
    # check too small expected counts
    nTooSmall <- sum(x$expected < 5)
    pTooSmall <- nTooSmall / length(x$expected)
    smallest <- min(x$expected)
    # print LaTeX table
    if (count == 0) cat("\n")
    if (x$type == "goodness-of-fit") {
      # put test results into SPSS format
      rn <- c("Chi-Square", "df", "Asymp. Sig.")
      chisq <- data.frame(unlist(x$chisq), row.names = rn)
      names(chisq) <- x$variables
      # define footnote
      footnote <- paste0(nTooSmall, " cells (", sprintf(fmt, pTooSmall),
                         "\\%) have expected\nfrequencies less than 5. The\nminimum expected cell\nfrequency is ",
                         formatSPSS(smallest, digits = digits[1]), ".")
      footnotes <- data.frame(marker = "a", row = 1, column = 1,
                              text = footnote)
      # print table
      latexTableSPSS(chisq, main = "Test Statistics", rowNames = TRUE,
                     info = 0, theme = theme, footnotes = footnotes,
                     digits = digits[2], pValue = !legacy, checkInt = TRUE)
    } else if (x$type == "independence") {
      # put test results into SPSS format
      rn <- c("Pearson Chi-Square", "Likelihood Ratio", "N of Valid Cases")
      chisq <- data.frame("Value" = c(x$chisq$statistic, x$lr$statistic, x$n),
                          "df" = as.integer(c(x$chisq$parameter, x$lr$parameter,
                                              NA_integer_)),
                          "Asymp. Sig. (2-sided)" = c(x$chisq$p.value,
                                                      x$lr$p.value,
                                                      NA_real_),
                          check.names = FALSE, row.names = rn)
      # define header with line breaks
      header <- c("", gsub("Sig.", "Sig.\n", names(chisq), fixed = TRUE))
      # define footnote
      footnote <- paste0(nTooSmall, " cells (", sprintf(fmt, pTooSmall),
                         "\\%) have expected count less than 5.\nThe minimum expected count is ",
                         formatSPSS(smallest, digits = digits[1]), ".")
      footnotes <- data.frame(marker = "a", row = 1, column = 1,
                              text = footnote)
      # print table
      latexTableSPSS(chisq, main = "Chi-Square Tests", header = header,
                     rowNames = TRUE, info = 0, theme = theme,
                     footnotes = footnotes, digits = digits[2],
                     checkInt = c(TRUE, FALSE, FALSE))
    } else stop("type of test not supported")
    cat("\n")
  }
}
