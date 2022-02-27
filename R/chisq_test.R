# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' \eqn{\chi^{2}}{Chi-squared} Tests
#'
#' Perform a \eqn{\chi^{2}}{chi-squared} goodness-of-fit test or a
#' \eqn{\chi^{2}}{chi-squared} test on independence on variables of
#' a data set.  The output is printed as a LaTeX table that mimics
#' the look of SPSS output.
#'
#' The \code{print} method first calls the \code{to_SPSS} method followed
#' by \code{\link{to_latex}}.  Further customization can be done by calling
#' those two functions separately, and modifying the object returned by
#' \code{to_SPSS}.
#'
#' @param data  a data frame containing the variables.
#' @param variables  a character vector specifying the categorical variable(s)
#' of interest.  If only one variable is specified, a goodness-of-fit test is
#' performed.  If two variables are specified, a test on independence is
#' performed (with the first variable used for the rows and the second variable
#' for the columns of the crosstabulation).
#' @param p  a vector of probabilities for the categories in the
#' goodness-of-fit test.
#' @param object,x  an object of class \code{"chisq_test_SPSS"} as returned by
#' function \code{chisq_test}.
#' @param statistics  a character string or vector specifying which SPSS tables
#' to produce.  Available options are \code{"frequencies"} for a table of the
#' observed and expected frequencies, and \code{"test"} for test results.  For
#' the \code{to_SPSS} method, only one option is allowed (the default is the
#' table of test results), but the \code{print} method allows several options
#' (the default is to print all tables).
#' @param version  a character string specifying whether the table should
#' mimic the content and look of recent SPSS versions (\code{"modern"}) or
#' older versions (<24; \code{"legacy"}).  The main difference in terms of
#' content is that small p-values are displayed differently.
#' @param digits  an integer vector giving the number of digits after the comma
#' to be printed in the SPSS tables.  The first element corresponds to the
#' number of digits for the expected frequencies, and the second element
#' corresponds to the number of digits in the table for the test.
#' @param \dots additional arguments to be passed down to
#' \code{\link{format_SPSS}}.
#'
#' @return
#' An object of class \code{"chisq_test_SPSS"} with the following components:
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
#' The \code{to_SPSS} method returns an object of class \code{"SPSS_table"}
#' which contains all relevant information in the required format to produce
#' the LaTeX table.  See \code{\link{to_latex}} for possible components and
#' how to further customize the LaTeX table based on the returned object.
#'
#' The \code{print} method produces a LaTeX table that mimics the look of SPSS
#' output.
#'
#' @note
#' The test on independence also reports the results of a likelihood
#' ratio test.
#'
#' LaTeX tables that mimic recent versions of SPSS (\code{version = "modern"})
#' may require several LaTeX compilations to be displayed correctly.
#'
#' @author Andreas Alfons
#'
#' @examples
#' # load data
#' data("Eredivisie")
#'
#' # test whether the traditional Dutch 4-3-3 (total football)
#' # is still reflected in player composition
#' chisq_test(Eredivisie, "Position", p = c(1, 4, 3, 3)/11)
#'
#' # test whether playing position and dummy variable for
#' # foreign players are independent
#' chisq_test(Eredivisie, c("Position", "Foreign"))
#'
#' @keywords htest
#'
#' @importFrom stats pchisq
#' @export

chisq_test <- function(data, variables, p = NULL) {
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
    observed <- table(x, dnn = variables[1])
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
    p <- pchisq(stat, df = k-1, lower.tail = FALSE)
    chisq <- list(statistic = stat, parameter = k-1, p.value = p)
    # construct object
    out <- list(chisq = chisq, observed = observed, expected = expected, n = n,
                k = k, variables = variables[1], type = "goodness-of-fit")
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
    observed <- table(row, col, dnn = variables[1:2])
    n <- sum(observed)
    if (n == 0) stop("at least one cell must be nonzero")
    expected <- outer(rowSums(observed), colSums(observed), "*") / n
    df <- (r-1) * (c-1)
    # perform chi-square test
    stat <- sum((observed - expected)^2 / expected)
    p <- pchisq(stat, df = df, lower.tail = FALSE)
    chisq <- list(statistic = stat, parameter = df, p.value = p)
    # perform likelihood ratio test
    keep <- observed != 0
    stat <- 2 * sum(observed[keep] * log(observed[keep] / expected[keep]))
    p <- pchisq(stat, df = df, lower.tail = FALSE)
    lr <- list(statistic = stat, parameter = df, p.value = p)
    # construct object
    out <- list(chisq = chisq, lr = lr, observed = observed,
                expected = expected, n = n, r = r, c = c,
                variables = variables[1:2], type = "independence")
  }
  ## return results
  class(out) <- "chisq_test_SPSS"
  out
}


#' @rdname chisq_test
#' @export

to_SPSS.chisq_test_SPSS <- function(object, statistics = c("test", "frequencies"),
                                    version = r2spss_options$get("version"),
                                    digits = c(1, 3), ...) {
  ## initializations
  statistics <- match.arg(statistics)
  digits <- rep_len(digits, 2)

  ## put requested results into SPSS format
  if (statistics == "frequencies") {

    # extract frequencies
    observed <- object$observed
    expected <- object$expected
    # prepare necessary information
    if (object$type == "goodness-of-fit") {
      # create table of frequencies in SPSS format
      observed <- c(observed, Total = object$n)
      expected <- c(expected, Total = NA_real_)
      frequencies <- data.frame("Observed N" = observed,
                                "Expected N" = expected,
                                Residual = observed - expected,
                                check.names = FALSE)
      # format table nicely
      formatted <- format_SPSS(frequencies, digits = digits[1], ...)
      # define positions for minor grid lines
      minor <- seq_len(nrow(formatted) - 1)
      # construct list containing all necessary information
      spss <- list(table = formatted, main = object$variables, header = TRUE,
                   row_names = TRUE, info = 0, minor = minor)
    } else if (object$type == "independence") {
      # add totals
      observed <- cbind(observed, Total = rowSums(observed))
      observed <- rbind(observed, Total = colSums(observed))
      expected <- cbind(expected, Total = rowSums(expected))
      expected <- rbind(expected, Total = colSums(expected))
      # number format for expected counts
      fmt <- paste0("%.", digits[1], "f")
      # create cross table in SPSS format
      row_names <- rownames(observed)
      count_labels <- c("Count", "Expected Count")
      crosstab <- lapply(row_names, function(i) {
        if (i == row_names[1]) label <- c(object$variables[1], "")
        else if (i == "Total") label <- c(i, "")
        else label <- c("", "")
        row <- if (i == "Total") c("", "") else c(i, "")
        counts <- rbind(as.character(observed[i, ]),
                        sprintf(fmt, expected[i, ]))
        data.frame(Label = label, Row = row, Type = count_labels, counts,
                   check.names = FALSE, stringsAsFactors = FALSE)
      })
      crosstab <- do.call(rbind, crosstab)
      # construct main title
      main <- paste(object$variable[1], "*", object$variable[2],
                    "Crosstabulation")
      # construct list defining header layout
      cn <- c("", "", "", colnames(object$observed), "Total")
      nc <- length(cn)
      top <- data.frame(first = c(1:3, 4, nc), last = c(1:3, nc-1, nc),
                        text = c("", "", "", object$variables[2], ""))
      header <- list(top, cn)
      # define major grid lines
      major <- rbind(data.frame(row = 2 * seq_len(object$r-1),
                                first = 2, last = ncol(crosstab)),
                     data.frame(row = 2 * object$r, first = 1,
                                last = ncol(crosstab)))
      # define minor grid lines
      minor <- data.frame(row = seq(from = 1, by = 2, length.out = object$r+1),
                          first = 3, last = ncol(crosstab))
      # construct list containing all necessary information
      spss <- list(table = crosstab, main = main, header = header,
                   row_names = FALSE, info = 3, major = major,
                   minor = minor)
    } else stop("type of test not supported")

  } else if (statistics == "test") {

    # initializations
    version <- match.arg(version, choices = get_version_values())
    legacy <- version == "legacy"
    # check too small expected counts
    n_too_small <- sum(object$expected < 5)
    p_too_small <- n_too_small / length(object$expected)
    smallest <- min(object$expected)
    # number format for percentage of cells in footnote
    fmt <- paste0("%.", digits[1], "f")
    # prepare necessary information
    if (object$type == "goodness-of-fit") {
      # extract results
      rn <- c("Chi-Square", "df", "Asymp. Sig.")
      values <- unlist(object$chisq)
      # format results nicely
      args <- list(values, digits = digits[2], ...)
      if (is.null(args$p_value)) args$p_value <- c(FALSE, FALSE, !legacy)
      if (is.null(args$check_int)) args$check_int <- c(FALSE, TRUE, FALSE)
      formatted <- do.call(format_SPSS, args)
      # put test results into SPSS format
      chisq <- data.frame(formatted, row.names = rn)
      names(chisq) <- object$variables
      # define footnote
      footnote <- paste0(n_too_small, " cells (", sprintf(fmt, p_too_small),
                         "\\%) have expected frequencies less than 5. ",
                         "The minimum expected cell frequency is ",
                         sprintf(fmt, smallest), ".")
      footnotes <- data.frame(marker = "a", row = 1, column = 1,
                              text = wrap_text(footnote, limit = 30))
      # define positions for minor grid lines
      minor <- seq_len(nrow(chisq) - 1)
      # construct list containing all necessary information
      spss <- list(table = chisq, main = "Test Statistics", header = TRUE,
                   row_names = TRUE, info = 0, footnotes = footnotes,
                   minor = minor, version = version)
    } else if (object$type == "independence") {
      # put test results into SPSS format
      rn <- c("Pearson Chi-Square", "Likelihood Ratio", "N of Valid Cases")
      chisq <- data.frame("Value" = c(object$chisq$statistic,
                                      object$lr$statistic,
                                      object$n),
                          "df" = as.integer(c(object$chisq$parameter,
                                              object$lr$parameter,
                                              NA_integer_)),
                          "Asymp. Sig. (2-sided)" = c(object$chisq$p.value,
                                                      object$lr$p.value,
                                                      NA_real_),
                          check.names = FALSE, row.names = rn)
      # format table nicely
      args <- list(chisq, digits = digits[2], ...)
      if (is.null(args$p_value)) args$p_value <- c(FALSE, FALSE, !legacy)
      if (is.null(args$check_int)) args$check_int <- c(TRUE, FALSE, FALSE)
      formatted <- do.call(format_SPSS, args)
      # define header with line breaks
      header <- c("", wrap_text(names(chisq), limit = 15))
      # define footnote
      footnote <- paste0(n_too_small, " cells (", sprintf(fmt, p_too_small),
                         "\\%) have expected count less than 5. ",
                         "The minimum expected count is ",
                         sprintf(fmt, smallest), ".")
      footnotes <- data.frame(marker = "a", row = 1, column = 1,
                              text = wrap_text(footnote, limit = 50))
      # define positions for minor grid lines
      minor <- seq_len(nrow(formatted) - 1)
      # construct list containing all necessary information
      spss <- list(table = formatted, main = "Chi-Square Tests",
                   header = header, row_names = TRUE, info = 0,
                   footnotes = footnotes, minor = minor,
                   version = version)
    } else stop("type of test not supported")

  } else stop ("type of 'statistics' not supported")  # shouldn't happen

  # add class and return object
  class(spss) <- "SPSS_table"
  spss

}


#' @rdname chisq_test
#' @export

print.chisq_test_SPSS <- function(x, statistics = c("frequencies", "test"),
                                  version = r2spss_options$get("version"),
                                  digits = c(1, 3), ...) {

  ## initializations
  count <- 0
  statistics <- match.arg(statistics, several.ok = TRUE)
  version <- match.arg(version, choices = get_version_values())
  digits <- rep_len(digits, 2)

  ## print LaTeX table for frequencies
  if ("frequencies" %in% statistics) {
    cat("\n")
    # put frequencies into SPSS format
    spss <- to_SPSS(x, digits = digits, statistics = "frequencies",
                    version = version, ...)
    # print LaTeX table
    to_latex(spss, version = version)
    cat("\n")
    count <- count + 1
  }

  ## print LaTeX table for chi-square test
  if ("test" %in% statistics) {
    if (count == 0) cat("\n")
    else cat("\\medskip\n")
    # put test results into SPSS format
    spss <- to_SPSS(x, digits = digits, statistics = "test",
                    version = version, ...)
    # print LaTeX table
    to_latex(spss, version = version)
    cat("\n")
  }

}


#' @rdname chisq_test
#' @export

chisqTest <- function(data, variables, p = NULL) {
  .Deprecated("chisq_test")
  chisq_test(data, variables = variables, p = p)
}
